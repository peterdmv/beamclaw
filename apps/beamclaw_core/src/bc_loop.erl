%% @doc Agentic loop — gen_statem.
%%
%% States:
%%   idle → compacting (optional) → streaming → awaiting_approval (optional)
%%        → executing_tools → streaming (loop) → finalizing → idle
%%
%% bc_loop is transient. A crash is restarted by bc_session_sup.
%% bc_session (permanent) retains history across restarts.
%%
%% Response routing (route_response/2):
%%   - reply_pid set in bc_channel_message → send {bc_chunk, SId, Chunk} and
%%     {bc_done, SId, Msg} directly to that pid (HTTP/WS handlers)
%%   - reply_pid undefined → call ChannelMod:send_response(SId, Msg) on the
%%     named channel gen_server (Telegram, TUI)
-module(bc_loop).
-behaviour(gen_statem).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1]).
-export([callback_mode/0, init/1, terminate/3, code_change/4, format_status/1]).
-export([idle/3, compacting/3, streaming/3,
         awaiting_approval/3, executing_tools/3, finalizing/3]).

-record(loop_data, {
    session_pid    :: pid(),
    session_id     :: binary(),
    user_id        :: binary(),
    agent_id       :: binary(),
    provider_mod   :: module(),
    provider_state :: term(),
    reply_pid      = undefined :: pid() | undefined,  %% for HTTP/WS routing
    reply_channel  = undefined :: atom() | undefined,  %% per-run channel for routing
    config         :: map(),
    current_run    :: term() | undefined,
    tool_calls     :: [#bc_tool_call{}],
    iteration      :: non_neg_integer()
}).

start_link(Config) ->
    gen_statem:start_link(?MODULE, Config, []).

callback_mode() -> [state_functions, state_enter].

init(Config) ->
    SessionId   = maps:get(session_id,   Config),
    ProviderMod = maps:get(provider_mod, Config, bc_provider_openrouter),
    %% bc_session registers in bc_session_registry synchronously (call, not cast)
    %% during its own init/1. The supervisor starts bc_session before bc_loop,
    %% so this lookup always succeeds.
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),
    %% Fetch agent ID for system prompt assembly.
    AgentId    = bc_session:get_agent_id(SessionPid),
    %% Initialise the provider.
    ProvConfig = get_provider_config(ProviderMod),
    {ok, ProvState} = ProviderMod:init(ProvConfig),
    %% Tell bc_session our PID so it can dispatch runs.
    bc_session:set_loop_pid(SessionPid, self()),
    bc_obs:emit(agent_start, #{session_id => SessionId}),
    Data = #loop_data{
        session_pid    = SessionPid,
        session_id     = SessionId,
        user_id        = maps:get(user_id, Config, <<"anonymous">>),
        agent_id       = AgentId,
        provider_mod   = ProviderMod,
        provider_state = ProvState,
        reply_pid      = undefined,
        reply_channel  = undefined,
        config         = Config,
        current_run    = undefined,
        tool_calls     = [],
        iteration      = 0
    },
    {ok, idle, Data}.

%% ---- States ----

idle(cast, {run, Message}, Data) ->
    logger:debug("[loop] run received: session=~s content=~s",
                 [Data#loop_data.session_id, Message#bc_channel_message.content]),
    %% Extract per-run reply_pid and channel for routing.
    ReplyPid     = Message#bc_channel_message.reply_pid,
    ReplyChannel = Message#bc_channel_message.channel,
    %% Append the user message to history before the LLM call.
    %% append_message is a cast; get_history is a call to the same bc_session
    %% process, so FIFO ordering guarantees the cast is processed first.
    UserMsg = #bc_message{
        id      = generate_id(),
        role    = user,
        content = Message#bc_channel_message.content,
        ts      = Message#bc_channel_message.ts
    },
    bc_session:append_message(Data#loop_data.session_pid, UserMsg),
    History   = bc_session:get_history(Data#loop_data.session_pid),
    LoopCfg   = bc_config:get(beamclaw_core, agentic_loop, #{}),
    Threshold = maps:get(compaction_threshold, LoopCfg, 50),
    NextState = case length(History) > Threshold of
        true  -> compacting;
        false -> streaming
    end,
    {next_state, NextState,
     Data#loop_data{current_run = Message, iteration = 0,
                    reply_pid = ReplyPid, reply_channel = ReplyChannel}};
idle(EventType, EventContent, Data) ->
    handle_common(idle, EventType, EventContent, Data).

compacting(enter, _OldState, Data) ->
    gen_statem:cast(self(), do_compact),
    {keep_state, Data};
compacting(cast, do_compact, Data) ->
    bc_compactor:compact(Data#loop_data.session_pid),
    {next_state, streaming, Data};
compacting(EventType, EventContent, Data) ->
    handle_common(compacting, EventType, EventContent, Data).

streaming(enter, _OldState, Data) ->
    gen_statem:cast(self(), do_stream),
    {keep_state, Data};
streaming(cast, do_stream, Data) ->
    History    = bc_session:get_history(Data#loop_data.session_pid),
    SystemMsgs = bc_system_prompt:assemble(Data#loop_data.agent_id),
    FullHistory = SystemMsgs ++ History,
    LoopCfg   = bc_config:get(beamclaw_core, agentic_loop, #{}),
    ChunkSize = maps:get(stream_chunk_size, LoopCfg, 80),
    Tools     = bc_tool_registry:list(),
    ToolDefs  = [Def || {_Name, _Mod, Def} <- Tools],
    T0 = erlang:monotonic_time(millisecond),
    bc_obs:emit(llm_request, #{session_id    => Data#loop_data.session_id,
                                message_count => length(FullHistory)}),
    ProvMod   = Data#loop_data.provider_mod,
    ProvState = Data#loop_data.provider_state,
    Options   = #{chunk_size => ChunkSize, tools => ToolDefs},
    case ProvMod:stream(FullHistory, Options, self(), ProvState) of
        {ok, NewProvState} ->
            receive_stream(Data#loop_data{provider_state = NewProvState}, T0);
        {error, Reason, NewProvState} ->
            bc_obs:emit(llm_response, #{session_id => Data#loop_data.session_id,
                                        success => false, error => Reason}),
            ErrContent = iolist_to_binary(io_lib:format("[Error: ~p]", [Reason])),
            route_response(Data, #bc_message{role    = assistant,
                                             content = ErrContent,
                                             ts      = erlang:system_time(millisecond)}),
            {next_state, finalizing, Data#loop_data{provider_state = NewProvState}}
    end;
streaming(EventType, EventContent, Data) ->
    handle_common(streaming, EventType, EventContent, Data).

awaiting_approval(enter, _OldState, Data) ->
    {keep_state, Data};
awaiting_approval(info, {approval_result, _ToolCallId, approved}, Data) ->
    {next_state, executing_tools, Data};
awaiting_approval(info, {approval_result, _ToolCallId, denied}, Data) ->
    {next_state, finalizing, Data};
awaiting_approval(EventType, EventContent, Data) ->
    handle_common(awaiting_approval, EventType, EventContent, Data).

executing_tools(enter, _OldState, Data) ->
    gen_statem:cast(self(), do_execute),
    {keep_state, Data};
executing_tools(cast, do_execute, Data) ->
    LoopCfg = bc_config:get(beamclaw_core, agentic_loop, #{}),
    MaxIter = maps:get(max_tool_iterations, LoopCfg, 10),
    case Data#loop_data.iteration >= MaxIter of
        true ->
            logger:warning("[loop] max tool iterations (~p) reached", [MaxIter]),
            {next_state, finalizing, Data};
        false ->
            execute_tool_calls(Data)
    end;
executing_tools(EventType, EventContent, Data) ->
    handle_common(executing_tools, EventType, EventContent, Data).

finalizing(enter, _OldState, Data) ->
    gen_statem:cast(self(), do_finalize),
    {keep_state, Data};
finalizing(cast, do_finalize, Data) ->
    %% Signal reply_pid consumers that the full turn is done (all tool rounds).
    _ = case Data#loop_data.reply_pid of
        undefined -> ok;
        RPid      -> RPid ! {bc_turn_complete, Data#loop_data.session_id}
    end,
    bc_session:turn_complete(Data#loop_data.session_pid, ok),
    bc_obs:emit(turn_complete, #{session_id => Data#loop_data.session_id}),
    {next_state, idle,
     Data#loop_data{current_run = undefined, tool_calls = [],
                    reply_pid = undefined, reply_channel = undefined}};
finalizing(EventType, EventContent, Data) ->
    handle_common(finalizing, EventType, EventContent, Data).

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% Redact provider_state (contains raw API keys) from OTP crash/introspection reports.
format_status(Status) ->
    maps:map(fun
        (data, Data) when is_record(Data, loop_data) ->
            Data#loop_data{provider_state = redacted};
        (_Key, Value) ->
            Value
    end, Status).

%% ---- Internal ----

handle_common(_State, cast, {run, Message}, Data) ->
    %% A new run arrived while we are busy; forward to bc_session for queueing.
    bc_session:dispatch_run(Data#loop_data.session_pid, Message),
    keep_state_and_data;
handle_common(_State, _Type, _Content, _Data) ->
    keep_state_and_data.

make_session_ref(Data) ->
    #bc_session_ref{
        session_id  = Data#loop_data.session_id,
        user_id     = Data#loop_data.user_id,
        session_pid = Data#loop_data.session_pid,
        autonomy    = supervised,  %% TODO: pull from bc_session config
        agent_id    = Data#loop_data.agent_id
    }.

receive_stream(Data, T0) ->
    receive
        {stream_chunk, _Pid, Chunk} ->
            %% Forward to direct reply target (HTTP/WS); channel gen-servers
            %% don't need intermediate chunks.
            _ = case Data#loop_data.reply_pid of
                undefined -> ok;
                RPid -> RPid ! {bc_chunk, Data#loop_data.session_id, Chunk}
            end,
            receive_stream(Data, T0);
        {stream_done, _Pid, FullMsg} ->
            Duration = erlang:monotonic_time(millisecond) - T0,
            bc_obs:emit(llm_response, #{session_id  => Data#loop_data.session_id,
                                        duration_ms => Duration, success => true}),
            ScrubbedMsg = bc_scrubber:scrub_message(FullMsg),
            bc_session:append_message(Data#loop_data.session_pid, ScrubbedMsg),
            route_response(Data, ScrubbedMsg),
            ToolCalls = bc_tool_parser:parse(ScrubbedMsg),
            NewData = Data#loop_data{tool_calls = ToolCalls},
            case ToolCalls of
                [] -> {next_state, finalizing, NewData};
                _  -> maybe_await_approval(NewData)
            end;
        {stream_error, _Pid, Reason} ->
            logger:error("[loop] stream error: ~p", [Reason]),
            ErrContent = iolist_to_binary(io_lib:format("[Error: ~p]", [Reason])),
            route_response(Data, #bc_message{role    = assistant,
                                             content = ErrContent,
                                             ts      = erlang:system_time(millisecond)}),
            {next_state, finalizing, Data}
    after 60000 ->
        logger:error("[loop] stream timeout"),
        route_response(Data, #bc_message{role    = assistant,
                                         content = <<"[Error: LLM response timed out]">>,
                                         ts      = erlang:system_time(millisecond)}),
        {next_state, finalizing, Data}
    end.

%% Route the completed LLM response to the appropriate destination.
route_response(Data, Msg) ->
    SessionId = Data#loop_data.session_id,
    logger:debug("[loop] route_response: session=~s reply_channel=~p reply_pid=~p",
                 [SessionId, Data#loop_data.reply_channel, Data#loop_data.reply_pid]),
    case Data#loop_data.reply_pid of
        undefined ->
            %% Named channel gen-server (Telegram, TUI).
            case channel_mod_for(Data#loop_data.reply_channel) of
                undefined -> ok;
                Mod       -> Mod:send_response(SessionId, Msg)
            end;
        RPid ->
            %% HTTP handler or WebSocket handler waiting directly.
            RPid ! {bc_done, SessionId, Msg}
    end.

channel_mod_for(tui)       -> bc_channel_tui;
channel_mod_for(telegram)  -> bc_channel_telegram;
channel_mod_for(_)         -> undefined.

maybe_await_approval(#loop_data{tool_calls = Calls} = Data) ->
    NeedsApproval = lists:any(fun(TC) ->
        case bc_tool_registry:lookup(TC#bc_tool_call.name) of
            {ok, {Mod, _}} -> Mod:requires_approval();
            _               -> false
        end
    end, Calls),
    case NeedsApproval of
        true  -> {next_state, awaiting_approval, Data};
        false -> {next_state, executing_tools, Data}
    end.

execute_tool_calls(Data) ->
    SessionRef = make_session_ref(Data),
    Results = lists:map(fun(TC) ->
        bc_obs:emit(tool_call_start, #{tool_name  => TC#bc_tool_call.name,
                                       args       => TC#bc_tool_call.args,
                                       session_id => Data#loop_data.session_id}),
        T0 = erlang:monotonic_time(millisecond),
        Result = run_tool(TC, SessionRef),
        Duration = erlang:monotonic_time(millisecond) - T0,
        bc_obs:emit(tool_call_result, #{tool_name  => TC#bc_tool_call.name,
                                        duration_ms => Duration,
                                        success    => element(1, Result) =:= ok,
                                        session_id => Data#loop_data.session_id}),
        #bc_tool_result{
            tool_call_id = TC#bc_tool_call.id,
            name         = TC#bc_tool_call.name,
            content      = element(2, Result),
            is_error     = element(1, Result) =:= error
        }
    end, Data#loop_data.tool_calls),
    Scrubbed = [bc_scrubber:scrub_result(R) || R <- Results],
    ToolMsgs = [result_to_message(R) || R <- Scrubbed],
    lists:foreach(fun(M) ->
        bc_session:append_message(Data#loop_data.session_pid, M)
    end, ToolMsgs),
    NewData = Data#loop_data{tool_calls = [], iteration = Data#loop_data.iteration + 1},
    {next_state, streaming, NewData}.

run_tool(#bc_tool_call{name = Name, args = Args}, SessionRef) ->
    case bc_tool_registry:lookup(Name) of
        {ok, {Mod, _Def}} ->
            Mod:execute(Args, SessionRef, #{});
        {error, not_found} ->
            case bc_mcp_registry:lookup(Name) of
                {ok, {ServerPid, _}} ->
                    bc_mcp_server:call_tool(ServerPid, Name, Args);
                {error, not_found} ->
                    {error, <<"tool not found">>}
            end
    end.

result_to_message(#bc_tool_result{tool_call_id = Id, name = Name,
                                   content = Content, is_error = IsErr}) ->
    #bc_message{
        id           = generate_id(),
        role         = tool,
        content      = Content,
        tool_call_id = Id,
        name         = Name,
        ts           = erlang:system_time(millisecond),
        tool_calls   = [{is_error, IsErr}]
    }.

get_provider_config(ProviderMod) ->
    ProviderKey = case ProviderMod of
        bc_provider_openrouter -> openrouter;
        bc_provider_openai     -> openai;
        _                      -> openrouter
    end,
    Providers = application:get_env(beamclaw_core, providers, []),
    ProvMap = proplists:get_value(ProviderKey, Providers, #{}),
    try bc_config:resolve(ProvMap)
    catch error:{missing_env_var, Var} ->
        logger:warning("[bc_loop] env var ~s not set; "
                       "provider calls will fail with auth error", [Var]),
        %% Substitute empty string so init/1 succeeds. The HTTP call will
        %% return 401, which stream/4 sends as {stream_error,...}; that is
        %% already routed to the user (receive_stream/2) and now also via
        %% the {error, Reason, NewProvState} arm above.
        maps:map(fun(_, {env, _}) -> ""; (_, V) -> V end, ProvMap)
    end.

generate_id() ->
    <<N:128>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~32.16.0b", [N])).
