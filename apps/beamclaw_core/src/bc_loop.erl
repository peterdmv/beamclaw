%%
%% Copyright Péter Dimitrov 2026, All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(bc_loop).
-moduledoc """
Agentic loop — gen_statem.

States:
  idle → compacting (optional) → streaming → awaiting_approval (optional)
       → executing_tools → streaming (loop) → finalizing → idle

bc_loop is transient. A crash is restarted by bc_session_sup.
bc_session (permanent) retains history across restarts.

Response routing (route_response/2):
  - reply_pid set in bc_channel_message → send {bc_chunk, SId, Chunk} and
    {bc_done, SId, Msg} directly to that pid (HTTP/WS handlers)
  - reply_pid undefined → call ChannelMod:send_response(SId, Msg) on the
    named channel gen_server (Telegram, TUI)
""".
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
    iteration      :: non_neg_integer(),
    typing_tick_ref = undefined :: reference() | undefined
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
        id          = generate_id(),
        role        = user,
        content     = Message#bc_channel_message.content,
        ts          = Message#bc_channel_message.ts,
        attachments = Message#bc_channel_message.attachments
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
    emit_typing(Data),
    TickRef = schedule_typing_tick(),
    LoopCfg = bc_config:get(beamclaw_core, agentic_loop, #{}),
    case maps:get(memory_flush, LoopCfg, true) of
        true  -> gen_statem:cast(self(), do_memory_flush);
        false -> gen_statem:cast(self(), do_compact)
    end,
    {keep_state, Data#loop_data{typing_tick_ref = TickRef}};
compacting(cast, do_memory_flush, Data) ->
    logger:debug("[loop] pre-compaction memory flush: session=~s",
                 [Data#loop_data.session_id]),
    try run_memory_flush(Data)
    catch Class:Reason:Stack ->
        logger:warning("[loop] memory flush failed: ~p:~p ~p",
                       [Class, Reason, Stack])
    end,
    gen_statem:cast(self(), do_compact),
    {keep_state, Data};
compacting(cast, do_compact, Data) ->
    cancel_typing_tick(Data#loop_data.typing_tick_ref),
    bc_compactor:compact(Data#loop_data.session_pid),
    {next_state, streaming, Data#loop_data{typing_tick_ref = undefined}};
compacting(info, typing_tick, Data) ->
    emit_typing(Data),
    TickRef = schedule_typing_tick(),
    {keep_state, Data#loop_data{typing_tick_ref = TickRef}};
compacting(EventType, EventContent, Data) ->
    handle_common(compacting, EventType, EventContent, Data).

streaming(enter, _OldState, Data) ->
    emit_typing(Data),
    gen_statem:cast(self(), do_stream),
    {keep_state, Data};
streaming(cast, do_stream, Data) ->
    History    = bc_session:get_history(Data#loop_data.session_pid),
    SystemMsgs = bc_system_prompt:assemble(Data#loop_data.agent_id),
    AutoCtxMsgs = maybe_auto_context(History, Data),
    FullHistory = SystemMsgs ++ AutoCtxMsgs ++ History,
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
    TickRef = schedule_typing_tick(),
    case ProvMod:stream(FullHistory, Options, self(), ProvState) of
        {ok, NewProvState} ->
            receive_stream(Data#loop_data{provider_state = NewProvState}, T0, TickRef);
        {error, Reason, NewProvState} ->
            cancel_typing_tick(TickRef),
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

awaiting_approval(enter, _OldState, _Data) ->
    %% TODO: wire up bc_approval channel prompting for interactive approval.
    %% For now, auto-approve (matches bc_approval's current supervised-mode behaviour).
    self() ! {approval_result, all, approved},
    keep_state_and_data;
awaiting_approval(info, {approval_result, _ToolCallId, approved}, Data) ->
    {next_state, executing_tools, Data};
awaiting_approval(info, {approval_result, _ToolCallId, denied}, Data) ->
    {next_state, finalizing, Data};
awaiting_approval(EventType, EventContent, Data) ->
    handle_common(awaiting_approval, EventType, EventContent, Data).

executing_tools(enter, _OldState, Data) ->
    emit_typing(Data),
    TickRef = schedule_typing_tick(),
    gen_statem:cast(self(), do_execute),
    {keep_state, Data#loop_data{typing_tick_ref = TickRef}};
executing_tools(cast, do_execute, Data) ->
    LoopCfg = bc_config:get(beamclaw_core, agentic_loop, #{}),
    MaxIter = maps:get(max_tool_iterations, LoopCfg, 10),
    case Data#loop_data.iteration >= MaxIter of
        true ->
            cancel_typing_tick(Data#loop_data.typing_tick_ref),
            logger:warning("[loop] max tool iterations (~p) reached", [MaxIter]),
            {next_state, finalizing, Data#loop_data{typing_tick_ref = undefined}};
        false ->
            %% Spawn tool execution so the gen_statem stays responsive to
            %% typing_tick messages during long-running tools (e.g. bash, curl).
            Self = self(),
            SessionRef = make_session_ref(Data),
            ToolCalls = Data#loop_data.tool_calls,
            SId = Data#loop_data.session_id,
            spawn_link(fun() ->
                ToolMsgs = execute_tools_sync(ToolCalls, SessionRef, SId),
                gen_statem:cast(Self, {tool_results, ToolMsgs})
            end),
            {keep_state, Data}
    end;
executing_tools(cast, {tool_results, ToolMsgs}, Data) ->
    cancel_typing_tick(Data#loop_data.typing_tick_ref),
    lists:foreach(fun(M) ->
        bc_session:append_message(Data#loop_data.session_pid, M)
    end, ToolMsgs),
    NewData = Data#loop_data{tool_calls = [], iteration = Data#loop_data.iteration + 1,
                             typing_tick_ref = undefined},
    {next_state, streaming, NewData};
executing_tools(info, typing_tick, Data) ->
    emit_typing(Data),
    TickRef = schedule_typing_tick(),
    {keep_state, Data#loop_data{typing_tick_ref = TickRef}};
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

receive_stream(Data, T0, TickRef) ->
    receive
        {stream_chunk, _Pid, Chunk} ->
            %% Forward to direct reply target (HTTP/WS); channel gen-servers
            %% don't need intermediate chunks.
            _ = case Data#loop_data.reply_pid of
                undefined -> ok;
                RPid -> RPid ! {bc_chunk, Data#loop_data.session_id, Chunk}
            end,
            receive_stream(Data, T0, TickRef);
        {stream_done, _Pid, FullMsg} ->
            Duration = erlang:monotonic_time(millisecond) - T0,
            bc_obs:emit(llm_response, #{session_id  => Data#loop_data.session_id,
                                        duration_ms => Duration, success => true}),
            ScrubbedMsg = bc_scrubber:scrub_message(FullMsg),
            bc_session:append_message(Data#loop_data.session_pid, ScrubbedMsg),
            %% Emit a fresh typing indicator before sending the message so the
            %% user sees "typing..." right until the message arrives. Telegram
            %% auto-clears typing when the message is delivered.
            emit_typing(Data),
            route_response(Data, ScrubbedMsg),
            cancel_typing_tick(TickRef),
            ToolCalls = bc_tool_parser:parse(ScrubbedMsg),
            NewData = Data#loop_data{tool_calls = ToolCalls},
            case ToolCalls of
                [] -> {next_state, finalizing, NewData};
                _  -> maybe_await_approval(NewData)
            end;
        {stream_error, _Pid, Reason} ->
            cancel_typing_tick(TickRef),
            logger:error("[loop] stream error: ~p", [Reason]),
            ErrContent = iolist_to_binary(io_lib:format("[Error: ~p]", [Reason])),
            route_response(Data, #bc_message{role    = assistant,
                                             content = ErrContent,
                                             ts      = erlang:system_time(millisecond)}),
            {next_state, finalizing, Data};
        typing_tick ->
            emit_typing(Data),
            NewTickRef = schedule_typing_tick(),
            receive_stream(Data, T0, NewTickRef)
    after 60000 ->
        cancel_typing_tick(TickRef),
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

emit_typing(Data) ->
    case Data#loop_data.reply_pid of
        undefined ->
            case channel_mod_for(Data#loop_data.reply_channel) of
                undefined -> ok;
                Mod       -> Mod:notify_typing(Data#loop_data.session_id)
            end;
        _RPid ->
            %% HTTP/WS consumers don't need typing indicators
            ok
    end.

schedule_typing_tick() ->
    erlang:send_after(3000, self(), typing_tick).

cancel_typing_tick(undefined) -> ok;
cancel_typing_tick(Ref) ->
    _ = erlang:cancel_timer(Ref),
    %% Flush any tick that arrived between cancel and now.
    receive typing_tick -> ok after 0 -> ok end.

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

%% Execute tool calls synchronously. Designed to run in a spawned process so
%% the gen_statem mailbox remains responsive to typing_tick messages.
execute_tools_sync(ToolCalls, SessionRef, SessionId) ->
    Results = lists:map(fun(TC) ->
        bc_obs:emit(tool_call_start, #{tool_name  => TC#bc_tool_call.name,
                                       args       => TC#bc_tool_call.args,
                                       session_id => SessionId}),
        T0 = erlang:monotonic_time(millisecond),
        Result = run_tool(TC, SessionRef),
        Duration = erlang:monotonic_time(millisecond) - T0,
        bc_obs:emit(tool_call_result, #{tool_name  => TC#bc_tool_call.name,
                                        duration_ms => Duration,
                                        success    => element(1, Result) =:= ok,
                                        session_id => SessionId}),
        #bc_tool_result{
            tool_call_id = TC#bc_tool_call.id,
            name         = TC#bc_tool_call.name,
            content      = element(2, Result),
            is_error     = element(1, Result) =:= error
        }
    end, ToolCalls),
    Scrubbed = [bc_scrubber:scrub_result(R) || R <- Results],
    [result_to_message(R) || R <- Scrubbed].

run_tool(#bc_tool_call{name = Name, args = Args}, SessionRef) ->
    Context = #{tool_bridge_fn => make_tool_bridge_fn(SessionRef)},
    try
        case bc_tool_registry:lookup(Name) of
            {ok, {Mod, _Def}} ->
                Mod:execute(Args, SessionRef, Context);
            {error, not_found} ->
                case bc_mcp_registry:lookup(Name) of
                    {ok, {ServerPid, _}} ->
                        bc_mcp_server:call_tool(ServerPid, Name, Args);
                    {error, not_found} ->
                        {error, <<"tool not found">>}
                end
        end
    catch
        Class:Reason:Stack ->
            logger:error("[loop] tool ~s crashed: ~p:~p~n~p",
                         [Name, Class, Reason, Stack]),
            ErrMsg = iolist_to_binary(
                io_lib:format("Tool crashed: ~p:~p", [Class, Reason])),
            {error, ErrMsg}
    end.

%% Build a tool bridge callback for sandbox use. Captures access to both
%% bc_tool_registry and bc_mcp_registry without creating dependency cycles.
make_tool_bridge_fn(SessionRef) ->
    fun(ToolName, ToolArgs) ->
        case bc_tool_registry:lookup(ToolName) of
            {ok, {Mod, _Def}} ->
                Mod:execute(ToolArgs, SessionRef, #{});
            {error, not_found} ->
                case bc_mcp_registry:lookup(ToolName) of
                    {ok, {ServerPid, _}} ->
                        bc_mcp_server:call_tool(ServerPid, ToolName, ToolArgs);
                    {error, not_found} ->
                        {error, <<"Tool not found: ", ToolName/binary>>}
                end
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

%% Optional auto-context: BM25-only search of structured memory using
%% user's latest message, prepended as a system message.
%% Gated by `auto_context => true` in agentic_loop config (default: false).
maybe_auto_context(History, Data) ->
    LoopCfg = bc_config:get(beamclaw_core, agentic_loop, #{}),
    case maps:get(auto_context, LoopCfg, false) of
        true ->
            Limit = maps:get(auto_context_limit, LoopCfg, 3),
            case last_user_content(History) of
                undefined -> [];
                UserText  ->
                    try auto_context_search(UserText, Limit, Data)
                    catch _:_ -> []
                    end
            end;
        false ->
            []
    end.

last_user_content(History) ->
    UserMsgs = [M || M <- lists:reverse(History),
                     M#bc_message.role =:= user,
                     M#bc_message.content =/= undefined],
    case UserMsgs of
        [Last | _] -> Last#bc_message.content;
        []         -> undefined
    end.

auto_context_search(Query, Limit, Data) ->
    %% Use workspace memory search (BM25 only, no embedding API call)
    AgentId = Data#loop_data.agent_id,
    MemPath = bc_workspace_path:bootstrap_file(AgentId, <<"MEMORY.md">>),
    Chunks = case file:read_file(MemPath) of
        {ok, Bin} when byte_size(Bin) > 0 ->
            Paragraphs = re:split(Bin, <<"\\n\\n+">>, [{return, binary}]),
            [{iolist_to_binary(io_lib:format("[MEMORY.md:para ~B]", [I])),
              string:trim(P)}
             || {I, P} <- lists:zip(lists:seq(1, length(Paragraphs)), Paragraphs),
                string:trim(P) =/= <<>>];
        _ -> []
    end,
    case Chunks of
        [] -> [];
        _ ->
            Ranked = bc_bm25:rank(Query, Chunks),
            Top = lists:sublist(Ranked, Limit),
            case Top of
                [] -> [];
                _ ->
                    ChunkMap = maps:from_list(Chunks),
                    Entries = [maps:get(K, ChunkMap, <<>>) || {K, _} <- Top],
                    Content = iolist_to_binary([
                        <<"[Relevant memories]\n">>,
                        lists:join(<<"\n---\n">>, Entries)
                    ]),
                    [#bc_message{
                        id      = generate_id(),
                        role    = system,
                        content = Content,
                        ts      = erlang:system_time(millisecond)
                    }]
            end
    end.

%% Pre-compaction memory flush: ask the LLM to save durable memories before
%% history is compacted. This is a hidden turn — not routed to the user.
run_memory_flush(Data) ->
    History    = bc_session:get_history(Data#loop_data.session_pid),
    SystemMsgs = bc_system_prompt:assemble(Data#loop_data.agent_id),
    FlushMsg   = #bc_message{
        id      = generate_id(),
        role    = system,
        content = <<"Session history is about to be compacted. Before the older messages "
                    "are summarized and trimmed, save any durable facts, observations, or "
                    "context worth preserving. Use the workspace_memory tool:\n"
                    "- Identity info → update_bootstrap (file: IDENTITY.md)\n"
                    "- User info → update_bootstrap (file: USER.md)\n"
                    "- Session observations → append_daily\n"
                    "- Long-term facts → append (to MEMORY.md)\n"
                    "If there is nothing worth saving, respond with just 'ok'.">>,
        ts      = erlang:system_time(millisecond)
    },
    FullHistory = SystemMsgs ++ History ++ [FlushMsg],
    Tools    = bc_tool_registry:list(),
    ToolDefs = [Def || {_Name, _Mod, Def} <- Tools],
    Options  = #{tools => ToolDefs},
    ProvMod   = Data#loop_data.provider_mod,
    ProvState = Data#loop_data.provider_state,
    case ProvMod:complete(FullHistory, Options, ProvState) of
        {ok, ResponseMsg, _NewProvState} ->
            ToolCalls = bc_tool_parser:parse(ResponseMsg),
            execute_flush_tool_calls(ToolCalls, Data);
        {error, Reason, _NewProvState} ->
            logger:warning("[loop] memory flush LLM call failed: ~p", [Reason]),
            ok
    end.

execute_flush_tool_calls([], _Data) -> ok;
execute_flush_tool_calls(Calls, Data) ->
    SessionRef = make_session_ref(Data),
    lists:foreach(fun(TC) ->
        logger:debug("[loop] memory flush tool call: ~s", [TC#bc_tool_call.name]),
        try run_tool(TC, SessionRef)
        catch Class:Reason ->
            logger:warning("[loop] memory flush tool ~s failed: ~p:~p",
                           [TC#bc_tool_call.name, Class, Reason])
        end
    end, Calls).
