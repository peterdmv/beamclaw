%% @doc TUI (terminal UI) channel — reads from stdin, writes to stdout.
%% Implements bc_channel behaviour.
%%
%% bc_loop calls send_response/2 after each completed turn. The gen_server
%% handles {send_response, ...} casts and writes the response to stdout.
-module(bc_channel_tui).
-behaviour(gen_server).
%% Implements bc_channel callbacks.

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1, send_response/2]).
-export([init/1, listen/1, send/3, send_typing/2,
         update_draft/4, finalize_draft/3, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Called by bc_loop to deliver a completed response to the TUI.
-spec send_response(SessionId :: binary(), Msg :: #bc_message{}) -> ok.
send_response(SessionId, Msg) ->
    gen_server:cast(?MODULE, {send_response, SessionId, Msg}).

%% bc_channel callbacks

init(Config) ->
    Enabled = maps:get(enabled, Config, false),
    AgentId = bc_config:get(beamclaw_core, default_agent, <<"default">>),
    UserId  = tui_user_id(),
    SessionId = bc_session_registry:derive_session_id(UserId, AgentId, tui),
    State   = #{enabled    => Enabled,
                session_id => SessionId,
                user_id    => UserId,
                agent_id   => AgentId},
    case Enabled of
        true  -> self() ! start_io;
        false -> ok
    end,
    {ok, State}.

listen(State) ->
    {ok, State}.

send(_, #bc_message{content = Content}, State) when is_binary(Content) ->
    io:format("~n[assistant] ~s~n> ", [Content]),
    {ok, State};
send(_, _, State) ->
    {ok, State}.

send_typing(_, _State) ->
    io:format("[...thinking...]~n"),
    ok.

update_draft(_, _, Content, State) ->
    io:format("\r~s", [Content]),
    {ok, State}.

finalize_draft(_, _, State) ->
    io:format("~n"),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% gen_server callbacks

handle_cast({send_response, SessionId, Msg}, State) ->
    {ok, NewState} = send(SessionId, Msg, State),
    self() ! read_line,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_io, State) ->
    io:format("BeamClaw TUI — type a message and press Enter.~n> "),
    self() ! read_line,
    {noreply, State};
handle_info(read_line, State) ->
    Self = self(),
    spawn(fun() -> Self ! {line_result, io:get_line("")} end),
    {noreply, State};
handle_info({line_result, eof}, State) ->
    {stop, normal, State};
handle_info({line_result, {error, Reason}}, State) ->
    logger:warning("[bc_channel_tui] io:get_line failed (~p); "
                   "stdin unavailable, entering dormant mode", [Reason]),
    {noreply, State};
handle_info({line_result, Line}, #{session_id := SessionId,
                                    user_id := UserId,
                                    agent_id := AgentId} = State) ->
    Text = string:trim(unicode:characters_to_binary(Line)),
    case Text of
        <<>> ->
            %% Empty line — no LLM call, start next read immediately.
            self() ! read_line;
        _ ->
            ChannelMsg = #bc_channel_message{
                session_id = SessionId,
                user_id    = UserId,
                agent_id   = AgentId,
                channel    = tui,
                content    = Text,
                raw        = Line,
                ts         = erlang:system_time(millisecond)
                %% reply_pid unset — responses routed via send_response/2
            },
            ensure_session_and_dispatch(SessionId, ChannelMsg, UserId, AgentId)
            %% Do NOT send read_line here — wait for send_response to do it.
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) -> {reply, {error, unknown}, State}.
code_change(_OldVsn, State, _)  -> {ok, State}.

%% Internal

tui_user_id() ->
    case os:getenv("BEAMCLAW_USER") of
        false ->
            case os:getenv("USER") of
                false -> <<"local:anonymous">>;
                U     -> iolist_to_binary(["local:", U])
            end;
        U -> iolist_to_binary(["local:", U])
    end.

ensure_session_and_dispatch(SessionId, Msg, UserId, AgentId) ->
    case bc_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            bc_session:dispatch_run(Pid, Msg);
        {error, not_found} ->
            Config = #{session_id  => SessionId,
                       user_id     => UserId,
                       channel_id  => SessionId,
                       channel_mod => bc_channel_tui,
                       agent_id    => AgentId},
            {ok, _} = bc_sessions_sup:start_session(Config),
            %% bc_session_registry:register/2 is now a synchronous call,
            %% so the session is in the registry by the time start_session returns.
            {ok, Pid} = bc_session_registry:lookup(SessionId),
            bc_session:dispatch_run(Pid, Msg)
    end.
