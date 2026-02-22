%% @doc WebSocket handler — GET /ws
%%
%% Each WebSocket connection gets a unique session. Messages are dispatched to
%% bc_session with reply_pid = self(), so bc_loop sends {bc_chunk, SId, Chunk}
%% and {bc_done, SId, Msg} directly to this handler process.
-module(bc_ws_h).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([init/2, websocket_init/1, websocket_handle/2,
         websocket_info/2, terminate/3]).

init(Req, _State) ->
    {cowboy_websocket, Req, #{session_id => undefined}, #{idle_timeout => 3600000}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    case jsx:decode(Msg, [return_maps]) of
        Decoded when is_map(Decoded) ->
            handle_ws_message(Decoded, State);
        _ ->
            {ok, State}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({bc_chunk, _SId, Chunk}, State) ->
    Frame = jsx:encode(#{type => <<"chunk">>, content => Chunk}),
    {reply, {text, Frame}, State};
websocket_info({bc_done, _SId, #bc_message{content = Content}}, State) ->
    Frame = jsx:encode(#{type => <<"done">>, content => Content}),
    {reply, {text, Frame}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% Internal

handle_ws_message(#{<<"type">> := <<"message">>, <<"content">> := Content} = Msg, State) ->
    AgentId   = maps:get(<<"agent_id">>, Msg, <<"default">>),
    RawUserId = maps:get(<<"user_id">>, Msg, <<"anonymous">>),
    UserId    = <<"ws:", RawUserId/binary>>,
    SessionId = bc_session_registry:derive_session_id(UserId, AgentId, websocket),
    ChannelMsg = #bc_channel_message{
        session_id = SessionId,
        user_id    = UserId,
        agent_id   = AgentId,
        channel    = websocket,
        content    = Content,
        raw        = Content,
        ts         = erlang:system_time(millisecond),
        reply_pid  = self()
    },
    SessionPid = get_or_create_session(SessionId, UserId, AgentId),
    bc_session:dispatch_run(SessionPid, ChannelMsg),
    {ok, State#{session_id => SessionId}};
handle_ws_message(_Msg, State) ->
    {ok, State}.

get_or_create_session(SessionId, UserId, AgentId) ->
    case bc_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            Pid;
        {error, not_found} ->
            Config = #{session_id  => SessionId,
                       user_id     => UserId,
                       channel_id  => SessionId,
                       channel_mod => undefined,
                       agent_id    => AgentId},
            {ok, _} = bc_sessions_sup:start_session(Config),
            {ok, Pid} = bc_session_registry:lookup(SessionId),
            Pid
    end.

