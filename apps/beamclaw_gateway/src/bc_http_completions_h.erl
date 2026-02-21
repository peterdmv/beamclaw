%% @doc OpenAI-compatible chat completions handler — POST /v1/chat/completions
%%
%% Accepts OpenAI-format request bodies and dispatches to a BeamClaw session.
%% Supports both:
%%   - SSE streaming (stream: true):  text/event-stream with data: {...} events
%%   - Synchronous (stream: false):   standard JSON response
%%
%% The handler registers itself as reply_pid in the bc_channel_message, so
%% bc_loop sends {bc_chunk, SId, Chunk} and {bc_done, SId, Msg} directly here.
%% Session ID is taken from the "session_id" field in the request, or generated.
-module(bc_http_completions_h).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([init/2]).

-define(TIMEOUT_MS, 120000).

init(Req, State) ->
    ClientIp = peer_ip(Req),
    case bc_rate_limiter:check(ClientIp, <<"/v1/chat/completions">>) of
        {error, rate_limited} ->
            Req2 = cowboy_req:reply(429,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => <<"rate limited">>}), Req),
            {ok, Req2, State};
        ok ->
            handle_completion(Req, State)
    end.

handle_completion(Req, State) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            case catch jsx:decode(Body, [return_maps]) of
                Decoded when is_map(Decoded) ->
                    dispatch_and_respond(Decoded, Req2, State);
                _ ->
                    Req3 = cowboy_req:reply(400,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{error => <<"invalid JSON">>}), Req2),
                    {ok, Req3, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(400, #{}, <<"bad request">>, Req),
            {ok, Req2, State}
    end.

dispatch_and_respond(Decoded, Req, State) ->
    Messages  = maps:get(<<"messages">>, Decoded, []),
    Stream    = maps:get(<<"stream">>,   Decoded, false),
    SessionId = maps:get(<<"session_id">>, Decoded, generate_session_id()),
    Content   = get_last_user_message(Messages),
    ChannelMsg = #bc_channel_message{
        session_id = SessionId,
        user_id    = <<"api_user">>,
        channel    = http,
        content    = Content,
        raw        = Decoded,
        ts         = erlang:system_time(millisecond),
        reply_pid  = self()
    },
    SessionPid = get_or_create_session(SessionId),
    bc_session:dispatch_run(SessionPid, ChannelMsg),
    case Stream of
        true  -> respond_sse(Req, SessionId, State);
        false -> respond_sync(Req, SessionId, State)
    end.

%% SSE streaming response

respond_sse(Req, SessionId, State) ->
    Req2 = cowboy_req:stream_reply(200,
        #{<<"content-type">>  => <<"text/event-stream">>,
          <<"cache-control">> => <<"no-cache">>,
          <<"connection">>    => <<"keep-alive">>},
        Req),
    stream_sse_loop(Req2, SessionId, State).

stream_sse_loop(Req, SessionId, State) ->
    receive
        {bc_chunk, SessionId, Chunk} ->
            Data = sse_event(#{choices => [#{delta => #{content => Chunk},
                                             finish_reason => null}]}),
            cowboy_req:stream_body(Data, nofin, Req),
            stream_sse_loop(Req, SessionId, State);
        {bc_done, SessionId, Msg} ->
            %% Send a final delta with finish_reason, then [DONE]
            DoneData = sse_event(#{choices => [#{delta => #{content => <<>>},
                                                 finish_reason => <<"stop">>}]}),
            cowboy_req:stream_body(DoneData, nofin, Req),
            cowboy_req:stream_body(<<"data: [DONE]\n\n">>, fin, Req),
            _ = Msg,
            {ok, Req, State}
    after ?TIMEOUT_MS ->
        cowboy_req:stream_body(<<"data: [DONE]\n\n">>, fin, Req),
        {ok, Req, State}
    end.

sse_event(Map) ->
    Json = jsx:encode(Map),
    <<"data: ", Json/binary, "\n\n">>.

%% Synchronous (non-streaming) response

respond_sync(Req, SessionId, State) ->
    receive
        {bc_chunk, SessionId, _} ->
            %% Discard intermediate chunks; wait for final message.
            respond_sync(Req, SessionId, State);
        {bc_done, SessionId, #bc_message{content = Content}} ->
            Body = jsx:encode(#{
                id      => <<"chatcmpl-", SessionId/binary>>,
                object  => <<"chat.completion">>,
                choices => [#{message       => #{role    => <<"assistant">>,
                                                 content => Content},
                              finish_reason => <<"stop">>}]
            }),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Body, Req),
            {ok, Req2, State}
    after ?TIMEOUT_MS ->
        Req2 = cowboy_req:reply(504,
            #{<<"content-type">> => <<"application/json">>},
            jsx:encode(#{error => <<"timeout">>}), Req),
        {ok, Req2, State}
    end.

%% Helpers

get_last_user_message(Messages) ->
    UserContents = [maps:get(<<"content">>, M, <<>>) ||
                    M <- Messages,
                    maps:get(<<"role">>, M, <<>>) =:= <<"user">>],
    case lists:reverse(UserContents) of
        [Last | _] -> Last;
        []          -> <<>>
    end.

get_or_create_session(SessionId) ->
    case bc_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            Pid;
        {error, not_found} ->
            Config = #{session_id  => SessionId,
                       user_id     => <<"api_user">>,
                       channel_id  => SessionId,
                       channel_mod => undefined},
            {ok, _} = bc_sessions_sup:start_session(Config),
            {ok, Pid} = bc_session_registry:lookup(SessionId),
            Pid
    end.

peer_ip(Req) ->
    case cowboy_req:peer(Req) of
        {Ip, _Port} -> Ip;
        _            -> unknown
    end.

generate_session_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format(
        "~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
        [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E])).
