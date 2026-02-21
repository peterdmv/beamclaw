%% @doc OpenRouter LLM provider stub.
-module(bc_provider_openrouter).
-behaviour(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1]).
-export([init/1, complete/3, stream/4, capabilities/1, terminate/2]).

start_link(Config) ->
    %% Providers run as gen_servers; this is the entry point from bc_session_sup.
    %% For now, init is called directly by bc_loop.
    {ok, _Pid} = gen_server:start_link(?MODULE, Config, []).

init(Config) ->
    ApiKey  = bc_config:resolve(maps:get(api_key,  Config, {env, "OPENROUTER_API_KEY"})),
    BaseUrl = maps:get(base_url, Config, "https://openrouter.ai/api/v1"),
    Model   = maps:get(model,    Config, "anthropic/claude-sonnet-4-5"),
    {ok, #{api_key => ApiKey, base_url => BaseUrl, model => Model}}.

complete(Messages, Options, State) ->
    Body = build_request_body(Messages, Options, State, false),
    case post(State, "/chat/completions", Body) of
        {ok, RespBody} ->
            Msg = parse_response(RespBody),
            {ok, Msg, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

stream(Messages, Options, CallerPid, State) ->
    Body = build_request_body(Messages, Options, State, false),
    %% TODO: implement SSE streaming via hackney async
    %% For now: fall back to complete and send as single chunk
    case post(State, "/chat/completions", Body) of
        {ok, RespBody} ->
            Msg = parse_response(RespBody),
            CallerPid ! {stream_chunk, self(), Msg#bc_message.content},
            CallerPid ! {stream_done,  self(), Msg},
            {ok, State};
        {error, Reason} ->
            CallerPid ! {stream_error, self(), Reason},
            {error, Reason, State}
    end.

capabilities(_State) ->
    #{supports_streaming => true, supports_tools => true}.

terminate(_Reason, _State) ->
    ok.

%% Internal

build_request_body(Messages, _Options, #{model := Model} = _State, Stream) ->
    jsx:encode(#{
        model    => list_to_binary(Model),
        messages => [message_to_map(M) || M <- Messages],
        stream   => Stream
    }).

message_to_map(#bc_message{role = Role, content = Content}) ->
    #{role    => atom_to_binary(Role, utf8),
      content => case Content of undefined -> <<"">>;  _ -> Content end}.

post(#{api_key := Key, base_url := Base}, Path, Body) ->
    Url     = Base ++ Path,
    Headers = [{"authorization", "Bearer " ++ Key},
               {"content-type",  "application/json"}],
    case httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            {ok, iolist_to_binary(RespBody)};
        {ok, {{_, Status, _}, _, RespBody}} ->
            {error, {Status, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

parse_response(Body) ->
    Decoded = jsx:decode(Body, [return_maps]),
    Choice  = hd(maps:get(<<"choices">>, Decoded, [#{}])),
    MsgMap  = maps:get(<<"message">>, Choice, #{}),
    Content = maps:get(<<"content">>, MsgMap, <<>>),
    ToolCalls = maps:get(<<"tool_calls">>, MsgMap, []),
    #bc_message{
        id         = maps:get(<<"id">>, Decoded, <<>>),
        role       = assistant,
        content    = Content,
        tool_calls = ToolCalls,
        ts         = erlang:system_time(millisecond)
    }.
