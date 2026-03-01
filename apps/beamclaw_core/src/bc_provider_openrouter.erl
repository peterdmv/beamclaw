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

-module(bc_provider_openrouter).
-moduledoc "OpenRouter LLM provider stub.".
-behaviour(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1]).
-export([init/1, complete/3, stream/4, capabilities/1, terminate/2]).
-export([message_to_map/1]).

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

build_request_body(Messages, Options, #{model := Model} = _State, Stream) ->
    Base = #{
        model    => list_to_binary(Model),
        messages => [message_to_map(M) || M <- Messages],
        stream   => Stream
    },
    case maps:get(tools, Options, []) of
        [] ->
            jsx:encode(Base);
        ToolDefs ->
            jsx:encode(Base#{tools => [tool_def_to_map(T) || T <- ToolDefs]})
    end.

message_to_map(#bc_message{role = Role, content = Content, attachments = Att}) ->
    RoleBin = atom_to_binary(Role, utf8),
    TextContent = case Content of undefined -> <<"">>; _ -> Content end,
    case Att of
        [_ | _] when Role =:= user ->
            TextPart = #{<<"type">> => <<"text">>, <<"text">> => TextContent},
            ImageParts = [attachment_to_part(A) || A <- Att],
            #{role => RoleBin, content => [TextPart | ImageParts]};
        _ ->
            #{role => RoleBin, content => TextContent}
    end.

attachment_to_part({MimeType, Base64Data}) ->
    DataUrl = <<"data:", MimeType/binary, ";base64,", Base64Data/binary>>,
    #{<<"type">> => <<"image_url">>,
      <<"image_url">> => #{<<"url">> => DataUrl}}.

tool_def_to_map(#{name := N, description := D, parameters := P}) ->
    #{type => <<"function">>,
      function => #{name => N, description => D, parameters => encode_params(P)}}.

%% Convert atom keys in parameter schemas to binary keys for JSON encoding.
encode_params(M) when is_map(M) ->
    maps:fold(fun(K, V, Acc) ->
        BK = if is_atom(K) -> atom_to_binary(K, utf8); true -> K end,
        Acc#{BK => encode_params(V)}
    end, #{}, M);
encode_params(L) when is_list(L) ->
    [encode_params(E) || E <- L];
encode_params(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
encode_params(V) ->
    V.

post(#{api_key := Key, base_url := Base}, Path, Body) ->
    Url     = list_to_binary(Base ++ Path),
    Headers = [{<<"authorization">>, list_to_binary("Bearer " ++ Key)},
               {<<"content-type">>,  <<"application/json">>}],
    case hackney:request(post, Url, Headers, Body,
                         [{recv_timeout, 120000}, {connect_timeout, 10000},
                          with_body]) of
        {ok, 200, _RespHeaders, RespBody} ->
            {ok, RespBody};
        {ok, Status, _RespHeaders, RespBody} ->
            {error, {Status, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

parse_response(Body) ->
    Decoded = jsx:decode(Body, [return_maps]),
    Choice  = hd(maps:get(<<"choices">>, Decoded, [#{}])),
    MsgMap  = maps:get(<<"message">>, Choice, #{}),
    RawContent0 = maps:get(<<"content">>, MsgMap, <<>>),
    RawContent = case RawContent0 of null -> <<>>; _ -> RawContent0 end,
    Content = bc_thinking:strip(RawContent),
    ToolCalls = maps:get(<<"tool_calls">>, MsgMap, []),
    #bc_message{
        id         = maps:get(<<"id">>, Decoded, <<>>),
        role       = assistant,
        content    = Content,
        tool_calls = ToolCalls,
        ts         = erlang:system_time(millisecond)
    }.
