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

-module(bc_embedding).
-moduledoc """
Embedding API client via hackney (OpenAI-compatible /v1/embeddings).

API key resolved at call time from environment variables — never stored or logged.
Supports BEAMCLAW_EMBEDDING_API_KEY (fallback: OPENAI_API_KEY).
""".

-export([embed/1, embed_batch/1, is_configured/0]).

-doc "Embed a single text. Returns {ok, [float()]} or {error, term()}.".
-spec embed(binary()) -> {ok, [float()]} | {error, term()}.
embed(Text) when is_binary(Text) ->
    case embed_batch([Text]) of
        {ok, [Embedding]} -> {ok, Embedding};
        {error, _} = Err  -> Err
    end.

-doc "Embed a batch of texts. Returns {ok, [[float()]]} or {error, term()}.".
-spec embed_batch([binary()]) -> {ok, [[float()]]} | {error, term()}.
embed_batch(Texts) when is_list(Texts), length(Texts) > 0 ->
    case resolve_api_key() of
        undefined ->
            {error, no_api_key};
        ApiKey ->
            BaseUrl = resolve_base_url(),
            Model   = resolve_model(),
            Url     = list_to_binary(BaseUrl ++ "/embeddings"),
            Body    = jsx:encode(#{model => list_to_binary(Model),
                                   input => Texts}),
            Headers = [{<<"Authorization">>, list_to_binary("Bearer " ++ ApiKey)},
                       {<<"Content-Type">>, <<"application/json">>}],
            case hackney:request(post, Url, Headers, Body,
                                 [{recv_timeout, 30000}, {connect_timeout, 5000},
                                  with_body]) of
                {ok, 200, _RespHeaders, RespBody} ->
                    parse_embedding_response(RespBody);
                {ok, Status, _RespHeaders, RespBody} ->
                    {error, {http_error, Status, RespBody}};
                {error, Reason} ->
                    {error, {request_failed, Reason}}
            end
    end;
embed_batch(_) ->
    {error, empty_input}.

-doc "Check whether embedding is configured (API key available).".
-spec is_configured() -> boolean().
is_configured() ->
    resolve_api_key() =/= undefined.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

resolve_api_key() ->
    case os:getenv("BEAMCLAW_EMBEDDING_API_KEY") of
        false ->
            case os:getenv("OPENAI_API_KEY") of
                false -> undefined;
                Key   -> Key
            end;
        Key -> Key
    end.

resolve_base_url() ->
    case os:getenv("BEAMCLAW_EMBEDDING_URL") of
        false -> "https://api.openai.com/v1";
        Url   -> Url
    end.

resolve_model() ->
    case os:getenv("BEAMCLAW_EMBEDDING_MODEL") of
        false -> "text-embedding-3-small";
        Model -> Model
    end.

parse_embedding_response(Body) ->
    try
        Json = jsx:decode(Body, [return_maps]),
        Data = maps:get(<<"data">>, Json, []),
        Embeddings = lists:sort(fun(A, B) ->
            maps:get(<<"index">>, A, 0) =< maps:get(<<"index">>, B, 0)
        end, Data),
        Vectors = [maps:get(<<"embedding">>, E) || E <- Embeddings],
        {ok, Vectors}
    catch _:_ ->
        {error, {parse_error, Body}}
    end.
