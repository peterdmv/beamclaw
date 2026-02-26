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

-module(bc_http_integration_SUITE).
-moduledoc """
HTTP gateway integration tests (Tier 2).

Starts the full gateway with a mock LLM provider and exercises Cowboy
handlers end-to-end. Uses httpc for HTTP requests (already available
via inets). No new deps.

Mock provider: bc_provider_smoke_mock (canned "Hello, world!" responses).
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% CT callbacks
-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([health_endpoint/1, completions_sync/1,
         completions_bad_json/1, rate_limit_exceeded/1]).

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [health_endpoint, completions_sync,
     completions_bad_json, rate_limit_exceeded].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    %% Pick a random high port to avoid conflicts
    Port = 19000 + (erlang:unique_integer([positive]) rem 1000),

    %% Configure gateway with our test port, no channels
    application:set_env(beamclaw_gateway, http, #{port => Port}),
    application:set_env(beamclaw_gateway, channels, [
        {tui, #{enabled => false}}
    ]),

    %% Configure core: disable persistence, full autonomy, high compaction threshold
    application:set_env(beamclaw_core, session_persistence, false),
    application:set_env(beamclaw_core, autonomy_level, full),
    application:set_env(beamclaw_core, agentic_loop,
                        #{compaction_threshold => 9999,
                          memory_flush => false}),

    %% Set dummy API key so provider init doesn't crash
    os:putenv("OPENROUTER_API_KEY", "test-dummy-key"),

    %% Start inets for httpc
    {ok, _} = application:ensure_all_started(inets),

    %% Start the full gateway (which starts core and all deps)
    {ok, Started} = application:ensure_all_started(beamclaw_gateway),

    BaseUrl = "http://127.0.0.1:" ++ integer_to_list(Port),
    [{started_apps, Started},
     {port, Port},
     {base_url, BaseUrl} | Config].

end_per_suite(Config) ->
    Started = proplists:get_value(started_apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Started)),
    os:unsetenv("OPENROUTER_API_KEY"),
    ok.

%% ---------------------------------------------------------------------------
%% Per-testcase lifecycle
%% ---------------------------------------------------------------------------

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% Test cases
%% ---------------------------------------------------------------------------

health_endpoint(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/health",
    {ok, {{_, 200, _}, _Headers, Body}} = httpc:request(get, {Url, []}, [], []),
    Decoded = jsx:decode(iolist_to_binary(Body), [return_maps]),
    ?assertEqual(<<"ok">>, maps:get(<<"status">>, Decoded)),
    ok.

completions_sync(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/v1/chat/completions",
    SessionId = iolist_to_binary([
        "ct-http-", integer_to_list(erlang:unique_integer([positive]))]),

    %% Pre-create session with mock provider so the HTTP handler finds it
    %% already running with bc_provider_smoke_mock instead of openrouter
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_smoke_mock,
        autonomy     => full
    },
    {ok, _} = bc_sessions_sup:start_session(SessionConfig),

    ReqBody = jsx:encode(#{
        <<"messages">> => [#{<<"role">> => <<"user">>,
                             <<"content">> => <<"hello">>}],
        <<"stream">> => false,
        <<"session_id">> => SessionId
    }),
    {ok, {{_, StatusCode, _}, _Headers, RespBody}} =
        httpc:request(post,
            {Url, [], "application/json", binary_to_list(ReqBody)},
            [{timeout, 15000}], []),
    ?assertEqual(200, StatusCode),
    Decoded = jsx:decode(iolist_to_binary(RespBody), [return_maps]),
    Choices = maps:get(<<"choices">>, Decoded, []),
    ?assert(length(Choices) >= 1),
    [First | _] = Choices,
    Msg = maps:get(<<"message">>, First),
    ?assertEqual(<<"assistant">>, maps:get(<<"role">>, Msg)),
    Content = maps:get(<<"content">>, Msg),
    ?assert(byte_size(Content) > 0),
    ok.

completions_bad_json(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/v1/chat/completions",
    {ok, {{_, StatusCode, _}, _Headers, _Body}} =
        httpc:request(post,
            {Url, [], "application/json", "not valid json{{{"},
            [{timeout, 5000}], []),
    ?assertEqual(400, StatusCode),
    ok.

rate_limit_exceeded(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/v1/chat/completions",
    %% The default rate limit is 60 requests per 60s window.
    %% Flood with 62 requests; expect at least one 429.
    %% Use unique session IDs per request to avoid session accumulation.
    Results = lists:map(fun(N) ->
        Sid = iolist_to_binary([
            "ct-rl-", integer_to_list(N), "-",
            integer_to_list(erlang:unique_integer([positive]))]),
        Body = jsx:encode(#{
            <<"messages">> => [#{<<"role">> => <<"user">>,
                                 <<"content">> => <<"test">>}],
            <<"stream">> => false,
            <<"session_id">> => Sid
        }),
        case httpc:request(post,
                {Url, [], "application/json", binary_to_list(Body)},
                [{timeout, 5000}], []) of
            {ok, {{_, Code, _}, _, _}} -> Code;
            _ -> error
        end
    end, lists:seq(1, 62)),
    Has429 = lists:member(429, Results),
    ?assert(Has429),
    ok.
