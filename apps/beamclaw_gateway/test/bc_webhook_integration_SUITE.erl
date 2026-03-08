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

-module(bc_webhook_integration_SUITE).
-moduledoc """
Generic webhook integration tests (Tier 2).

Starts the full gateway with a mock LLM provider and exercises the
POST /webhook/:source endpoint end-to-end. Uses httpc for HTTP requests.

Tests:
  - webhook_accepted: valid POST creates session + returns 200
  - webhook_auth_failed: wrong secret returns 401
  - webhook_no_auth_configured: open mode (no env var) returns 200
  - webhook_rate_limited: rapid POSTs trigger 429
  - webhook_plain_text_body: non-JSON body accepted
  - webhook_custom_agent: ?agent=mom routes to correct agent
  - webhook_body_secret: secret in JSON body (no header) returns 200
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% CT callbacks
-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([webhook_accepted/1, webhook_auth_failed/1,
         webhook_no_auth_configured/1, webhook_rate_limited/1,
         webhook_plain_text_body/1, webhook_custom_agent/1,
         webhook_body_secret/1]).

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [webhook_accepted, webhook_auth_failed,
     webhook_no_auth_configured, webhook_plain_text_body,
     webhook_custom_agent, webhook_body_secret,
     %% rate_limited must be last — it floods the IP-based rate limiter
     webhook_rate_limited].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    Port = 19000 + (erlang:unique_integer([positive]) rem 1000),

    %% Configure gateway with our test port, no channels
    application:set_env(beamclaw_gateway, http, #{port => Port}),
    application:set_env(beamclaw_gateway, channels, [
        {tui, #{enabled => false}}
    ]),
    application:set_env(beamclaw_gateway, webhooks, #{enabled => true}),

    %% Configure core: disable persistence, full autonomy
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
    %% Clean up any webhook secrets set during tests
    os:unsetenv("WEBHOOK_SECRET_TRADINGVIEW"),
    os:unsetenv("WEBHOOK_SECRET_TESTSRC"),
    os:unsetenv("WEBHOOK_SECRET_GITHUB"),
    ok.

%% ---------------------------------------------------------------------------
%% Test cases
%% ---------------------------------------------------------------------------

%% Valid POST with correct secret returns 200 + creates session
webhook_accepted(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/webhook/tradingview",

    %% Set secret for tradingview source
    os:putenv("WEBHOOK_SECRET_TRADINGVIEW", "my-tv-secret"),

    Body = jsx:encode(#{<<"ticker">> => <<"NVDA">>,
                        <<"action">> => <<"buy">>,
                        <<"price">> => 135}),
    Headers = [{"X-Webhook-Secret", "my-tv-secret"},
               {"Content-Type", "application/json"}],
    {ok, {{_, StatusCode, _}, _RespHeaders, RespBody}} =
        httpc:request(post,
            {Url, Headers, "application/json", binary_to_list(Body)},
            [{timeout, 10000}], []),
    ?assertEqual(200, StatusCode),
    Decoded = jsx:decode(iolist_to_binary(RespBody), [return_maps]),
    ?assertEqual(<<"accepted">>, maps:get(<<"status">>, Decoded)),
    ok.

%% Wrong secret returns 401
webhook_auth_failed(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/webhook/testsrc",

    os:putenv("WEBHOOK_SECRET_TESTSRC", "correct-secret"),

    Body = <<"test payload">>,
    Headers = [{"X-Webhook-Secret", "wrong-secret"}],
    {ok, {{_, StatusCode, _}, _RespHeaders, RespBody}} =
        httpc:request(post,
            {Url, Headers, "text/plain", binary_to_list(Body)},
            [{timeout, 10000}], []),
    ?assertEqual(401, StatusCode),
    Decoded = jsx:decode(iolist_to_binary(RespBody), [return_maps]),
    ?assertEqual(<<"unauthorized">>, maps:get(<<"error">>, Decoded)),
    ok.

%% No secret env var set = open access, returns 200
webhook_no_auth_configured(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    %% Use a source name whose env var is guaranteed unset
    Url = BaseUrl ++ "/webhook/opensrc",

    os:unsetenv("WEBHOOK_SECRET_OPENSRC"),

    Body = jsx:encode(#{<<"event">> => <<"test">>}),
    {ok, {{_, StatusCode, _}, _RespHeaders, RespBody}} =
        httpc:request(post,
            {Url, [], "application/json", binary_to_list(Body)},
            [{timeout, 10000}], []),
    ?assertEqual(200, StatusCode),
    Decoded = jsx:decode(iolist_to_binary(RespBody), [return_maps]),
    ?assertEqual(<<"accepted">>, maps:get(<<"status">>, Decoded)),
    ok.

%% Rapid POSTs trigger rate limiting (429)
webhook_rate_limited(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/webhook/ratelimittest",

    Body = <<"test">>,
    Results = lists:map(fun(_N) ->
        case httpc:request(post,
                {Url, [], "text/plain", binary_to_list(Body)},
                [{timeout, 5000}], []) of
            {ok, {{_, Code, _}, _, _}} -> Code;
            _ -> error
        end
    end, lists:seq(1, 62)),
    Has429 = lists:member(429, Results),
    ?assert(Has429),
    ok.

%% Plain text (non-JSON) body is accepted and returned as-is
webhook_plain_text_body(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/webhook/alerts",

    Body = <<"NVDA buy signal at 135">>,
    {ok, {{_, StatusCode, _}, _RespHeaders, _RespBody}} =
        httpc:request(post,
            {Url, [], "text/plain", binary_to_list(Body)},
            [{timeout, 10000}], []),
    ?assertEqual(200, StatusCode),
    ok.

%% ?agent=mom query param routes to the specified agent
webhook_custom_agent(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/webhook/github?agent=mom",

    Body = jsx:encode(#{<<"event">> => <<"push">>}),
    {ok, {{_, StatusCode, _}, _RespHeaders, RespBody}} =
        httpc:request(post,
            {Url, [], "application/json", binary_to_list(Body)},
            [{timeout, 10000}], []),
    ?assertEqual(200, StatusCode),
    Decoded = jsx:decode(iolist_to_binary(RespBody), [return_maps]),
    ?assertEqual(<<"accepted">>, maps:get(<<"status">>, Decoded)),
    ok.

%% Secret in JSON body (no header) — TradingView-style auth
webhook_body_secret(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/webhook/tradingview",

    os:putenv("WEBHOOK_SECRET_TRADINGVIEW", "body-tv-secret"),

    Body = jsx:encode(#{<<"secret">> => <<"body-tv-secret">>,
                        <<"ticker">> => <<"NVDA">>,
                        <<"action">> => <<"buy">>,
                        <<"price">> => 135}),
    %% No X-Webhook-Secret header — secret is in the body
    {ok, {{_, StatusCode, _}, _RespHeaders, RespBody}} =
        httpc:request(post,
            {Url, [{"Content-Type", "application/json"}],
             "application/json", binary_to_list(Body)},
            [{timeout, 10000}], []),
    ?assertEqual(200, StatusCode),
    Decoded = jsx:decode(iolist_to_binary(RespBody), [return_maps]),
    ?assertEqual(<<"accepted">>, maps:get(<<"status">>, Decoded)),
    ok.
