%%
%% Copyright The BeamClaw Authors 2026, All Rights Reserved.
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

-module(bc_a2a_http_integration_SUITE).
-moduledoc """
A2A HTTP endpoint integration tests (Tier 2).

Starts the full gateway with A2A routes and exercises the Agent Card
discovery and JSON-RPC endpoints end-to-end.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([agent_card_endpoint/1,
         a2a_jsonrpc_method_not_found/1,
         a2a_malformed_json/1,
         a2a_rate_limit/1,
         a2a_auth_missing_header/1,
         a2a_auth_invalid_token/1,
         a2a_auth_valid_token/1,
         a2a_no_auth_when_unconfigured/1,
         agent_card_shows_auth_when_configured/1]).

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [agent_card_endpoint,
     a2a_jsonrpc_method_not_found,
     a2a_malformed_json,
     a2a_auth_missing_header,
     a2a_auth_invalid_token,
     a2a_auth_valid_token,
     a2a_no_auth_when_unconfigured,
     agent_card_shows_auth_when_configured,
     a2a_rate_limit].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    Port = 19000 + (erlang:unique_integer([positive]) rem 1000),

    application:set_env(beamclaw_gateway, http, #{port => Port}),
    application:set_env(beamclaw_gateway, channels, [
        {tui, #{enabled => false}}
    ]),

    application:set_env(beamclaw_core, session_persistence, false),
    application:set_env(beamclaw_core, autonomy_level, full),
    application:set_env(beamclaw_core, agentic_loop,
                        #{compaction_threshold => 9999,
                          memory_flush => false}),

    os:putenv("OPENROUTER_API_KEY", "test-dummy-key"),

    {ok, _} = application:ensure_all_started(inets),
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

init_per_testcase(TC, Config) when TC =:= a2a_auth_missing_header;
                                   TC =:= a2a_auth_invalid_token;
                                   TC =:= a2a_auth_valid_token;
                                   TC =:= agent_card_shows_auth_when_configured ->
    os:putenv("A2A_BEARER_TOKEN", "ct-test-secret"),
    Config;
init_per_testcase(a2a_no_auth_when_unconfigured, Config) ->
    os:unsetenv("A2A_BEARER_TOKEN"),
    Config;
init_per_testcase(_TC, Config) ->
    os:unsetenv("A2A_BEARER_TOKEN"),
    Config.

end_per_testcase(_TC, _Config) ->
    os:unsetenv("A2A_BEARER_TOKEN"),
    %% Reset rate limiter to avoid test ordering issues
    catch ets:delete_all_objects(bc_rate_limiter),
    ok.

%% ---------------------------------------------------------------------------
%% Test cases
%% ---------------------------------------------------------------------------

agent_card_endpoint(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/.well-known/agent.json",
    {ok, {{_, 200, _}, _Headers, Body}} = httpc:request(get, {Url, []}, [], []),
    Decoded = jsx:decode(iolist_to_binary(Body), [return_maps]),
    ?assert(maps:is_key(<<"name">>, Decoded)),
    ?assert(maps:is_key(<<"version">>, Decoded)),
    ?assert(maps:is_key(<<"capabilities">>, Decoded)),
    ?assert(maps:is_key(<<"skills">>, Decoded)),
    ok.

a2a_jsonrpc_method_not_found(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/a2a",
    ReqBody = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"nonexistent/method">>
    }),
    {ok, {{_, 200, _}, _Headers, RespBody}} =
        httpc:request(post,
            {Url, [], "application/json", binary_to_list(ReqBody)},
            [{timeout, 5000}], []),
    Decoded = jsx:decode(iolist_to_binary(RespBody), [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ok.

a2a_malformed_json(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/a2a",
    {ok, {{_, 400, _}, _Headers, RespBody}} =
        httpc:request(post,
            {Url, [], "application/json", "not valid json{{{"},
            [{timeout, 5000}], []),
    Decoded = jsx:decode(iolist_to_binary(RespBody), [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32700, maps:get(<<"code">>, Error)),
    ok.

a2a_rate_limit(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/.well-known/agent.json",
    %% Flood with requests to hit rate limit (default 60 per 60s window)
    Results = lists:map(fun(_N) ->
        case httpc:request(get, {Url, []}, [{timeout, 5000}], []) of
            {ok, {{_, Code, _}, _, _}} -> Code;
            _ -> error
        end
    end, lists:seq(1, 62)),
    Has429 = lists:member(429, Results),
    ?assert(Has429),
    ok.

%% --- Auth test cases ---

a2a_auth_missing_header(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/a2a",
    ReqBody = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
                           <<"method">> => <<"tasks/list">>}),
    {ok, {{_, 401, _}, Headers, _RespBody}} =
        httpc:request(post,
            {Url, [], "application/json", binary_to_list(ReqBody)},
            [{timeout, 5000}], []),
    %% RFC 6750: WWW-Authenticate header present
    ?assertNotEqual(false, lists:keyfind("www-authenticate", 1, Headers)),
    ok.

a2a_auth_invalid_token(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/a2a",
    ReqBody = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
                           <<"method">> => <<"tasks/list">>}),
    {ok, {{_, 401, _}, _, RespBody}} =
        httpc:request(post,
            {Url, [{"Authorization", "Bearer wrong-token"}],
             "application/json", binary_to_list(ReqBody)},
            [{timeout, 5000}], []),
    Decoded = jsx:decode(iolist_to_binary(RespBody), [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32000, maps:get(<<"code">>, Error)),
    ok.

a2a_auth_valid_token(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/a2a",
    ReqBody = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
                           <<"method">> => <<"tasks/list">>}),
    {ok, {{_, 200, _}, _, _RespBody}} =
        httpc:request(post,
            {Url, [{"Authorization", "Bearer ct-test-secret"}],
             "application/json", binary_to_list(ReqBody)},
            [{timeout, 5000}], []),
    ok.

a2a_no_auth_when_unconfigured(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/a2a",
    ReqBody = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
                           <<"method">> => <<"tasks/list">>}),
    %% No Authorization header, no token configured → should pass through
    {ok, {{_, 200, _}, _, _RespBody}} =
        httpc:request(post,
            {Url, [], "application/json", binary_to_list(ReqBody)},
            [{timeout, 5000}], []),
    ok.

agent_card_shows_auth_when_configured(Config) ->
    BaseUrl = proplists:get_value(base_url, Config),
    Url = BaseUrl ++ "/.well-known/agent.json",
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, []}, [], []),
    Decoded = jsx:decode(iolist_to_binary(Body), [return_maps]),
    Auth = maps:get(<<"authentication">>, Decoded),
    ?assert(is_map(Auth)),
    ?assertEqual([<<"bearer">>], maps:get(<<"schemes">>, Auth)),
    ok.
