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

-module(bc_tool_web_search_tests).
-moduledoc "EUnit tests for bc_tool_web_search.".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

session_ref() ->
    #bc_session_ref{
        session_id  = <<"test-session">>,
        user_id     = <<"test-user">>,
        session_pid = self(),
        autonomy    = supervised,
        agent_id    = <<"test-agent">>
    }.

%% ---- definition ----

definition_test() ->
    Def = bc_tool_web_search:definition(),
    ?assertEqual(<<"web_search">>, maps:get(name, Def)),
    ?assertEqual(builtin, maps:get(source, Def)),
    Params = maps:get(parameters, Def),
    ?assertEqual([<<"query">>], maps:get(required, Params)),
    Props = maps:get(properties, Params),
    ?assert(maps:is_key(query, Props)),
    ?assert(maps:is_key(count, Props)),
    ?assert(maps:is_key(freshness, Props)),
    ?assert(maps:is_key(country, Props)).

%% ---- requires_approval ----

requires_approval_test() ->
    ?assertEqual(false, bc_tool_web_search:requires_approval()).

%% ---- min_autonomy ----

min_autonomy_test() ->
    ?assertEqual(read_only, bc_tool_web_search:min_autonomy()).

%% ---- no API key ----

no_api_key_test() ->
    %% Ensure no config is set
    application:unset_env(beamclaw_tools, web_search),
    {error, Msg} = bc_tool_web_search:execute(
        #{<<"query">> => <<"test">>}, session_ref(), #{}),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"not configured">>)).

no_api_key_empty_string_test() ->
    application:set_env(beamclaw_tools, web_search, #{api_key => ""}),
    {error, Msg} = bc_tool_web_search:execute(
        #{<<"query">> => <<"test">>}, session_ref(), #{}),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"not configured">>)),
    application:unset_env(beamclaw_tools, web_search).

%% ---- missing query ----

missing_query_test() ->
    {error, Msg} = bc_tool_web_search:execute(#{}, session_ref(), #{}),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"query">>)).

%% ---- format_results: empty ----

format_results_empty_test() ->
    Result = bc_tool_web_search:format_results(<<"erlang">>, []),
    ?assertNotEqual(nomatch, binary:match(Result, <<"No results found">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"erlang">>)).

%% ---- format_results: single result ----

format_results_single_test() ->
    Results = [#{<<"title">> => <<"Erlang Official">>,
                 <<"url">> => <<"https://www.erlang.org">>,
                 <<"description">> => <<"The Erlang Programming Language">>}],
    Formatted = bc_tool_web_search:format_results(<<"erlang">>, Results),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Erlang Official">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"https://www.erlang.org">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"The Erlang Programming Language">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"(1 result)">>)).

%% ---- format_results: multiple results ----

format_results_multiple_test() ->
    Results = [#{<<"title">> => <<"Result One">>,
                 <<"url">> => <<"https://example.com/1">>,
                 <<"description">> => <<"First result">>},
               #{<<"title">> => <<"Result Two">>,
                 <<"url">> => <<"https://example.com/2">>,
                 <<"description">> => <<"Second result">>},
               #{<<"title">> => <<"Result Three">>,
                 <<"url">> => <<"https://example.com/3">>,
                 <<"description">> => <<"Third result">>}],
    Formatted = bc_tool_web_search:format_results(<<"test query">>, Results),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"test query">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Result One">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Result Three">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"(3 results)">>)).

%% ---- format_results: missing fields ----

format_results_missing_fields_test() ->
    Results = [#{<<"url">> => <<"https://example.com">>}],
    Formatted = bc_tool_web_search:format_results(<<"q">>, Results),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"(no title)">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"(1 result)">>)).
