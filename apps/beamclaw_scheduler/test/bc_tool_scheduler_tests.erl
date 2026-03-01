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

-module(bc_tool_scheduler_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% --- definition/0 ---

definition_test() ->
    Def = bc_tool_scheduler:definition(),
    ?assertEqual(<<"scheduler">>, maps:get(name, Def)),
    ?assert(is_binary(maps:get(description, Def))),
    ?assert(is_map(maps:get(parameters, Def))).

%% --- requires_approval/0 ---

requires_approval_test() ->
    ?assert(bc_tool_scheduler:requires_approval()).

%% --- min_autonomy/0 ---

min_autonomy_test() ->
    ?assertEqual(supervised, bc_tool_scheduler:min_autonomy()).

%% --- Validation tests (via execute with scheduler disabled for safety) ---

disabled_returns_error_test() ->
    %% When scheduler is not enabled, execute returns error
    application:set_env(beamclaw_scheduler, enabled, false),
    SessionRef = #bc_session_ref{
        session_id = <<"test-sess">>,
        user_id = <<"test-user">>,
        session_pid = self(),
        autonomy = supervised,
        agent_id = <<"test-agent">>
    },
    Result = bc_tool_scheduler:execute(
        #{<<"action">> => <<"list">>}, SessionRef, #{}),
    ?assertMatch({error, _}, Result).

%% --- Interval parsing integration ---

interval_parse_via_validate_test() ->
    %% bc_sched_interval is used by the tool
    ?assertEqual({ok, 1800000}, bc_sched_interval:parse(<<"30m">>)),
    ?assertEqual({ok, 86400000}, bc_sched_interval:parse(<<"24h">>)).

%% --- ISO 8601 parsing ---

%% We test the internal parse function indirectly through the tool's
%% validate logic. For now, test the interval module directly.

missing_action_test() ->
    application:set_env(beamclaw_scheduler, enabled, true),
    SessionRef = #bc_session_ref{
        session_id = <<"test-sess">>,
        user_id = <<"test-user">>,
        session_pid = self(),
        autonomy = supervised,
        agent_id = <<"test-agent">>
    },
    %% Missing action
    Result = bc_tool_scheduler:execute(#{}, SessionRef, #{}),
    ?assertMatch({error, _}, Result),
    application:set_env(beamclaw_scheduler, enabled, false).

unknown_action_test() ->
    application:set_env(beamclaw_scheduler, enabled, true),
    SessionRef = #bc_session_ref{
        session_id = <<"test-sess">>,
        user_id = <<"test-user">>,
        session_pid = self(),
        autonomy = supervised,
        agent_id = <<"test-agent">>
    },
    Result = bc_tool_scheduler:execute(
        #{<<"action">> => <<"explode">>}, SessionRef, #{}),
    ?assertMatch({error, _}, Result),
    application:set_env(beamclaw_scheduler, enabled, false).
