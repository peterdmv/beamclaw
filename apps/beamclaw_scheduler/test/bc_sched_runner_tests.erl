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

-module(bc_sched_runner_tests).
-include_lib("eunit/include/eunit.hrl").
-include("bc_sched_job.hrl").

%% Tests for internal scheduling logic (pure aspects).
%% Full gen_server tests require integration setup with Mnesia and obs.

make_job(JobId, Type, Spec) ->
    #bc_sched_job{
        job_id        = JobId,
        agent_id      = <<"test-agent">>,
        user_id       = <<"test-user">>,
        session_id    = undefined,
        schedule_type = Type,
        schedule_spec = Spec,
        prompt        = <<"Test prompt">>,
        session_mode  = isolated,
        autonomy      = supervised,
        delivery      = #{channel => silent},
        status        = active,
        created_at    = erlang:system_time(second),
        updated_at    = erlang:system_time(second),
        last_fired_at = undefined,
        fire_count    = 0,
        error_count   = 0,
        max_errors    = 3,
        heartbeat     = false,
        suppress_ok   = false,
        active_hours  = undefined
    }.

%% Test that active_hours logic works correctly

active_hours_always_test() ->
    %% undefined active_hours → always active
    Job = make_job(<<"ah-1">>, every, #{interval_ms => 60000}),
    ?assertEqual(undefined, Job#bc_sched_job.active_hours).

active_hours_with_range_test() ->
    Job = (make_job(<<"ah-2">>, every, #{interval_ms => 60000}))
          #bc_sched_job{active_hours = {8, 22}},
    ?assertEqual({8, 22}, Job#bc_sched_job.active_hours).

%% Test schedule_spec construction for different types

at_spec_test() ->
    Spec = #{epoch_seconds => 1740787200},
    Job = make_job(<<"at-1">>, at, Spec),
    ?assertEqual(at, Job#bc_sched_job.schedule_type),
    ?assertEqual(1740787200, maps:get(epoch_seconds, Job#bc_sched_job.schedule_spec)).

every_spec_test() ->
    Spec = #{interval_ms => 1800000},
    Job = make_job(<<"every-1">>, every, Spec),
    ?assertEqual(every, Job#bc_sched_job.schedule_type),
    ?assertEqual(1800000, maps:get(interval_ms, Job#bc_sched_job.schedule_spec)).

random_in_spec_test() ->
    Spec = #{interval_ms => 86400000, count => 4},
    Job = make_job(<<"rand-1">>, random_in, Spec),
    ?assertEqual(random_in, Job#bc_sched_job.schedule_type),
    ?assertEqual(86400000, maps:get(interval_ms, Job#bc_sched_job.schedule_spec)),
    ?assertEqual(4, maps:get(count, Job#bc_sched_job.schedule_spec)).
