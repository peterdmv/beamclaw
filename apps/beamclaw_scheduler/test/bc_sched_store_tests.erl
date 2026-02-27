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

-module(bc_sched_store_tests).
-include_lib("eunit/include/eunit.hrl").
-include("bc_sched_job.hrl").

%% Test setup: ensure Mnesia is running with ram_copies
setup() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        _   ->
            mnesia:start(),
            ok
    end,
    %% Drop existing table if schema mismatch (e.g., from prior test runs)
    catch mnesia:delete_table(bc_sched_jobs),
    bc_sched_store:init_table().

make_job(JobId) ->
    #bc_sched_job{
        job_id        = JobId,
        agent_id      = <<"test-agent">>,
        user_id       = <<"test-user">>,
        session_id    = undefined,
        schedule_type = every,
        schedule_spec = #{interval_ms => 60000},
        prompt        = <<"Hello">>,
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

%% --- Tests ---

save_and_load_test() ->
    setup(),
    Job = make_job(<<"test-save-load">>),
    ok = bc_sched_store:save(Job),
    {ok, Loaded} = bc_sched_store:load(<<"test-save-load">>),
    ?assertEqual(<<"test-save-load">>, Loaded#bc_sched_job.job_id),
    ?assertEqual(<<"test-agent">>, Loaded#bc_sched_job.agent_id),
    ?assertEqual(every, Loaded#bc_sched_job.schedule_type).

load_not_found_test() ->
    setup(),
    ?assertEqual({error, not_found}, bc_sched_store:load(<<"nonexistent">>)).

delete_test() ->
    setup(),
    Job = make_job(<<"test-delete">>),
    ok = bc_sched_store:save(Job),
    ok = bc_sched_store:delete(<<"test-delete">>),
    ?assertEqual({error, not_found}, bc_sched_store:load(<<"test-delete">>)).

list_active_test() ->
    setup(),
    J1 = make_job(<<"test-list-active-1">>),
    J2 = (make_job(<<"test-list-active-2">>))#bc_sched_job{status = paused},
    J3 = make_job(<<"test-list-active-3">>),
    ok = bc_sched_store:save(J1),
    ok = bc_sched_store:save(J2),
    ok = bc_sched_store:save(J3),
    Active = bc_sched_store:list_active(),
    ActiveIds = [J#bc_sched_job.job_id || J <- Active],
    ?assert(lists:member(<<"test-list-active-1">>, ActiveIds)),
    ?assert(not lists:member(<<"test-list-active-2">>, ActiveIds)),
    ?assert(lists:member(<<"test-list-active-3">>, ActiveIds)).

list_by_agent_test() ->
    setup(),
    J1 = make_job(<<"test-by-agent-1">>),
    J2 = (make_job(<<"test-by-agent-2">>))#bc_sched_job{agent_id = <<"other-agent">>},
    ok = bc_sched_store:save(J1),
    ok = bc_sched_store:save(J2),
    ByAgent = bc_sched_store:list_by_agent(<<"test-agent">>),
    ByAgentIds = [J#bc_sched_job.job_id || J <- ByAgent],
    ?assert(lists:member(<<"test-by-agent-1">>, ByAgentIds)),
    ?assert(not lists:member(<<"test-by-agent-2">>, ByAgentIds)).

update_status_test() ->
    setup(),
    Job = make_job(<<"test-status">>),
    ok = bc_sched_store:save(Job),
    ok = bc_sched_store:update_status(<<"test-status">>, paused),
    {ok, Updated} = bc_sched_store:load(<<"test-status">>),
    ?assertEqual(paused, Updated#bc_sched_job.status).

update_after_fire_test() ->
    setup(),
    Job = make_job(<<"test-fire">>),
    ok = bc_sched_store:save(Job),
    Now = erlang:system_time(second),
    ok = bc_sched_store:update_after_fire(<<"test-fire">>, Now),
    {ok, Updated} = bc_sched_store:load(<<"test-fire">>),
    ?assertEqual(1, Updated#bc_sched_job.fire_count),
    ?assertEqual(0, Updated#bc_sched_job.error_count),
    ?assertEqual(Now, Updated#bc_sched_job.last_fired_at).

update_error_test() ->
    setup(),
    Job = make_job(<<"test-error">>),
    ok = bc_sched_store:save(Job),
    Now = erlang:system_time(second),
    {ok, active} = bc_sched_store:update_error(<<"test-error">>, Now),
    {ok, J1} = bc_sched_store:load(<<"test-error">>),
    ?assertEqual(1, J1#bc_sched_job.error_count).

auto_pause_on_max_errors_test() ->
    setup(),
    Job = (make_job(<<"test-auto-pause">>))#bc_sched_job{error_count = 2, max_errors = 3},
    ok = bc_sched_store:save(Job),
    Now = erlang:system_time(second),
    {ok, paused} = bc_sched_store:update_error(<<"test-auto-pause">>, Now),
    {ok, Updated} = bc_sched_store:load(<<"test-auto-pause">>),
    ?assertEqual(paused, Updated#bc_sched_job.status),
    ?assertEqual(3, Updated#bc_sched_job.error_count).
