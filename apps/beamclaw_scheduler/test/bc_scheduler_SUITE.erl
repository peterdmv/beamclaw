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

-module(bc_scheduler_SUITE).
-moduledoc """
Scheduler integration tests (Tier 2).

Tests the full scheduler flow: tool actions, timer fire → executor dispatch →
session LLM response → delivery/suppression. Uses mock providers and shared-mode
sessions pre-created before jobs fire to avoid real HTTP calls.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").
-include_lib("beamclaw_scheduler/include/bc_sched_job.hrl").

%% CT callbacks
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases — tool_actions group
-export([tool_create_every_job/1, tool_create_at_job/1,
         tool_list_jobs/1, tool_cancel_job/1,
         tool_pause_resume_job/1]).

%% Test cases — timer_fire group
-export([fire_every_dispatches/1, fire_at_one_shot/1]).

%% Test cases — heartbeat group
-export([heartbeat_ok_suppressed/1, heartbeat_alert_delivered/1]).

%% Test cases — error_handling group
-export([auto_pause_on_errors/1]).

%% Test cases — concurrency group
-export([multiple_concurrent_jobs/1]).

suite() ->
    [{timetrap, {seconds, 30}}].

groups() ->
    [{tool_actions,   [sequence], [tool_create_every_job, tool_create_at_job,
                                    tool_list_jobs, tool_cancel_job,
                                    tool_pause_resume_job]},
     {timer_fire,     [],         [fire_every_dispatches, fire_at_one_shot]},
     {heartbeat,      [],         [heartbeat_ok_suppressed, heartbeat_alert_delivered]},
     {error_handling, [],         [auto_pause_on_errors]},
     {concurrency,    [],         [multiple_concurrent_jobs]}].

all() ->
    [{group, tool_actions},
     {group, timer_fire},
     {group, heartbeat},
     {group, error_handling},
     {group, concurrency}].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    %% Configure core for testing
    application:set_env(beamclaw_core, session_persistence, false),
    application:set_env(beamclaw_core, agentic_loop,
        #{compaction_threshold => 9999, memory_flush => false}),
    application:set_env(beamclaw_core, autonomy_level, full),

    %% Configure scheduler
    application:set_env(beamclaw_scheduler, enabled, true),
    application:set_env(beamclaw_scheduler, max_errors, 3),
    application:set_env(beamclaw_scheduler, max_jobs_per_agent, 50),
    application:set_env(beamclaw_scheduler, default_autonomy, full),
    application:set_env(beamclaw_scheduler, heartbeat,
        #{suppress_ok => true, active_hours => undefined,
          default_interval_ms => 1800000}),

    %% Dummy API key so provider init doesn't fail
    os:putenv("OPENROUTER_API_KEY", "test-dummy-key"),

    %% Start all apps (scheduler pulls in core and deps)
    {ok, Started} = application:ensure_all_started(beamclaw_scheduler),

    %% Ensure clean Mnesia table
    catch mnesia:delete_table(bc_sched_jobs),
    bc_sched_store:init_table(),

    [{started_apps, Started} | Config].

end_per_suite(Config) ->
    Started = proplists:get_value(started_apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Started)),
    os:unsetenv("OPENROUTER_API_KEY"),
    ok.

%% ---------------------------------------------------------------------------
%% Group lifecycle
%% ---------------------------------------------------------------------------

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% Per-testcase lifecycle
%% ---------------------------------------------------------------------------

init_per_testcase(_TC, Config) ->
    N = erlang:unique_integer([positive]),
    SessionId = iolist_to_binary(
        ["ct-sched-", integer_to_list(N)]),
    JobId = iolist_to_binary(
        ["ct-job-", integer_to_list(N)]),
    AgentId = <<"default">>,
    [{session_id, SessionId}, {job_id, JobId}, {agent_id, AgentId} | Config].

end_per_testcase(_TC, Config) ->
    %% Best-effort cleanup: cancel job in runner, delete from Mnesia
    JobId = proplists:get_value(job_id, Config),
    catch bc_sched_runner:cancel(JobId),
    catch bc_sched_store:delete(JobId),
    ok.

%% ===========================================================================
%% Group: tool_actions — test bc_tool_scheduler:execute/3 actions
%% ===========================================================================

tool_create_every_job(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    AgentId = proplists:get_value(agent_id, Config),
    SessionRef = #bc_session_ref{
        session_id = SessionId,
        user_id    = <<"ct-user">>,
        session_pid = self(),
        autonomy   = full,
        agent_id   = AgentId
    },
    Args = #{
        <<"action">>        => <<"create">>,
        <<"schedule_type">>  => <<"every">>,
        <<"interval">>       => <<"10s">>,
        <<"prompt">>         => <<"Test every job">>,
        <<"channel">>        => <<"silent">>
    },
    {ok, Result} = bc_tool_scheduler:execute(Args, SessionRef, #{}),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Scheduled job created">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"every">>)),

    %% Verify job in Mnesia — find by agent
    Jobs = bc_sched_store:list_by_agent(AgentId),
    EveryJobs = [J || J <- Jobs,
                      J#bc_sched_job.schedule_type =:= every,
                      J#bc_sched_job.prompt =:= <<"Test every job">>],
    ?assert(length(EveryJobs) >= 1),
    [Job | _] = EveryJobs,
    ?assertEqual(every, Job#bc_sched_job.schedule_type),
    ?assertEqual(#{interval_ms => 10000}, Job#bc_sched_job.schedule_spec),
    ?assertEqual(active, Job#bc_sched_job.status),

    %% Clean up this job (sequence group shares state)
    bc_sched_runner:cancel(Job#bc_sched_job.job_id),
    bc_sched_store:delete(Job#bc_sched_job.job_id),
    ok.

tool_create_at_job(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    AgentId = proplists:get_value(agent_id, Config),
    SessionRef = #bc_session_ref{
        session_id = SessionId,
        user_id    = <<"ct-user">>,
        session_pid = self(),
        autonomy   = full,
        agent_id   = AgentId
    },
    FutureEpoch = erlang:system_time(second) + 3600,
    {{Y, Mo, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(
        FutureEpoch + 62167219200),
    DatetimeStr = iolist_to_binary(io_lib:format(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Y, Mo, D, H, Mi, S])),
    Args = #{
        <<"action">>        => <<"create">>,
        <<"schedule_type">>  => <<"at">>,
        <<"datetime">>       => DatetimeStr,
        <<"prompt">>         => <<"Test at job">>,
        <<"channel">>        => <<"silent">>
    },
    {ok, Result} = bc_tool_scheduler:execute(Args, SessionRef, #{}),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Scheduled job created">>)),

    %% Verify job stored with correct epoch
    Jobs = bc_sched_store:list_by_agent(AgentId),
    AtJobs = [J || J <- Jobs,
                   J#bc_sched_job.schedule_type =:= at,
                   J#bc_sched_job.prompt =:= <<"Test at job">>],
    ?assert(length(AtJobs) >= 1),
    [Job | _] = AtJobs,
    ?assertEqual(at, Job#bc_sched_job.schedule_type),
    #{epoch_seconds := StoredEpoch} = Job#bc_sched_job.schedule_spec,
    ?assertEqual(FutureEpoch, StoredEpoch),

    %% Clean up
    bc_sched_runner:cancel(Job#bc_sched_job.job_id),
    bc_sched_store:delete(Job#bc_sched_job.job_id),
    ok.

tool_list_jobs(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    AgentId = proplists:get_value(agent_id, Config),
    SessionRef = #bc_session_ref{
        session_id = SessionId,
        user_id    = <<"ct-user">>,
        session_pid = self(),
        autonomy   = full,
        agent_id   = AgentId
    },
    %% Create two jobs
    Args1 = #{<<"action">> => <<"create">>, <<"schedule_type">> => <<"every">>,
              <<"interval">> => <<"1h">>, <<"prompt">> => <<"List test A">>,
              <<"channel">> => <<"silent">>},
    {ok, _} = bc_tool_scheduler:execute(Args1, SessionRef, #{}),
    Args2 = #{<<"action">> => <<"create">>, <<"schedule_type">> => <<"every">>,
              <<"interval">> => <<"2h">>, <<"prompt">> => <<"List test B">>,
              <<"channel">> => <<"silent">>},
    {ok, _} = bc_tool_scheduler:execute(Args2, SessionRef, #{}),

    %% List them
    {ok, ListResult} = bc_tool_scheduler:execute(
        #{<<"action">> => <<"list">>}, SessionRef, #{}),
    ?assertNotEqual(nomatch, binary:match(ListResult, <<"List test A">>)),
    ?assertNotEqual(nomatch, binary:match(ListResult, <<"List test B">>)),

    %% Clean up
    Jobs = bc_sched_store:list_by_agent(AgentId),
    lists:foreach(fun(J) ->
        bc_sched_runner:cancel(J#bc_sched_job.job_id),
        bc_sched_store:delete(J#bc_sched_job.job_id)
    end, Jobs),
    ok.

tool_cancel_job(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    AgentId = proplists:get_value(agent_id, Config),
    SessionRef = #bc_session_ref{
        session_id = SessionId,
        user_id    = <<"ct-user">>,
        session_pid = self(),
        autonomy   = full,
        agent_id   = AgentId
    },
    %% Create a job
    {ok, _CreateResult} = bc_tool_scheduler:execute(
        #{<<"action">> => <<"create">>, <<"schedule_type">> => <<"every">>,
          <<"interval">> => <<"1h">>, <<"prompt">> => <<"Cancel test">>,
          <<"channel">> => <<"silent">>},
        SessionRef, #{}),
    %% Extract job ID from result
    Jobs = bc_sched_store:list_by_agent(AgentId),
    [Job | _] = [J || J <- Jobs, J#bc_sched_job.prompt =:= <<"Cancel test">>],
    CreatedJobId = Job#bc_sched_job.job_id,

    %% Cancel it
    {ok, CancelResult} = bc_tool_scheduler:execute(
        #{<<"action">> => <<"cancel">>, <<"job_id">> => CreatedJobId},
        SessionRef, #{}),
    ?assertNotEqual(nomatch, binary:match(CancelResult, <<"cancelled">>)),

    %% Verify status is completed
    {ok, CancelledJob} = bc_sched_store:load(CreatedJobId),
    ?assertEqual(completed, CancelledJob#bc_sched_job.status),

    %% Clean up
    bc_sched_store:delete(CreatedJobId),
    ok.

tool_pause_resume_job(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    AgentId = proplists:get_value(agent_id, Config),
    SessionRef = #bc_session_ref{
        session_id = SessionId,
        user_id    = <<"ct-user">>,
        session_pid = self(),
        autonomy   = full,
        agent_id   = AgentId
    },
    %% Create a job
    {ok, _} = bc_tool_scheduler:execute(
        #{<<"action">> => <<"create">>, <<"schedule_type">> => <<"every">>,
          <<"interval">> => <<"1h">>, <<"prompt">> => <<"Pause test">>,
          <<"channel">> => <<"silent">>},
        SessionRef, #{}),
    Jobs = bc_sched_store:list_by_agent(AgentId),
    [Job | _] = [J || J <- Jobs, J#bc_sched_job.prompt =:= <<"Pause test">>],
    CreatedJobId = Job#bc_sched_job.job_id,

    %% Pause
    {ok, PauseResult} = bc_tool_scheduler:execute(
        #{<<"action">> => <<"pause">>, <<"job_id">> => CreatedJobId},
        SessionRef, #{}),
    ?assertNotEqual(nomatch, binary:match(PauseResult, <<"paused">>)),
    {ok, PausedJob} = bc_sched_store:load(CreatedJobId),
    ?assertEqual(paused, PausedJob#bc_sched_job.status),

    %% Resume
    {ok, ResumeResult} = bc_tool_scheduler:execute(
        #{<<"action">> => <<"resume">>, <<"job_id">> => CreatedJobId},
        SessionRef, #{}),
    ?assertNotEqual(nomatch, binary:match(ResumeResult, <<"resumed">>)),
    {ok, ResumedJob} = bc_sched_store:load(CreatedJobId),
    ?assertEqual(active, ResumedJob#bc_sched_job.status),

    %% Clean up
    bc_sched_runner:cancel(CreatedJobId),
    bc_sched_store:delete(CreatedJobId),
    ok.

%% ===========================================================================
%% Group: timer_fire — timer fire → executor → session → history
%% ===========================================================================

fire_every_dispatches(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    JobId = proplists:get_value(job_id, Config),

    %% Pre-create session with mock provider
    create_mock_session(SessionId, bc_provider_smoke_mock),

    %% Build and save job (shared mode, session pre-exists)
    Job = make_test_job(JobId, SessionId, #{
        schedule_type => every,
        schedule_spec => #{interval_ms => 500},
        prompt        => <<"Scheduled hello">>,
        delivery      => #{channel => silent}
    }),
    ok = bc_sched_store:save(Job),
    bc_sched_runner:schedule(Job),

    %% Wait for at least one fire
    ok = wait_for_job_fire(JobId, 1, 5000),

    %% Verify fire metadata
    {ok, FiredJob} = bc_sched_store:load(JobId),
    ?assert(FiredJob#bc_sched_job.fire_count >= 1),
    ?assertEqual(0, FiredJob#bc_sched_job.error_count),
    ?assertNotEqual(undefined, FiredJob#bc_sched_job.last_fired_at),

    %% Verify session history contains the assistant response
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),
    ok = wait_for_history(SessionPid, fun(History) ->
        AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
        length(AssistantMsgs) >= 1
    end, 5000),
    ok.

fire_at_one_shot(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    JobId = proplists:get_value(job_id, Config),

    %% Pre-create session with mock provider
    create_mock_session(SessionId, bc_provider_smoke_mock),

    %% Schedule an "at" job 1 second from now
    NowEpoch = erlang:system_time(second),
    Job = make_test_job(JobId, SessionId, #{
        schedule_type => at,
        schedule_spec => #{epoch_seconds => NowEpoch + 1},
        prompt        => <<"One-shot test">>,
        delivery      => #{channel => silent}
    }),
    ok = bc_sched_store:save(Job),
    bc_sched_runner:schedule(Job),

    %% Wait for fire
    ok = wait_for_job_fire(JobId, 1, 5000),

    %% Verify it fired exactly once (at jobs get status=completed)
    {ok, FiredJob} = bc_sched_store:load(JobId),
    ?assertEqual(1, FiredJob#bc_sched_job.fire_count),

    %% Wait a bit more and verify no re-fire
    timer:sleep(2000),
    {ok, StillJob} = bc_sched_store:load(JobId),
    ?assertEqual(1, StillJob#bc_sched_job.fire_count),
    ?assertEqual(completed, StillJob#bc_sched_job.status),
    ok.

%% ===========================================================================
%% Group: heartbeat — suppression and delivery
%% ===========================================================================

heartbeat_ok_suppressed(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    JobId = proplists:get_value(job_id, Config),

    %% Pre-create session with heartbeat OK mock
    create_mock_session(SessionId, bc_provider_heartbeat_ok_mock),

    Job = make_test_job(JobId, SessionId, #{
        schedule_type => every,
        schedule_spec => #{interval_ms => 500},
        prompt        => <<"Check system health">>,
        heartbeat     => true,
        suppress_ok   => true,
        delivery      => #{channel => silent}
    }),
    ok = bc_sched_store:save(Job),
    bc_sched_runner:schedule(Job),

    %% Wait for at least one fire
    ok = wait_for_job_fire(JobId, 1, 5000),

    %% Verify the fire was successful (no error_count increase)
    {ok, FiredJob} = bc_sched_store:load(JobId),
    ?assert(FiredJob#bc_sched_job.fire_count >= 1),
    ?assertEqual(0, FiredJob#bc_sched_job.error_count),

    %% Verify session history contains "HEARTBEAT_OK"
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),
    ok = wait_for_history(SessionPid, fun(History) ->
        AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
        lists:any(fun(M) ->
            M#bc_message.content =:= <<"HEARTBEAT_OK">>
        end, AssistantMsgs)
    end, 5000),
    ok.

heartbeat_alert_delivered(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    JobId = proplists:get_value(job_id, Config),

    %% Pre-create session with heartbeat alert mock
    create_mock_session(SessionId, bc_provider_heartbeat_alert_mock),

    Job = make_test_job(JobId, SessionId, #{
        schedule_type => every,
        schedule_spec => #{interval_ms => 500},
        prompt        => <<"Check disk usage">>,
        heartbeat     => true,
        suppress_ok   => true,
        delivery      => #{channel => silent}
    }),
    ok = bc_sched_store:save(Job),
    bc_sched_runner:schedule(Job),

    %% Wait for fire
    ok = wait_for_job_fire(JobId, 1, 5000),

    %% Verify no errors
    {ok, FiredJob} = bc_sched_store:load(JobId),
    ?assert(FiredJob#bc_sched_job.fire_count >= 1),
    ?assertEqual(0, FiredJob#bc_sched_job.error_count),

    %% Verify session history contains the alert text
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),
    ok = wait_for_history(SessionPid, fun(History) ->
        AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
        lists:any(fun(M) ->
            case M#bc_message.content of
                undefined -> false;
                Content -> binary:match(Content, <<"WARNING">>) =/= nomatch
            end
        end, AssistantMsgs)
    end, 5000),
    ok.

%% ===========================================================================
%% Group: error_handling — auto-pause on consecutive failures
%% ===========================================================================

auto_pause_on_errors(Config) ->
    JobId = proplists:get_value(job_id, Config),
    SessionId = proplists:get_value(session_id, Config),

    %% Create a job with max_errors=2 for fast auto-pause
    Job = make_test_job(JobId, SessionId, #{
        schedule_type => every,
        schedule_spec => #{interval_ms => 60000},
        prompt        => <<"Error test">>,
        max_errors    => 2,
        delivery      => #{channel => silent}
    }),
    ok = bc_sched_store:save(Job),

    %% Simulate two consecutive errors
    Now = erlang:system_time(second),
    {ok, active} = bc_sched_store:update_error(JobId, Now),
    {ok, paused} = bc_sched_store:update_error(JobId, Now + 1),

    %% Verify auto-paused
    {ok, PausedJob} = bc_sched_store:load(JobId),
    ?assertEqual(paused, PausedJob#bc_sched_job.status),
    ?assertEqual(2, PausedJob#bc_sched_job.error_count),

    %% Resume and verify it reactivates
    ok = bc_sched_runner:resume(JobId),
    {ok, ResumedJob} = bc_sched_store:load(JobId),
    ?assertEqual(active, ResumedJob#bc_sched_job.status),

    %% Clean up
    bc_sched_runner:cancel(JobId),
    ok.

%% ===========================================================================
%% Group: concurrency — multiple jobs firing simultaneously
%% ===========================================================================

multiple_concurrent_jobs(_Config) ->
    %% Create 3 sessions and 3 jobs
    Specs = lists:map(fun(I) ->
        N = erlang:unique_integer([positive]),
        SId = iolist_to_binary(
            ["ct-conc-", integer_to_list(I), "-", integer_to_list(N)]),
        JId = iolist_to_binary(
            ["ct-conc-job-", integer_to_list(I), "-", integer_to_list(N)]),
        Prompt = iolist_to_binary(
            ["Concurrent test ", integer_to_list(I)]),
        create_mock_session(SId, bc_provider_smoke_mock),
        Job = make_test_job(JId, SId, #{
            schedule_type => every,
            schedule_spec => #{interval_ms => 500},
            prompt        => Prompt,
            delivery      => #{channel => silent}
        }),
        ok = bc_sched_store:save(Job),
        bc_sched_runner:schedule(Job),
        {SId, JId, Prompt}
    end, [1, 2, 3]),

    %% Wait for all 3 jobs to fire at least once
    lists:foreach(fun({_SId, JId, _Prompt}) ->
        ok = wait_for_job_fire(JId, 1, 5000)
    end, Specs),

    %% Verify each session has an assistant response in history
    lists:foreach(fun({SId, _JId, _Prompt}) ->
        {ok, SessionPid} = bc_session_registry:lookup(SId),
        ok = wait_for_history(SessionPid, fun(History) ->
            AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
            length(AssistantMsgs) >= 1
        end, 5000)
    end, Specs),

    %% Clean up
    lists:foreach(fun({_SId, JId, _Prompt}) ->
        bc_sched_runner:cancel(JId),
        bc_sched_store:delete(JId)
    end, Specs),
    ok.

%% ===========================================================================
%% Helpers
%% ===========================================================================

%% Build a test job record with sensible defaults and overrides.
make_test_job(JobId, SessionId, Overrides) ->
    Now = erlang:system_time(second),
    #bc_sched_job{
        job_id        = JobId,
        agent_id      = maps:get(agent_id, Overrides, <<"default">>),
        user_id       = maps:get(user_id, Overrides, <<"ct-user">>),
        session_id    = SessionId,
        schedule_type = maps:get(schedule_type, Overrides, every),
        schedule_spec = maps:get(schedule_spec, Overrides, #{interval_ms => 1000}),
        prompt        = maps:get(prompt, Overrides, <<"Test prompt">>),
        session_mode  = maps:get(session_mode, Overrides, shared),
        autonomy      = maps:get(autonomy, Overrides, full),
        delivery      = maps:get(delivery, Overrides, #{channel => silent}),
        status        = maps:get(status, Overrides, active),
        created_at    = Now,
        updated_at    = Now,
        last_fired_at = undefined,
        fire_count    = 0,
        error_count   = 0,
        max_errors    = maps:get(max_errors, Overrides, 3),
        heartbeat     = maps:get(heartbeat, Overrides, false),
        suppress_ok   = maps:get(suppress_ok, Overrides, false),
        active_hours  = maps:get(active_hours, Overrides, undefined)
    }.

%% Create a session with the given mock provider.
create_mock_session(SessionId, ProviderMod) ->
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => ProviderMod,
        autonomy     => full
    },
    {ok, _SupPid} = bc_sessions_sup:start_session(SessionConfig),
    ok.

%% Poll bc_sched_store:load/1 until fire_count >= MinCount.
wait_for_job_fire(JobId, MinCount, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_job_fire_loop(JobId, MinCount, Deadline).

wait_for_job_fire_loop(JobId, MinCount, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= Deadline of
        true ->
            {ok, Job} = bc_sched_store:load(JobId),
            ct:fail({timeout_waiting_for_fire,
                     {job_id, JobId},
                     {fire_count, Job#bc_sched_job.fire_count},
                     {status, Job#bc_sched_job.status},
                     {expected_min, MinCount}});
        false ->
            case bc_sched_store:load(JobId) of
                {ok, #bc_sched_job{fire_count = FC}} when FC >= MinCount ->
                    ok;
                _ ->
                    timer:sleep(100),
                    wait_for_job_fire_loop(JobId, MinCount, Deadline)
            end
    end.

%% Poll bc_session:get_history/1 until Predicate returns true.
wait_for_history(SessionPid, Predicate, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_history_loop(SessionPid, Predicate, Deadline).

wait_for_history_loop(SessionPid, Predicate, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= Deadline of
        true ->
            History = bc_session:get_history(SessionPid),
            ct:fail({timeout_waiting_for_history,
                     {history_length, length(History)},
                     {roles, [M#bc_message.role || M <- History]}});
        false ->
            History = bc_session:get_history(SessionPid),
            case Predicate(History) of
                true  -> ok;
                false ->
                    timer:sleep(100),
                    wait_for_history_loop(SessionPid, Predicate, Deadline)
            end
    end.
