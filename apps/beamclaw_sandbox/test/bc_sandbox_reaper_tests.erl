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

-module(bc_sandbox_reaper_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------------------
%% Test fixtures
%% ---------------------------------------------------------------------------

reaper_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun starts_with_scheduled_sweep_t/1,
      fun sweep_now_triggers_sweep_t/1,
      fun configurable_interval_t/1,
      fun handles_unknown_calls_t/1,
      fun handles_unknown_info_t/1]}.

setup() ->
    %% Set a fast interval for testing
    application:set_env(beamclaw_sandbox, reaper_interval_ms, 600000),
    %% Start the registry (reaper depends on it for live_container_names)
    {ok, RegPid} = bc_sandbox_registry:start_link(),
    {ok, ReaperPid} = bc_sandbox_reaper:start_link(),
    {RegPid, ReaperPid}.

cleanup({RegPid, ReaperPid}) ->
    unlink(ReaperPid),
    RefR = erlang:monitor(process, ReaperPid),
    exit(ReaperPid, shutdown),
    receive {'DOWN', RefR, process, ReaperPid, _} -> ok
    after 5000 -> error(reaper_cleanup_timeout)
    end,
    unlink(RegPid),
    RefReg = erlang:monitor(process, RegPid),
    exit(RegPid, shutdown),
    receive {'DOWN', RefReg, process, RegPid, _} -> ok
    after 5000 -> error(registry_cleanup_timeout)
    end,
    ok.

%% ---------------------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------------------

starts_with_scheduled_sweep_t({_RegPid, ReaperPid}) ->
    [fun() ->
        %% Reaper should be alive after start
        ?assert(is_process_alive(ReaperPid))
    end].

sweep_now_triggers_sweep_t({_RegPid, _ReaperPid}) ->
    [fun() ->
        %% sweep_now should not crash (no Docker needed — os:cmd returns empty)
        ?assertEqual(ok, bc_sandbox_reaper:sweep_now()),
        %% Give the cast time to be processed
        timer:sleep(50),
        ok
    end].

configurable_interval_t({_RegPid, _ReaperPid}) ->
    [fun() ->
        %% Verify the interval was picked up from app env
        Interval = application:get_env(beamclaw_sandbox, reaper_interval_ms,
                                       60000),
        ?assertEqual(600000, Interval)
    end].

handles_unknown_calls_t({_RegPid, ReaperPid}) ->
    [fun() ->
        ?assertEqual({error, unknown},
                     gen_server:call(ReaperPid, some_unknown_call))
    end].

handles_unknown_info_t({_RegPid, ReaperPid}) ->
    [fun() ->
        %% Sending unknown info should not crash the reaper
        ReaperPid ! {some, random, message},
        timer:sleep(50),
        ?assert(is_process_alive(ReaperPid))
    end].

%% ---------------------------------------------------------------------------
%% Registry container cleanup tests
%% ---------------------------------------------------------------------------

registry_down_cleanup_test_() ->
    {foreach,
     fun reg_setup/0,
     fun reg_cleanup/1,
     [fun down_handler_stores_container_name_t/1]}.

reg_setup() ->
    {ok, RegPid} = bc_sandbox_registry:start_link(),
    RegPid.

reg_cleanup(RegPid) ->
    unlink(RegPid),
    Ref = erlang:monitor(process, RegPid),
    exit(RegPid, shutdown),
    receive {'DOWN', Ref, process, RegPid, _} -> ok
    after 5000 -> error(cleanup_timeout)
    end,
    ok.

down_handler_stores_container_name_t(RegPid) ->
    [fun() ->
        %% Simulate a monitored process dying — the registry should
        %% remove the ETS entry (container cleanup is best-effort via spawn,
        %% won't actually run Docker in tests)
        Proc = spawn(fun() -> receive stop -> ok end end),
        Key = {<<"sess-reaper-test">>, session},
        ets:insert(bc_sandbox_registry, {Key, Proc}),

        %% Set up the monitor map and a fake monitor ref
        FakeRef = make_ref(),
        sys:replace_state(bc_sandbox_registry, fun(#{monitors := Mons} = S) ->
            S#{monitors := Mons#{Proc => {FakeRef, Key, "beamclaw-sbx-test-123"}}}
        end),

        %% Send a synthetic DOWN message to the registry (simulating
        %% what happens when a monitored process dies)
        RegPid ! {'DOWN', FakeRef, process, Proc, killed},
        timer:sleep(100),

        %% ETS entry should be cleaned up
        ?assertEqual([], ets:lookup(bc_sandbox_registry, Key)),
        %% Clean up the actual process
        exit(Proc, kill)
    end].
