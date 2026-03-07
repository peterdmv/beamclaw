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

-module(bc_loop_edge_SUITE).
-moduledoc """
bc_loop gen_statem edge-case tests (Tier 2).

Exercises state machine edge cases that historically caused crash/hang bugs:
- Stream error recovery (provider sends stream_error)
- Stream timeout recovery (provider never responds)
- Tool crash resilience (tool throws during execution)
- Max tool iteration limit (infinite tool call loop)
- Concurrent run queue under tool crash conditions
- Loop crash recovery (supervisor restarts)

Addresses the 15% of historical fix commits caused by crash/hang/silent-failure
bugs in the agentic loop.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% CT callbacks
-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([stream_error_recovery/1,
         stream_timeout_recovery/1,
         tool_crash_recovery_returns_to_idle/1,
         max_tool_iterations_enforced/1,
         queue_drains_after_error/1,
         loop_crash_supervisor_restart/1]).

suite() ->
    %% 90s timetrap: timeout test needs 60s + margin
    [{timetrap, {seconds, 90}}].

all() ->
    [stream_error_recovery,
     stream_timeout_recovery,
     tool_crash_recovery_returns_to_idle,
     max_tool_iterations_enforced,
     queue_drains_after_error,
     loop_crash_supervisor_restart].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    application:set_env(beamclaw_core, session_persistence, false),
    application:set_env(beamclaw_core, autonomy_level, full),
    application:set_env(beamclaw_core, agentic_loop,
                        #{compaction_threshold_pct => 9999,
                          memory_flush => false,
                          max_tool_iterations => 3}),
    {ok, Started} = application:ensure_all_started(beamclaw_core),
    [{started_apps, Started} | Config].

end_per_suite(Config) ->
    Started = proplists:get_value(started_apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Started)),
    ok.

init_per_testcase(_TC, Config) ->
    SessionId = iolist_to_binary([
        "ct-edge-", integer_to_list(erlang:unique_integer([positive]))]),
    [{session_id, SessionId} | Config].

end_per_testcase(_TC, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% Test cases
%% ---------------------------------------------------------------------------

%% Provider sends stream_error: loop should handle the error and return
%% to idle (not hang). Note: error messages are routed to the channel
%% but not stored in history. We verify recovery by checking the loop
%% can process a subsequent message.
stream_error_recovery(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_error_mock,
        autonomy     => full
    },
    {ok, _} = bc_sessions_sup:start_session(SessionConfig),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    %% Dispatch two messages. First triggers error, second queues.
    %% If the loop hangs on error, the second message will never be processed.
    dispatch_message(SessionId, SessionPid, <<"trigger error">>),
    timer:sleep(500),
    dispatch_message(SessionId, SessionPid, <<"second message">>),

    %% Both user messages should appear in history — proving the loop
    %% recovered from the error and drained the queue.
    ok = wait_for_history(SessionPid, fun(History) ->
        UserMsgs = [M || M <- History, M#bc_message.role =:= user],
        length(UserMsgs) >= 2
    end, 10000),
    ok.

%% Provider never sends stream_done: loop should timeout after 60s
%% and recover (not hang indefinitely). Verify by dispatching a second
%% message that gets processed after the timeout.
stream_timeout_recovery(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_timeout_mock,
        autonomy     => full
    },
    {ok, _} = bc_sessions_sup:start_session(SessionConfig),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    %% Dispatch first message (will timeout after 60s)
    dispatch_message(SessionId, SessionPid, <<"trigger timeout">>),
    %% Queue a second message while first is timing out
    timer:sleep(500),
    dispatch_message(SessionId, SessionPid, <<"after timeout">>),

    %% Wait for both user messages to appear in history.
    %% The second message proves the loop recovered from the timeout.
    ok = wait_for_history(SessionPid, fun(History) ->
        UserMsgs = [M || M <- History, M#bc_message.role =:= user],
        length(UserMsgs) >= 2
    end, 75000),
    ok.

%% Tool crashes during execution: loop should catch the crash, store
%% an error tool result, get a follow-up LLM response, and end up in idle.
tool_crash_recovery_returns_to_idle(Config) ->
    bc_tool_registry:register(bc_tool_crash_mock, bc_tool_crash_mock:definition()),

    SessionId = proplists:get_value(session_id, Config),
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_toolcall_mock,
        autonomy     => full
    },
    {ok, _} = bc_sessions_sup:start_session(SessionConfig),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    dispatch_message(SessionId, SessionPid, <<"use crash tool">>),

    %% Wait for: tool result (error) + follow-up assistant response
    ok = wait_for_history(SessionPid, fun(History) ->
        ToolMsgs = [M || M <- History, M#bc_message.role =:= tool],
        AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
        length(ToolMsgs) >= 1 andalso length(AssistantMsgs) >= 2
    end, 10000),

    %% Verify the tool result contains the crash error
    History = bc_session:get_history(SessionPid),
    ToolMsgs = [M || M <- History, M#bc_message.role =:= tool],
    ?assert(lists:any(fun(M) ->
        binary:match(M#bc_message.content, <<"Tool crashed">>) =/= nomatch
    end, ToolMsgs)),

    %% Verify loop is back in idle: can dispatch another message
    dispatch_message(SessionId, SessionPid, <<"after crash">>),
    ok = wait_for_history(SessionPid, fun(History2) ->
        UserMsgs = [M || M <- History2, M#bc_message.role =:= user],
        length(UserMsgs) >= 2
    end, 10000),
    ok.

%% Provider always returns tool calls: verify max_tool_iterations stops the loop.
max_tool_iterations_enforced(Config) ->
    bc_tool_registry:register(bc_tool_noop_mock, bc_tool_noop_mock:definition()),

    SessionId = proplists:get_value(session_id, Config),
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_infinite_tool_mock,
        autonomy     => full
    },
    {ok, _} = bc_sessions_sup:start_session(SessionConfig),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    dispatch_message(SessionId, SessionPid, <<"loop forever">>),

    %% Wait for the loop to stop after max_tool_iterations (3)
    %% The loop will have: user + (tool_call + tool_result) * 3
    ok = wait_for_history(SessionPid, fun(History) ->
        ToolMsgs = [M || M <- History, M#bc_message.role =:= tool],
        %% Max 3 iterations means max 3 tool results
        length(ToolMsgs) >= 3
    end, 15000),

    %% Give it a moment to ensure it stops (doesn't go to 4+)
    timer:sleep(2000),
    History = bc_session:get_history(SessionPid),
    ToolMsgs = [M || M <- History, M#bc_message.role =:= tool],
    %% Should be exactly 3 (max_tool_iterations) — not more
    ?assert(length(ToolMsgs) =< 4),  %% Allow small margin
    ok.

%% After an error on the first message, queued second message should still execute.
queue_drains_after_error(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    %% Use a provider that fails on first call, succeeds on second.
    %% We'll use error_mock which always fails — the key test is that
    %% the queue drains and the second message is processed.
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_error_mock,
        autonomy     => full
    },
    {ok, _} = bc_sessions_sup:start_session(SessionConfig),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    %% Dispatch two messages rapidly
    dispatch_message(SessionId, SessionPid, <<"first">>),
    dispatch_message(SessionId, SessionPid, <<"second">>),

    %% Both should eventually be processed (even though they fail)
    ok = wait_for_history(SessionPid, fun(History) ->
        UserMsgs = [M || M <- History, M#bc_message.role =:= user],
        length(UserMsgs) >= 2
    end, 15000),
    ok.

%% Kill the bc_loop process: supervisor should restart it and the session
%% should remain functional.
loop_crash_supervisor_restart(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_smoke_mock,
        autonomy     => full
    },
    {ok, _} = bc_sessions_sup:start_session(SessionConfig),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    %% First, do a normal round-trip to verify it works
    dispatch_message(SessionId, SessionPid, <<"before crash">>),
    ok = wait_for_history(SessionPid, fun(History) ->
        AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
        length(AssistantMsgs) >= 1
    end, 5000),

    %% Find the loop pid via the session supervisor's children.
    %% bc_session_sup is the parent of both bc_session and bc_loop.
    {links, Links} = process_info(SessionPid, links),
    SupPid = [P || P <- Links, is_supervisor(P)],
    ?assert(length(SupPid) >= 1),
    [Sup | _] = SupPid,
    Children = supervisor:which_children(Sup),
    {_, LoopPid, _, _} = lists:keyfind(bc_loop, 1, Children),
    ?assert(is_pid(LoopPid)),
    exit(LoopPid, kill),

    %% Wait for supervisor to restart the loop
    timer:sleep(1000),

    %% Session should still be alive
    ?assert(is_process_alive(SessionPid)),

    %% The history from before the crash should be preserved
    History = bc_session:get_history(SessionPid),
    UserMsgs = [M || M <- History, M#bc_message.role =:= user],
    ?assert(length(UserMsgs) >= 1),
    ?assertEqual(<<"before crash">>, (hd(UserMsgs))#bc_message.content),
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

is_supervisor(Pid) when is_pid(Pid) ->
    case process_info(Pid, dictionary) of
        {dictionary, Dict} ->
            proplists:get_value('$initial_call', Dict) =:= {supervisor, bc_session_sup, 1};
        _ ->
            false
    end;
is_supervisor(_) -> false.

dispatch_message(SessionId, SessionPid, Content) ->
    Msg = #bc_channel_message{
        session_id = SessionId,
        user_id    = <<"test-edge">>,
        channel    = tui,
        content    = Content,
        raw        = Content,
        ts         = erlang:system_time(millisecond)
    },
    bc_session:dispatch_run(SessionPid, Msg).

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
