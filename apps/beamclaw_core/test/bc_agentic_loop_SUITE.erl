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

-module(bc_agentic_loop_SUITE).
-moduledoc """
Agentic loop integration tests (Tier 2).

Replaces bc_smoke_tests with proper CT lifecycle and polling helpers.
Exercises the full agentic loop: session → dispatch → bc_loop → provider
→ history update. No HTTP, no gateway. Uses mock providers.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% CT callbacks
-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([smoke_roundtrip/1, tool_crash_resilience/1,
         tool_call_and_result/1, session_queue_drains/1]).

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [smoke_roundtrip, tool_crash_resilience,
     tool_call_and_result, session_queue_drains].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    application:set_env(beamclaw_core, session_persistence, false),
    {ok, Started} = application:ensure_all_started(beamclaw_core),
    [{started_apps, Started} | Config].

end_per_suite(Config) ->
    Started = proplists:get_value(started_apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Started)),
    ok.

%% ---------------------------------------------------------------------------
%% Per-testcase lifecycle
%% ---------------------------------------------------------------------------

init_per_testcase(_TC, Config) ->
    SessionId = iolist_to_binary([
        "ct-loop-", integer_to_list(erlang:unique_integer([positive]))]),
    [{session_id, SessionId} | Config].

end_per_testcase(_TC, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% Test cases
%% ---------------------------------------------------------------------------

%% Dispatch a user message; assert the assistant reply appears in history.
smoke_roundtrip(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_smoke_mock,
        autonomy     => full
    },
    {ok, _SupPid} = bc_sessions_sup:start_session(SessionConfig),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    Msg = #bc_channel_message{
        session_id = SessionId,
        user_id    = <<"test">>,
        channel    = tui,
        content    = <<"hello">>,
        raw        = <<"hello">>,
        ts         = 0
    },
    bc_session:dispatch_run(SessionPid, Msg),

    ok = wait_for_history(SessionPid, fun(History) ->
        AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
        length(AssistantMsgs) >= 1
    end, 5000),
    ok.

%% Tool crash: register a crashing tool, dispatch a message that triggers it,
%% verify the loop survives and produces both a tool error result and a
%% follow-up assistant response.
tool_crash_resilience(Config) ->
    %% Register the crashing mock tool
    bc_tool_registry:register(bc_tool_crash_mock, bc_tool_crash_mock:definition()),

    SessionId = proplists:get_value(session_id, Config),
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_toolcall_mock,
        autonomy     => full
    },
    {ok, _SupPid} = bc_sessions_sup:start_session(SessionConfig),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    Msg = #bc_channel_message{
        session_id = SessionId,
        user_id    = <<"test">>,
        channel    = tui,
        content    = <<"use the crash tool">>,
        raw        = <<"use the crash tool">>,
        ts         = 0
    },
    bc_session:dispatch_run(SessionPid, Msg),

    %% Wait for: stream(tool_call) → execute(crash) → stream(response)
    ok = wait_for_history(SessionPid, fun(History) ->
        ToolMsgs = [M || M <- History, M#bc_message.role =:= tool],
        AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
        length(ToolMsgs) >= 1 andalso length(AssistantMsgs) >= 2
    end, 10000),

    %% Verify the tool result contains the crash error
    History = bc_session:get_history(SessionPid),
    ToolMsgs = [M || M <- History, M#bc_message.role =:= tool],
    [ToolMsg | _] = ToolMsgs,
    ?assertNotEqual(nomatch,
        binary:match(ToolMsg#bc_message.content, <<"Tool crashed">>)),
    ok.

%% Mock provider returns a tool call, verify tool executes and result
%% appears in history.
tool_call_and_result(Config) ->
    bc_tool_registry:register(bc_tool_crash_mock, bc_tool_crash_mock:definition()),

    SessionId = proplists:get_value(session_id, Config),
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_toolcall_mock,
        autonomy     => full
    },
    {ok, _SupPid} = bc_sessions_sup:start_session(SessionConfig),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    Msg = #bc_channel_message{
        session_id = SessionId,
        user_id    = <<"test">>,
        channel    = tui,
        content    = <<"call a tool">>,
        raw        = <<"call a tool">>,
        ts         = 0
    },
    bc_session:dispatch_run(SessionPid, Msg),

    %% Verify tool result and follow-up response both appear
    ok = wait_for_history(SessionPid, fun(History) ->
        ToolMsgs = [M || M <- History, M#bc_message.role =:= tool],
        length(ToolMsgs) >= 1
    end, 10000),

    History = bc_session:get_history(SessionPid),
    %% Should have: user, assistant(tool_call), tool(result), assistant(response)
    ?assert(length(History) >= 4),
    ok.

%% Dispatch two messages while loop is busy, verify both execute in order.
session_queue_drains(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    SessionConfig = #{
        session_id   => SessionId,
        provider_mod => bc_provider_smoke_mock,
        autonomy     => full
    },
    {ok, _SupPid} = bc_sessions_sup:start_session(SessionConfig),
    {ok, SessionPid} = bc_session_registry:lookup(SessionId),

    Msg1 = #bc_channel_message{
        session_id = SessionId,
        user_id    = <<"test">>,
        channel    = tui,
        content    = <<"first message">>,
        raw        = <<"first message">>,
        ts         = 0
    },
    Msg2 = #bc_channel_message{
        session_id = SessionId,
        user_id    = <<"test">>,
        channel    = tui,
        content    = <<"second message">>,
        raw        = <<"second message">>,
        ts         = 1
    },

    %% Dispatch both rapidly — second will be queued
    bc_session:dispatch_run(SessionPid, Msg1),
    bc_session:dispatch_run(SessionPid, Msg2),

    %% Wait for both user messages and their assistant replies
    ok = wait_for_history(SessionPid, fun(History) ->
        UserMsgs = [M || M <- History, M#bc_message.role =:= user],
        AssistantMsgs = [M || M <- History, M#bc_message.role =:= assistant],
        length(UserMsgs) >= 2 andalso length(AssistantMsgs) >= 2
    end, 10000),

    %% Verify ordering: first user msg appears before second
    History = bc_session:get_history(SessionPid),
    UserMsgs = [M || M <- History, M#bc_message.role =:= user],
    [First, Second | _] = UserMsgs,
    ?assertEqual(<<"first message">>, First#bc_message.content),
    ?assertEqual(<<"second message">>, Second#bc_message.content),
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

%% Poll bc_session:get_history/1 until Predicate returns true, with timeout.
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
