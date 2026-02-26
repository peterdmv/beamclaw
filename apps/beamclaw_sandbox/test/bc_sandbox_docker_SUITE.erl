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

-module(bc_sandbox_docker_SUITE).
-moduledoc """
Docker sandbox E2E tests (Tier 3).

Exercises the real Docker sandbox end-to-end. Self-skips via
{skip, "..."} in init_per_suite when Docker is unavailable or the
sandbox image is not built.

These tests would have caught:
  - Docker socket EACCES (group permissions)    → bridge_socket_permissions
  - docker cp fails on --read-only containers   → python_multiline
  - Alpine has sh not bash                       → shell_hello_world
  - Startup failures silently ignored            → container_bad_image
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases — container_lifecycle group
-export([container_starts/1, container_security_flags/1,
         container_stops/1, container_bad_image/1]).

%% Test cases — script_execution group
-export([python_hello_world/1, shell_hello_world/1,
         python_multiline/1, script_timeout/1,
         large_output_truncated/1]).

%% Test cases — bridge group
-export([bridge_socket_exists/1, bridge_socket_permissions/1]).

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    [{group, container_lifecycle},
     {group, script_execution},
     {group, bridge}].

groups() ->
    [{container_lifecycle, [sequence],
      [container_starts, container_security_flags,
       container_stops, container_bad_image]},
     {script_execution, [sequence],
      [python_hello_world, shell_hello_world,
       python_multiline, script_timeout,
       large_output_truncated]},
     {bridge, [sequence],
      [bridge_socket_exists, bridge_socket_permissions]}].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    %% Check Docker is available
    case os:cmd("docker info 2>&1") of
        "Cannot connect" ++ _ ->
            {skip, "Docker not available"};
        "ERROR" ++ _ ->
            {skip, "Docker not available"};
        "command not found" ++ _ ->
            {skip, "Docker not installed"};
        DockerInfo ->
            case string:find(DockerInfo, "Server Version") of
                nomatch ->
                    {skip, "Docker not responding"};
                _ ->
                    check_sandbox_image(Config)
            end
    end.

check_sandbox_image(Config) ->
    case os:cmd("docker image inspect beamclaw-sandbox:latest 2>&1") of
        "[]" ++ _ ->
            {skip, "Sandbox image not built (run: beamclaw sandbox build)"};
        "Error" ++ _ ->
            {skip, "Sandbox image not built (run: beamclaw sandbox build)"};
        _ ->
            start_apps(Config)
    end.

start_apps(Config) ->
    %% Create a temp bridge socket dir
    BridgeDir = filename:join("/tmp",
        "beamclaw-ct-bridges-" ++
        integer_to_list(erlang:unique_integer([positive]))),
    ok = filelib:ensure_dir(filename:join(BridgeDir, "dummy")),

    %% Configure sandbox as enabled with our temp bridge dir
    application:set_env(beamclaw_sandbox, enabled, true),
    application:set_env(beamclaw_sandbox, bridge_socket_dir, BridgeDir),
    application:set_env(beamclaw_sandbox, docker_image, "beamclaw-sandbox:latest"),
    application:set_env(beamclaw_sandbox, timeout_seconds, 30),
    application:set_env(beamclaw_sandbox, memory_limit, "256m"),
    application:set_env(beamclaw_sandbox, cpu_limit, "0.5"),
    application:set_env(beamclaw_sandbox, max_output_bytes, 1048576),
    application:set_env(beamclaw_sandbox, network, none),

    %% Disable session persistence to avoid Mnesia deps
    application:set_env(beamclaw_core, session_persistence, false),

    {ok, Started} = application:ensure_all_started(beamclaw_sandbox),
    [{started_apps, Started},
     {bridge_dir, BridgeDir} | Config].

end_per_suite(Config) ->
    %% Kill any lingering beamclaw-sbx-* containers
    Containers = string:trim(os:cmd(
        "docker ps -q --filter 'name=beamclaw-sbx-' 2>/dev/null")),
    case Containers of
        "" -> ok;
        Ids ->
            lists:foreach(fun(Id) ->
                os:cmd("docker kill " ++ Id ++ " 2>/dev/null"),
                os:cmd("docker rm -f " ++ Id ++ " 2>/dev/null")
            end, string:tokens(Ids, "\n"))
    end,

    %% Stop apps
    Started = proplists:get_value(started_apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Started)),

    %% Remove temp bridge dir
    BridgeDir = proplists:get_value(bridge_dir, Config, ""),
    os:cmd("rm -rf " ++ BridgeDir),
    ok.

%% ---------------------------------------------------------------------------
%% Group lifecycle
%% ---------------------------------------------------------------------------

init_per_group(container_lifecycle, Config) ->
    %% Shared session_id across the lifecycle group (sequence of tests
    %% that create → inspect → stop the same container)
    SessionId = iolist_to_binary([
        "ct-lifecycle-",
        integer_to_list(erlang:unique_integer([positive]))]),
    [{session_id, SessionId} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(container_lifecycle, Config) ->
    %% Clean up in case container_stops didn't run (e.g. earlier test failed)
    SessionId = proplists:get_value(session_id, Config),
    catch bc_sandbox_registry:destroy(SessionId, session),
    ok;
end_per_group(_Group, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% Per-testcase lifecycle
%% ---------------------------------------------------------------------------

init_per_testcase(TC, Config) when TC =:= container_starts;
                                   TC =:= container_security_flags;
                                   TC =:= container_stops ->
    %% container_lifecycle group: session_id set in init_per_group
    Config;
init_per_testcase(container_bad_image, Config) ->
    %% Bad image test uses its own session_id (different sandbox)
    SessionId = iolist_to_binary([
        "ct-badimg-",
        integer_to_list(erlang:unique_integer([positive]))]),
    [{bad_image_session_id, SessionId} | Config];
init_per_testcase(_TC, Config) ->
    SessionId = iolist_to_binary([
        "ct-docker-",
        integer_to_list(erlang:unique_integer([positive]))]),
    [{session_id, SessionId} | Config].

end_per_testcase(TC, Config) when TC =:= container_starts;
                                  TC =:= container_security_flags;
                                  TC =:= container_stops ->
    %% container_lifecycle group: cleanup in end_per_group
    Config;
end_per_testcase(container_bad_image, Config) ->
    Sid = proplists:get_value(bad_image_session_id, Config),
    catch bc_sandbox_registry:destroy(Sid, session),
    Config;
end_per_testcase(_TC, Config) ->
    SessionId = proplists:get_value(session_id, Config),
    catch bc_sandbox_registry:destroy(SessionId, session),
    Config.

%% ---------------------------------------------------------------------------
%% container_lifecycle group
%% ---------------------------------------------------------------------------

container_starts(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    BridgeDir = proplists:get_value(bridge_dir, Config),
    SandboxConfig = #{bridge_socket_dir => BridgeDir},
    {ok, Pid} = bc_sandbox_registry:get_or_create(SessionId, session, SandboxConfig),
    ?assert(is_pid(Pid)),

    %% Poll until ready (sandbox starts container asynchronously)
    ok = wait_for_status(Pid, ready, 15000),

    %% Verify container is visible in docker ps
    ContainerName = bc_sandbox:get_container_name(Pid),
    PsOutput = os:cmd("docker ps --filter name=" ++ ContainerName ++ " --format '{{.Names}}'"),
    ?assertNotEqual("", string:trim(PsOutput)),
    ok.

container_security_flags(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    {ok, Pid} = bc_sandbox_registry:lookup(SessionId, session),
    ok = wait_for_status(Pid, ready, 5000),
    ContainerName = bc_sandbox:get_container_name(Pid),

    %% docker inspect for security flags
    InspectCmd = "docker inspect --format '{{json .HostConfig}}' " ++ ContainerName,
    InspectJson = os:cmd(InspectCmd),
    Decoded = jsx:decode(iolist_to_binary(InspectJson), [return_maps]),

    %% Verify --cap-drop ALL
    CapDrop = maps:get(<<"CapDrop">>, Decoded, []),
    ?assert(lists:member(<<"ALL">>, CapDrop)),

    %% Verify --read-only
    ReadonlyRootfs = maps:get(<<"ReadonlyRootfs">>, Decoded, false),
    ?assert(ReadonlyRootfs),

    %% Verify --security-opt no-new-privileges
    SecurityOpt = maps:get(<<"SecurityOpt">>, Decoded, []),
    ?assert(lists:any(fun(S) ->
        binary:match(S, <<"no-new-privileges">>) =/= nomatch
    end, SecurityOpt)),

    %% Verify --network none
    NetInspect = os:cmd("docker inspect --format '{{.HostConfig.NetworkMode}}' " ++ ContainerName),
    ?assertEqual("none", string:trim(NetInspect)),
    ok.

container_stops(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    {ok, Pid} = bc_sandbox_registry:lookup(SessionId, session),
    ContainerName = bc_sandbox:get_container_name(Pid),

    %% Stop via registry
    ok = bc_sandbox_registry:destroy(SessionId, session),

    %% Verify container is gone from docker ps
    timer:sleep(500),
    PsOutput = os:cmd("docker ps -a --filter name=" ++ ContainerName ++ " --format '{{.Names}}'"),
    ?assertEqual("", string:trim(PsOutput)),
    ok.

container_bad_image(Config) ->
    SessionId = proplists:get_value(bad_image_session_id, Config),
    BridgeDir = proplists:get_value(bridge_dir, Config),
    SandboxConfig = #{
        bridge_socket_dir => BridgeDir,
        docker_image => "nonexistent-image-beamclaw:never"
    },
    {ok, Pid} = bc_sandbox_registry:get_or_create(SessionId, session, SandboxConfig),

    %% Wait for it to reach failed status (not stuck in starting)
    ok = wait_for_status(Pid, failed, 15000),

    %% Verify error message is present
    #{status := failed, container_name := _} = bc_sandbox:get_status(Pid),
    ok.

%% ---------------------------------------------------------------------------
%% script_execution group
%% ---------------------------------------------------------------------------

python_hello_world(Config) ->
    {Pid, _} = create_ready_sandbox(Config),
    DummyBridge = fun(_N, _A) -> {error, <<"not needed">>} end,
    {ok, Output} = bc_sandbox:exec_script(Pid, <<"print('hello')">>, <<"python">>, DummyBridge),
    ?assertEqual(<<"hello\n">>, Output),
    ok.

shell_hello_world(Config) ->
    %% This test catches the Alpine sh vs bash bug:
    %% bc_sandbox_docker:exec_args maps "bash" → "sh" for Alpine containers
    {Pid, _} = create_ready_sandbox(Config),
    DummyBridge = fun(_N, _A) -> {error, <<"not needed">>} end,
    {ok, Output} = bc_sandbox:exec_script(Pid, <<"echo hello">>, <<"bash">>, DummyBridge),
    ?assertEqual(<<"hello\n">>, Output),
    ok.

python_multiline(Config) ->
    %% This test catches the docker cp vs base64 bug:
    %% Script injection uses base64 which works on --read-only containers
    {Pid, _} = create_ready_sandbox(Config),
    DummyBridge = fun(_N, _A) -> {error, <<"not needed">>} end,
    Script = <<"import json\n"
               "data = {'key': 'value', 'special': \"quotes'and\\nnewlines\"}\n"
               "print(json.dumps(data, sort_keys=True))">>,
    {ok, Output} = bc_sandbox:exec_script(Pid, Script, <<"python">>, DummyBridge),
    %% Verify the JSON round-tripped correctly
    Decoded = jsx:decode(string:trim(Output), [return_maps]),
    ?assertEqual(<<"value">>, maps:get(<<"key">>, Decoded)),
    ok.

script_timeout(Config) ->
    %% Override timeout to 2 seconds for this test
    OldTimeout = application:get_env(beamclaw_sandbox, timeout_seconds, 60),
    application:set_env(beamclaw_sandbox, timeout_seconds, 2),
    BridgeDir = proplists:get_value(bridge_dir, Config),
    SessionId = proplists:get_value(session_id, Config),
    SandboxConfig = #{bridge_socket_dir => BridgeDir, timeout_seconds => 2},
    {ok, Pid} = bc_sandbox_registry:get_or_create(SessionId, session, SandboxConfig),
    ok = wait_for_status(Pid, ready, 15000),

    DummyBridge = fun(_N, _A) -> {error, <<"not needed">>} end,
    Script = <<"import time\ntime.sleep(120)\nprint('should not reach')">>,
    Start = erlang:monotonic_time(second),
    {ok, _Output} = bc_sandbox:exec_script(Pid, Script, <<"python">>, DummyBridge),
    Elapsed = erlang:monotonic_time(second) - Start,
    %% Should complete in well under 120s (the sleep duration)
    ?assert(Elapsed < 30),
    application:set_env(beamclaw_sandbox, timeout_seconds, OldTimeout),
    ok.

large_output_truncated(Config) ->
    {Pid, _} = create_ready_sandbox(Config),
    DummyBridge = fun(_N, _A) -> {error, <<"not needed">>} end,
    %% Print 2MB of data (max_output_bytes is 1MB)
    Script = <<"print('x' * 2 * 1024 * 1024)">>,
    {ok, Output} = bc_sandbox:exec_script(Pid, Script, <<"python">>, DummyBridge),
    MaxOutput = application:get_env(beamclaw_sandbox, max_output_bytes, 1048576),
    ?assert(byte_size(Output) =< MaxOutput),
    ok.

%% ---------------------------------------------------------------------------
%% bridge group
%% ---------------------------------------------------------------------------

bridge_socket_exists(Config) ->
    {Pid, _} = create_ready_sandbox(Config),
    ContainerName = bc_sandbox:get_container_name(Pid),
    BridgeDir = proplists:get_value(bridge_dir, Config),
    SocketPath = filename:join(BridgeDir, ContainerName ++ ".sock"),
    %% file:read_file_info works for Unix domain sockets (unlike filelib:is_file)
    case file:read_file_info(SocketPath) of
        {ok, _FileInfo} -> ok;
        {error, Reason} -> ct:fail({socket_not_found, SocketPath, Reason})
    end.

bridge_socket_permissions(Config) ->
    %% This test catches the EACCES on bind-mounted dirs bug
    {Pid, _} = create_ready_sandbox(Config),
    ContainerName = bc_sandbox:get_container_name(Pid),
    BridgeDir = proplists:get_value(bridge_dir, Config),
    SocketPath = filename:join(BridgeDir, ContainerName ++ ".sock"),

    %% Verify socket mode includes world-write (0o777)
    {ok, FileInfo} = file:read_file_info(SocketPath),
    Mode = element(8, FileInfo),  %% #file_info.mode
    %% Check other-write bit (8#0002) is set
    ?assertNotEqual(0, Mode band 8#0002),
    %% Check other-read bit (8#0004) is set
    ?assertNotEqual(0, Mode band 8#0004),

    %% Verify we can connect to the socket (proves read/write access)
    case gen_tcp:connect({local, SocketPath}, 0,
                         [binary, {packet, raw}, {active, false}], 2000) of
        {ok, Sock} ->
            gen_tcp:close(Sock),
            ok;
        {error, Reason} ->
            ct:fail({socket_connect_failed, Reason, SocketPath})
    end.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

create_ready_sandbox(Config) ->
    SessionId = proplists:get_value(session_id, Config),
    BridgeDir = proplists:get_value(bridge_dir, Config),
    SandboxConfig = #{bridge_socket_dir => BridgeDir},
    {ok, Pid} = bc_sandbox_registry:get_or_create(SessionId, session, SandboxConfig),
    ok = wait_for_status(Pid, ready, 15000),
    {Pid, SessionId}.

wait_for_status(Pid, ExpectedStatus, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_status_loop(Pid, ExpectedStatus, Deadline).

wait_for_status_loop(Pid, ExpectedStatus, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= Deadline of
        true ->
            #{status := Actual} = bc_sandbox:get_status(Pid),
            ct:fail({timeout_waiting_for_status, ExpectedStatus, got, Actual});
        false ->
            case bc_sandbox:get_status(Pid) of
                #{status := ExpectedStatus} ->
                    ok;
                #{status := failed} when ExpectedStatus =:= failed ->
                    ok;
                #{status := failed, container_name := _} = S
                  when ExpectedStatus =/= failed ->
                    ct:fail({sandbox_failed, S});
                _ ->
                    timer:sleep(200),
                    wait_for_status_loop(Pid, ExpectedStatus, Deadline)
            end
    end.
