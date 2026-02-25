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

-module(bc_sandbox_docker_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---------------------------------------------------------------------------
%% run_args tests
%% ---------------------------------------------------------------------------

run_args_basic_test() ->
    Config = #{container_name => "beamclaw-sbx-test-1",
               docker_image => "beamclaw-sandbox:latest",
               sandbox_key => {<<"sess1">>, session}},
    Args = bc_sandbox_docker:run_args(Config),
    ?assert(lists:member("run", Args)),
    ?assert(lists:member("-d", Args)),
    ?assert(lists:member("--name", Args)),
    ?assert(lists:member("beamclaw-sbx-test-1", Args)),
    ?assert(lists:member("beamclaw-sandbox:latest", Args)).

run_args_security_flags_test() ->
    Config = #{container_name => "test-1", docker_image => "img:latest",
               sandbox_key => {<<"s">>, session}},
    Args = bc_sandbox_docker:run_args(Config),
    ?assert(lists:member("--cap-drop", Args)),
    ?assert(lists:member("ALL", Args)),
    ?assert(lists:member("--read-only", Args)),
    ?assert(lists:member("--security-opt", Args)),
    ?assert(lists:member("no-new-privileges", Args)),
    ?assert(lists:member("--pids-limit", Args)),
    ?assert(lists:member("256", Args)).

run_args_resource_limits_test() ->
    Config = #{container_name => "test-1", docker_image => "img:latest",
               memory_limit => "1g", cpu_limit => "2.0",
               sandbox_key => {<<"s">>, session}},
    Args = bc_sandbox_docker:run_args(Config),
    ?assert(lists:member("--memory", Args)),
    ?assert(lists:member("1g", Args)),
    ?assert(lists:member("--cpus", Args)),
    ?assert(lists:member("2.0", Args)).

run_args_network_none_test() ->
    Config = #{container_name => "test-1", docker_image => "img:latest",
               network => none, sandbox_key => {<<"s">>, session}},
    Args = bc_sandbox_docker:run_args(Config),
    ?assert(lists:member("--network", Args)),
    ?assert(lists:member("none", Args)).

run_args_network_bridge_test() ->
    Config = #{container_name => "test-1", docker_image => "img:latest",
               network => bridge, sandbox_key => {<<"s">>, session}},
    Args = bc_sandbox_docker:run_args(Config),
    ?assert(lists:member("bridge", Args)).

run_args_workspace_mount_ro_test() ->
    Config = #{container_name => "test-1", docker_image => "img:latest",
               workspace_mount => ro, workspace_dir => "/home/user/ws",
               sandbox_key => {<<"s">>, session}},
    Args = bc_sandbox_docker:run_args(Config),
    ?assert(lists:member("-v", Args)),
    ?assert(lists:member("/home/user/ws:/workspace:ro", Args)).

run_args_workspace_mount_none_test() ->
    Config = #{container_name => "test-1", docker_image => "img:latest",
               workspace_mount => none,
               sandbox_key => {<<"s">>, session}},
    Args = bc_sandbox_docker:run_args(Config),
    ?assertNot(lists:member("/workspace:ro", Args)).

run_args_socket_mount_test() ->
    Config = #{container_name => "test-1", docker_image => "img:latest",
               bridge_socket => "/tmp/beamclaw-bridges/test.sock",
               bridge_socket_dir => "/tmp/beamclaw-bridges",
               sandbox_key => {<<"s">>, session}},
    Args = bc_sandbox_docker:run_args(Config),
    ?assert(lists:member("-v", Args)),
    ?assert(lists:member("/tmp/beamclaw-bridges/test.sock:/tmp/bridge.sock", Args)).

run_args_env_vars_test() ->
    Config = #{container_name => "test-1", docker_image => "img:latest",
               env_vars => [{"FOO", "bar"}, {"BAZ", "qux"}],
               sandbox_key => {<<"s">>, session}},
    Args = bc_sandbox_docker:run_args(Config),
    ?assert(lists:member("-e", Args)),
    ?assert(lists:member("FOO=bar", Args)),
    ?assert(lists:member("BAZ=qux", Args)).

%% ---------------------------------------------------------------------------
%% exec_args tests
%% ---------------------------------------------------------------------------

exec_args_python_test() ->
    Args = bc_sandbox_docker:exec_args("container-1", "/tmp/script.py", "python"),
    ?assertEqual(["exec", "container-1", "python3", "/tmp/script.py"], Args).

exec_args_bash_test() ->
    Args = bc_sandbox_docker:exec_args("container-1", "/tmp/script.sh", "bash"),
    ?assertEqual(["exec", "container-1", "bash", "/tmp/script.sh"], Args).

exec_args_default_python_test() ->
    Args = bc_sandbox_docker:exec_args("container-1", "/tmp/script.py", "other"),
    ?assertEqual(["exec", "container-1", "python3", "/tmp/script.py"], Args).

%% ---------------------------------------------------------------------------
%% kill_args / rm_args tests
%% ---------------------------------------------------------------------------

kill_args_test() ->
    ?assertEqual(["kill", "my-container"], bc_sandbox_docker:kill_args("my-container")).

rm_args_test() ->
    ?assertEqual(["rm", "-f", "my-container"], bc_sandbox_docker:rm_args("my-container")).

%% ---------------------------------------------------------------------------
%% container_name tests
%% ---------------------------------------------------------------------------

container_name_format_test() ->
    Config = #{sandbox_key => {<<"abc12345-more-stuff">>, session}},
    Name = bc_sandbox_docker:container_name(Config),
    ?assert(lists:prefix("beamclaw-sbx-session-", Name)).

container_name_scope_test() ->
    Config = #{sandbox_key => {<<"sess">>, agent}},
    Name = bc_sandbox_docker:container_name(Config),
    ?assert(lists:prefix("beamclaw-sbx-agent-", Name)).

container_name_unique_test() ->
    Config = #{sandbox_key => {<<"sess">>, session}},
    Name1 = bc_sandbox_docker:container_name(Config),
    Name2 = bc_sandbox_docker:container_name(Config),
    ?assertNotEqual(Name1, Name2).

%% ---------------------------------------------------------------------------
%% image_exists_args / build_args tests
%% ---------------------------------------------------------------------------

image_exists_args_test() ->
    ?assertEqual(["image", "inspect", "--format", "{{.Id}}", "myimg:latest"],
                 bc_sandbox_docker:image_exists_args("myimg:latest")).

build_args_test() ->
    Args = bc_sandbox_docker:build_args("/path/to/Dockerfile.sandbox", "myimg:latest"),
    ?assertEqual(["build", "-f", "/path/to/Dockerfile.sandbox",
                  "-t", "myimg:latest", "/path/to"], Args).

%% ---------------------------------------------------------------------------
%% container_name session-prefix stripping test
%% ---------------------------------------------------------------------------

container_name_strips_session_prefix_test() ->
    Config = #{sandbox_key => {<<"session-a1b2c3d4e5f6">>, session}},
    Name = bc_sandbox_docker:container_name(Config),
    ?assert(lists:prefix("beamclaw-sbx-session-a1b2c3d4", Name)),
    %% Must NOT contain "session-session"
    ?assertEqual(nomatch, string:find(Name, "session-session")).
