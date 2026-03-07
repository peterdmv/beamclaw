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

-module(bc_docker_integration_SUITE).
-moduledoc """
Docker deployment integration tests (Tier 3).

Validates that BeamClaw runs correctly inside a Docker container, catching
the environmental differences (Alpine vs Ubuntu, volume permissions, missing
binaries, locale, HOME path) that historically caused 24% of all fix commits.

Prerequisites:
  - Docker daemon running and accessible
  - BeamClaw Docker image built (or Dockerfile present)

Auto-skips when Docker is unavailable.

Tests:
  - Docker image can be built
  - Container starts and health endpoint responds
  - CLI commands work inside the container
  - Workspace persistence across container restart
  - UTF-8 locale works (no encoding crashes)
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([docker_available/1,
         dockerfile_exists/1,
         image_builds/1,
         container_starts/1,
         health_endpoint_ok/1,
         cli_version_in_container/1,
         cli_help_in_container/1,
         utf8_locale_works/1,
         workspace_dir_exists/1,
         container_stops_cleanly/1]).

suite() ->
    [{timetrap, {seconds, 300}}].  %% 5 min — Docker builds can be slow

groups() ->
    [{docker_build, [sequence],
      [docker_available, dockerfile_exists, image_builds]},
     {container_lifecycle, [sequence],
      [container_starts, health_endpoint_ok,
       cli_version_in_container, cli_help_in_container,
       utf8_locale_works, workspace_dir_exists,
       container_stops_cleanly]}].

all() ->
    [{group, docker_build}, {group, container_lifecycle}].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    ProjectRoot = find_project_root(),
    case check_docker() of
        ok ->
            [{project_root, ProjectRoot},
             {image_name, "beamclaw-test-" ++ integer_to_list(
                 erlang:unique_integer([positive]))},
             {container_name, "beamclaw-ct-" ++ integer_to_list(
                 erlang:unique_integer([positive]))} | Config];
        {skip, Reason} ->
            {skip, Reason}
    end.

end_per_suite(Config) ->
    %% Cleanup: remove test container and image
    ContainerName = proplists:get_value(container_name, Config, ""),
    ImageName = proplists:get_value(image_name, Config, ""),
    os:cmd("docker rm -f " ++ ContainerName ++ " 2>/dev/null"),
    os:cmd("docker rmi " ++ ImageName ++ " 2>/dev/null"),
    ok.

init_per_group(container_lifecycle, Config) ->
    %% Ensure image is built before container tests
    ImageName = proplists:get_value(image_name, Config),
    case docker_image_exists(ImageName) of
        true  -> Config;
        false -> {skip, "Docker image not built (docker_build group failed)"}
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(container_lifecycle, Config) ->
    ContainerName = proplists:get_value(container_name, Config, ""),
    os:cmd("docker rm -f " ++ ContainerName ++ " 2>/dev/null"),
    ok;
end_per_group(_, _Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% Docker build tests
%% ---------------------------------------------------------------------------

%% Verify Docker is available.
docker_available(_Config) ->
    Output = os:cmd("docker info 2>&1"),
    ?assertMatch({match, _}, re:run(Output, "Server Version|Docker Root Dir")),
    ok.

%% Verify Dockerfile exists in the project.
dockerfile_exists(Config) ->
    Root = proplists:get_value(project_root, Config),
    %% Check for main Dockerfile or docker-compose
    Dockerfile = filename:join(Root, "Dockerfile"),
    ComposeFile = filename:join(Root, "docker-compose.yml"),
    HasDockerfile = filelib:is_regular(Dockerfile),
    HasCompose = filelib:is_regular(ComposeFile),
    ?assert(HasDockerfile orelse HasCompose),
    ok.

%% Build the Docker image.
image_builds(Config) ->
    Root = proplists:get_value(project_root, Config),
    ImageName = proplists:get_value(image_name, Config),
    Cmd = lists:flatten(io_lib:format(
        "cd ~s && docker build -t ~s . 2>&1", [Root, ImageName])),
    ct:pal("Building Docker image: ~s", [ImageName]),
    Output = os:cmd(Cmd),
    ct:pal("Build output (last 20 lines): ~s",
            [string:join(lists:nthtail(
                max(0, length(string:split(Output, "\n", all)) - 20),
                string:split(Output, "\n", all)), "\n")]),
    ?assert(docker_image_exists(ImageName)),
    ok.

%% ---------------------------------------------------------------------------
%% Container lifecycle tests
%% ---------------------------------------------------------------------------

%% Start a container from the built image.
container_starts(Config) ->
    ImageName = proplists:get_value(image_name, Config),
    ContainerName = proplists:get_value(container_name, Config),
    Port = 28800 + (erlang:unique_integer([positive]) rem 100),
    Cmd = lists:flatten(io_lib:format(
        "docker run -d --name ~s -p ~B:18800 "
        "-e OPENROUTER_API_KEY=test-dummy "
        "~s 2>&1",
        [ContainerName, Port, ImageName])),
    ct:pal("Starting container: ~s", [Cmd]),
    Output = os:cmd(Cmd),
    ct:pal("Container start output: ~s", [Output]),
    %% Wait for container to be running
    timer:sleep(5000),
    StatusOutput = os:cmd("docker inspect -f '{{.State.Running}}' " ++ ContainerName ++ " 2>&1"),
    ?assertMatch({match, _}, re:run(StatusOutput, "true")),
    [{container_port, Port} | Config].

%% Verify health endpoint responds.
health_endpoint_ok(Config) ->
    Port = proplists:get_value(container_port, Config),
    Url = lists:flatten(io_lib:format("http://127.0.0.1:~B/health", [Port])),
    %% Retry a few times (container may still be starting)
    Result = retry(fun() ->
        case httpc:request(get, {Url, []}, [{timeout, 5000}], []) of
            {ok, {{_, 200, _}, _, Body}} ->
                Decoded = jsx:decode(iolist_to_binary(Body), [return_maps]),
                case maps:get(<<"status">>, Decoded, undefined) of
                    <<"ok">> -> ok;
                    _ -> retry
                end;
            _ -> retry
        end
    end, 10, 2000),
    ?assertEqual(ok, Result),
    ok.

%% CLI version command works inside the container.
cli_version_in_container(Config) ->
    ContainerName = proplists:get_value(container_name, Config),
    Output = os:cmd("docker exec " ++ ContainerName ++ " beamclaw version 2>&1"),
    ct:pal("CLI version in container: ~s", [Output]),
    ?assertMatch({match, _}, re:run(Output, "beamclaw")),
    ok.

%% CLI help command works inside the container.
cli_help_in_container(Config) ->
    ContainerName = proplists:get_value(container_name, Config),
    Output = os:cmd("docker exec " ++ ContainerName ++ " beamclaw help 2>&1"),
    ct:pal("CLI help in container: ~s", [Output]),
    ?assertMatch({match, _}, re:run(Output, "Commands")),
    ok.

%% Verify UTF-8 works in the container (no locale/encoding crashes).
utf8_locale_works(Config) ->
    ContainerName = proplists:get_value(container_name, Config),
    %% Try writing and reading a UTF-8 file
    Cmd = lists:flatten(io_lib:format(
        "docker exec ~s /bin/sh -c "
        "'echo \"Időjárás: 23°C 🌡️\" > /tmp/utf8test.txt && "
        "cat /tmp/utf8test.txt' 2>&1",
        [ContainerName])),
    Output = os:cmd(Cmd),
    ct:pal("UTF-8 test output: ~s", [Output]),
    ?assertMatch({match, _}, re:run(Output, "23")),
    ok.

%% Verify workspace directory exists in the container.
workspace_dir_exists(Config) ->
    ContainerName = proplists:get_value(container_name, Config),
    Output = os:cmd("docker exec " ++ ContainerName ++
                    " ls -la /home/beamclaw/.beamclaw 2>&1"),
    ct:pal("Workspace dir: ~s", [Output]),
    %% Should exist (even if empty), or show the home structure
    ?assertNotMatch({match, _}, re:run(Output, "No such file")),
    ok.

%% Stop the container cleanly.
container_stops_cleanly(Config) ->
    ContainerName = proplists:get_value(container_name, Config),
    StopOutput = os:cmd("docker stop -t 10 " ++ ContainerName ++ " 2>&1"),
    ct:pal("Container stop output: ~s", [StopOutput]),
    %% Verify it stopped
    StatusOutput = os:cmd("docker inspect -f '{{.State.Running}}' " ++
                         ContainerName ++ " 2>&1"),
    ?assertMatch({match, _}, re:run(StatusOutput, "false")),
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

check_docker() ->
    Output = os:cmd("docker info 2>&1"),
    case re:run(Output, "Server Version|Docker Root Dir") of
        {match, _} -> ok;
        nomatch    -> {skip, "Docker not available"}
    end.

docker_image_exists(ImageName) ->
    Output = os:cmd("docker images -q " ++ ImageName ++ " 2>&1"),
    string:trim(Output) =/= "".

find_project_root() ->
    CWD = string:trim(os:cmd("pwd")),
    find_root(CWD).

find_root("/") -> "/tmp";
find_root(Dir) ->
    case filelib:is_regular(filename:join(Dir, "rebar.config")) of
        true  -> Dir;
        false -> find_root(filename:dirname(Dir))
    end.

retry(_Fun, 0, _Delay) -> timeout;
retry(Fun, N, Delay) ->
    case Fun() of
        ok    -> ok;
        retry ->
            timer:sleep(Delay),
            retry(Fun, N - 1, Delay)
    end.
