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

-module(bc_sandbox_docker).
-moduledoc """
Pure-function module for building Docker command arguments.

Generates `docker run`, `docker exec`, `docker kill`, `docker rm` args
from sandbox configuration. Container naming:
`beamclaw-sbx-<prefix>-<unique>`. Fully testable without Docker.
""".

-export([run_args/1, exec_args/3, kill_args/1, rm_args/1,
         container_name/1, image_exists_args/1, build_args/2]).

-doc "Build `docker run` args for starting a sandbox container.".
-spec run_args(Config :: map()) -> [string()].
run_args(Config) ->
    Name = maps:get(container_name, Config),
    Image = maps:get(docker_image, Config, "beamclaw-sandbox:latest"),
    MemLimit = maps:get(memory_limit, Config, "512m"),
    CpuLimit = maps:get(cpu_limit, Config, "1.0"),
    Network = maps:get(network, Config, none),
    BridgeSocketDir = maps:get(bridge_socket_dir, Config, "/tmp/beamclaw-bridges"),
    BridgeSocket = maps:get(bridge_socket, Config, undefined),
    WorkspaceMount = maps:get(workspace_mount, Config, none),
    WorkspaceDir = maps:get(workspace_dir, Config, undefined),

    Base = ["run", "-d", "--name", Name,
            "--cap-drop", "ALL",
            "--read-only",
            "--security-opt", "no-new-privileges",
            "--tmpfs", "/tmp:rw,noexec,nosuid,size=64m",
            "--memory", MemLimit,
            "--cpus", CpuLimit,
            "--pids-limit", "256"],

    NetArgs = network_args(Network),
    SocketArgs = socket_mount_args(BridgeSocket, BridgeSocketDir),
    WsArgs = workspace_mount_args(WorkspaceMount, WorkspaceDir),
    EnvArgs = env_args(Config),

    Base ++ NetArgs ++ SocketArgs ++ WsArgs ++ EnvArgs ++ [Image].

-doc "Build `docker exec` args for running a script inside a container.".
-spec exec_args(ContainerName :: string(), ScriptPath :: string(),
                Language :: string()) -> [string()].
exec_args(ContainerName, ScriptPath, Language) ->
    Interpreter = case Language of
        "python" -> "python3";
        "bash"   -> "sh";
        _        -> "python3"
    end,
    ["exec", ContainerName, Interpreter, ScriptPath].

-doc "Build `docker kill` args.".
-spec kill_args(ContainerName :: string()) -> [string()].
kill_args(ContainerName) ->
    ["kill", ContainerName].

-doc "Build `docker rm -f` args.".
-spec rm_args(ContainerName :: string()) -> [string()].
rm_args(ContainerName) ->
    ["rm", "-f", ContainerName].

-doc "Build `docker image inspect` args for checking image existence.".
-spec image_exists_args(Image :: string()) -> [string()].
image_exists_args(Image) ->
    ["image", "inspect", "--format", "{{.Id}}", Image].

-doc "Build `docker build` args from a Dockerfile path and image tag.".
-spec build_args(DockerfilePath :: string(), Tag :: string()) -> [string()].
build_args(DockerfilePath, Tag) ->
    Dir = filename:dirname(DockerfilePath),
    ["build", "-f", DockerfilePath, "-t", Tag, Dir].

-doc "Generate a unique container name from config.".
-spec container_name(Config :: map()) -> string().
container_name(Config) ->
    {SessionId, Scope} = maps:get(sandbox_key, Config, {<<"unknown">>, session}),
    Prefix = truncate_session_id(SessionId),
    ScopeStr = atom_to_list(Scope),
    Unique = integer_to_list(erlang:unique_integer([positive])),
    "beamclaw-sbx-" ++ ScopeStr ++ "-" ++ Prefix ++ "-" ++ Unique.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

network_args(none)    -> ["--network", "none"];
network_args(bridge)  -> ["--network", "bridge"];
network_args(host)    -> ["--network", "host"];
network_args(Other)   -> ["--network", atom_to_list(Other)].

socket_mount_args(undefined, _Dir) -> [];
socket_mount_args(Socket, _Dir) ->
    %% Mount the specific bridge socket file into the container
    ["-v", Socket ++ ":" ++ "/tmp/bridge.sock"].

workspace_mount_args(none, _Dir) -> [];
workspace_mount_args(_Mode, undefined) -> [];
workspace_mount_args(ro, Dir) ->
    ["-v", Dir ++ ":" ++ "/workspace:ro"];
workspace_mount_args(rw, Dir) ->
    ["-v", Dir ++ ":" ++ "/workspace:rw"].

env_args(Config) ->
    EnvVars = maps:get(env_vars, Config, []),
    lists:flatmap(fun({K, V}) ->
        ["-e", K ++ "=" ++ V]
    end, EnvVars).

truncate_session_id(SessionId) when is_binary(SessionId) ->
    %% Session IDs are <<"session-<hex>">>, skip the prefix to get the unique part
    Stripped = case SessionId of
        <<"session-", Rest/binary>> -> Rest;
        Other -> Other
    end,
    Prefix = binary_to_list(binary:part(Stripped, 0,
                                        min(8, byte_size(Stripped)))),
    %% Sanitize for Docker naming (alphanumeric + dash)
    [case C of
         C when C >= $a, C =< $z -> C;
         C when C >= $A, C =< $Z -> C + 32;  %% lowercase
         C when C >= $0, C =< $9 -> C;
         _ -> $-
     end || C <- Prefix].
