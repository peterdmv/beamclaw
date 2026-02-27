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

-module(bc_sandbox).
-moduledoc """
Per-sandbox Docker container lifecycle manager.

Each bc_sandbox gen_server owns one Docker container. On init it:
  1. Generates a unique container name
  2. Creates a Unix domain socket for the bridge
  3. Starts the container via `docker run`
  4. Handles bridge requests from the Python code inside the container

API:
  - exec_script/4: Write and execute a script in the container
  - handle_bridge_request/2: Process a bridge JSON-RPC request
  - stop/1: Graceful shutdown (kills + removes container)
""".
-behaviour(gen_server).

-export([start_link/1, exec_script/4, handle_bridge_request/2, stop/1,
         get_status/1, get_container_name/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(sandbox_state, {
    id               :: binary(),
    session_id       :: binary(),
    scope            :: atom(),
    container_name   :: string(),
    bridge_socket    :: string() | undefined,
    listen_socket    :: port() | undefined,
    client_socket    :: port() | undefined,
    bridge_buf       :: binary(),
    status           :: starting | ready | executing | stopping | stopped | failed,
    config           :: map(),
    tool_bridge_fn   :: function() | undefined,
    pending_exec     :: {pid(), reference()} | undefined,
    error_msg        :: binary() | undefined
}).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

-doc "Execute a script inside the sandbox container. Returns stdout/stderr output.".
-spec exec_script(Pid :: pid(), Script :: binary(), Language :: binary(),
                  ToolBridgeFn :: function()) ->
    {ok, binary()} | {error, term()}.
exec_script(Pid, Script, Language, ToolBridgeFn) ->
    Timeout = application:get_env(beamclaw_sandbox, timeout_seconds, 60) * 1000,
    gen_server:call(Pid, {exec_script, Script, Language, ToolBridgeFn},
                    Timeout + 5000).

-doc "Process a bridge request from the container. Called internally.".
-spec handle_bridge_request(Pid :: pid(), Request :: binary()) -> ok.
handle_bridge_request(Pid, Request) ->
    gen_server:cast(Pid, {bridge_request, Request}).

-doc "Stop the sandbox (kills and removes the Docker container).".
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop, 15000).

-doc "Get sandbox status.".
-spec get_status(Pid :: pid()) -> map().
get_status(Pid) ->
    gen_server:call(Pid, get_status).

-doc "Get the Docker container name.".
-spec get_container_name(Pid :: pid()) -> string().
get_container_name(Pid) ->
    gen_server:call(Pid, get_container_name).

%% ---------------------------------------------------------------------------
%% gen_server callbacks
%% ---------------------------------------------------------------------------

init(Config) ->
    {SessionId, Scope} = maps:get(sandbox_key, Config, {<<"unknown">>, session}),
    ContainerName = bc_sandbox_docker:container_name(Config),
    Id = list_to_binary(ContainerName),

    BridgeSocketDir = maps:get(bridge_socket_dir, Config,
                               application:get_env(beamclaw_sandbox,
                                                   bridge_socket_dir,
                                                   "/tmp/beamclaw-bridges")),
    BridgeSocket = filename:join(BridgeSocketDir, ContainerName ++ ".sock"),
    ok = filelib:ensure_dir(BridgeSocket),

    FullConfig = Config#{container_name => ContainerName,
                         bridge_socket => BridgeSocket},

    State = #sandbox_state{
        id             = Id,
        session_id     = SessionId,
        scope          = Scope,
        container_name = ContainerName,
        bridge_socket  = BridgeSocket,
        listen_socket  = undefined,
        client_socket  = undefined,
        bridge_buf     = <<>>,
        status         = starting,
        config         = FullConfig,
        tool_bridge_fn = undefined,
        pending_exec   = undefined,
        error_msg      = undefined
    },

    %% Start container asynchronously
    self() ! start_container,
    {ok, State}.

handle_call({exec_script, Script, Language, ToolBridgeFn}, From,
            #sandbox_state{status = ready, container_name = Name,
                           config = Config} = State) ->
    TimeoutSec = maps:get(timeout_seconds, Config,
                          application:get_env(beamclaw_sandbox,
                                              timeout_seconds, 60)),
    MaxOutput = maps:get(max_output_bytes, Config,
                         application:get_env(beamclaw_sandbox,
                                             max_output_bytes, 1048576)),
    %% Write script to temp file in container's /tmp via docker exec.
    %% We use base64 encoding to safely transfer arbitrary script content,
    %% because `docker cp` fails on --read-only containers (it writes to the
    %% rootfs layer, not to tmpfs mounts).
    ScriptName = "script_" ++ integer_to_list(erlang:unique_integer([positive])),
    Extension = case binary_to_list(Language) of
        "python" -> ".py";
        "bash"   -> ".sh";
        _        -> ".py"
    end,
    ScriptPath = "/tmp/" ++ ScriptName ++ Extension,

    %% Base64 is safe for shell embedding (only [A-Za-z0-9+/=]).
    B64 = binary_to_list(base64:encode(Script)),
    WriteCmd = "docker exec " ++ Name
               ++ " sh -c 'echo " ++ B64 ++ " | base64 -d > "
               ++ ScriptPath ++ "' 2>&1",
    _ = os:cmd(WriteCmd),

    %% Execute script via docker exec with timeout
    LangStr = binary_to_list(Language),
    ExecArgs = bc_sandbox_docker:exec_args(Name, ScriptPath, LangStr),
    ExecCmd = string:join(["docker" | ExecArgs], " ")
              ++ " 2>&1",

    %% Run async so we can handle bridge requests while script runs
    Parent = self(),
    _WorkerPid = spawn_link(fun() ->
        Output = os:cmd("timeout " ++ integer_to_list(TimeoutSec) ++ " "
                         ++ ExecCmd),
        Bin = unicode:characters_to_binary(Output),
        Truncated = case byte_size(Bin) > MaxOutput of
            true  -> binary:part(Bin, 0, MaxOutput);
            false -> Bin
        end,
        Parent ! {exec_complete, Truncated}
    end),

    {noreply, State#sandbox_state{status = executing,
                                  tool_bridge_fn = ToolBridgeFn,
                                  pending_exec = From}};
handle_call({exec_script, _Script, _Language, _Fn}, _From,
            #sandbox_state{status = failed, error_msg = Err} = State) ->
    {reply, {error, {sandbox_failed, Err}}, State};
handle_call({exec_script, _Script, _Language, _Fn}, _From,
            #sandbox_state{status = Status} = State) ->
    {reply, {error, {not_ready, Status}}, State};

handle_call(stop, _From, State) ->
    NewState = do_stop(State),
    {stop, normal, ok, NewState};

handle_call(get_status, _From, #sandbox_state{status = Status,
                                               container_name = Name,
                                               session_id = Sid,
                                               scope = Scope} = State) ->
    {reply, #{status => Status, container_name => list_to_binary(Name),
              session_id => Sid, scope => Scope}, State};

handle_call(get_container_name, _From, #sandbox_state{container_name = N} = State) ->
    {reply, N, State};

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast({bridge_request, RequestBin},
            #sandbox_state{tool_bridge_fn = BridgeFn} = State) ->
    %% Decode and dispatch bridge request
    case bc_sandbox_bridge:decode_request(RequestBin) of
        {ok, #{method := Method, params := Params, id := ReqId}} ->
            Fn = case BridgeFn of
                undefined -> fun(_N, _A) -> {error, <<"No tool bridge available">>} end;
                F -> F
            end,
            Result = bc_sandbox_bridge:dispatch(Method, Params, Fn),
            ResponseBin = case Result of
                {ok, R} -> bc_sandbox_bridge:encode_response(ReqId, R);
                {error, Code, Msg} -> bc_sandbox_bridge:encode_error(ReqId, Code, Msg)
            end,
            %% Send response back through the bridge socket
            case State#sandbox_state.client_socket of
                undefined -> ok;
                Socket ->
                    Frame = bc_sandbox_bridge:encode_length_prefixed(ResponseBin),
                    gen_tcp:send(Socket, Frame)
            end;
        {error, _Reason} ->
            ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_container, #sandbox_state{config = Config,
                                             bridge_socket = SocketPath} = State) ->
    %% Start bridge listener first
    _ = file:delete(SocketPath),
    ListenResult = gen_tcp:listen(0, [binary, {packet, raw}, {active, true},
                                      {ifaddr, {local, SocketPath}}]),
    NewState = case ListenResult of
        {ok, ListenSock} ->
            %% Make socket world-writable so sandbox user (uid 1000) can connect
            file:change_mode(SocketPath, 8#0777),
            %% Start Docker container
            RunArgs = bc_sandbox_docker:run_args(Config),
            RunCmd = string:join(["docker" | RunArgs], " ") ++ " 2>&1",
            CmdOutput = os:cmd(RunCmd),
            Trimmed = string:trim(CmdOutput),
            case is_docker_success(Trimmed) of
                true ->
                    bc_obs:emit(sandbox_start, #{
                        sandbox_id => State#sandbox_state.id,
                        session_id => State#sandbox_state.session_id
                    }),
                    self() ! accept_bridge,
                    State#sandbox_state{status = ready,
                                        listen_socket = ListenSock};
                false ->
                    ErrBin = list_to_binary(Trimmed),
                    logger:warning("[sandbox] docker run failed for ~s: ~s",
                                   [State#sandbox_state.container_name, ErrBin]),
                    gen_tcp:close(ListenSock),
                    State#sandbox_state{status = failed, error_msg = ErrBin}
            end;
        {error, Reason} ->
            logger:warning("[sandbox] Bridge socket listen failed ~s: ~p",
                           [SocketPath, Reason]),
            State#sandbox_state{status = failed,
                                error_msg = iolist_to_binary(
                                    io_lib:format("Bridge socket failed: ~p",
                                                  [Reason]))}
    end,
    {noreply, NewState};

handle_info(accept_bridge, #sandbox_state{listen_socket = undefined} = State) ->
    {noreply, State};
handle_info(accept_bridge, #sandbox_state{listen_socket = LSock} = State) ->
    case gen_tcp:accept(LSock, 100) of
        {ok, ClientSock} ->
            {noreply, State#sandbox_state{client_socket = ClientSock}};
        {error, timeout} ->
            %% Retry accepting — container may not have connected yet
            erlang:send_after(500, self(), accept_bridge),
            {noreply, State};
        {error, _Reason} ->
            {noreply, State}
    end;

handle_info({tcp, Socket, Data}, #sandbox_state{client_socket = Socket,
                                                 bridge_buf = Buf} = State) ->
    NewBuf = <<Buf/binary, Data/binary>>,
    NewState = process_bridge_buffer(State#sandbox_state{bridge_buf = NewBuf}),
    {noreply, NewState};

handle_info({tcp_closed, Socket}, #sandbox_state{client_socket = Socket} = State) ->
    {noreply, State#sandbox_state{client_socket = undefined}};

handle_info({exec_complete, Output}, #sandbox_state{pending_exec = From} = State)
  when From =/= undefined ->
    gen_server:reply(From, {ok, Output}),
    {noreply, State#sandbox_state{status = ready, pending_exec = undefined,
                                  tool_bridge_fn = undefined}};
handle_info({exec_complete, _Output}, State) ->
    {noreply, State#sandbox_state{status = ready}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    do_stop(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

do_stop(#sandbox_state{container_name = Name, bridge_socket = Socket,
                        listen_socket = LSock, client_socket = CSock,
                        status = Status} = State) ->
    %% Close sockets
    case CSock of undefined -> ok; _ -> gen_tcp:close(CSock) end,
    case LSock of undefined -> ok; _ -> gen_tcp:close(LSock) end,
    %% Remove socket file
    case Socket of undefined -> ok; _ -> file:delete(Socket) end,
    %% Kill and remove container if not already stopped
    case Status of
        stopped -> ok;
        _ ->
            KillCmd = string:join(["docker" | bc_sandbox_docker:kill_args(Name)],
                                  " ") ++ " 2>&1",
            _ = os:cmd(KillCmd),
            RmCmd = string:join(["docker" | bc_sandbox_docker:rm_args(Name)],
                                " ") ++ " 2>&1",
            _ = os:cmd(RmCmd),
            bc_obs:emit(sandbox_stop, #{
                sandbox_id => State#sandbox_state.id,
                session_id => State#sandbox_state.session_id
            })
    end,
    State#sandbox_state{status = stopped, listen_socket = undefined,
                         client_socket = undefined}.

process_bridge_buffer(#sandbox_state{bridge_buf = Buf} = State) ->
    case bc_sandbox_bridge:decode_length_prefixed(Buf) of
        {ok, Payload, Rest} ->
            handle_bridge_request(self(), Payload),
            process_bridge_buffer(State#sandbox_state{bridge_buf = Rest});
        need_more ->
            State
    end.

is_docker_success("") -> false;
is_docker_success(Output) ->
    %% docker run -d prints a hex container ID on success (12 or 64 chars)
    lists:all(fun(C) ->
        (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f)
    end, Output).
