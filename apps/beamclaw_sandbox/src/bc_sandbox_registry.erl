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

-module(bc_sandbox_registry).
-moduledoc """
Sandbox registry — maps {SessionId, Scope} to sandbox pid.

Monitors sandbox processes; cleans up on DOWN.
Provides get_or_create/3 for lazy sandbox provisioning.
""".
-behaviour(gen_server).

-export([start_link/0, get_or_create/3, lookup/2, destroy/2, list_all/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB, bc_sandbox_registry).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc "Get existing sandbox or create a new one for {SessionId, Scope}.".
-spec get_or_create(SessionId :: binary(), Scope :: atom(),
                    Config :: map()) ->
    {ok, pid()} | {error, term()}.
get_or_create(SessionId, Scope, Config) ->
    gen_server:call(?MODULE, {get_or_create, SessionId, Scope, Config}, 30000).

-doc "Look up an existing sandbox by session and scope.".
-spec lookup(SessionId :: binary(), Scope :: atom()) ->
    {ok, pid()} | {error, not_found}.
lookup(SessionId, Scope) ->
    case ets:lookup(?TAB, {SessionId, Scope}) of
        [{{SessionId, Scope}, Pid}] ->
            case is_process_alive(Pid) of
                true  -> {ok, Pid};
                false -> {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

-doc "Destroy a sandbox by session and scope.".
-spec destroy(SessionId :: binary(), Scope :: atom()) -> ok.
destroy(SessionId, Scope) ->
    gen_server:call(?MODULE, {destroy, SessionId, Scope}).

-doc "List all registered sandboxes.".
-spec list_all() -> [{{binary(), atom()}, pid()}].
list_all() ->
    ets:tab2list(?TAB).

%% State: #{monitors => #{pid() => {reference(), Key, ContainerName}}}
init([]) ->
    _ = ets:new(?TAB, [set, named_table, public, {read_concurrency, true}]),
    {ok, #{monitors => #{}}}.

handle_call({get_or_create, SessionId, Scope, Config}, _From, State) ->
    Key = {SessionId, Scope},
    case ets:lookup(?TAB, Key) of
        [{Key, Pid}] when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    {reply, {ok, Pid}, State};
                false ->
                    ets:delete(?TAB, Key),
                    start_and_register(Key, Config, State)
            end;
        [] ->
            start_and_register(Key, Config, State)
    end;
handle_call({destroy, SessionId, Scope}, _From, State) ->
    Key = {SessionId, Scope},
    case ets:lookup(?TAB, Key) of
        [{Key, Pid}] ->
            bc_sandbox:stop(Pid),
            ets:delete(?TAB, Key),
            {reply, ok, remove_monitor(Pid, State)};
        [] ->
            {reply, ok, State}
    end;
handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #{monitors := Mons} = State) ->
    case maps:get(Pid, Mons, undefined) of
        undefined ->
            {noreply, State};
        {_Ref2, Key, ContainerName} ->
            ets:delete(?TAB, Key),
            %% Best-effort immediate container cleanup (fire-and-forget)
            cleanup_container_async(ContainerName),
            {noreply, State#{monitors := maps:remove(Pid, Mons)}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

start_and_register(Key, Config, #{monitors := Mons} = State) ->
    FullConfig = Config#{sandbox_key => Key},
    case bc_sandbox_sup:start_sandbox(FullConfig) of
        {ok, Pid} ->
            ets:insert(?TAB, {Key, Pid}),
            Ref = erlang:monitor(process, Pid),
            %% Fetch container name so DOWN handler can clean up immediately
            ContainerName = try bc_sandbox:get_container_name(Pid)
                            catch _:_ -> "unknown"
                            end,
            NewMons = Mons#{Pid => {Ref, Key, ContainerName}},
            {reply, {ok, Pid}, State#{monitors := NewMons}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

remove_monitor(Pid, #{monitors := Mons} = State) ->
    case maps:get(Pid, Mons, undefined) of
        undefined -> State;
        {Ref, _Key, _ContainerName} ->
            erlang:demonitor(Ref, [flush]),
            State#{monitors := maps:remove(Pid, Mons)}
    end.

cleanup_container_async(ContainerName) ->
    spawn(fun() ->
        try
            KillCmd = "docker kill " ++ ContainerName ++ " 2>/dev/null",
            _ = os:cmd(KillCmd),
            RmCmd = "docker rm -f " ++ ContainerName ++ " 2>/dev/null",
            _ = os:cmd(RmCmd),
            %% Clean up bridge socket
            BridgeDir = application:get_env(beamclaw_sandbox, bridge_socket_dir,
                                            "/tmp/beamclaw-bridges"),
            SocketPath = filename:join(BridgeDir, ContainerName ++ ".sock"),
            _ = file:delete(SocketPath),
            logger:info("[sandbox_registry] cleaned up container on DOWN: ~s",
                        [ContainerName])
        catch
            _:_ -> ok
        end
    end).
