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

-module(bc_sandbox_reaper).
-moduledoc """
Periodic reaper for orphaned Docker sandbox containers.

Sweeps for `beamclaw-sbx-*` containers not tracked in bc_sandbox_registry
and kills/removes them. Catches ghost containers from process crashes,
supervisor force-kills, and VM restarts.
""".
-behaviour(gen_server).

-export([start_link/0, sweep_now/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_INTERVAL_MS, 60000).
-define(INITIAL_DELAY_MS, 30000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc "Trigger an immediate sweep (for testing/manual use).".
-spec sweep_now() -> ok.
sweep_now() ->
    gen_server:cast(?MODULE, sweep).

init([]) ->
    Interval = application:get_env(beamclaw_sandbox, reaper_interval_ms,
                                   ?DEFAULT_INTERVAL_MS),
    erlang:send_after(?INITIAL_DELAY_MS, self(), sweep),
    {ok, #{interval => Interval}}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(sweep, State) ->
    do_sweep(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sweep, #{interval := Interval} = State) ->
    do_sweep(),
    erlang:send_after(Interval, self(), sweep),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

-doc false.
do_sweep() ->
    try
        DockerContainers = list_sandbox_containers(),
        LiveNames = live_container_names(),
        Orphans = DockerContainers -- LiveNames,
        lists:foreach(fun(Name) ->
            reap_container(Name)
        end, Orphans)
    catch
        Class:Reason:Stack ->
            logger:warning("[sandbox_reaper] sweep failed: ~p:~p~n~p",
                           [Class, Reason, Stack])
    end.

-doc false.
list_sandbox_containers() ->
    %% List running containers matching the beamclaw-sbx- prefix.
    %% Returns container names (stripped of leading /).
    Cmd = "docker ps -q --filter name=beamclaw-sbx- 2>/dev/null",
    Output = os:cmd(Cmd),
    case string:trim(Output) of
        "" -> [];
        Ids ->
            IdList = string:tokens(Ids, "\n"),
            lists:filtermap(fun(Id) ->
                inspect_name(string:trim(Id))
            end, IdList)
    end.

-doc false.
inspect_name(ContainerId) ->
    Cmd = "docker inspect --format={{.Name}} " ++ ContainerId ++ " 2>/dev/null",
    Output = string:trim(os:cmd(Cmd)),
    %% Docker returns names with a leading /
    case Output of
        [$/ | Name] -> {true, Name};
        "" -> false;
        _Other -> false
    end.

-doc false.
live_container_names() ->
    Entries = bc_sandbox_registry:list_all(),
    lists:filtermap(fun({_Key, Pid}) ->
        try
            case is_process_alive(Pid) of
                true ->
                    Name = bc_sandbox:get_container_name(Pid),
                    {true, Name};
                false ->
                    false
            end
        catch
            _:_ -> false
        end
    end, Entries).

-doc false.
reap_container(Name) ->
    logger:info("[sandbox_reaper] reaping orphaned container: ~s", [Name]),
    KillCmd = "docker kill " ++ Name ++ " 2>/dev/null",
    _ = os:cmd(KillCmd),
    RmCmd = "docker rm -f " ++ Name ++ " 2>/dev/null",
    _ = os:cmd(RmCmd),
    %% Clean up bridge socket if it exists
    BridgeDir = application:get_env(beamclaw_sandbox, bridge_socket_dir,
                                    "/tmp/beamclaw-bridges"),
    SocketPath = filename:join(BridgeDir, Name ++ ".sock"),
    _ = file:delete(SocketPath),
    bc_obs:emit(sandbox_reaped, #{container_name => list_to_binary(Name)}),
    ok.
