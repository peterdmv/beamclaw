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

-module(bc_sched_store).
-moduledoc """
Mnesia-backed persistence for scheduler jobs.

Provides CRUD operations on the bc_sched_jobs Mnesia table. Storage type
is disc_copies when a disc schema exists (production), ram_copies otherwise
(dev/test). Same pattern as bc_session_store.

All reads/writes use dirty operations for speed.
""".
-behaviour(gen_server).

-include("bc_sched_job.hrl").

-export([start_link/0, init_table/0]).
-export([save/1, load/1, delete/1, list_active/0, list_by_agent/1,
         update_status/2, update_after_fire/2, update_error/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Mnesia dirty_match_object uses '_' atom as wildcard in record fields.
-dialyzer({nowarn_function, [list_active/0, list_by_agent/1]}).

%% ---------------------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc "Initialize the Mnesia table (called from app start).".
-spec init_table() -> ok.
init_table() ->
    case mnesia:system_info(is_running) of
        yes ->
            case mnesia:system_info(use_dir) of
                true ->
                    ensure_table();
                false ->
                    mnesia:stop(),
                    ok = ensure_schema(),
                    ok = mnesia:start(),
                    ensure_table()
            end;
        _ ->
            ok = ensure_schema(),
            ok = mnesia:start(),
            ensure_table()
    end.

ensure_schema() ->
    case mnesia:create_schema([node()]) of
        ok                                -> ok;
        {error, {_, {already_exists, _}}} -> ok
    end.

-doc "Save (upsert) a job record.".
-spec save(#bc_sched_job{}) -> ok.
save(#bc_sched_job{} = Job) ->
    Now = erlang:system_time(second),
    Updated = Job#bc_sched_job{updated_at = Now},
    ok = mnesia:dirty_write(bc_sched_jobs, Updated).

-doc "Load a job by ID.".
-spec load(binary()) -> {ok, #bc_sched_job{}} | {error, not_found}.
load(JobId) ->
    case mnesia:dirty_read(bc_sched_jobs, JobId) of
        [Job] -> {ok, Job};
        []    -> {error, not_found}
    end.

-doc "Delete a job by ID.".
-spec delete(binary()) -> ok.
delete(JobId) ->
    ok = mnesia:dirty_delete(bc_sched_jobs, JobId).

-doc "List all active jobs (for runner initialization).".
-spec list_active() -> [#bc_sched_job{}].
list_active() ->
    All = mnesia:dirty_match_object(bc_sched_jobs, match_pattern()),
    [J || J <- All, J#bc_sched_job.status =:= active].

-doc "List all jobs for a given agent (any status except completed).".
-spec list_by_agent(binary()) -> [#bc_sched_job{}].
list_by_agent(AgentId) ->
    All = mnesia:dirty_match_object(bc_sched_jobs, match_pattern()),
    [J || J <- All,
          J#bc_sched_job.agent_id =:= AgentId,
          J#bc_sched_job.status =/= completed].

-doc "Update job status.".
-spec update_status(binary(), atom()) -> ok | {error, not_found}.
update_status(JobId, NewStatus) ->
    case load(JobId) of
        {ok, Job} ->
            save(Job#bc_sched_job{status = NewStatus});
        {error, not_found} ->
            {error, not_found}
    end.

-doc "Update job after successful fire (increment fire_count, reset error_count).".
-spec update_after_fire(binary(), non_neg_integer()) -> ok | {error, not_found}.
update_after_fire(JobId, NowEpoch) ->
    case load(JobId) of
        {ok, Job} ->
            save(Job#bc_sched_job{
                last_fired_at = NowEpoch,
                fire_count    = Job#bc_sched_job.fire_count + 1,
                error_count   = 0
            });
        {error, not_found} ->
            {error, not_found}
    end.

-doc "Update job after failed fire (increment error_count, maybe auto-pause).".
-spec update_error(binary(), non_neg_integer()) ->
    {ok, active | paused} | {error, not_found}.
update_error(JobId, NowEpoch) ->
    case load(JobId) of
        {ok, Job} ->
            NewErrorCount = Job#bc_sched_job.error_count + 1,
            MaxErrors = Job#bc_sched_job.max_errors,
            NewStatus = case NewErrorCount >= MaxErrors of
                true  -> paused;
                false -> Job#bc_sched_job.status
            end,
            save(Job#bc_sched_job{
                last_fired_at = NowEpoch,
                error_count   = NewErrorCount,
                status        = NewStatus
            }),
            {ok, NewStatus};
        {error, not_found} ->
            {error, not_found}
    end.

%% ---------------------------------------------------------------------------
%% gen_server callbacks (minimal — state is in Mnesia)
%% ---------------------------------------------------------------------------

init([]) ->
    {ok, #{}}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

ensure_table() ->
    StorageType = case mnesia:system_info(use_dir) of
        true  -> disc_copies;
        false -> ram_copies
    end,
    case mnesia:create_table(bc_sched_jobs, [
            {attributes, record_info(fields, bc_sched_job)},
            {record_name, bc_sched_job},
            {StorageType, [node()]},
            {type, set}]) of
        {atomic, ok}                   -> ok;
        {aborted, {already_exists, _}} -> ok
    end.

match_pattern() ->
    #bc_sched_job{
        job_id        = '_',
        agent_id      = '_',
        user_id       = '_',
        session_id    = '_',
        schedule_type = '_',
        schedule_spec = '_',
        prompt        = '_',
        session_mode  = '_',
        autonomy      = '_',
        delivery      = '_',
        status        = '_',
        created_at    = '_',
        updated_at    = '_',
        last_fired_at = '_',
        fire_count    = '_',
        error_count   = '_',
        max_errors    = '_',
        heartbeat     = '_',
        suppress_ok   = '_',
        active_hours  = '_'
    }.
