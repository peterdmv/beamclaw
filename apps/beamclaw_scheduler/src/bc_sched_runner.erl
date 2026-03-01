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

-module(bc_sched_runner).
-moduledoc """
Timer management gen_server for the scheduler.

Loads all active jobs on init, schedules erlang:send_after timers for each.
When a timer fires, sends {execute, Job} to bc_sched_executor and reschedules
as appropriate.

Manages #{job_id => timer_ref} and #{job_id => {slot_idx, slots, cycle_start}}
for random_in jobs.

Lightweight — never blocks. Heavy work is delegated to bc_sched_executor.
""".
-behaviour(gen_server).

-include("bc_sched_job.hrl").

-export([start_link/0]).
-export([schedule/1, cancel/1, pause/1, resume/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    timers  = #{} :: #{binary() => reference()},
    random  = #{} :: #{binary() => {non_neg_integer(), [non_neg_integer()], non_neg_integer()}}
    %% random: job_id => {slot_idx, slots, cycle_start_ms}
}).

%% ---------------------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc "Schedule timers for a job.".
-spec schedule(#bc_sched_job{}) -> ok.
schedule(Job) ->
    gen_server:cast(?MODULE, {schedule, Job}).

-doc "Cancel all timers for a job.".
-spec cancel(binary()) -> ok.
cancel(JobId) ->
    gen_server:cast(?MODULE, {cancel, JobId}).

-doc "Pause a job: cancel timers and update status.".
-spec pause(binary()) -> ok | {error, not_found}.
pause(JobId) ->
    gen_server:call(?MODULE, {pause, JobId}).

-doc "Resume a paused job: reload from store and reschedule.".
-spec resume(binary()) -> ok | {error, not_found | not_paused}.
resume(JobId) ->
    gen_server:call(?MODULE, {resume, JobId}).

%% ---------------------------------------------------------------------------
%% gen_server callbacks
%% ---------------------------------------------------------------------------

init([]) ->
    %% Load active jobs after a short delay to let Mnesia finish initializing
    erlang:send_after(100, self(), load_active_jobs),
    {ok, #state{}}.

handle_call({pause, JobId}, _From, State) ->
    case bc_sched_store:load(JobId) of
        {ok, #bc_sched_job{status = active}} ->
            NewState = cancel_timer(JobId, State),
            ok = bc_sched_store:update_status(JobId, paused),
            bc_obs:emit(sched_job_paused, #{job_id => JobId, reason => manual}),
            {reply, ok, NewState};
        {ok, _} ->
            {reply, {error, not_active}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({resume, JobId}, _From, State) ->
    case bc_sched_store:load(JobId) of
        {ok, #bc_sched_job{status = paused} = Job} ->
            ok = bc_sched_store:update_status(JobId, active),
            NewState = schedule_job(Job#bc_sched_job{status = active}, State),
            {reply, ok, NewState};
        {ok, _} ->
            {reply, {error, not_paused}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast({schedule, Job}, State) ->
    NewState = schedule_job(Job, State),
    {noreply, NewState};

handle_cast({cancel, JobId}, State) ->
    NewState = cancel_timer(JobId, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(load_active_jobs, State) ->
    Jobs = try bc_sched_store:list_active()
           catch _:_ -> [] end,
    NewState = lists:foldl(fun(Job, AccState) ->
        schedule_job(Job, AccState)
    end, State, Jobs),
    logger:info("[sched_runner] loaded ~p active jobs", [length(Jobs)]),
    {noreply, NewState};

handle_info({fire, JobId}, State) ->
    NewState = handle_fire(JobId, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal — scheduling
%% ---------------------------------------------------------------------------

schedule_job(#bc_sched_job{job_id = JobId, schedule_type = at,
                           schedule_spec = #{epoch_seconds := EpochSec}}, State) ->
    NowMs = erlang:system_time(millisecond),
    TargetMs = EpochSec * 1000,
    Delay = max(0, TargetMs - NowMs),
    set_timer(JobId, Delay, State);

schedule_job(#bc_sched_job{job_id = JobId, schedule_type = every,
                           schedule_spec = #{interval_ms := IntervalMs},
                           last_fired_at = LastFired}, State) ->
    NowSec = erlang:system_time(second),
    Delay = case LastFired of
        undefined -> 0;
        _ ->
            ElapsedMs = (NowSec - LastFired) * 1000,
            max(0, IntervalMs - ElapsedMs)
    end,
    set_timer(JobId, Delay, State);

schedule_job(#bc_sched_job{job_id = JobId, schedule_type = random_in,
                           schedule_spec = #{interval_ms := IntervalMs, count := Count},
                           last_fired_at = LastFired, fire_count = FireCount}, State) ->
    NowMs = erlang:system_time(millisecond),
    %% Determine cycle state
    {SlotIdx, Slots, CycleStartMs} = case maps:get(JobId, State#state.random, undefined) of
        undefined ->
            %% Fresh start or restart: figure out where we are in the cycle
            NewSlots = bc_sched_random:generate_slots(IntervalMs, Count),
            CS = case LastFired of
                undefined -> NowMs;
                _ ->
                    %% Determine which cycle we're in
                    FiredInCycle = FireCount rem Count,
                    case FiredInCycle of
                        0 -> NowMs;  %% Start of new cycle
                        _ ->
                            %% Estimate cycle start from last fire
                            LastFiredMs = LastFired * 1000,
                            LastFiredMs - (lists:nth(FiredInCycle, NewSlots))
                    end
            end,
            SI = case LastFired of
                undefined -> 0;
                _ -> FireCount rem Count
            end,
            {SI, NewSlots, CS};
        Existing ->
            Existing
    end,
    %% Schedule next slot
    NewState = State#state{random = maps:put(JobId, {SlotIdx, Slots, CycleStartMs},
                                             State#state.random)},
    case bc_sched_random:next_slot_delay(SlotIdx, CycleStartMs, Slots, IntervalMs) of
        {fire_now, _} ->
            %% Fire immediately
            set_timer(JobId, 0, NewState);
        {delay, DelayMs, _} ->
            set_timer(JobId, DelayMs, NewState);
        {cycle_done, NextCycleStartMs} ->
            %% Generate new slots for next cycle
            NewSlots2 = bc_sched_random:generate_slots(IntervalMs, Count),
            NowMs2 = erlang:system_time(millisecond),
            CycleDelay = max(0, NextCycleStartMs - NowMs2),
            State2 = NewState#state{
                random = maps:put(JobId, {0, NewSlots2, NextCycleStartMs},
                                  NewState#state.random)
            },
            set_timer(JobId, CycleDelay, State2)
    end;

schedule_job(_, State) ->
    State.

set_timer(JobId, DelayMs, State) ->
    %% Cancel existing timer if any
    State1 = cancel_timer(JobId, State),
    Ref = erlang:send_after(DelayMs, self(), {fire, JobId}),
    State1#state{timers = maps:put(JobId, Ref, State1#state.timers)}.

cancel_timer(JobId, #state{timers = Timers, random = Random} = State) ->
    case maps:get(JobId, Timers, undefined) of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    State#state{
        timers = maps:remove(JobId, Timers),
        random = maps:remove(JobId, Random)
    }.

%% ---------------------------------------------------------------------------
%% Internal — fire handling
%% ---------------------------------------------------------------------------

handle_fire(JobId, State) ->
    case bc_sched_store:load(JobId) of
        {ok, #bc_sched_job{status = active} = Job} ->
            %% Check active hours
            case is_within_active_hours(Job) of
                true ->
                    %% Send to executor
                    bc_sched_executor:execute(Job),
                    %% Reschedule based on type
                    reschedule_after_fire(Job, State);
                false ->
                    %% Outside active hours — reschedule without firing
                    reschedule_after_fire(Job, State)
            end;
        _ ->
            %% Job no longer active — clean up timer state
            cancel_timer(JobId, State)
    end.

reschedule_after_fire(#bc_sched_job{job_id = JobId, schedule_type = at}, State) ->
    %% One-shot: mark completed, no reschedule
    bc_sched_store:update_status(JobId, completed),
    bc_obs:emit(sched_job_completed, #{job_id => JobId}),
    cancel_timer(JobId, State);

reschedule_after_fire(#bc_sched_job{job_id = JobId, schedule_type = every,
                                    schedule_spec = #{interval_ms := IntervalMs}}, State) ->
    set_timer(JobId, IntervalMs, State);

reschedule_after_fire(#bc_sched_job{job_id = JobId, schedule_type = random_in,
                                    schedule_spec = #{interval_ms := IntervalMs, count := Count}},
                      State) ->
    case maps:get(JobId, State#state.random, undefined) of
        {SlotIdx, Slots, CycleStartMs} ->
            NextIdx = SlotIdx + 1,
            case bc_sched_random:next_slot_delay(NextIdx, CycleStartMs, Slots, IntervalMs) of
                {fire_now, _} ->
                    NewState = State#state{
                        random = maps:put(JobId, {NextIdx, Slots, CycleStartMs},
                                          State#state.random)
                    },
                    set_timer(JobId, 0, NewState);
                {delay, DelayMs, _} ->
                    NewState = State#state{
                        random = maps:put(JobId, {NextIdx, Slots, CycleStartMs},
                                          State#state.random)
                    },
                    set_timer(JobId, DelayMs, NewState);
                {cycle_done, NextCycleStartMs} ->
                    NewSlots = bc_sched_random:generate_slots(IntervalMs, Count),
                    NowMs = erlang:system_time(millisecond),
                    CycleDelay = max(0, NextCycleStartMs - NowMs),
                    NewState = State#state{
                        random = maps:put(JobId, {0, NewSlots, NextCycleStartMs},
                                          State#state.random)
                    },
                    set_timer(JobId, CycleDelay, NewState)
            end;
        undefined ->
            %% Lost random state — regenerate
            NewSlots = bc_sched_random:generate_slots(IntervalMs, Count),
            NowMs = erlang:system_time(millisecond),
            NewState = State#state{
                random = maps:put(JobId, {0, NewSlots, NowMs}, State#state.random)
            },
            set_timer(JobId, 0, NewState)
    end.

is_within_active_hours(#bc_sched_job{active_hours = undefined}) ->
    true;
is_within_active_hours(#bc_sched_job{active_hours = {Start, End}}) ->
    {_, {Hour, _, _}} = calendar:universal_time(),
    case Start =< End of
        true  -> Hour >= Start andalso Hour < End;
        false -> Hour >= Start orelse Hour < End   %% wraps midnight
    end.
