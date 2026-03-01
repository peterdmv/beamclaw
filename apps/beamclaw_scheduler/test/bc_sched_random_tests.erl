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

-module(bc_sched_random_tests).
-include_lib("eunit/include/eunit.hrl").

%% --- generate_slots ---

slot_count_test() ->
    %% Must return exactly N slots
    ?assertEqual(4, length(bc_sched_random:generate_slots(86400000, 4))),
    ?assertEqual(1, length(bc_sched_random:generate_slots(3600000, 1))),
    ?assertEqual(10, length(bc_sched_random:generate_slots(86400000, 10))).

slots_sorted_test() ->
    %% Offsets must be sorted ascending
    Slots = bc_sched_random:generate_slots(86400000, 4),
    ?assertEqual(Slots, lists:sort(Slots)).

slots_within_interval_test() ->
    %% All offsets must be within [0, IntervalMs)
    IntervalMs = 86400000,
    Slots = bc_sched_random:generate_slots(IntervalMs, 4),
    lists:foreach(fun(Offset) ->
        ?assert(Offset >= 0),
        ?assert(Offset < IntervalMs)
    end, Slots).

slots_in_correct_slots_test() ->
    %% Each offset must fall within its slot's range
    IntervalMs = 86400000,
    Count = 4,
    SlotMs = IntervalMs div Count,
    Slots = bc_sched_random:generate_slots(IntervalMs, Count),
    lists:foreach(fun({Idx, Offset}) ->
        SlotStart = Idx * SlotMs,
        SlotEnd = (Idx + 1) * SlotMs,
        ?assert(Offset >= SlotStart),
        ?assert(Offset < SlotEnd)
    end, lists:zip(lists:seq(0, Count - 1), Slots)).

slots_padded_test() ->
    %% Each offset must be within the inner 80% of its slot (10% padding each side)
    IntervalMs = 100000,  %% 100s, easy to reason about
    Count = 4,
    SlotMs = IntervalMs div Count,  %% 25000ms each
    Padding = SlotMs div 10,         %% 2500ms
    Slots = bc_sched_random:generate_slots(IntervalMs, Count),
    lists:foreach(fun({Idx, Offset}) ->
        InnerStart = (Idx * SlotMs) + Padding,
        InnerEnd = ((Idx + 1) * SlotMs) - Padding,
        ?assert(Offset >= InnerStart),
        ?assert(Offset < InnerEnd)
    end, lists:zip(lists:seq(0, Count - 1), Slots)).

single_slot_test() ->
    %% N=1: single slot covering the entire interval
    Slots = bc_sched_random:generate_slots(3600000, 1),
    ?assertEqual(1, length(Slots)),
    [Offset] = Slots,
    ?assert(Offset >= 0),
    ?assert(Offset < 3600000).

empty_on_invalid_test() ->
    ?assertEqual([], bc_sched_random:generate_slots(0, 4)),
    ?assertEqual([], bc_sched_random:generate_slots(1000, 0)).

%% --- next_slot_delay ---

next_slot_delay_future_test() ->
    %% When the slot time is in the future, return delay
    CycleStartMs = erlang:system_time(millisecond),
    Slots = [5000, 15000, 25000],  %% offsets
    {delay, Delay, 0} = bc_sched_random:next_slot_delay(0, CycleStartMs, Slots, 30000),
    ?assert(Delay > 0),
    ?assert(Delay =< 5000).

next_slot_delay_past_test() ->
    %% When the slot time has passed, return fire_now
    CycleStartMs = erlang:system_time(millisecond) - 10000,
    Slots = [5000, 15000, 25000],
    {fire_now, 0} = bc_sched_random:next_slot_delay(0, CycleStartMs, Slots, 30000).

next_slot_cycle_done_test() ->
    %% When all slots have been processed, return cycle_done
    CycleStartMs = erlang:system_time(millisecond),
    Slots = [5000, 15000],
    IntervalMs = 30000,
    {cycle_done, NextStart} = bc_sched_random:next_slot_delay(2, CycleStartMs, Slots, IntervalMs),
    ?assertEqual(CycleStartMs + IntervalMs, NextStart).
