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

-module(bc_sched_random).
-moduledoc """
Random slot-based scheduling algorithm for random_in schedule type.

Divides an interval into N equal slots, then picks a random point within
the inner 80% of each slot (10% padding on each side to prevent boundary
clustering). Returns offsets in milliseconds relative to cycle start.

Pure function module — no state, no side effects.
""".

-export([generate_slots/2, next_slot_delay/4]).

-doc """
Generate N random offsets within an interval.

Each offset falls within one of N equal-sized slots, padded by 10% on
each side to prevent boundary clustering.

Returns a sorted list of N offsets in milliseconds.
""".
-spec generate_slots(IntervalMs :: pos_integer(), Count :: pos_integer()) ->
    [non_neg_integer()].
generate_slots(IntervalMs, Count) when IntervalMs > 0, Count > 0 ->
    SlotMs = IntervalMs div Count,
    %% 10% padding on each side → inner 80% of each slot
    Padding = max(1, SlotMs div 10),
    InnerStart = Padding,
    InnerEnd = max(InnerStart + 1, SlotMs - Padding),
    InnerRange = InnerEnd - InnerStart,
    lists:sort([
        (SlotIdx * SlotMs) + InnerStart + (rand:uniform(InnerRange) - 1)
        || SlotIdx <- lists:seq(0, Count - 1)
    ]);
generate_slots(_, _) ->
    [].

-doc """
Calculate the delay in milliseconds until the next slot should fire.

Given the current slot index (0-based), cycle start time (epoch ms),
the list of slot offsets, and the interval, returns:
  - {fire_now, NextSlotIdx} if the slot time has passed
  - {delay, DelayMs, SlotIdx} for the next slot to schedule
  - {cycle_done, NextCycleStartMs} when all slots in this cycle are done
""".
-spec next_slot_delay(
    SlotIdx :: non_neg_integer(),
    CycleStartMs :: non_neg_integer(),
    Slots :: [non_neg_integer()],
    IntervalMs :: pos_integer()
) -> {fire_now, non_neg_integer()} |
     {delay, non_neg_integer(), non_neg_integer()} |
     {cycle_done, non_neg_integer()}.
next_slot_delay(SlotIdx, CycleStartMs, Slots, IntervalMs) ->
    Count = length(Slots),
    case SlotIdx >= Count of
        true ->
            %% All slots in this cycle have fired
            {cycle_done, CycleStartMs + IntervalMs};
        false ->
            Offset = lists:nth(SlotIdx + 1, Slots),
            FireTimeMs = CycleStartMs + Offset,
            NowMs = erlang:system_time(millisecond),
            case FireTimeMs =< NowMs of
                true ->
                    {fire_now, SlotIdx};
                false ->
                    {delay, FireTimeMs - NowMs, SlotIdx}
            end
    end.
