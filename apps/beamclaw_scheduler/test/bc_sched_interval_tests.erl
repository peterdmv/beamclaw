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

-module(bc_sched_interval_tests).
-include_lib("eunit/include/eunit.hrl").

%% --- Seconds ---

seconds_test() ->
    ?assertEqual({ok, 1000}, bc_sched_interval:parse(<<"1s">>)),
    ?assertEqual({ok, 30000}, bc_sched_interval:parse(<<"30s">>)),
    ?assertEqual({ok, 1000}, bc_sched_interval:parse("1s")).

%% --- Minutes ---

minutes_test() ->
    ?assertEqual({ok, 60000}, bc_sched_interval:parse(<<"1m">>)),
    ?assertEqual({ok, 300000}, bc_sched_interval:parse(<<"5m">>)),
    ?assertEqual({ok, 1800000}, bc_sched_interval:parse(<<"30m">>)).

%% --- Hours ---

hours_test() ->
    ?assertEqual({ok, 3600000}, bc_sched_interval:parse(<<"1h">>)),
    ?assertEqual({ok, 7200000}, bc_sched_interval:parse(<<"2h">>)),
    ?assertEqual({ok, 86400000}, bc_sched_interval:parse(<<"24h">>)).

%% --- Days ---

days_test() ->
    ?assertEqual({ok, 86400000}, bc_sched_interval:parse(<<"1d">>)),
    ?assertEqual({ok, 604800000}, bc_sched_interval:parse(<<"7d">>)).

%% --- Case insensitive ---

case_insensitive_test() ->
    ?assertEqual({ok, 3600000}, bc_sched_interval:parse(<<"1H">>)),
    ?assertEqual({ok, 60000}, bc_sched_interval:parse(<<"1M">>)),
    ?assertEqual({ok, 1000}, bc_sched_interval:parse(<<"1S">>)),
    ?assertEqual({ok, 86400000}, bc_sched_interval:parse(<<"1D">>)).

%% --- Whitespace handling ---

whitespace_test() ->
    ?assertEqual({ok, 1000}, bc_sched_interval:parse(<<"  1s  ">>)).

%% --- Large values ---

large_value_test() ->
    ?assertEqual({ok, 365 * 86400000}, bc_sched_interval:parse(<<"365d">>)).

%% --- Error cases ---

empty_test() ->
    ?assertMatch({error, _}, bc_sched_interval:parse(<<>>)),
    ?assertMatch({error, _}, bc_sched_interval:parse(<<" ">>)).

missing_suffix_test() ->
    ?assertMatch({error, _}, bc_sched_interval:parse(<<"30">>)).

missing_number_test() ->
    ?assertMatch({error, _}, bc_sched_interval:parse(<<"s">>)).

invalid_suffix_test() ->
    ?assertMatch({error, _}, bc_sched_interval:parse(<<"5x">>)),
    ?assertMatch({error, _}, bc_sched_interval:parse(<<"10ms">>)).

zero_test() ->
    ?assertMatch({error, _}, bc_sched_interval:parse(<<"0s">>)).

invalid_type_test() ->
    ?assertMatch({error, _}, bc_sched_interval:parse(123)).
