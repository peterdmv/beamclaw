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

-module(bc_sched_interval).
-moduledoc """
Parse human-friendly interval strings into milliseconds.

Supported suffixes: s (seconds), m (minutes), h (hours), d (days).
Examples: "30s" -> 30000, "5m" -> 300000, "2h" -> 7200000, "7d" -> 604800000.

Pure function module — no state, no side effects.
""".

-export([parse/1]).

-doc """
Parse an interval string into milliseconds.
Returns {ok, Milliseconds} or {error, Reason}.
""".
-spec parse(binary() | string()) -> {ok, non_neg_integer()} | {error, binary()}.
parse(Bin) when is_binary(Bin) ->
    parse(binary_to_list(Bin));
parse(Str) when is_list(Str) ->
    Trimmed = string:trim(Str),
    case Trimmed of
        [] ->
            {error, <<"Empty interval string">>};
        _ ->
            parse_trimmed(Trimmed)
    end;
parse(_) ->
    {error, <<"Invalid interval: expected string">>}.

%% Internal

parse_trimmed(Str) ->
    %% Split into numeric part and suffix
    {NumStr, Suffix} = split_number_suffix(Str),
    case parse_number(NumStr) of
        {error, _} = Err ->
            Err;
        {ok, Num} when Num < 0 ->
            {error, <<"Interval must be non-negative">>};
        {ok, 0} ->
            {error, <<"Interval must be greater than zero">>};
        {ok, Num} ->
            case suffix_to_ms(Suffix) of
                {ok, Multiplier} ->
                    {ok, Num * Multiplier};
                {error, _} = Err ->
                    Err
            end
    end.

split_number_suffix(Str) ->
    split_number_suffix(Str, []).

split_number_suffix([], Acc) ->
    {lists:reverse(Acc), []};
split_number_suffix([C | Rest], Acc) when C >= $0, C =< $9 ->
    split_number_suffix(Rest, [C | Acc]);
split_number_suffix([C | Rest], Acc) when C =:= $. ->
    split_number_suffix(Rest, [C | Acc]);
split_number_suffix(Suffix, Acc) ->
    {lists:reverse(Acc), string:lowercase(Suffix)}.

parse_number([]) ->
    {error, <<"Missing numeric value">>};
parse_number(Str) ->
    try
        case lists:member($., Str) of
            true ->
                %% Float: truncate to integer milliseconds
                {ok, trunc(list_to_float(Str))};
            false ->
                {ok, list_to_integer(Str)}
        end
    catch
        _:_ ->
            {error, iolist_to_binary(
                [<<"Invalid number: '">>, Str, <<"'">>])}
    end.

suffix_to_ms("s") -> {ok, 1000};
suffix_to_ms("m") -> {ok, 60000};
suffix_to_ms("h") -> {ok, 3600000};
suffix_to_ms("d") -> {ok, 86400000};
suffix_to_ms([])   -> {error, <<"Missing time suffix. Use s, m, h, or d">>};
suffix_to_ms(Other) ->
    {error, iolist_to_binary(
        [<<"Unknown suffix '">>, Other, <<"'. Use s, m, h, or d">>])}.
