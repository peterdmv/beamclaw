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

-module(bc_user_env_tests).
-include_lib("eunit/include/eunit.hrl").

%% ---- format_idle_time/1 tests ----

idle_just_now_test() ->
    ?assertEqual(<<"just now">>, bc_user_env:format_idle_time(0)),
    ?assertEqual(<<"just now">>, bc_user_env:format_idle_time(30)),
    ?assertEqual(<<"just now">>, bc_user_env:format_idle_time(59)).

idle_minutes_test() ->
    ?assertEqual(<<"1 minute ago">>, bc_user_env:format_idle_time(60)),
    ?assertEqual(<<"5 minutes ago">>, bc_user_env:format_idle_time(300)),
    ?assertEqual(<<"59 minutes ago">>, bc_user_env:format_idle_time(3599)).

idle_hours_test() ->
    ?assertEqual(<<"1 hour ago">>, bc_user_env:format_idle_time(3600)),
    ?assertEqual(<<"2 hours ago">>, bc_user_env:format_idle_time(7200)),
    ?assertEqual(<<"23 hours ago">>, bc_user_env:format_idle_time(86399)).

idle_days_test() ->
    ?assertEqual(<<"1 day ago">>, bc_user_env:format_idle_time(86400)),
    ?assertEqual(<<"3 days ago">>, bc_user_env:format_idle_time(259200)),
    ?assertEqual(<<"7 days ago">>, bc_user_env:format_idle_time(604800)).

%% ---- format_time_section/3 tests ----

time_section_basic_test() ->
    %% 2026-03-07 is a Saturday
    Result = bc_user_env:format_time_section(<<"UTC">>, 0, {{2026, 3, 7}, {14, 23, 0}}),
    ?assertEqual(<<"Saturday, March 7, 2026 14:23 (UTC)">>, Result).

time_section_with_offset_test() ->
    %% CET = UTC+1, so 14:23 UTC → 15:23 CET
    Result = bc_user_env:format_time_section(<<"Europe/Budapest">>, 1, {{2026, 3, 7}, {14, 23, 0}}),
    ?assertEqual(<<"Saturday, March 7, 2026 15:23 (Europe/Budapest)">>, Result).

time_section_negative_offset_test() ->
    %% EST = UTC-5, so 14:23 UTC → 09:23 EST
    Result = bc_user_env:format_time_section(<<"America/New_York">>, -5, {{2026, 3, 7}, {14, 23, 0}}),
    ?assertEqual(<<"Saturday, March 7, 2026 09:23 (America/New_York)">>, Result).

time_section_midnight_wrap_test() ->
    %% UTC+9 from 20:00 UTC → 05:00 next day (hour only, date not adjusted)
    Result = bc_user_env:format_time_section(<<"Asia/Tokyo">>, 9, {{2026, 3, 7}, {20, 0, 0}}),
    ?assertMatch(<<_/binary>>, Result),
    ?assert(binary:match(Result, <<"05:00">>) =/= nomatch).

%% ---- parse_weather_json/1 tests (Open-Meteo format) ----

parse_weather_valid_test() ->
    Json = jsx:encode(#{
        <<"current">> => #{
            <<"temperature_2m">> => 10.2,
            <<"relative_humidity_2m">> => 71,
            <<"weather_code">> => 0
        }
    }),
    {ok, Text} = bc_user_env:parse_weather_json(Json),
    ?assert(binary:match(Text, <<"10.2">>) =/= nomatch),
    ?assert(binary:match(Text, <<"clear sky">>) =/= nomatch),
    ?assert(binary:match(Text, <<"71">>) =/= nomatch).

parse_weather_integer_temp_test() ->
    Json = jsx:encode(#{
        <<"current">> => #{
            <<"temperature_2m">> => 5,
            <<"relative_humidity_2m">> => 80,
            <<"weather_code">> => 3
        }
    }),
    {ok, Text} = bc_user_env:parse_weather_json(Json),
    ?assert(binary:match(Text, <<"5">>) =/= nomatch),
    ?assert(binary:match(Text, <<"overcast">>) =/= nomatch).

parse_weather_rain_code_test() ->
    Json = jsx:encode(#{
        <<"current">> => #{
            <<"temperature_2m">> => 7.5,
            <<"relative_humidity_2m">> => 90,
            <<"weather_code">> => 63
        }
    }),
    {ok, Text} = bc_user_env:parse_weather_json(Json),
    ?assert(binary:match(Text, <<"rain">>) =/= nomatch).

parse_weather_malformed_test() ->
    ?assertEqual({error, bad_json}, bc_user_env:parse_weather_json(<<"not json">>)),
    ?assertEqual({error, bad_json}, bc_user_env:parse_weather_json(<<"{}">>)).

%% ---- wmo_description/1 tests ----

wmo_clear_test() ->
    ?assertEqual(<<"clear sky">>, bc_user_env:wmo_description(0)),
    ?assertEqual(<<"mainly clear">>, bc_user_env:wmo_description(1)),
    ?assertEqual(<<"partly cloudy">>, bc_user_env:wmo_description(2)),
    ?assertEqual(<<"overcast">>, bc_user_env:wmo_description(3)).

wmo_precipitation_test() ->
    ?assertEqual(<<"light drizzle">>, bc_user_env:wmo_description(51)),
    ?assertEqual(<<"light rain">>, bc_user_env:wmo_description(61)),
    ?assertEqual(<<"rain">>, bc_user_env:wmo_description(63)),
    ?assertEqual(<<"heavy rain">>, bc_user_env:wmo_description(65)),
    ?assertEqual(<<"light snow">>, bc_user_env:wmo_description(71)),
    ?assertEqual(<<"snow">>, bc_user_env:wmo_description(73)).

wmo_severe_test() ->
    ?assertEqual(<<"thunderstorm">>, bc_user_env:wmo_description(95)),
    ?assertEqual(<<"thunderstorm with hail">>, bc_user_env:wmo_description(96)),
    ?assertEqual(<<"thunderstorm with heavy hail">>, bc_user_env:wmo_description(99)).

wmo_fog_test() ->
    ?assertEqual(<<"foggy">>, bc_user_env:wmo_description(45)),
    ?assertEqual(<<"foggy">>, bc_user_env:wmo_description(48)).

wmo_unknown_test() ->
    ?assertEqual(<<"unknown">>, bc_user_env:wmo_description(-1)),
    ?assertEqual(<<"unknown">>, bc_user_env:wmo_description(999)).

wmo_showers_test() ->
    ?assertEqual(<<"light rain showers">>, bc_user_env:wmo_description(80)),
    ?assertEqual(<<"rain showers">>, bc_user_env:wmo_description(81)),
    ?assertEqual(<<"heavy rain showers">>, bc_user_env:wmo_description(82)).

%% ---- parse_news_json/1 tests ----

parse_news_valid_test() ->
    Articles = [#{<<"headline">> => <<"Breaking news one">>},
                #{<<"headline">> => <<"Breaking news two">>},
                #{<<"headline">> => <<"Breaking news three">>}],
    Json = jsx:encode(Articles),
    {ok, Text} = bc_user_env:parse_news_json(Json),
    ?assert(binary:match(Text, <<"Headlines:">>) =/= nomatch),
    ?assert(binary:match(Text, <<"Breaking news one">>) =/= nomatch),
    ?assert(binary:match(Text, <<"- ">>) =/= nomatch).

parse_news_empty_test() ->
    ?assertEqual({ok, <<>>}, bc_user_env:parse_news_json(<<"[]">>)).

parse_news_max_five_test() ->
    Articles = [#{<<"headline">> => iolist_to_binary(io_lib:format("Headline ~B", [I]))}
                || I <- lists:seq(1, 10)],
    Json = jsx:encode(Articles),
    {ok, Text} = bc_user_env:parse_news_json(Json),
    %% Should contain headlines 1-5 but not 6-10
    ?assert(binary:match(Text, <<"Headline 5">>) =/= nomatch),
    ?assertEqual(nomatch, binary:match(Text, <<"Headline 6">>)).

parse_news_malformed_test() ->
    ?assertEqual({error, bad_json}, bc_user_env:parse_news_json(<<"not json">>)).

%% ---- parse_timezone_from_user_md/1 tests ----

parse_tz_present_test() ->
    Md = <<"# User\n\n- **Timezone:** Europe/Budapest\n- **Context:** developer">>,
    ?assertEqual(<<"Europe/Budapest">>, bc_user_env:parse_timezone_from_user_md(Md)).

parse_tz_missing_test() ->
    Md = <<"# User\n\n- **Name:** Alice\n">>,
    ?assertEqual(undefined, bc_user_env:parse_timezone_from_user_md(Md)).

parse_tz_placeholder_test() ->
    Md = <<"# User\n\n- **Timezone:** (useful for scheduling and greetings)\n">>,
    ?assertEqual(undefined, bc_user_env:parse_timezone_from_user_md(Md)).

parse_tz_empty_test() ->
    Md = <<"# User\n\n- **Timezone:** \n">>,
    ?assertEqual(undefined, bc_user_env:parse_timezone_from_user_md(Md)).

%% ---- parse_location_from_user_md/1 tests ----

parse_location_present_test() ->
    Md = <<"# User\n\n- **Location:** Budapest, 47.50, 19.04\n- **Context:** developer">>,
    ?assertEqual({<<"Budapest">>, 47.50, 19.04}, bc_user_env:parse_location_from_user_md(Md)).

parse_location_missing_test() ->
    Md = <<"# User\n\n- **Name:** Alice\n- **Timezone:** Europe/Stockholm\n">>,
    ?assertEqual(undefined, bc_user_env:parse_location_from_user_md(Md)).

parse_location_hungarian_test() ->
    Md = <<"# Felhasználó\n\n- **Helyszín:** Budapest, 47.50, 19.04\n">>,
    ?assertEqual({<<"Budapest">>, 47.50, 19.04}, bc_user_env:parse_location_from_user_md(Md)).

parse_location_bad_format_test() ->
    %% Just a city name without coordinates
    Md = <<"# User\n\n- **Location:** Stockholm\n">>,
    ?assertEqual(undefined, bc_user_env:parse_location_from_user_md(Md)).

parse_location_placeholder_test() ->
    Md = <<"# User\n\n- **Location:** (city name, latitude, longitude)\n">>,
    ?assertEqual(undefined, bc_user_env:parse_location_from_user_md(Md)).

parse_location_integer_coords_test() ->
    Md = <<"# User\n\n- **Location:** London, 51, 0\n">>,
    ?assertEqual({<<"London">>, 51.0, 0.0}, bc_user_env:parse_location_from_user_md(Md)).

%% ---- tz_offset/1 tests ----

tz_offset_known_zones_test() ->
    ?assertEqual(1,  bc_user_env:tz_offset("Europe/Budapest")),
    ?assertEqual(-5, bc_user_env:tz_offset("America/New_York")),
    ?assertEqual(9,  bc_user_env:tz_offset("Asia/Tokyo")),
    ?assertEqual(0,  bc_user_env:tz_offset("UTC")),
    ?assertEqual(0,  bc_user_env:tz_offset("GMT")).

tz_offset_utc_plus_minus_test() ->
    ?assertEqual(3,  bc_user_env:tz_offset("UTC+3")),
    ?assertEqual(-7, bc_user_env:tz_offset("UTC-7")),
    ?assertEqual(5,  bc_user_env:tz_offset("GMT+5")).

tz_offset_unknown_fallback_test() ->
    ?assertEqual(0, bc_user_env:tz_offset("Mars/Olympus_Mons")).

tz_offset_binary_input_test() ->
    ?assertEqual(1, bc_user_env:tz_offset(<<"Europe/Budapest">>)),
    ?assertEqual(0, bc_user_env:tz_offset(<<"UTC">>)).

%% ---- build_env_block/1 tests ----

build_block_all_sections_test() ->
    Sections = [<<"Monday, March 3, 2026 10:00 (UTC)">>,
                <<"Time since last message: 5 minutes ago">>,
                <<"Weather in Stockholm: 12\302\260C, clear sky, 50% humidity">>,
                <<"Headlines:\n- Big news">>],
    Block = bc_user_env:build_env_block(Sections),
    ?assert(binary:match(Block, <<"[user:environment]">>) =/= nomatch),
    ?assert(binary:match(Block, <<"Monday">>) =/= nomatch),
    ?assert(binary:match(Block, <<"Weather">>) =/= nomatch),
    ?assert(binary:match(Block, <<"Headlines">>) =/= nomatch).

build_block_skip_empty_test() ->
    Sections = [<<"Monday, March 3, 2026 10:00 (UTC)">>,
                <<>>,
                <<"Weather in Stockholm: 12\302\260C, clear sky, 50% humidity">>,
                <<>>],
    Block = bc_user_env:build_env_block(Sections),
    ?assert(binary:match(Block, <<"Monday">>) =/= nomatch),
    ?assert(binary:match(Block, <<"Weather">>) =/= nomatch),
    %% No double blank lines from empty sections
    ?assertEqual(nomatch, binary:match(Block, <<"\n\n\n">>)).

build_block_all_empty_test() ->
    ?assertEqual(<<>>, bc_user_env:build_env_block([<<>>, <<>>, <<>>, <<>>])).

%% ---- gen_server: disabled returns [] ----

disabled_returns_empty_test() ->
    %% Set user_env to disabled
    application:set_env(beamclaw_core, user_env, #{enabled => false}),
    {ok, Pid} = bc_user_env:start_link(),
    try
        Result = gen_server:call(Pid, {get_env_message, <<"test">>, self()}),
        ?assertEqual([], Result)
    after
        gen_server:stop(Pid)
    end.
