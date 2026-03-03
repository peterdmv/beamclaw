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

-module(bc_env_context_tests).
-moduledoc "EUnit tests for bc_env_context.".

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% format_time_ago/1 — duration formatting
%% ===================================================================

time_ago_zero_test() ->
    ?assertEqual(<<"just now">>, bc_env_context:format_time_ago(0)).

time_ago_seconds_test() ->
    ?assertEqual(<<"30 seconds ago">>, bc_env_context:format_time_ago(30)).

time_ago_one_minute_test() ->
    ?assertEqual(<<"1 minute ago">>, bc_env_context:format_time_ago(60)).

time_ago_minutes_test() ->
    ?assertEqual(<<"5 minutes ago">>, bc_env_context:format_time_ago(300)).

time_ago_one_hour_test() ->
    ?assertEqual(<<"1 hour ago">>, bc_env_context:format_time_ago(3600)).

time_ago_hours_and_minutes_test() ->
    %% 2 hours 15 minutes = 8100 seconds
    ?assertEqual(<<"2 hours 15 min ago">>, bc_env_context:format_time_ago(8100)).

time_ago_hours_exact_test() ->
    %% 3 hours = 10800 seconds
    ?assertEqual(<<"3 hours ago">>, bc_env_context:format_time_ago(10800)).

time_ago_one_hour_with_minutes_test() ->
    %% 1 hour 30 minutes = 5400 seconds
    ?assertEqual(<<"1 hour 30 min ago">>, bc_env_context:format_time_ago(5400)).

time_ago_one_day_test() ->
    ?assertEqual(<<"1 day ago">>, bc_env_context:format_time_ago(86400)).

time_ago_multiple_days_test() ->
    ?assertEqual(<<"3 days ago">>, bc_env_context:format_time_ago(259200)).

%% ===================================================================
%% parse_user_location/1 — USER.md parsing
%% ===================================================================

parse_location_undefined_test() ->
    ?assertEqual(undefined, bc_env_context:parse_user_location(undefined)).

parse_location_empty_test() ->
    ?assertEqual(undefined, bc_env_context:parse_user_location(<<>>)).

parse_location_with_location_field_test() ->
    Content = <<"# User\n\n- **Location:** Helsinki\n- **Name:** Peter\n">>,
    ?assertEqual(<<"Helsinki">>, bc_env_context:parse_user_location(Content)).

parse_location_with_city_field_test() ->
    Content = <<"# User\n\n- **City:** Berlin\n">>,
    ?assertEqual(<<"Berlin">>, bc_env_context:parse_user_location(Content)).

parse_location_with_timezone_field_test() ->
    Content = <<"# User\n\n- **Timezone:** Europe/London\n">>,
    ?assertEqual(<<"Europe/London">>, bc_env_context:parse_user_location(Content)).

parse_location_strips_parenthetical_test() ->
    Content = <<"# User\n\n- **Timezone:** CET (useful for scheduling)\n">>,
    ?assertEqual(<<"CET">>, bc_env_context:parse_user_location(Content)).

parse_location_no_match_test() ->
    Content = <<"# User\n\n- **Name:** Peter\n- **Pronouns:** he/him\n">>,
    ?assertEqual(undefined, bc_env_context:parse_user_location(Content)).

parse_location_location_takes_precedence_test() ->
    Content = <<"# User\n\n- **Location:** Tokyo\n- **Timezone:** JST\n">>,
    ?assertEqual(<<"Tokyo">>, bc_env_context:parse_user_location(Content)).

parse_location_empty_value_skipped_test() ->
    Content = <<"# User\n\n- **Location:**  \n- **Timezone:** UTC+2\n">>,
    ?assertEqual(<<"UTC+2">>, bc_env_context:parse_user_location(Content)).

%% ===================================================================
%% format_context/1 — context text formatting
%% ===================================================================

format_context_time_only_test() ->
    %% 2026-03-03 10:30:00 UTC = a known timestamp
    %% Use a fixed timestamp for predictable output
    Now = calendar:datetime_to_gregorian_seconds({{2026, 3, 3}, {10, 30, 0}})
          - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Result = bc_env_context:format_context(#{now => Now}),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Tuesday">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"March 3, 2026">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"10:30 UTC">>)),
    %% Should not have weather or headlines sections
    ?assertEqual(nomatch, binary:match(Result, <<"Weather">>)),
    ?assertEqual(nomatch, binary:match(Result, <<"Headlines">>)).

format_context_with_weather_test() ->
    Now = calendar:datetime_to_gregorian_seconds({{2026, 6, 15}, {14, 0, 0}})
          - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Result = bc_env_context:format_context(#{
        now => Now,
        weather => <<"Sunny, 24C">>,
        location => <<"Helsinki">>
    }),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Helsinki">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Sunny, 24C">>)).

format_context_with_weather_no_location_test() ->
    Now = calendar:datetime_to_gregorian_seconds({{2026, 1, 1}, {0, 0, 0}})
          - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Result = bc_env_context:format_context(#{
        now => Now,
        weather => <<"Cloudy, 5C">>
    }),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Cloudy, 5C">>)).

format_context_with_news_test() ->
    Now = calendar:datetime_to_gregorian_seconds({{2026, 3, 3}, {12, 0, 0}})
          - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Result = bc_env_context:format_context(#{
        now => Now,
        news => [<<"Headline one">>, <<"Headline two">>]
    }),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Headlines">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Headline one">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Headline two">>)).

format_context_with_last_activity_test() ->
    Now = calendar:datetime_to_gregorian_seconds({{2026, 3, 3}, {12, 0, 0}})
          - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    LastActivity = Now - 3600,  %% 1 hour ago
    Result = bc_env_context:format_context(#{
        now => Now,
        last_activity => LastActivity
    }),
    ?assertNotEqual(nomatch, binary:match(Result, <<"1 hour ago">>)).

format_context_new_session_no_elapsed_test() ->
    Now = calendar:datetime_to_gregorian_seconds({{2026, 3, 3}, {12, 0, 0}})
          - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Result = bc_env_context:format_context(#{
        now => Now,
        last_activity => 0
    }),
    ?assertEqual(nomatch, binary:match(Result, <<"Since last interaction">>)).

format_context_undefined_last_activity_test() ->
    Now = erlang:system_time(second),
    Result = bc_env_context:format_context(#{
        now => Now,
        last_activity => undefined
    }),
    ?assertEqual(nomatch, binary:match(Result, <<"Since last interaction">>)).

format_context_full_test() ->
    Now = calendar:datetime_to_gregorian_seconds({{2026, 12, 25}, {8, 15, 0}})
          - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Result = bc_env_context:format_context(#{
        now => Now,
        last_activity => Now - 7200,  %% 2 hours ago
        weather => <<"Snow, -5C, wind 20 km/h NW">>,
        news => [<<"Santa spotted over Lapland">>, <<"Markets closed for holiday">>],
        location => <<"Helsinki">>
    }),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Friday">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"December 25, 2026">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"2 hours ago">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Helsinki">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Snow">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Santa spotted">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"organically">>)).

format_context_always_has_time_test() ->
    Now = erlang:system_time(second),
    Result = bc_env_context:format_context(#{now => Now}),
    ?assertNotEqual(nomatch, binary:match(Result, <<"**Time:**">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"UTC">>)).

format_context_always_has_instructions_test() ->
    Now = erlang:system_time(second),
    Result = bc_env_context:format_context(#{now => Now}),
    ?assertNotEqual(nomatch, binary:match(Result, <<"naturally">>)).

format_context_empty_news_no_section_test() ->
    Now = erlang:system_time(second),
    Result = bc_env_context:format_context(#{now => Now, news => []}),
    ?assertEqual(nomatch, binary:match(Result, <<"Headlines">>)).

%% ===================================================================
%% Token budget estimation
%% ===================================================================

format_context_token_budget_test() ->
    %% Full context with all sections should stay under ~150 tokens (~600 bytes)
    Now = erlang:system_time(second),
    Result = bc_env_context:format_context(#{
        now => Now,
        last_activity => Now - 1800,
        weather => <<"Partly cloudy, 18C, humidity 65%, wind 12 km/h W">>,
        news => [<<"First headline here">>,
                 <<"Second headline here">>,
                 <<"Third headline here">>,
                 <<"Fourth headline here">>,
                 <<"Fifth headline here">>],
        location => <<"London">>
    }),
    EstTokens = byte_size(Result) div 4,
    %% Should be well under 200 tokens even with all sections populated
    ?assert(EstTokens < 200).
