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

-module(bc_channel_telegram_tests).
-moduledoc "EUnit tests for bc_channel_telegram:command_name/1.".

-include_lib("eunit/include/eunit.hrl").

%% Regression: mobile keyboards autocapitalize the first letter of a
%% message, turning "/new" into "/New". A case-sensitive match silently
%% fell through to the generic chat path, re-sending (and failing to
%% repair) a corrupted session.
command_name_lowercase_test() ->
    ?assertEqual(<<"new">>, bc_channel_telegram:command_name(<<"/new">>)).

command_name_autocapitalized_test() ->
    ?assertEqual(<<"new">>, bc_channel_telegram:command_name(<<"/New">>)).

command_name_bot_username_suffix_test() ->
    ?assertEqual(<<"new">>, bc_channel_telegram:command_name(<<"/new@JarvisBot">>)).

command_name_trailing_space_test() ->
    ?assertEqual(<<"new">>, bc_channel_telegram:command_name(<<"/new ">>)).

command_name_leading_whitespace_test() ->
    ?assertEqual(<<"context">>, bc_channel_telegram:command_name(<<" /context">>)).

command_name_plain_text_test() ->
    ?assertEqual(none, bc_channel_telegram:command_name(<<"hello there">>)).

command_name_unknown_command_test() ->
    ?assertEqual(<<"start">>, bc_channel_telegram:command_name(<<"/start">>)).
