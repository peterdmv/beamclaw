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

-module(bc_thinking_tests).
-moduledoc "EUnit tests for bc_thinking tag stripping.".

-include_lib("eunit/include/eunit.hrl").

passthrough_plain_test() ->
    ?assertEqual(<<"Hello">>, bc_thinking:strip(<<"Hello">>)).

passthrough_undefined_test() ->
    ?assertEqual(undefined, bc_thinking:strip(undefined)).

empty_binary_test() ->
    ?assertEqual(<<>>, bc_thinking:strip(<<>>)).

strip_think_tags_test() ->
    ?assertEqual(<<"Answer">>,
                 bc_thinking:strip(<<"<think>reasoning</think>Answer">>)).

strip_thinking_tags_test() ->
    ?assertEqual(<<"Answer">>,
                 bc_thinking:strip(<<"<thinking>reason</thinking>Answer">>)).

strip_thought_tags_test() ->
    ?assertEqual(<<"Answer">>,
                 bc_thinking:strip(<<"<thought>reason</thought>Answer">>)).

strip_antthinking_test() ->
    ?assertEqual(<<"Answer">>,
                 bc_thinking:strip(<<"<antThinking>reason</antThinking>Answer">>)).

strip_final_tags_test() ->
    %% <final> strips tags only, keeps content
    ?assertEqual(<<"xAnswer">>,
                 bc_thinking:strip(<<"<final>x</final>Answer">>)).

strip_whitespace_in_tag_test() ->
    ?assertEqual(<<"Answer">>,
                 bc_thinking:strip(<<"< thinking >reason</ thinking >Answer">>)).

preserve_code_block_test() ->
    Input = <<"```\n<thinking>example</thinking>\n```\nAnswer">>,
    Result = bc_thinking:strip(Input),
    %% The thinking tags inside the code block should be preserved
    ?assertNotEqual(nomatch, binary:match(Result, <<"<thinking>">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Answer">>)).

preserve_inline_code_test() ->
    Input = <<"`<thinking>x</thinking>` is a tag">>,
    Result = bc_thinking:strip(Input),
    ?assertNotEqual(nomatch, binary:match(Result, <<"<thinking>">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"is a tag">>)).

strip_null_test() ->
    ?assertEqual(<<>>, bc_thinking:strip(null)).

strip_all_thinking_preserves_content_test() ->
    %% When stripping would empty the response, preserve inner text
    ?assertEqual(<<"The stock price is $150.">>,
                 bc_thinking:strip(<<"<thinking>The stock price is $150.</thinking>">>)).

strip_all_think_preserves_content_test() ->
    ?assertEqual(<<"response text">>,
                 bc_thinking:strip(<<"<think>response text</think>">>)).

strict_unclosed_test() ->
    %% Unclosed tag: preserve inner text rather than returning empty
    ?assertEqual(<<"partial reasoning">>,
                 bc_thinking:strip(<<"<thinking>partial reasoning">>)).

multiple_blocks_test() ->
    ?assertEqual(<<"Hithere">>,
                 bc_thinking:strip(<<"<think>a</think>Hi<think>b</think>there">>)).

case_insensitive_test() ->
    ?assertEqual(<<"Answer">>,
                 bc_thinking:strip(<<"<THINKING>x</THINKING>Answer">>)).

nested_no_crash_test() ->
    Result = bc_thinking:strip(<<"<think><think>x</think></think>Y">>),
    %% Should not crash; Y should be in the result
    ?assertNotEqual(nomatch, binary:match(Result, <<"Y">>)).

no_angle_bracket_fast_path_test() ->
    ?assertEqual(<<"no tags here">>, bc_thinking:strip(<<"no tags here">>)).

thinking_with_newlines_test() ->
    Input = <<"<thinking>\nI need to think about this.\nLet me reason.\n</thinking>\nHere is my answer.">>,
    ?assertEqual(<<"Here is my answer.">>,
                 bc_thinking:strip(Input)).

mixed_tags_test() ->
    Input = <<"<think>first</think>Hello <thinking>second</thinking>World">>,
    ?assertEqual(<<"Hello World">>,
                 bc_thinking:strip(Input)).
