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

-module(bc_context_tests).
-moduledoc "EUnit tests for bc_context.".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% ---- Token estimation ----

estimate_tokens_test() ->
    %% ~4 chars per token
    ?assertEqual(0, bc_context:estimate_tokens(<<>>)),
    ?assertEqual(0, bc_context:estimate_tokens(undefined)),
    ?assertEqual(2, bc_context:estimate_tokens(<<"12345678">>)),
    ?assertEqual(1, bc_context:estimate_tokens(<<"abcd">>)).

%% ---- Context window lookup ----

context_window_known_models_test() ->
    ?assertEqual(200000, bc_context:context_window("anthropic/claude-sonnet-4-5")),
    ?assertEqual(200000, bc_context:context_window("anthropic/claude-opus-4-6")),
    ?assertEqual(200000, bc_context:context_window(<<"anthropic/claude-sonnet-4-5">>)),
    ?assertEqual(128000, bc_context:context_window("gpt-4o")),
    ?assertEqual(128000, bc_context:context_window("gpt-4o-mini")),
    ?assertEqual(256000, bc_context:context_window("moonshotai/kimi-k2.5")),
    ?assertEqual(1000000, bc_context:context_window("google/gemini-pro")),
    ?assertEqual(128000, bc_context:context_window("deepseek/deepseek-chat")).

context_window_unknown_model_test() ->
    ?assertEqual(128000, bc_context:context_window("unknown-model-v1")),
    ?assertEqual(128000, bc_context:context_window("some/random-model")).

%% ---- Format size ----

format_size_test() ->
    ?assertEqual(<<"0">>, bc_context:format_size(0)),
    ?assertEqual(<<"500">>, bc_context:format_size(500)),
    ?assertEqual(<<"999">>, bc_context:format_size(999)),
    ?assertEqual(<<"1.0k">>, bc_context:format_size(1000)),
    ?assertEqual(<<"1.5k">>, bc_context:format_size(1536)),
    ?assertEqual(<<"200.0k">>, bc_context:format_size(200000)),
    ?assertEqual(<<"4.2k">>, bc_context:format_size(4200)).

%% ---- Gather with empty history ----

gather_empty_history_test_() ->
    {setup,
     fun setup_gather/0,
     fun teardown_gather/1,
     fun(_) ->
         [fun() ->
             Info = bc_context:gather(#{agent_id => <<"default">>,
                                        history => []}),
             ?assertEqual(0, maps:get(message_tokens, Info)),
             ?assertEqual(0, maps:get(message_count, Info)),
             ?assert(maps:get(context_window, Info) > 0),
             ?assert(is_list(maps:get(categories, Info))),
             ?assertEqual(7, length(maps:get(categories, Info)))
         end]
     end}.

gather_with_history_test_() ->
    {setup,
     fun setup_gather/0,
     fun teardown_gather/1,
     fun(_) ->
         [fun() ->
             History = [
                 #bc_message{id = <<"1">>, role = user,
                             content = <<"Hello, how are you?">>},
                 #bc_message{id = <<"2">>, role = assistant,
                             content = <<"I am fine, thank you!">>}
             ],
             Info = bc_context:gather(#{agent_id => <<"default">>,
                                        history => History}),
             ?assert(maps:get(message_tokens, Info) > 0),
             ?assertEqual(2, maps:get(message_count, Info)),
             %% Total should include messages
             ?assert(maps:get(total, Info) >= maps:get(message_tokens, Info))
         end]
     end}.

%% ---- Format text output ----

format_text_output_test_() ->
    {setup,
     fun setup_gather/0,
     fun teardown_gather/1,
     fun(_) ->
         [fun() ->
             Info = bc_context:gather(#{agent_id => <<"default">>,
                                        history => []}),
             Output = bc_context:format_text(Info),
             %% Should contain key sections
             ?assertNotEqual(nomatch, binary:match(Output, <<"Context Usage">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"tokens">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Bootstrap files">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Messages">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Free space">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Compaction buffer">>))
         end]
     end}.

format_text_ansi_test_() ->
    {setup,
     fun setup_gather/0,
     fun teardown_gather/1,
     fun(_) ->
         [fun() ->
             Info = bc_context:gather(#{agent_id => <<"default">>,
                                        history => []}),
             Output = bc_context:format_text(Info, #{ansi => true}),
             %% ANSI output should contain escape codes
             ?assertNotEqual(nomatch, binary:match(Output, <<"\e[">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Context Usage">>))
         end]
     end}.

format_text_no_ansi_test_() ->
    {setup,
     fun setup_gather/0,
     fun teardown_gather/1,
     fun(_) ->
         [fun() ->
             Info = bc_context:gather(#{agent_id => <<"default">>,
                                        history => []}),
             Output = bc_context:format_text(Info),
             %% Plain text should NOT contain escape codes
             ?assertEqual(nomatch, binary:match(Output, <<"\e[">>))
         end]
     end}.

%% ---- Bar rendering ----

bar_rendering_test() ->
    %% Build a properly structured info map for a known ratio
    Info = #{model => "test-model", context_window => 1000,
             total => 500, bootstrap_tokens => 100,
             daily_tokens => 50, skill_tokens => 50, tool_tokens => 100,
             message_tokens => 200, compaction_buffer => 200,
             free_space => 300, message_count => 10,
             bootstrap_files => [{<<"IDENTITY.md">>, 100}],
             categories => [
                 {<<"Bootstrap files">>, 100},
                 {<<"Daily logs">>, 50},
                 {<<"Skills">>, 50},
                 {<<"Tool definitions">>, 100},
                 {<<"Messages">>, 200},
                 {<<"Free space">>, 300},
                 {<<"Compaction buffer">>, 200}
             ]},
    Output = bc_context:format_text(Info),
    %% Output should contain all three symbol types
    ?assertNotEqual(nomatch, binary:match(Output, <<"\xe2\x9b\x81">>)),  %% ⛁ filled
    ?assertNotEqual(nomatch, binary:match(Output, <<"\xe2\x9b\xb6">>)),  %% ⛶ free
    ?assertNotEqual(nomatch, binary:match(Output, <<"\xe2\x9b\x9d">>)).  %% ⛝ compaction

%% ---- SVG rendering ----

render_svg_test_() ->
    {setup,
     fun setup_gather/0,
     fun teardown_gather/1,
     fun(_) ->
         [fun() ->
             Info = bc_context:gather(#{agent_id => <<"default">>,
                                        history => []}),
             Svg = bc_context:render_svg(Info),
             %% Should be valid SVG-ish XML
             ?assertNotEqual(nomatch, binary:match(Svg, <<"<svg">>)),
             ?assertNotEqual(nomatch, binary:match(Svg, <<"</svg>">>)),
             %% Should contain grid cells
             ?assertNotEqual(nomatch, binary:match(Svg, <<"<rect">>)),
             %% Should contain color values
             ?assertNotEqual(nomatch, binary:match(Svg, <<"fill=\"#">>)),
             %% Should contain Context Usage title
             ?assertNotEqual(nomatch, binary:match(Svg, <<"Context Usage">>))
         end]
     end}.

%% ---- PNG rendering ----

render_png_test_() ->
    {setup,
     fun setup_gather/0,
     fun teardown_gather/1,
     fun(_) ->
         [fun() ->
             Info = bc_context:gather(#{agent_id => <<"default">>,
                                        history => []}),
             case os:find_executable("rsvg-convert") of
                 false ->
                     %% rsvg-convert not installed — verify error
                     {error, _} = bc_context:render_png(Info);
                 _ ->
                     %% rsvg-convert available — verify PNG bytes
                     {ok, PngBin} = bc_context:render_png(Info),
                     %% PNG magic bytes
                     <<137, 80, 78, 71, _/binary>> = PngBin
             end
         end]
     end}.

%% ---- Exit code parsing (regression for render_png bug) ----

exit_code_parsing_test() ->
    %% Simulate what render_png does to parse os:cmd exit codes.
    %% string:split("0\n", "\n", all) returns ["0", []] — the trailing
    %% element is [] (empty list), not "0". The old code used lists:last/1
    %% directly, which returned [] and never matched "0".
    Parse = fun(Result) ->
        Lines = string:split(Result, "\n", all),
        NonEmpty = [L || L <- Lines, L =/= "", L =/= []],
        case NonEmpty of
            [] -> "unknown";
            _  -> string:trim(lists:last(NonEmpty))
        end
    end,
    %% Success, no stderr output
    ?assertEqual("0", Parse("0\n")),
    %% Success with stderr warnings
    ?assertEqual("0", Parse("some warning\n0\n")),
    %% Failure
    ?assertEqual("1", Parse("error message\n1\n")),
    %% Empty result
    ?assertEqual("unknown", Parse("")),
    %% Only newlines
    ?assertEqual("unknown", Parse("\n\n")).

%% ---- Telegram format ----

format_telegram_output_test_() ->
    {setup,
     fun setup_gather/0,
     fun teardown_gather/1,
     fun(_) ->
         [fun() ->
             Info = bc_context:gather(#{agent_id => <<"default">>,
                                        history => []}),
             Output = bc_context:format_telegram(Info),
             %% Should contain bold header with emoji
             ?assertNotEqual(nomatch, binary:match(Output, <<"<b>">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Context Usage">>)),
             %% Should contain emoji circles (not ANSI codes)
             ?assertNotEqual(nomatch, binary:match(Output, <<"\xe2\xac\x9c">>)),  %% ⬜
             %% Should NOT contain ANSI escape codes
             ?assertEqual(nomatch, binary:match(Output, <<"\e[">>)),
             %% Should NOT contain <pre> tags
             ?assertEqual(nomatch, binary:match(Output, <<"<pre>">>))
         end]
     end}.

format_telegram_legend_test_() ->
    {setup,
     fun setup_gather/0,
     fun teardown_gather/1,
     fun(_) ->
         [fun() ->
             Info = bc_context:gather(#{agent_id => <<"default">>,
                                        history => []}),
             Output = bc_context:format_telegram(Info),
             %% Legend should contain category names with emoji prefixes
             ?assertNotEqual(nomatch, binary:match(Output, <<"Bootstrap files">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Daily logs">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Skills">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Tool definitions">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Messages">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Free space">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"Compaction buffer">>)),
             %% Should contain percentage markers
             ?assertNotEqual(nomatch, binary:match(Output, <<"%)">>))
         end]
     end}.

format_telegram_bootstrap_test_() ->
    {setup,
     fun setup_gather/0,
     fun teardown_gather/1,
     fun(_) ->
         [fun() ->
             Info = bc_context:gather(#{agent_id => <<"default">>,
                                        history => []}),
             Output = bc_context:format_telegram(Info),
             %% Should contain bootstrap section header
             ?assertNotEqual(nomatch, binary:match(Output, <<"<b>Bootstrap files</b>">>)),
             %% Bootstrap file lines should use <code> tags
             ?assertNotEqual(nomatch, binary:match(Output, <<"<code>">>)),
             ?assertNotEqual(nomatch, binary:match(Output, <<"tokens</code>">>))
         end]
     end}.

format_telegram_emoji_grid_test() ->
    %% Verify emoji mapping covers all cell types
    Info = #{model => "test-model", context_window => 1000,
             total => 500, bootstrap_tokens => 100,
             daily_tokens => 50, skill_tokens => 50, tool_tokens => 100,
             message_tokens => 200, compaction_buffer => 200,
             free_space => 300, message_count => 10,
             bootstrap_files => [{<<"TEST.md">>, 100}],
             categories => [
                 {<<"Bootstrap files">>, 100},
                 {<<"Daily logs">>, 50},
                 {<<"Skills">>, 50},
                 {<<"Tool definitions">>, 100},
                 {<<"Messages">>, 200},
                 {<<"Free space">>, 300},
                 {<<"Compaction buffer">>, 200}
             ]},
    Output = bc_context:format_telegram(Info),
    %% Brown circle (bootstrap)
    ?assertNotEqual(nomatch, binary:match(Output, <<"\xf0\x9f\x9f\xa4">>)),
    %% Red circle (daily)
    ?assertNotEqual(nomatch, binary:match(Output, <<"\xf0\x9f\x94\xb4">>)),
    %% Yellow circle (skills)
    ?assertNotEqual(nomatch, binary:match(Output, <<"\xf0\x9f\x9f\xa1">>)),
    %% Purple circle (tools)
    ?assertNotEqual(nomatch, binary:match(Output, <<"\xf0\x9f\x9f\xa3">>)),
    %% Blue circle (messages)
    ?assertNotEqual(nomatch, binary:match(Output, <<"\xf0\x9f\x94\xb5">>)),
    %% White square (free)
    ?assertNotEqual(nomatch, binary:match(Output, <<"\xe2\xac\x9c">>)),
    %% Black circle (compaction)
    ?assertNotEqual(nomatch, binary:match(Output, <<"\xe2\x9a\xab">>)).

%% ---- Test setup/teardown ----

setup_gather() ->
    %% Set up minimal application env for gather/1 to work
    application:ensure_all_started(crypto),
    application:set_env(beamclaw_core, default_provider, openrouter),
    application:set_env(beamclaw_core, providers,
                        [{openrouter, #{model => "anthropic/claude-sonnet-4-5"}}]),
    application:set_env(beamclaw_core, agentic_loop,
                        #{compaction_target => 20}),
    application:set_env(beamclaw_core, default_agent, <<"default">>),
    %% Ensure the default agent workspace exists
    ensure_test_workspace(),
    ok.

teardown_gather(_) ->
    ok.

ensure_test_workspace() ->
    case bc_workspace:agent_exists(<<"default">>) of
        true -> ok;
        false -> bc_workspace:create_agent(<<"default">>)
    end.
