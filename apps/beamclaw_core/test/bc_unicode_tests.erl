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

-module(bc_unicode_tests).
-moduledoc """
Cross-cutting UTF-8 tests.

Pushes non-ASCII content (Hungarian, emoji, CJK, Arabic, Cyrillic) through
bc_scrubber, bc_telegram_format, and bc_tool_parser to ensure no crashes or
data corruption on multibyte characters. Addresses the 9% of historical
fix commits caused by Unicode/encoding issues.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% ---- Test data constants ----

-define(HUNGARIAN, <<"Időjárás: Budapest, Magyarország — hőmérséklet 23°C">>).
-define(EMOJI, <<"Hello 🌍 world 🎉 test 🚀 done ✅"/utf8>>).
-define(CJK, <<"你好世界 こんにちは世界 안녕하세요"/utf8>>).
-define(ARABIC, <<"مرحبا بالعالم"/utf8>>).
-define(CYRILLIC, <<"Привет мир — тест"/utf8>>).
-define(MIXED, <<"Szia! 🇭🇺 Időjárás: 你好 Привет مرحبا"/utf8>>).

%% ============================================================
%% bc_scrubber — Unicode content around secrets
%% ============================================================

scrub_hungarian_passthrough_test() ->
    ?assertEqual(?HUNGARIAN, bc_scrubber:scrub(?HUNGARIAN)).

scrub_emoji_passthrough_test() ->
    ?assertEqual(?EMOJI, bc_scrubber:scrub(?EMOJI)).

scrub_cjk_passthrough_test() ->
    ?assertEqual(?CJK, bc_scrubber:scrub(?CJK)).

scrub_arabic_passthrough_test() ->
    ?assertEqual(?ARABIC, bc_scrubber:scrub(?ARABIC)).

scrub_cyrillic_passthrough_test() ->
    ?assertEqual(?CYRILLIC, bc_scrubber:scrub(?CYRILLIC)).

scrub_mixed_passthrough_test() ->
    ?assertEqual(?MIXED, bc_scrubber:scrub(?MIXED)).

%% Secret embedded in Unicode text — secret should be redacted,
%% surrounding Unicode preserved.
scrub_secret_in_hungarian_test() ->
    Input = <<"Időjárás api_key=titkos_kulcs beállítás"/utf8>>,
    R = bc_scrubber:scrub(Input),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch),
    ?assert(binary:match(R, <<"titkos_kulcs">>) =:= nomatch),
    %% Hungarian text preserved
    ?assert(binary:match(R, <<"Időjárás"/utf8>>) =/= nomatch).

scrub_bearer_in_cjk_test() ->
    Input = <<"认证 Bearer sk-abcdefghijklmnopqrstuvwxyz1234 完成"/utf8>>,
    R = bc_scrubber:scrub(Input),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch),
    ?assert(binary:match(R, <<"认证"/utf8>>) =/= nomatch),
    ?assert(binary:match(R, <<"完成"/utf8>>) =/= nomatch).

scrub_openai_key_in_emoji_test() ->
    Input = <<"🔑 sk-abcdefghijklmnopqrstuvwxyz1234 🔐"/utf8>>,
    R = bc_scrubber:scrub(Input),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch).

scrub_map_unicode_values_test() ->
    M = #{<<"name">> => <<"Péter"/utf8>>,
          <<"key">>  => <<"api_key=secret123">>},
    R = bc_scrubber:scrub_map(M),
    ?assertEqual(<<"Péter"/utf8>>, maps:get(<<"name">>, R)),
    ?assert(binary:match(maps:get(<<"key">>, R), <<"[REDACTED]">>) =/= nomatch).

scrub_message_unicode_content_test() ->
    Msg = #bc_message{id = <<"u1">>, role = user,
                      content = <<"Jelszó: password=titok 🔒"/utf8>>},
    Scrubbed = bc_scrubber:scrub_message(Msg),
    ?assert(binary:match(Scrubbed#bc_message.content, <<"[REDACTED]">>) =/= nomatch),
    ?assert(binary:match(Scrubbed#bc_message.content, <<"Jelszó"/utf8>>) =/= nomatch).

%% ============================================================
%% bc_telegram_format — Unicode in Telegram HTML conversion
%% ============================================================

format_hungarian_plain_test() ->
    Input = <<"Az időjárás ma szép lesz."/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertEqual(Input, Result).

format_emoji_bold_test() ->
    Input = <<"**Szia 🌍!**"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertEqual(<<"<b>Szia 🌍!</b>"/utf8>>, Result).

format_cjk_italic_test() ->
    Input = <<"*你好世界*"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertEqual(<<"<i>你好世界</i>"/utf8>>, Result).

format_cyrillic_header_test() ->
    Input = <<"# Привет мир"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertEqual(<<"<b>Привет мир</b>"/utf8>>, Result).

format_arabic_blockquote_test() ->
    Input = <<"> مرحبا بالعالم"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"<blockquote>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"مرحبا"/utf8>>)).

format_emoji_list_test() ->
    Input = <<"- 🍎 Alma\n- 🍊 Narancs"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<16#2022/utf8>>)),
    ?assertMatch({match, _}, re:run(Result, <<"🍎 Alma"/utf8>>)),
    ?assertMatch({match, _}, re:run(Result, <<"🍊 Narancs"/utf8>>)).

format_unicode_code_block_test() ->
    Input = <<"```python\nprint('Héllo wörld 🌍')\n```"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"<pre><code>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"Héllo wörld"/utf8>>)).

format_unicode_inline_code_test() ->
    Input = <<"Használd a `függvény()` hívást"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"<code>függvény\\(\\)</code>"/utf8>>)).

format_html_escape_unicode_test() ->
    %% HTML entities should work alongside Unicode
    Input = <<"Feltétel: a < b & c > d 🎯"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"&lt;">>)),
    ?assertMatch({match, _}, re:run(Result, <<"&amp;">>)),
    ?assertMatch({match, _}, re:run(Result, <<"&gt;">>)),
    ?assertMatch({match, _}, re:run(Result, <<"🎯"/utf8>>)).

format_mixed_unicode_link_test() ->
    Input = <<"Nézd meg a [dokumentáció](https://example.com) oldalt"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"<a href=">>)),
    ?assertMatch({match, _}, re:run(Result, <<"dokumentáció"/utf8>>)).

format_unicode_strikethrough_test() ->
    Input = <<"~~régi szöveg~~"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertEqual(<<"<s>régi szöveg</s>"/utf8>>, Result).

format_unicode_bold_italic_test() ->
    %% *** at start of line is treated as horizontal rule by block_convert.
    %% Use in mid-sentence to test bold+italic with Unicode.
    Input = <<"Ez ***nagyon fontos*** szöveg"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"<b><i>nagyon fontos</i></b>">>)).

format_chunk_unicode_test() ->
    %% Chunking with multibyte characters should not split in the middle
    %% of a character (since we chunk by byte_size and find split points
    %% at whitespace/newline boundaries).
    Text = iolist_to_binary(lists:duplicate(10, <<"Időjárás Budapest 🌤️ "/utf8>>)),
    Chunks = bc_telegram_format:chunk(Text, 100),
    ?assert(length(Chunks) >= 2),
    %% Each chunk should be valid UTF-8 — try converting to list
    lists:foreach(fun(C) ->
        ?assertMatch([_ | _], unicode:characters_to_list(C))
    end, Chunks).

format_realistic_hungarian_response_test() ->
    Input = <<"# Időjárás\n\n"
              "**Budapest** hőmérséklete ma: *23°C* 🌡️\n\n"
              "> A délutáni órákban esőre lehet számítani.\n\n"
              "- 🌅 Napkelte: 06:42\n"
              "- 🌇 Napnyugta: 19:18\n\n"
              "Részletek: [met.hu](https://met.hu)"/utf8>>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"<b>Időjárás</b>"/utf8>>)),
    ?assertMatch({match, _}, re:run(Result, <<"<b>Budapest</b>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"<i>23°C</i>"/utf8>>)),
    ?assertMatch({match, _}, re:run(Result, <<"<blockquote>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"<a href=">>)).

%% ============================================================
%% bc_tool_parser — Unicode in tool call content
%% ============================================================

parser_unicode_plain_text_no_match_test() ->
    Msg = #bc_message{id = <<"t">>, role = assistant,
                      content = <<"Időjárás: szép lesz ma 🌞"/utf8>>},
    ?assertEqual([], bc_tool_parser:parse(Msg)).

parser_xml_unicode_args_test() ->
    Content = <<"Válasz: <tool_call><name>read_file</name>"
                "<args>{\"path\":\"/home/felhasználó/fájl.txt\"}</args></tool_call>"/utf8>>,
    Msg = #bc_message{id = <<"t">>, role = assistant, content = Content},
    [TC] = bc_tool_parser:parse(Msg),
    ?assertEqual(<<"read_file">>, TC#bc_tool_call.name),
    ?assertEqual(<<"/home/felhasználó/fájl.txt"/utf8>>,
                 maps:get(<<"path">>, TC#bc_tool_call.args)).

parser_xml_unicode_surrounding_text_test() ->
    Content = <<"让我来搜索 <tool_call><name>bash</name>"
                "<args>{\"cmd\":\"ls\"}</args></tool_call> 完成了"/utf8>>,
    Msg = #bc_message{id = <<"t">>, role = assistant, content = Content},
    [TC] = bc_tool_parser:parse(Msg),
    ?assertEqual(<<"bash">>, TC#bc_tool_call.name),
    ?assertEqual(xml, TC#bc_tool_call.source).

parser_markdown_unicode_args_test() ->
    Content = <<"Keresés:\n```json\n"
                "{\"tool\":\"web_search\",\"args\":{\"query\":\"időjárás Budapest\"}}"
                "\n```"/utf8>>,
    Msg = #bc_message{id = <<"t">>, role = assistant, content = Content},
    [TC] = bc_tool_parser:parse(Msg),
    ?assertEqual(<<"web_search">>, TC#bc_tool_call.name),
    ?assertEqual(<<"időjárás Budapest"/utf8>>,
                 maps:get(<<"query">>, TC#bc_tool_call.args)).

parser_native_unicode_args_test() ->
    %% Build JSON args with proper UTF-8 encoding
    ArgsJson = <<"{\"path\":\"/tmp/"/utf8,
                  16#D1, 16#82, 16#D0, 16#B5, 16#D1, 16#81, 16#D1, 16#82,  %% "тест" in UTF-8
                  ".txt\",\"content\":\""/utf8,
                  16#D0, 16#9F, 16#D1, 16#80, 16#D0, 16#B8, 16#D0, 16#B2,  %% "Прив"
                  16#D0, 16#B5, 16#D1, 16#82,                                %% "ет"
                  " "/utf8,
                  16#D0, 16#BC, 16#D0, 16#B8, 16#D1, 16#80,                  %% "мир"
                  "\"}"/utf8>>,
    Call = #{<<"id">>       => <<"tc_u1">>,
             <<"function">> => #{<<"name">> => <<"write_file">>,
                                 <<"arguments">> => ArgsJson}},
    Msg = #bc_message{id = <<"u1">>, role = assistant,
                      content = <<>>, tool_calls = [Call]},
    [TC] = bc_tool_parser:parse(Msg),
    ?assertEqual(<<"write_file">>, TC#bc_tool_call.name),
    %% Verify the decoded args contain the UTF-8 Cyrillic content
    Path = maps:get(<<"path">>, TC#bc_tool_call.args),
    Content = maps:get(<<"content">>, TC#bc_tool_call.args),
    ?assertMatch({match, _}, re:run(Path, "/tmp/")),
    ?assert(byte_size(Path) > 5),
    ?assert(byte_size(Content) > 0).

parser_free_text_unicode_json_no_match_test() ->
    %% Security: free-text JSON (even with Unicode) should NOT be extracted.
    Content = <<"{\"tool\":\"bash\",\"args\":{\"cmd\":\"ékezetes parancs\"}}"/utf8>>,
    Msg = #bc_message{id = <<"t">>, role = assistant, content = Content},
    ?assertEqual([], bc_tool_parser:parse(Msg)).

parser_emoji_in_tool_response_no_match_test() ->
    Msg = #bc_message{id = <<"t">>, role = assistant,
                      content = <<"Kész! ✅ A fájl sikeresen mentve. 📁"/utf8>>},
    ?assertEqual([], bc_tool_parser:parse(Msg)).

%% ============================================================
%% bc_scrubber — scrub_result with Unicode tool output
%% ============================================================

scrub_result_unicode_test() ->
    R = #bc_tool_result{tool_call_id = <<"u1">>, name = <<"bash">>,
                        content = <<"Kimenet: api_key=titkos 🔑 Kész"/utf8>>},
    Scrubbed = bc_scrubber:scrub_result(R),
    ?assert(binary:match(Scrubbed#bc_tool_result.content, <<"[REDACTED]">>) =/= nomatch),
    ?assert(binary:match(Scrubbed#bc_tool_result.content, <<"titkos">>) =:= nomatch),
    ?assert(binary:match(Scrubbed#bc_tool_result.content, <<"Kész"/utf8>>) =/= nomatch).

%% ============================================================
%% bc_telegram_format:escape_html — Unicode passthrough
%% ============================================================

escape_html_unicode_test() ->
    Input = <<"Időjárás: hőmérséklet < 0°C & szél > 50 km/h"/utf8>>,
    Result = bc_telegram_format:escape_html(Input),
    ?assertMatch({match, _}, re:run(Result, <<"&lt;">>)),
    ?assertMatch({match, _}, re:run(Result, <<"&amp;">>)),
    ?assertMatch({match, _}, re:run(Result, <<"&gt;">>)),
    ?assertMatch({match, _}, re:run(Result, <<"Időjárás"/utf8>>)),
    ?assertMatch({match, _}, re:run(Result, <<"hőmérséklet"/utf8>>)).
