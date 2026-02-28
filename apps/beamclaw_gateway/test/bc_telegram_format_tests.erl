-module(bc_telegram_format_tests).
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------
%% HTML Escaping
%%----------------------------------------------------------------------

escape_ampersand_test() ->
    ?assertEqual(<<"Tom &amp; Jerry">>, bc_telegram_format:escape_html(<<"Tom & Jerry">>)).

escape_lt_gt_test() ->
    ?assertEqual(<<"&lt;div&gt;">>, bc_telegram_format:escape_html(<<"<div>">>)).

escape_mixed_test() ->
    ?assertEqual(<<"a &amp; b &lt; c &gt; d">>,
                 bc_telegram_format:escape_html(<<"a & b < c > d">>)).

escape_empty_test() ->
    ?assertEqual(<<>>, bc_telegram_format:escape_html(<<>>)).

%%----------------------------------------------------------------------
%% Code Blocks
%%----------------------------------------------------------------------

fenced_code_block_test() ->
    Input = <<"```python\nprint('hello')\n```">>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"<pre><code>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"</code></pre>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"print">>)).

fenced_code_no_lang_test() ->
    Input = <<"```\nsome code\n```">>,
    Result = bc_telegram_format:format(Input),
    ?assertEqual(<<"<pre><code>some code</code></pre>">>, Result).

fenced_code_html_escaped_test() ->
    Input = <<"```\na < b & c > d\n```">>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"a &lt; b &amp; c &gt; d">>)).

inline_code_test() ->
    Input = <<"Use `foo()` here">>,
    Result = bc_telegram_format:format(Input),
    ?assertEqual(<<"Use <code>foo()</code> here">>, Result).

markdown_inside_code_preserved_test() ->
    Input = <<"```\n**not bold** *not italic*\n```">>,
    Result = bc_telegram_format:format(Input),
    %% Inside code, ** and * should NOT be converted to tags
    ?assertMatch(nomatch, re:run(Result, <<"<b>">>)),
    ?assertMatch(nomatch, re:run(Result, <<"<i>">>)).

unclosed_fence_test() ->
    Input = <<"```python\nprint('hello')">>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"<pre><code>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"</code></pre>">>)).

%%----------------------------------------------------------------------
%% Inline Formatting
%%----------------------------------------------------------------------

bold_test() ->
    ?assertEqual(<<"This is <b>bold</b> text">>,
                 bc_telegram_format:format(<<"This is **bold** text">>)).

italic_test() ->
    ?assertEqual(<<"This is <i>italic</i> text">>,
                 bc_telegram_format:format(<<"This is *italic* text">>)).

bold_italic_test() ->
    ?assertEqual(<<"This is <b><i>both</i></b> text">>,
                 bc_telegram_format:format(<<"This is ***both*** text">>)).

strikethrough_test() ->
    ?assertEqual(<<"This is <s>struck</s> text">>,
                 bc_telegram_format:format(<<"This is ~~struck~~ text">>)).

link_test() ->
    Input = <<"See [Google](https://google.com) for more">>,
    Result = bc_telegram_format:format(Input),
    ?assertEqual(<<"See <a href=\"https://google.com\">Google</a> for more">>, Result).

link_with_bold_test() ->
    Input = <<"Click [**here**](https://example.com)">>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"<a href=">>)),
    ?assertMatch({match, _}, re:run(Result, <<"<b>here</b>">>)).

%%----------------------------------------------------------------------
%% Block-level
%%----------------------------------------------------------------------

header_h1_test() ->
    ?assertEqual(<<"<b>Title</b>">>,
                 bc_telegram_format:format(<<"# Title">>)).

header_h3_test() ->
    ?assertEqual(<<"<b>Section</b>">>,
                 bc_telegram_format:format(<<"### Section">>)).

header_h6_test() ->
    ?assertEqual(<<"<b>Deep</b>">>,
                 bc_telegram_format:format(<<"###### Deep">>)).

blockquote_test() ->
    Input = <<"> This is a quote">>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<"<blockquote>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"</blockquote>">>)).

multiline_blockquote_test() ->
    Input = <<"> Line one\n> Line two">>,
    Result = bc_telegram_format:format(Input),
    %% Should be a single blockquote with both lines merged
    ?assertEqual(<<"<blockquote>Line one\nLine two</blockquote>">>, Result).

unordered_list_dash_test() ->
    Input = <<"- First\n- Second">>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<16#2022/utf8, " First">>)),
    ?assertMatch({match, _}, re:run(Result, <<16#2022/utf8, " Second">>)).

unordered_list_star_test() ->
    Input = <<"* First\n* Second">>,
    Result = bc_telegram_format:format(Input),
    ?assertMatch({match, _}, re:run(Result, <<16#2022/utf8, " First">>)),
    ?assertMatch({match, _}, re:run(Result, <<16#2022/utf8, " Second">>)).

ordered_list_test() ->
    Input = <<"1. First\n2. Second">>,
    Result = bc_telegram_format:format(Input),
    %% Ordered lists pass through unchanged
    ?assertMatch({match, _}, re:run(Result, <<"1\\. First">>)),
    ?assertMatch({match, _}, re:run(Result, <<"2\\. Second">>)).

horizontal_rule_test() ->
    Result = bc_telegram_format:format(<<"---">>),
    ?assertMatch({match, _}, re:run(Result, <<16#2500/utf8>>)).

%%----------------------------------------------------------------------
%% Edge Cases
%%----------------------------------------------------------------------

empty_input_test() ->
    ?assertEqual(<<>>, bc_telegram_format:format(<<>>)).

plain_text_passthrough_test() ->
    Input = <<"Hello, this is plain text.">>,
    ?assertEqual(Input, bc_telegram_format:format(Input)).

mixed_realistic_test() ->
    Input = <<"# Summary\n\nHere is **important** info:\n\n"
              "```python\ndef foo():\n    return 42\n```\n\n"
              "- Item one\n- Item two\n\n"
              "See [docs](https://example.com) for more.">>,
    Result = bc_telegram_format:format(Input),
    %% Verify key elements are present
    ?assertMatch({match, _}, re:run(Result, <<"<b>Summary</b>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"<b>important</b>">>)),
    ?assertMatch({match, _}, re:run(Result, <<"<pre><code>">>)),
    ?assertMatch({match, _}, re:run(Result, <<16#2022/utf8, " Item one">>)),
    ?assertMatch({match, _}, re:run(Result, <<"<a href=">>)).

unclosed_bold_test() ->
    %% Unclosed ** should still pass through (regex is non-greedy with +?)
    Input = <<"This is **unclosed">>,
    Result = bc_telegram_format:format(Input),
    %% Should NOT crash, content preserved
    ?assertMatch({match, _}, re:run(Result, <<"unclosed">>)).

%%----------------------------------------------------------------------
%% Chunking
%%----------------------------------------------------------------------

chunk_short_message_test() ->
    Text = <<"Short message">>,
    ?assertEqual([Text], bc_telegram_format:chunk(Text, 4096)).

chunk_long_message_test() ->
    %% Create a message longer than 100 chars for a small limit test
    Text = iolist_to_binary(lists:duplicate(20, <<"Hello world. ">>)),
    Chunks = bc_telegram_format:chunk(Text, 100),
    ?assert(length(Chunks) > 1),
    %% Each chunk should be within limit (plus possible close tags)
    lists:foreach(fun(C) -> ?assert(byte_size(C) =< 200) end, Chunks).

chunk_paragraph_boundary_test() ->
    Text = <<"First paragraph here.\n\nSecond paragraph here.">>,
    Chunks = bc_telegram_format:chunk(Text, 30),
    ?assert(length(Chunks) >= 2).

chunk_tag_balancing_test() ->
    Text = <<"<b>This is a long bold text that needs to be split across multiple chunks for testing</b>">>,
    Chunks = bc_telegram_format:chunk(Text, 50),
    ?assert(length(Chunks) >= 2),
    %% First chunk should have closing </b>
    FirstChunk = hd(Chunks),
    ?assertMatch({match, _}, re:run(FirstChunk, <<"</b>">>)),
    %% Second chunk should have opening <b>
    SecondChunk = lists:nth(2, Chunks),
    ?assertMatch({match, _}, re:run(SecondChunk, <<"<b>">>)).
