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

-module(bc_telegram_format).
-moduledoc """
Pure-function markdown-to-Telegram-HTML converter.

Converts GitHub-flavored markdown from LLM responses into Telegram-compatible
HTML (parse_mode: HTML). Strategy: HTML mode requires escaping only &, <, >
(vs 20+ chars for MarkdownV2).

Multi-pass algorithm:
  1. Extract fenced code blocks → placeholders
  2. Extract inline code → placeholders
  3. HTML-escape remaining text
  4. Block-level conversions (headers, blockquotes, lists, hr)
  5. Inline conversions (bold+italic, links, bold, italic, strikethrough)
  6. Restore placeholders
""".

-export([format/1, chunk/2, escape_html/1]).

%% Placeholder sentinel byte — won't appear in normal text
-define(PH, 0).

%% Unicode characters for block-level rendering
-define(BULLET, <<16#2022/utf8>>).   %% • U+2022 BULLET
-define(HR_LINE, <<16#2500/utf8, 16#2500/utf8, 16#2500/utf8, 16#2500/utf8,
                   16#2500/utf8, 16#2500/utf8, 16#2500/utf8, 16#2500/utf8>>).  %% ────────

%%----------------------------------------------------------------------
%% Public API
%%----------------------------------------------------------------------

-spec format(binary()) -> binary().
format(<<>>) -> <<>>;
format(Input) when is_binary(Input) ->
    %% Pass 1: Extract fenced code blocks
    {Text1, CodeBlocks} = extract_fenced_code(Input),
    %% Pass 2: Extract inline code
    {Text2, InlineCode} = extract_inline_code(Text1),
    %% Pass 3: HTML-escape remaining text (placeholders contain \x00, safe)
    Text3 = escape_html(Text2),
    %% Pass 4: Block-level conversions
    Text4 = block_convert(Text3),
    %% Pass 5: Inline conversions
    Text5 = inline_convert(Text4),
    %% Pass 6: Restore placeholders
    Text6 = restore_placeholders(Text5, CodeBlocks, InlineCode),
    Text6.

-spec escape_html(binary()) -> binary().
escape_html(Bin) ->
    B1 = binary:replace(Bin, <<"&">>, <<"&amp;">>, [global]),
    B2 = binary:replace(B1, <<"<">>, <<"&lt;">>, [global]),
    binary:replace(B2, <<">">>, <<"&gt;">>, [global]).

-spec chunk(binary(), pos_integer()) -> [binary()].
chunk(Text, MaxLen) when byte_size(Text) =< MaxLen ->
    [Text];
chunk(Text, MaxLen) ->
    do_chunk(Text, MaxLen, []).

%%----------------------------------------------------------------------
%% Pass 1: Fenced code blocks
%%----------------------------------------------------------------------

extract_fenced_code(Input) ->
    extract_fenced_code(Input, 0, [], <<>>).

extract_fenced_code(<<>>, _Idx, Acc, Out) ->
    {Out, lists:reverse(Acc)};
extract_fenced_code(Input, Idx, Acc, Out) ->
    case find_fence_start(Input) of
        {Before, Lang, Rest} ->
            case find_fence_end(Rest) of
                {CodeBody, After} ->
                    Escaped = escape_html(CodeBody),
                    Html = case Lang of
                        <<>> -> <<"<pre><code>", Escaped/binary, "</code></pre>">>;
                        _    -> <<"<pre><code>", Escaped/binary, "</code></pre>">>
                    end,
                    PH = make_placeholder(<<"CB">>, Idx),
                    extract_fenced_code(After, Idx + 1,
                                        [{Idx, Html} | Acc],
                                        <<Out/binary, Before/binary, PH/binary>>);
                unclosed ->
                    %% Treat rest as code (streaming tolerance)
                    Escaped = escape_html(Rest),
                    Html = <<"<pre><code>", Escaped/binary, "</code></pre>">>,
                    PH = make_placeholder(<<"CB">>, Idx),
                    {<<Out/binary, Before/binary, PH/binary>>,
                     lists:reverse([{Idx, Html} | Acc])}
            end;
        none ->
            {<<Out/binary, Input/binary>>, lists:reverse(Acc)}
    end.

find_fence_start(Input) ->
    case binary:match(Input, <<"```">>) of
        {Pos, 3} ->
            Before = binary:part(Input, 0, Pos),
            AfterTicks = binary:part(Input, Pos + 3, byte_size(Input) - Pos - 3),
            {Lang, Rest} = extract_lang(AfterTicks),
            {Before, Lang, Rest};
        nomatch ->
            none
    end.

extract_lang(<<"\n", Rest/binary>>) ->
    {<<>>, Rest};
extract_lang(Input) ->
    case binary:match(Input, <<"\n">>) of
        {Pos, 1} ->
            Lang = string:trim(binary:part(Input, 0, Pos)),
            Rest = binary:part(Input, Pos + 1, byte_size(Input) - Pos - 1),
            {Lang, Rest};
        nomatch ->
            %% No newline — entire rest is the lang spec (unlikely, edge case)
            {Input, <<>>}
    end.

find_fence_end(Input) ->
    case binary:match(Input, <<"\n```">>) of
        {Pos, 4} ->
            CodeBody = binary:part(Input, 0, Pos),
            AfterEnd = binary:part(Input, Pos + 4, byte_size(Input) - Pos - 4),
            %% Skip optional trailing chars on the closing fence line
            After = skip_fence_trail(AfterEnd),
            {CodeBody, After};
        nomatch ->
            %% Also check for ``` at start (code starts at line beginning)
            case binary:match(Input, <<"```">>) of
                {0, 3} ->
                    %% Empty code block at start
                    After = binary:part(Input, 3, byte_size(Input) - 3),
                    {<<>>, skip_fence_trail(After)};
                _ ->
                    unclosed
            end
    end.

skip_fence_trail(<<"\n", Rest/binary>>) -> Rest;
skip_fence_trail(<<"\r\n", Rest/binary>>) -> Rest;
skip_fence_trail(Input) ->
    %% Skip any trailing chars on the fence line (e.g., ``` followed by text)
    case binary:match(Input, <<"\n">>) of
        {Pos, 1} -> binary:part(Input, Pos + 1, byte_size(Input) - Pos - 1);
        nomatch -> Input
    end.

%%----------------------------------------------------------------------
%% Pass 2: Inline code
%%----------------------------------------------------------------------

extract_inline_code(Input) ->
    extract_inline_code(Input, 0, [], <<>>).

extract_inline_code(<<>>, _Idx, Acc, Out) ->
    {Out, lists:reverse(Acc)};
extract_inline_code(Input, Idx, Acc, Out) ->
    case binary:match(Input, <<"`">>) of
        {Pos, 1} ->
            %% Check it's not a placeholder
            case Pos > 0 andalso binary:at(Input, Pos - 1) =:= ?PH of
                true ->
                    %% Skip this backtick (part of placeholder)
                    Chunk = binary:part(Input, 0, Pos + 1),
                    Rest = binary:part(Input, Pos + 1, byte_size(Input) - Pos - 1),
                    extract_inline_code(Rest, Idx, Acc, <<Out/binary, Chunk/binary>>);
                false ->
                    Before = binary:part(Input, 0, Pos),
                    AfterTick = binary:part(Input, Pos + 1, byte_size(Input) - Pos - 1),
                    case binary:match(AfterTick, <<"`">>) of
                        {EndPos, 1} ->
                            CodeContent = binary:part(AfterTick, 0, EndPos),
                            Rest = binary:part(AfterTick, EndPos + 1,
                                               byte_size(AfterTick) - EndPos - 1),
                            Escaped = escape_html(CodeContent),
                            Html = <<"<code>", Escaped/binary, "</code>">>,
                            PH = make_placeholder(<<"IC">>, Idx),
                            extract_inline_code(Rest, Idx + 1,
                                                [{Idx, Html} | Acc],
                                                <<Out/binary, Before/binary, PH/binary>>);
                        nomatch ->
                            %% Unclosed backtick — pass through
                            {<<Out/binary, Input/binary>>, lists:reverse(Acc)}
                    end
            end;
        nomatch ->
            {<<Out/binary, Input/binary>>, lists:reverse(Acc)}
    end.

%%----------------------------------------------------------------------
%% Pass 3: HTML escaping (public, called inline above)
%%----------------------------------------------------------------------

%% escape_html/1 is defined in the public API section above.

%%----------------------------------------------------------------------
%% Pass 4: Block-level conversions
%%----------------------------------------------------------------------

block_convert(Text) ->
    Lines = binary:split(Text, <<"\n">>, [global]),
    Converted = convert_lines(Lines, []),
    iolist_to_binary(lists:join(<<"\n">>, Converted)).

convert_lines([], Acc) ->
    lists:reverse(Acc);
convert_lines([Line | Rest], Acc) ->
    {Converted, Remaining} = convert_line(Line, Rest),
    convert_lines(Remaining, [Converted | Acc]).

convert_line(Line, Rest) ->
    Trimmed = string:trim(Line, leading),
    case Trimmed of
        <<"###### ", Header/binary>> ->
            {<<"<b>", (string:trim(Header))/binary, "</b>">>, Rest};
        <<"##### ", Header/binary>> ->
            {<<"<b>", (string:trim(Header))/binary, "</b>">>, Rest};
        <<"#### ", Header/binary>> ->
            {<<"<b>", (string:trim(Header))/binary, "</b>">>, Rest};
        <<"### ", Header/binary>> ->
            {<<"<b>", (string:trim(Header))/binary, "</b>">>, Rest};
        <<"## ", Header/binary>> ->
            {<<"<b>", (string:trim(Header))/binary, "</b>">>, Rest};
        <<"# ", Header/binary>> ->
            {<<"<b>", (string:trim(Header))/binary, "</b>">>, Rest};
        <<"&gt; ", QuoteText/binary>> ->
            %% Blockquote — collect consecutive quote lines
            {QuoteLines, Remaining} = collect_quotes([QuoteText], Rest),
            Merged = iolist_to_binary(lists:join(<<"\n">>, QuoteLines)),
            {<<"<blockquote>", Merged/binary, "</blockquote>">>, Remaining};
        <<"&gt;", QuoteText/binary>> when QuoteText =:= <<>> ->
            %% Empty blockquote line
            {QuoteLines, Remaining} = collect_quotes([<<>>], Rest),
            Merged = iolist_to_binary(lists:join(<<"\n">>, QuoteLines)),
            {<<"<blockquote>", Merged/binary, "</blockquote>">>, Remaining};
        <<"- ", ItemText/binary>> ->
            {<<?BULLET/binary, " ", ItemText/binary>>, Rest};
        <<"* ", ItemText/binary>> ->
            %% Check this isn't a horizontal rule (*** or * * *)
            case is_hr_line(Trimmed) of
                true  -> {?HR_LINE, Rest};
                false -> {<<?BULLET/binary, " ", ItemText/binary>>, Rest}
            end;
        <<"---">> ->
            {?HR_LINE, Rest};
        <<"***">> ->
            {?HR_LINE, Rest};
        <<"___">> ->
            {?HR_LINE, Rest};
        _ ->
            case is_hr_line(Trimmed) of
                true  -> {?HR_LINE, Rest};
                false -> {Line, Rest}
            end
    end.

is_hr_line(<<"---", _/binary>>) -> is_all_hr_chars($-, <<"---">>);
is_hr_line(<<"***", _/binary>>) -> is_all_hr_chars($*, <<"***">>);
is_hr_line(<<"___", _/binary>>) -> is_all_hr_chars($_, <<"___">>);
is_hr_line(_) -> false.

is_all_hr_chars(Char, Bin) ->
    Trimmed = string:trim(Bin),
    case Trimmed of
        <<>> -> false;
        _ ->
            NoSpaces = binary:replace(Trimmed, <<" ">>, <<>>, [global]),
            lists:all(fun(C) -> C =:= Char end, binary_to_list(NoSpaces))
    end.

collect_quotes(Acc, []) ->
    {lists:reverse(Acc), []};
collect_quotes(Acc, [Line | Rest]) ->
    Trimmed = string:trim(Line, leading),
    case Trimmed of
        <<"&gt; ", QuoteText/binary>> ->
            collect_quotes([QuoteText | Acc], Rest);
        <<"&gt;", QuoteText/binary>> when QuoteText =:= <<>> ->
            collect_quotes([<<>> | Acc], Rest);
        _ ->
            {lists:reverse(Acc), [Line | Rest]}
    end.

%%----------------------------------------------------------------------
%% Pass 5: Inline conversions
%%----------------------------------------------------------------------

inline_convert(Text) ->
    %% Order matters: bold+italic before bold before italic
    T1 = replace_bold_italic(Text),
    T2 = replace_links(T1),
    T3 = replace_bold(T2),
    T4 = replace_italic(T3),
    T5 = replace_strikethrough(T4),
    T5.

replace_bold_italic(Text) ->
    re_replace(Text, <<"\\*\\*\\*(.+?)\\*\\*\\*">>, <<"<b><i>\\1</i></b>">>).

replace_links(Text) ->
    re_replace(Text, <<"\\[([^\\]]+)\\]\\(([^)]+)\\)">>, <<"<a href=\"\\2\">\\1</a>">>).

replace_bold(Text) ->
    re_replace(Text, <<"\\*\\*(.+?)\\*\\*">>, <<"<b>\\1</b>">>).

replace_italic(Text) ->
    %% Match single * not preceded/followed by *
    re_replace(Text, <<"(?<!\\*)\\*(?!\\*)(.+?)(?<!\\*)\\*(?!\\*)">>, <<"<i>\\1</i>">>).

replace_strikethrough(Text) ->
    re_replace(Text, <<"~~(.+?)~~">>, <<"<s>\\1</s>">>).

re_replace(Text, Pattern, Replacement) ->
    case re:compile(Pattern, [dotall]) of
        {ok, MP} ->
            iolist_to_binary(re:replace(Text, MP, Replacement, [global]));
        {error, _} ->
            Text
    end.

%%----------------------------------------------------------------------
%% Pass 6: Restore placeholders
%%----------------------------------------------------------------------

restore_placeholders(Text, CodeBlocks, InlineCode) ->
    T1 = lists:foldl(fun({Idx, Html}, Acc) ->
        PH = make_placeholder(<<"CB">>, Idx),
        binary:replace(Acc, PH, Html, [global])
    end, Text, CodeBlocks),
    lists:foldl(fun({Idx, Html}, Acc) ->
        PH = make_placeholder(<<"IC">>, Idx),
        binary:replace(Acc, PH, Html, [global])
    end, T1, InlineCode).

%%----------------------------------------------------------------------
%% Chunking
%%----------------------------------------------------------------------

do_chunk(<<>>, _MaxLen, Acc) ->
    lists:reverse(Acc);
do_chunk(Text, MaxLen, Acc) when byte_size(Text) =< MaxLen ->
    lists:reverse([Text | Acc]);
do_chunk(Text, MaxLen, Acc) ->
    Candidate = binary:part(Text, 0, MaxLen),
    {ChunkEnd, SplitLen} = find_split_point(Candidate),
    Chunk = binary:part(Text, 0, ChunkEnd),
    Rest = binary:part(Text, ChunkEnd + SplitLen,
                       byte_size(Text) - ChunkEnd - SplitLen),
    %% Balance tags
    {BalancedChunk, ReopenTags} = balance_tags(Chunk),
    NextChunk = case ReopenTags of
        <<>> -> Rest;
        _ -> <<ReopenTags/binary, Rest/binary>>
    end,
    do_chunk(NextChunk, MaxLen, [BalancedChunk | Acc]).

find_split_point(Candidate) ->
    Size = byte_size(Candidate),
    %% Try paragraph break first
    case find_last(Candidate, <<"\n\n">>) of
        {ok, Pos} when Pos > Size div 4 -> {Pos, 2};
        _ ->
            %% Try line break
            case find_last(Candidate, <<"\n">>) of
                {ok, Pos} when Pos > Size div 4 -> {Pos, 1};
                _ ->
                    %% Try space
                    case find_last(Candidate, <<" ">>) of
                        {ok, Pos} when Pos > Size div 4 -> {Pos, 1};
                        _ ->
                            %% Hard cut
                            {Size, 0}
                    end
            end
    end.

find_last(Bin, Pattern) ->
    find_last(Bin, Pattern, byte_size(Pattern), nomatch).

find_last(Bin, Pattern, PatLen, LastFound) ->
    Start = case LastFound of
        nomatch -> 0;
        {ok, Prev} -> Prev + PatLen
    end,
    case binary:match(Bin, Pattern, [{scope, {Start, byte_size(Bin) - Start}}]) of
        {Pos, _} -> find_last(Bin, Pattern, PatLen, {ok, Pos});
        nomatch -> LastFound
    end.

%%----------------------------------------------------------------------
%% Tag balancing for chunks
%%----------------------------------------------------------------------

-define(BALANCED_TAGS, [<<"pre">>, <<"code">>, <<"blockquote">>,
                        <<"b">>, <<"i">>, <<"s">>, <<"a">>]).

balance_tags(Chunk) ->
    OpenTags = find_unclosed_tags(Chunk),
    CloseTags = iolist_to_binary([<<"</", T/binary, ">">> || T <- OpenTags]),
    ReopenTags = iolist_to_binary(
        [reopen_tag(T, Chunk) || T <- lists:reverse(OpenTags)]),
    {<<Chunk/binary, CloseTags/binary>>, ReopenTags}.

reopen_tag(<<"a">>, Chunk) ->
    %% Try to find the href from the last unclosed <a> tag
    case re:run(Chunk, <<"<a href=\"([^\"]*)\">(?!.*</a>)">>,
                [{capture, [1], binary}, dotall]) of
        {match, [Href]} -> <<"<a href=\"", Href/binary, "\">">>;
        nomatch -> <<"<a>">>
    end;
reopen_tag(Tag, _Chunk) ->
    <<"<", Tag/binary, ">">>.

find_unclosed_tags(Chunk) ->
    lists:filter(fun(Tag) -> is_tag_unclosed(Tag, Chunk) end, ?BALANCED_TAGS).

is_tag_unclosed(Tag, Chunk) ->
    OpenCount = count_occurrences(Chunk, <<"<", Tag/binary>>),
    CloseCount = count_occurrences(Chunk, <<"</", Tag/binary, ">">>),
    OpenCount > CloseCount.

count_occurrences(Bin, Pattern) ->
    count_occurrences(Bin, Pattern, 0, 0).

count_occurrences(Bin, Pattern, Start, Count) ->
    PatLen = byte_size(Pattern),
    case binary:match(Bin, Pattern, [{scope, {Start, byte_size(Bin) - Start}}]) of
        {Pos, _} ->
            count_occurrences(Bin, Pattern, Pos + PatLen, Count + 1);
        nomatch ->
            Count
    end.

%%----------------------------------------------------------------------
%% Helpers
%%----------------------------------------------------------------------

make_placeholder(Type, Idx) ->
    <<?PH, Type/binary, Idx:32>>.
