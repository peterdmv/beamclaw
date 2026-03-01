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

-module(bc_thinking).
-moduledoc "Strip LLM thinking/reasoning tags from response content.".

-export([strip/1]).

-doc "Strip thinking tags from binary content, preserving code blocks.".
-spec strip(binary() | undefined | null) -> binary() | undefined.
strip(null) -> <<>>;           %% jsx decodes JSON null as atom
strip(undefined) -> undefined;
strip(<<>>) -> <<>>;
strip(Content) when is_binary(Content) ->
    %% Fast path: if no angle bracket, nothing to strip
    case binary:match(Content, <<"<">>) of
        nomatch -> Content;
        _ ->
            Stripped = do_strip(Content),
            case Stripped of
                <<>> ->
                    %% Stripping removed all content — preserve inner text
                    do_strip_tags_only(Content);
                _ ->
                    Stripped
            end
    end.

%% Internal

-spec do_strip(binary()) -> binary().
do_strip(Content) ->
    CodeRegions = find_code_regions(Content),
    %% Strip <final>...</final> tags first
    S1 = strip_tag_pair(Content, <<"final">>, CodeRegions, false),
    %% Recalculate code regions after content change
    CodeRegions2 = find_code_regions(S1),
    %% Strip thinking block tags and their content
    S2 = strip_thinking_blocks(S1, CodeRegions2),
    %% Trim leading whitespace
    trim_leading(S2).

%% Find all code regions (fenced and inline) as {Start, End} byte offset pairs.
-spec find_code_regions(binary()) -> [{non_neg_integer(), non_neg_integer()}].
find_code_regions(Content) ->
    Fenced = find_fenced_code(Content),
    Inline = find_inline_code(Content),
    lists:sort(Fenced ++ Inline).

%% Find fenced code blocks: ```...``` or ~~~...~~~
-spec find_fenced_code(binary()) -> [{non_neg_integer(), non_neg_integer()}].
find_fenced_code(Content) ->
    find_fenced_code(Content, 0, []).

find_fenced_code(Content, Pos, Acc) when Pos < byte_size(Content) ->
    Rest = binary:part(Content, Pos, byte_size(Content) - Pos),
    case match_fence_start(Rest) of
        {ok, Fence, FenceLen} ->
            OpenEnd = Pos + FenceLen,
            case find_fence_close(Content, OpenEnd, Fence) of
                {ok, CloseEnd} ->
                    find_fenced_code(Content, CloseEnd, [{Pos, CloseEnd} | Acc]);
                not_found ->
                    %% Unclosed fence — rest of content is code
                    [{Pos, byte_size(Content)} | Acc]
            end;
        nomatch ->
            find_fenced_code(Content, Pos + 1, Acc)
    end;
find_fenced_code(_Content, _Pos, Acc) ->
    lists:reverse(Acc).

match_fence_start(<<"`", "`", "`", _/binary>> = Bin) ->
    %% Count consecutive backticks
    N = count_char(Bin, $`, 0),
    %% Skip to end of line for the fence opener
    {ok, binary:copy(<<"`">>, N), N};
match_fence_start(<<"~", "~", "~", _/binary>> = Bin) ->
    N = count_char(Bin, $~, 0),
    {ok, binary:copy(<<"~">>, N), N};
match_fence_start(_) ->
    nomatch.

count_char(<<C, Rest/binary>>, C, N) -> count_char(Rest, C, N + 1);
count_char(_, _, N) -> N.

find_fence_close(Content, Pos, Fence) ->
    FenceSize = byte_size(Fence),
    case binary:match(Content, <<"\n", Fence/binary>>, [{scope, {Pos, byte_size(Content) - Pos}}]) of
        {MatchPos, _} ->
            %% Found newline + fence; end is after the fence
            {ok, MatchPos + 1 + FenceSize};
        nomatch ->
            not_found
    end.

%% Find inline code spans: `...`
-spec find_inline_code(binary()) -> [{non_neg_integer(), non_neg_integer()}].
find_inline_code(Content) ->
    find_inline_code(Content, 0, []).

find_inline_code(Content, Pos, Acc) when Pos < byte_size(Content) ->
    case binary:at(Content, Pos) of
        $` ->
            %% Count consecutive backticks for the opening
            N = count_char(binary:part(Content, Pos, byte_size(Content) - Pos), $`, 0),
            %% Check this isn't a fenced block (3+ backticks at line start)
            case N >= 3 of
                true ->
                    %% Skip over — handled by fenced code
                    find_inline_code(Content, Pos + N, Acc);
                false ->
                    Delim = binary:copy(<<"`">>, N),
                    SearchStart = Pos + N,
                    case binary:match(Content, Delim, [{scope, {SearchStart, byte_size(Content) - SearchStart}}]) of
                        {ClosePos, _} ->
                            find_inline_code(Content, ClosePos + N, [{Pos, ClosePos + N} | Acc]);
                        nomatch ->
                            find_inline_code(Content, Pos + N, Acc)
                    end
            end;
        _ ->
            find_inline_code(Content, Pos + 1, Acc)
    end;
find_inline_code(_Content, _Pos, Acc) ->
    lists:reverse(Acc).

%% Check if a byte offset falls inside any code region.
-spec in_code_region(non_neg_integer(), [{non_neg_integer(), non_neg_integer()}]) -> boolean().
in_code_region(Pos, Regions) ->
    lists:any(fun({S, E}) -> Pos >= S andalso Pos < E end, Regions).

%% Strip <Tag>...</Tag> pairs (tags + content between them), respecting code regions.
%% Used for thinking tags.
-spec strip_thinking_blocks(binary(), [{non_neg_integer(), non_neg_integer()}]) -> binary().
strip_thinking_blocks(Content, _CodeRegions) ->
    Tags = [<<"think">>, <<"thinking">>, <<"thought">>, <<"antThinking">>],
    Result = lists:foldl(fun(Tag, Acc) ->
        strip_all_occurrences(Acc, Tag, find_code_regions(Acc))
    end, Content, Tags),
    %% Strict mode: check for unclosed opening tags
    handle_unclosed(Result, Tags, find_code_regions(Result)).

%% Strip all occurrences of a given tag pair from content.
strip_all_occurrences(Content, Tag, CodeRegions) ->
    OpenPat = make_open_pattern(Tag),
    case re:run(Content, OpenPat, [caseless, {capture, all, index}]) of
        {match, [{Pos, Len}]} ->
            case in_code_region(Pos, CodeRegions) of
                true ->
                    %% Tag is inside code, skip it — search after this match
                    Before = binary:part(Content, 0, Pos + Len),
                    After = binary:part(Content, Pos + Len, byte_size(Content) - Pos - Len),
                    AfterCodeRegions = shift_regions(CodeRegions, -(Pos + Len)),
                    Stripped = strip_all_occurrences(After, Tag, AfterCodeRegions),
                    <<Before/binary, Stripped/binary>>;
                false ->
                    %% Find closing tag
                    ClosePat = make_close_pattern(Tag),
                    SearchStart = Pos + Len,
                    SearchBin = binary:part(Content, SearchStart, byte_size(Content) - SearchStart),
                    case re:run(SearchBin, ClosePat, [caseless, {capture, all, index}]) of
                        {match, [{CPos, CLen}]} ->
                            %% Remove from open tag start to close tag end
                            Before = binary:part(Content, 0, Pos),
                            CloseEnd = SearchStart + CPos + CLen,
                            After = binary:part(Content, CloseEnd, byte_size(Content) - CloseEnd),
                            NewContent = <<Before/binary, After/binary>>,
                            %% Recurse for more occurrences
                            strip_all_occurrences(NewContent, Tag, find_code_regions(NewContent));
                        nomatch ->
                            %% Unclosed tag — handled later by handle_unclosed
                            Content
                    end
            end;
        nomatch ->
            Content
    end.

%% Strip <Tag>...</Tag> (tags only, keep content) — used for <final>.
-spec strip_tag_pair(binary(), binary(), [{non_neg_integer(), non_neg_integer()}], boolean()) -> binary().
strip_tag_pair(Content, Tag, CodeRegions, _StripContent) ->
    OpenPat = make_open_pattern(Tag),
    ClosePat = make_close_pattern(Tag),
    %% Remove opening tags outside code regions
    S1 = strip_tags_only(Content, OpenPat, CodeRegions),
    %% Remove closing tags outside code regions
    strip_tags_only(S1, ClosePat, find_code_regions(S1)).

strip_tags_only(Content, Pat, CodeRegions) ->
    case re:run(Content, Pat, [caseless, {capture, all, index}]) of
        {match, [{Pos, Len}]} ->
            case in_code_region(Pos, CodeRegions) of
                true ->
                    %% Inside code region, skip
                    Before = binary:part(Content, 0, Pos + Len),
                    After = binary:part(Content, Pos + Len, byte_size(Content) - Pos - Len),
                    AfterRegions = shift_regions(CodeRegions, -(Pos + Len)),
                    <<Before/binary, (strip_tags_only(After, Pat, AfterRegions))/binary>>;
                false ->
                    Before = binary:part(Content, 0, Pos),
                    After = binary:part(Content, Pos + Len, byte_size(Content) - Pos - Len),
                    NewContent = <<Before/binary, After/binary>>,
                    strip_tags_only(NewContent, Pat, find_code_regions(NewContent))
            end;
        nomatch ->
            Content
    end.

%% Strict mode: if any opening thinking tag has no close, truncate from that tag.
handle_unclosed(Content, Tags, CodeRegions) ->
    Positions = lists:filtermap(fun(Tag) ->
        Pat = make_open_pattern(Tag),
        find_first_outside_code(Content, Pat, CodeRegions)
    end, Tags),
    case Positions of
        [] -> Content;
        _ ->
            MinPos = lists:min(Positions),
            trim_leading(binary:part(Content, 0, MinPos))
    end.

find_first_outside_code(Content, Pat, CodeRegions) ->
    find_first_outside_code(Content, Pat, CodeRegions, 0).

find_first_outside_code(Content, Pat, CodeRegions, Offset) ->
    SearchLen = byte_size(Content) - Offset,
    case SearchLen =< 0 of
        true -> false;
        false ->
            SearchBin = binary:part(Content, Offset, SearchLen),
            case re:run(SearchBin, Pat, [caseless, {capture, all, index}]) of
                {match, [{Pos, Len}]} ->
                    AbsPos = Offset + Pos,
                    case in_code_region(AbsPos, CodeRegions) of
                        true ->
                            find_first_outside_code(Content, Pat, CodeRegions, AbsPos + Len);
                        false ->
                            {true, AbsPos}
                    end;
                nomatch ->
                    false
            end
    end.

%% Strip thinking tag delimiters only (keep content) — fallback when
%% do_strip would return <<>> (all content was inside thinking blocks).
-spec do_strip_tags_only(binary()) -> binary().
do_strip_tags_only(Content) ->
    Tags = [<<"think">>, <<"thinking">>, <<"thought">>, <<"antThinking">>, <<"final">>],
    Result = lists:foldl(fun(Tag, Acc) ->
        strip_tag_pair(Acc, Tag, find_code_regions(Acc), false)
    end, Content, Tags),
    trim_leading(Result).

%% Build regex for opening tag: <\s*TagName\s*> (case insensitive)
make_open_pattern(Tag) ->
    <<"<\\s*", Tag/binary, "\\s*>">>.

%% Build regex for closing tag: <\s*/\s*TagName\s*>
make_close_pattern(Tag) ->
    <<"<\\s*/\\s*", Tag/binary, "\\s*>">>.

%% Shift code regions by an offset (for searching in sub-binaries).
shift_regions(Regions, Offset) ->
    [{S + Offset, E + Offset} || {S, E} <- Regions, S + Offset >= 0].

%% Trim leading whitespace (spaces, tabs, newlines).
trim_leading(<<" ", Rest/binary>>) -> trim_leading(Rest);
trim_leading(<<"\t", Rest/binary>>) -> trim_leading(Rest);
trim_leading(<<"\n", Rest/binary>>) -> trim_leading(Rest);
trim_leading(<<"\r", Rest/binary>>) -> trim_leading(Rest);
trim_leading(Bin) -> Bin.
