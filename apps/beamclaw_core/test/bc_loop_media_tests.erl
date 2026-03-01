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

-module(bc_loop_media_tests).
-moduledoc "EUnit tests for bc_loop MEDIA: token extraction.".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% ---- extract_media/1 — no media ----

no_media_plain_text_test() ->
    ?assertEqual([], bc_loop:extract_media(<<"some plain output">>)).

no_media_empty_test() ->
    ?assertEqual([], bc_loop:extract_media(<<>>)).

no_media_non_binary_test() ->
    ?assertEqual([], bc_loop:extract_media(undefined)).

no_media_integer_test() ->
    ?assertEqual([], bc_loop:extract_media(42)).

%% ---- extract_media/1 — MEDIA: token parsing ----

extracts_single_media_test() ->
    %% Create a real temp file
    Path = "/tmp/bc_loop_media_test.png",
    ok = file:write_file(Path, <<"PNG_DATA">>),
    Content = <<"some output\nMEDIA: /tmp/bc_loop_media_test.png\ndone">>,
    Result = bc_loop:extract_media(Content),
    ?assertEqual(1, length(Result)),
    [{Mime, B64}] = Result,
    ?assertEqual(<<"image/png">>, Mime),
    ?assertEqual(<<"PNG_DATA">>, base64:decode(B64)),
    file:delete(Path).

extracts_multiple_media_test() ->
    Path1 = "/tmp/bc_loop_media_test1.png",
    Path2 = "/tmp/bc_loop_media_test2.jpg",
    ok = file:write_file(Path1, <<"IMG1">>),
    ok = file:write_file(Path2, <<"IMG2">>),
    Content = <<"MEDIA: /tmp/bc_loop_media_test1.png\nstuff\nMEDIA: /tmp/bc_loop_media_test2.jpg">>,
    Result = bc_loop:extract_media(Content),
    ?assertEqual(2, length(Result)),
    [{M1, _}, {M2, _}] = Result,
    ?assertEqual(<<"image/png">>, M1),
    ?assertEqual(<<"image/jpeg">>, M2),
    file:delete(Path1),
    file:delete(Path2).

extracts_media_no_space_after_colon_test() ->
    Path = "/tmp/bc_loop_media_nospace.png",
    ok = file:write_file(Path, <<"DATA">>),
    Content = <<"MEDIA:/tmp/bc_loop_media_nospace.png">>,
    Result = bc_loop:extract_media(Content),
    ?assertEqual(1, length(Result)),
    file:delete(Path).

skips_nonexistent_file_test() ->
    Content = <<"MEDIA: /tmp/bc_loop_does_not_exist_xyz.png">>,
    ?assertEqual([], bc_loop:extract_media(Content)).

mixed_existing_and_missing_test() ->
    Path = "/tmp/bc_loop_media_mixed.png",
    ok = file:write_file(Path, <<"REAL">>),
    Content = <<"MEDIA: /tmp/bc_loop_media_mixed.png\nMEDIA: /tmp/bc_loop_no_such_file.png">>,
    Result = bc_loop:extract_media(Content),
    ?assertEqual(1, length(Result)),
    file:delete(Path).

%% ---- mime_from_path/1 ----

mime_png_test() ->
    ?assertEqual(<<"image/png">>, bc_loop:mime_from_path(<<"/tmp/foo.png">>)).

mime_jpg_test() ->
    ?assertEqual(<<"image/jpeg">>, bc_loop:mime_from_path(<<"/tmp/foo.jpg">>)).

mime_jpeg_test() ->
    ?assertEqual(<<"image/jpeg">>, bc_loop:mime_from_path(<<"/tmp/foo.jpeg">>)).

mime_gif_test() ->
    ?assertEqual(<<"image/gif">>, bc_loop:mime_from_path(<<"/tmp/foo.gif">>)).

mime_webp_test() ->
    ?assertEqual(<<"image/webp">>, bc_loop:mime_from_path(<<"/tmp/foo.webp">>)).

mime_svg_test() ->
    ?assertEqual(<<"image/svg+xml">>, bc_loop:mime_from_path(<<"/tmp/foo.svg">>)).

mime_bmp_test() ->
    ?assertEqual(<<"image/bmp">>, bc_loop:mime_from_path(<<"/tmp/foo.bmp">>)).

mime_unknown_test() ->
    ?assertEqual(<<"application/octet-stream">>, bc_loop:mime_from_path(<<"/tmp/foo.xyz">>)).

mime_case_insensitive_test() ->
    ?assertEqual(<<"image/png">>, bc_loop:mime_from_path(<<"/tmp/FOO.PNG">>)).
