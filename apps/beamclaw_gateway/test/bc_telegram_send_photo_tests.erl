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

-module(bc_telegram_send_photo_tests).
-moduledoc "EUnit tests for Telegram outgoing photo support.".

-include_lib("eunit/include/eunit.hrl").

%% ---- is_image_mime/1 ----

image_png_test() ->
    ?assert(bc_channel_telegram:is_image_mime(<<"image/png">>)).

image_jpeg_test() ->
    ?assert(bc_channel_telegram:is_image_mime(<<"image/jpeg">>)).

image_gif_test() ->
    ?assert(bc_channel_telegram:is_image_mime(<<"image/gif">>)).

image_webp_test() ->
    ?assert(bc_channel_telegram:is_image_mime(<<"image/webp">>)).

image_svg_test() ->
    ?assert(bc_channel_telegram:is_image_mime(<<"image/svg+xml">>)).

not_image_text_test() ->
    ?assertNot(bc_channel_telegram:is_image_mime(<<"text/plain">>)).

not_image_octet_test() ->
    ?assertNot(bc_channel_telegram:is_image_mime(<<"application/octet-stream">>)).

not_image_empty_test() ->
    ?assertNot(bc_channel_telegram:is_image_mime(<<>>)).

%% ---- truncate_caption/1 ----

short_caption_unchanged_test() ->
    Caption = <<"A short caption">>,
    ?assertEqual(Caption, bc_channel_telegram:truncate_caption(Caption)).

empty_caption_test() ->
    ?assertEqual(<<>>, bc_channel_telegram:truncate_caption(<<>>)).

exact_1024_unchanged_test() ->
    Caption = binary:copy(<<"x">>, 1024),
    ?assertEqual(1024, byte_size(bc_channel_telegram:truncate_caption(Caption))).

over_1024_truncated_test() ->
    Caption = binary:copy(<<"x">>, 2000),
    Result = bc_channel_telegram:truncate_caption(Caption),
    ?assertEqual(1024, byte_size(Result)),
    %% Ends with "..."
    ?assertEqual(<<"...">>, binary:part(Result, 1021, 3)).

%% ---- multipart_field/3 ----

multipart_field_format_test() ->
    Boundary = <<"----TestBoundary">>,
    Result = iolist_to_binary(bc_channel_telegram:multipart_field(Boundary, <<"chat_id">>, <<"12345">>)),
    ?assert(binary:match(Result, <<"------TestBoundary\r\n">>) =/= nomatch),
    ?assert(binary:match(Result, <<"Content-Disposition: form-data; name=\"chat_id\"">>) =/= nomatch),
    ?assert(binary:match(Result, <<"12345">>) =/= nomatch).

%% ---- multipart_file/5 ----

multipart_file_format_test() ->
    Boundary = <<"----TestBoundary">>,
    Result = iolist_to_binary(bc_channel_telegram:multipart_file(
        Boundary, <<"photo">>, <<"test.png">>, <<"image/png">>, <<"PNGDATA">>)),
    ?assert(binary:match(Result, <<"------TestBoundary\r\n">>) =/= nomatch),
    ?assert(binary:match(Result, <<"name=\"photo\"">>) =/= nomatch),
    ?assert(binary:match(Result, <<"filename=\"test.png\"">>) =/= nomatch),
    ?assert(binary:match(Result, <<"Content-Type: image/png">>) =/= nomatch),
    ?assert(binary:match(Result, <<"PNGDATA">>) =/= nomatch).
