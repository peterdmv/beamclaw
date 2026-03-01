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

-module(bc_stt_tests).
-moduledoc "EUnit tests for bc_stt — speech-to-text client.".

-include_lib("eunit/include/eunit.hrl").

%% ---- build_multipart_body/4 ----

multipart_body_contains_boundary_test() ->
    {CType, Body} = bc_stt:build_multipart_body(<<"audio">>, "whisper-1", <<"audio/ogg">>, #{}),
    ?assert(binary:match(CType, <<"multipart/form-data; boundary=">>) =/= nomatch),
    %% Final boundary marker
    ?assert(binary:match(Body, <<"------BeamClawSTTBoundary--">>) =/= nomatch).

multipart_body_has_file_field_test() ->
    {_, Body} = bc_stt:build_multipart_body(<<"fakeaudio">>, "whisper-1", <<"audio/ogg">>, #{}),
    ?assert(binary:match(Body, <<"name=\"file\"">>) =/= nomatch),
    ?assert(binary:match(Body, <<"filename=\"voice.ogg\"">>) =/= nomatch),
    ?assert(binary:match(Body, <<"Content-Type: audio/ogg">>) =/= nomatch),
    ?assert(binary:match(Body, <<"fakeaudio">>) =/= nomatch).

multipart_body_has_model_field_test() ->
    {_, Body} = bc_stt:build_multipart_body(<<"a">>, "whisper-large-v3-turbo", <<"audio/ogg">>, #{}),
    ?assert(binary:match(Body, <<"name=\"model\"">>) =/= nomatch),
    ?assert(binary:match(Body, <<"whisper-large-v3-turbo">>) =/= nomatch).

multipart_body_with_language_test() ->
    {_, Body} = bc_stt:build_multipart_body(<<"a">>, "w", <<"audio/ogg">>,
                                            #{language => <<"hu">>}),
    ?assert(binary:match(Body, <<"name=\"language\"">>) =/= nomatch),
    ?assert(binary:match(Body, <<"hu">>) =/= nomatch).

multipart_body_without_optional_fields_test() ->
    {_, Body} = bc_stt:build_multipart_body(<<"a">>, "w", <<"audio/ogg">>, #{}),
    ?assertEqual(nomatch, binary:match(Body, <<"name=\"language\"">>)),
    ?assertEqual(nomatch, binary:match(Body, <<"name=\"prompt\"">>)).

multipart_body_with_prompt_test() ->
    {_, Body} = bc_stt:build_multipart_body(<<"a">>, "w", <<"audio/ogg">>,
                                            #{prompt => <<"context hint">>}),
    ?assert(binary:match(Body, <<"name=\"prompt\"">>) =/= nomatch),
    ?assert(binary:match(Body, <<"context hint">>) =/= nomatch).

%% ---- mime_to_filename via multipart body ----

ogg_filename_test() ->
    {_, Body} = bc_stt:build_multipart_body(<<"a">>, "w", <<"audio/ogg">>, #{}),
    ?assert(binary:match(Body, <<"voice.ogg">>) =/= nomatch).

mp3_filename_test() ->
    {_, Body} = bc_stt:build_multipart_body(<<"a">>, "w", <<"audio/mpeg">>, #{}),
    ?assert(binary:match(Body, <<"voice.mp3">>) =/= nomatch).

m4a_filename_test() ->
    {_, Body} = bc_stt:build_multipart_body(<<"a">>, "w", <<"audio/mp4">>, #{}),
    ?assert(binary:match(Body, <<"voice.m4a">>) =/= nomatch).

wav_filename_test() ->
    {_, Body} = bc_stt:build_multipart_body(<<"a">>, "w", <<"audio/wav">>, #{}),
    ?assert(binary:match(Body, <<"voice.wav">>) =/= nomatch).

unknown_mime_defaults_ogg_test() ->
    {_, Body} = bc_stt:build_multipart_body(<<"a">>, "w", <<"audio/x-custom">>, #{}),
    ?assert(binary:match(Body, <<"voice.ogg">>) =/= nomatch).

%% ---- config resolution ----

no_api_key_returns_error_test() ->
    Result = bc_stt:transcribe(<<"audio">>, #{}),
    ?assertEqual({error, no_api_key}, Result).

empty_string_api_key_returns_error_test() ->
    Result = bc_stt:transcribe(<<"audio">>, #{api_key => <<>>}),
    ?assertEqual({error, no_api_key}, Result).

empty_list_api_key_returns_error_test() ->
    Result = bc_stt:transcribe(<<"audio">>, #{api_key => ""}),
    ?assertEqual({error, no_api_key}, Result).
