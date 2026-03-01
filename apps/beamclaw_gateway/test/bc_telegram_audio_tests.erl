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

-module(bc_telegram_audio_tests).
-moduledoc "EUnit tests for bc_telegram_audio — voice/audio extraction.".

-include_lib("eunit/include/eunit.hrl").

%% ---- extract_voice/1 — voice message present ----

voice_present_test() ->
    Msg = #{<<"voice">> => #{
        <<"file_id">> => <<"voice123">>,
        <<"duration">> => 5,
        <<"mime_type">> => <<"audio/ogg">>
    }},
    ?assertEqual({ok, <<"voice123">>, 5, <<"audio/ogg">>},
                 bc_telegram_audio:extract_voice(Msg)).

voice_default_mime_test() ->
    Msg = #{<<"voice">> => #{
        <<"file_id">> => <<"v1">>,
        <<"duration">> => 3
    }},
    {ok, <<"v1">>, 3, Mime} = bc_telegram_audio:extract_voice(Msg),
    ?assertEqual(<<"audio/ogg">>, Mime).

voice_default_duration_test() ->
    Msg = #{<<"voice">> => #{
        <<"file_id">> => <<"v2">>
    }},
    {ok, <<"v2">>, Duration, _} = bc_telegram_audio:extract_voice(Msg),
    ?assertEqual(0, Duration).

%% ---- extract_voice/1 — audio file present ----

audio_present_test() ->
    Msg = #{<<"audio">> => #{
        <<"file_id">> => <<"audio456">>,
        <<"duration">> => 120,
        <<"mime_type">> => <<"audio/mpeg">>
    }},
    ?assertEqual({ok, <<"audio456">>, 120, <<"audio/mpeg">>},
                 bc_telegram_audio:extract_voice(Msg)).

audio_default_mime_test() ->
    Msg = #{<<"audio">> => #{
        <<"file_id">> => <<"a1">>,
        <<"duration">> => 60
    }},
    {ok, <<"a1">>, 60, Mime} = bc_telegram_audio:extract_voice(Msg),
    ?assertEqual(<<"audio/mpeg">>, Mime).

%% ---- extract_voice/1 — neither present ----

no_voice_or_audio_test() ->
    Msg = #{<<"text">> => <<"hello">>},
    ?assertEqual(no_voice, bc_telegram_audio:extract_voice(Msg)).

empty_message_test() ->
    ?assertEqual(no_voice, bc_telegram_audio:extract_voice(#{})).

%% ---- extract_voice/1 — both present (voice wins) ----

voice_wins_over_audio_test() ->
    Msg = #{<<"voice">> => #{
                <<"file_id">> => <<"voice_id">>,
                <<"duration">> => 10,
                <<"mime_type">> => <<"audio/ogg">>
            },
            <<"audio">> => #{
                <<"file_id">> => <<"audio_id">>,
                <<"duration">> => 300,
                <<"mime_type">> => <<"audio/mpeg">>
            }},
    {ok, FileId, _, _} = bc_telegram_audio:extract_voice(Msg),
    ?assertEqual(<<"voice_id">>, FileId).

%% ---- extract_voice/1 — missing file_id ----

voice_missing_file_id_test() ->
    Msg = #{<<"voice">> => #{<<"duration">> => 5}},
    ?assertEqual(no_voice, bc_telegram_audio:extract_voice(Msg)).

audio_missing_file_id_test() ->
    Msg = #{<<"audio">> => #{<<"duration">> => 5}},
    ?assertEqual(no_voice, bc_telegram_audio:extract_voice(Msg)).
