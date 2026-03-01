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

-module(bc_telegram_audio).
-moduledoc """
Telegram voice/audio extraction and download.

Pure-function module — no process state. Used by bc_channel_telegram to handle
incoming voice messages and audio files. Downloads via the same Telegram Bot API
getFile → CDN pattern as bc_telegram_photo.
""".

-export([extract_voice/1, download/2]).

-doc """
Extract voice/audio metadata from a Telegram message.
Checks 'voice' key first (voice notes), then 'audio' (audio files).
""".
-spec extract_voice(TgMsg :: map()) ->
    {ok, FileId :: binary(), Duration :: non_neg_integer(), MimeType :: binary()} | no_voice.
extract_voice(TgMsg) ->
    case maps:get(<<"voice">>, TgMsg, undefined) of
        Voice when is_map(Voice) ->
            extract_audio_fields(Voice, <<"audio/ogg">>);
        _ ->
            case maps:get(<<"audio">>, TgMsg, undefined) of
                Audio when is_map(Audio) ->
                    DefaultMime = <<"audio/mpeg">>,
                    extract_audio_fields(Audio, DefaultMime);
                _ ->
                    no_voice
            end
    end.

-doc "Download a voice/audio file via Telegram Bot API (delegates to bc_telegram_photo:download/2).".
-spec download(FileId :: binary(), Token :: string()) ->
    {ok, MimeType :: binary(), AudioBin :: binary()} | {error, term()}.
download(FileId, Token) ->
    bc_telegram_photo:download(FileId, Token).

%% Internal

extract_audio_fields(AudioMap, DefaultMime) ->
    case maps:get(<<"file_id">>, AudioMap, undefined) of
        undefined ->
            no_voice;
        FileId ->
            Duration = maps:get(<<"duration">>, AudioMap, 0),
            MimeType = maps:get(<<"mime_type">>, AudioMap, DefaultMime),
            {ok, FileId, Duration, MimeType}
    end.
