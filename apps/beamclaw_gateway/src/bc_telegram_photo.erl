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

-module(bc_telegram_photo).
-moduledoc """
Telegram photo extraction, download, and base64 encoding.

Pure-function module — no process state. Used by bc_channel_telegram to handle
incoming photos and thread them as multimodal attachments through the message
pipeline to the LLM provider.
""".

-export([extract_photo/1, extract_caption/1, download/2,
         validate_size/2, to_attachment/2, mime_from_path/1]).

-doc "Pick the highest-resolution photo from a Telegram message's photo array.".
-spec extract_photo(TgMsg :: map()) -> {ok, binary()} | no_photo.
extract_photo(TgMsg) ->
    case maps:get(<<"photo">>, TgMsg, []) of
        [_ | _] = Photos ->
            %% Telegram orders photos smallest → largest; last = highest res
            Last = lists:last(Photos),
            {ok, maps:get(<<"file_id">>, Last)};
        _ ->
            no_photo
    end.

-doc "Extract the caption from a Telegram message, or undefined if absent.".
-spec extract_caption(TgMsg :: map()) -> binary() | undefined.
extract_caption(TgMsg) ->
    case maps:get(<<"caption">>, TgMsg, undefined) of
        Caption when is_binary(Caption), byte_size(Caption) > 0 -> Caption;
        _ -> undefined
    end.

-doc "Download a photo via Telegram Bot API: getFile → CDN download.".
-spec download(FileId :: binary(), Token :: string()) ->
    {ok, MimeType :: binary(), ImageBin :: binary()} | {error, term()}.
download(FileId, Token) ->
    case get_file_path(FileId, Token) of
        {ok, FilePath} ->
            download_file(FilePath, Token);
        {error, _} = Err ->
            Err
    end.

-doc "Validate image binary size against a maximum byte limit.".
-spec validate_size(ImageBin :: binary(), MaxBytes :: pos_integer()) ->
    ok | {error, too_large}.
validate_size(ImageBin, MaxBytes) ->
    case byte_size(ImageBin) =< MaxBytes of
        true  -> ok;
        false -> {error, too_large}
    end.

-doc "Base64-encode an image binary into an attachment tuple.".
-spec to_attachment(MimeType :: binary(), ImageBin :: binary()) ->
    {binary(), binary()}.
to_attachment(MimeType, ImageBin) ->
    {MimeType, base64:encode(ImageBin)}.

-doc "Determine MIME type from a file path extension.".
-spec mime_from_path(FilePath :: binary()) -> binary().
mime_from_path(FilePath) ->
    Lower = string:lowercase(FilePath),
    case filename:extension(Lower) of
        <<".jpg">>  -> <<"image/jpeg">>;
        <<".jpeg">> -> <<"image/jpeg">>;
        <<".png">>  -> <<"image/png">>;
        <<".webp">> -> <<"image/webp">>;
        <<".gif">>  -> <<"image/gif">>;
        _           -> <<"image/jpeg">>  %% safe default for Telegram
    end.

%% Internal

get_file_path(FileId, Token) ->
    Url = list_to_binary("https://api.telegram.org/bot" ++ Token ++
          "/getFile?file_id=" ++ binary_to_list(FileId)),
    case hackney:request(get, Url, [], <<>>,
                         [{recv_timeout, 10000}, with_body]) of
        {ok, 200, _, Body} ->
            Decoded = jsx:decode(Body, [return_maps]),
            Result = maps:get(<<"result">>, Decoded, #{}),
            case maps:get(<<"file_path">>, Result, undefined) of
                undefined -> {error, no_file_path};
                Path      -> {ok, Path}
            end;
        {ok, Code, _, _} ->
            {error, {http_status, Code}};
        {error, Reason} ->
            {error, Reason}
    end.

download_file(FilePath, Token) ->
    Url = list_to_binary("https://api.telegram.org/file/bot" ++ Token ++
          "/" ++ binary_to_list(iolist_to_binary(FilePath))),
    case hackney:request(get, Url, [], <<>>,
                         [{recv_timeout, 10000}, with_body]) of
        {ok, 200, _, ImageBin} ->
            MimeType = mime_from_path(iolist_to_binary(FilePath)),
            {ok, MimeType, ImageBin};
        {ok, Code, _, _} ->
            {error, {download_failed, Code}};
        {error, Reason} ->
            {error, Reason}
    end.
