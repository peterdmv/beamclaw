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

-module(bc_stt).
-moduledoc """
Speech-to-text client for OpenAI-compatible /audio/transcriptions API.

Pure-function module — no process state. Uses hackney multipart POST.
Designed for Groq's whisper-large-v3-turbo but works with any OpenAI-compatible
transcription endpoint.
""".

-export([transcribe/2, transcribe/3]).
%% Exported for testing
-export([build_multipart_body/4]).

-doc "Transcribe audio binary using the given STT config.".
-spec transcribe(AudioBin :: binary(), Config :: map()) ->
    {ok, Text :: binary()} | {error, term()}.
transcribe(AudioBin, Config) ->
    transcribe(AudioBin, Config, #{}).

-doc """
Transcribe audio binary with optional parameters.
Config = #{api_key => Key, base_url => Url, model => Model}
Opts = #{language => <<"hu">>, prompt => <<"...">>, ...}
""".
-spec transcribe(AudioBin :: binary(), Config :: map(), Opts :: map()) ->
    {ok, Text :: binary()} | {error, term()}.
transcribe(AudioBin, Config, Opts) ->
    case resolve_api_key(Config) of
        undefined ->
            {error, no_api_key};
        ApiKey ->
            BaseUrl = maps:get(base_url, Config, "https://api.groq.com/openai/v1"),
            Model = maps:get(model, Config, "whisper-large-v3-turbo"),
            Url = iolist_to_binary([ensure_binary(BaseUrl), <<"/audio/transcriptions">>]),
            MimeType = maps:get(mime_type, Opts, <<"audio/ogg">>),
            {ContentType, Body} = build_multipart_body(AudioBin, Model, MimeType, Opts),
            Headers = [{<<"Authorization">>, iolist_to_binary([<<"Bearer ">>, ensure_binary(ApiKey)])},
                       {<<"Content-Type">>, ContentType}],
            case hackney:request(post, Url, Headers, Body,
                                 [{recv_timeout, 30000}, {connect_timeout, 10000},
                                  with_body]) of
                {ok, 200, _RespHeaders, RespBody} ->
                    parse_transcription_response(RespBody);
                {ok, Status, _RespHeaders, RespBody} ->
                    {error, {http_error, Status, RespBody}};
                {error, Reason} ->
                    {error, {request_failed, Reason}}
            end
    end.

%% ---------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------

resolve_api_key(#{api_key := Key}) when is_list(Key), length(Key) > 0 -> Key;
resolve_api_key(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    binary_to_list(Key);
resolve_api_key(#{api_key := {env, Var}}) ->
    case os:getenv(Var) of
        false -> undefined;
        ""    -> undefined;
        Key   -> Key
    end;
resolve_api_key(_) ->
    undefined.

build_multipart_body(AudioBin, Model, MimeType, Opts) ->
    Boundary = <<"----BeamClawSTTBoundary">>,
    ContentType = <<"multipart/form-data; boundary=", Boundary/binary>>,
    FileName = mime_to_filename(MimeType),
    Parts = [
        multipart_file(Boundary, <<"file">>, FileName, MimeType, AudioBin),
        multipart_field(Boundary, <<"model">>, ensure_binary(Model))
    ] ++ optional_field(Boundary, <<"language">>, maps:get(language, Opts, undefined))
      ++ optional_field(Boundary, <<"prompt">>, maps:get(prompt, Opts, undefined)),
    Body = iolist_to_binary(Parts ++ [<<"--", Boundary/binary, "--\r\n">>]),
    {ContentType, Body}.

multipart_field(Boundary, Name, Value) ->
    [<<"--", Boundary/binary, "\r\n">>,
     <<"Content-Disposition: form-data; name=\"", Name/binary, "\"\r\n\r\n">>,
     Value, <<"\r\n">>].

multipart_file(Boundary, FieldName, FileName, MimeContentType, Data) ->
    [<<"--", Boundary/binary, "\r\n">>,
     <<"Content-Disposition: form-data; name=\"", FieldName/binary,
       "\"; filename=\"", FileName/binary, "\"\r\n">>,
     <<"Content-Type: ", MimeContentType/binary, "\r\n\r\n">>,
     Data, <<"\r\n">>].

optional_field(_Boundary, _Name, undefined) -> [];
optional_field(Boundary, Name, Value) when is_binary(Value) ->
    [multipart_field(Boundary, Name, Value)];
optional_field(_Boundary, _Name, _) -> [].

mime_to_filename(<<"audio/ogg">>) -> <<"voice.ogg">>;
mime_to_filename(<<"audio/mpeg">>) -> <<"voice.mp3">>;
mime_to_filename(<<"audio/mp4">>) -> <<"voice.m4a">>;
mime_to_filename(<<"audio/wav">>) -> <<"voice.wav">>;
mime_to_filename(<<"audio/webm">>) -> <<"voice.webm">>;
mime_to_filename(_) -> <<"voice.ogg">>.

parse_transcription_response(Body) ->
    try
        Json = jsx:decode(Body, [return_maps]),
        case maps:get(<<"text">>, Json, undefined) of
            undefined -> {error, {no_text_field, Body}};
            Text when is_binary(Text) -> {ok, Text};
            _ -> {error, {invalid_text_field, Body}}
        end
    catch _:_ ->
        {error, {parse_error, Body}}
    end.

ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_list(V) -> list_to_binary(V).
