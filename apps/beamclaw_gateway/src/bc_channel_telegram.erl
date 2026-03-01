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

-module(bc_channel_telegram).
-moduledoc """
Telegram channel — long-poll or webhook mode.
Implements bc_channel behaviour.

bc_loop calls send_response/2 after each completed turn. The gen_server
handles {send_response, ...} casts and sends the reply via Telegram API.
""".
-behaviour(gen_server).
%% Implements bc_channel callbacks.

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1, handle_webhook/1, send_response/2, notify_typing/1]).
-export([listen/1, send/3, send_typing/2, update_draft/4, finalize_draft/3]).
%% Exported for testing
-export([is_image_mime/1, truncate_caption/1, multipart_field/3, multipart_file/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-doc "Called by bc_loop to deliver a completed response to the Telegram user.".
-spec send_response(SessionId :: binary(), Msg :: #bc_message{}) -> ok.
send_response(SessionId, Msg) ->
    gen_server:cast(?MODULE, {send_response, SessionId, Msg}).

-doc "Called by bc_loop to send a typing indicator for the given session.".
-spec notify_typing(SessionId :: binary()) -> ok.
notify_typing(SessionId) ->
    gen_server:cast(?MODULE, {notify_typing, SessionId}).

-doc "Entry point for bc_webhook_telegram_h.".
handle_webhook(Update) ->
    dispatch_telegram_message(Update).

%% bc_channel callbacks

init(Config) ->
    Token = bc_config:resolve(maps:get(token, Config, {env, "TELEGRAM_BOT_TOKEN"})),
    Mode  = maps:get(mode, Config, long_poll),
    %% ETS table mapping SessionId → ChatId (needed since session IDs are now
    %% derived hashes, not raw ChatId values)
    _ = ets:new(bc_telegram_chat_map, [set, named_table, public]),
    State = #{token => Token, mode => Mode, offset => 0, seen_ids => sets:new()},
    case Mode of
        long_poll ->
            delete_webhook(State),
            self() ! poll;
        webhook ->
            ok
    end,
    {ok, State}.

listen(State) ->
    {ok, State}.

send(SessionId, #bc_message{content = <<>>}, State) ->
    logger:debug("[telegram] skipping empty content: session=~s", [SessionId]),
    {ok, State};
send(SessionId, #bc_message{content = undefined}, State) ->
    logger:debug("[telegram] skipping undefined content: session=~s", [SessionId]),
    {ok, State};
send(SessionId, #bc_message{content = Content, attachments = Attachments}, State) ->
    ChatId = resolve_chat_id(SessionId, State),
    ImageAttachments = [A || {Mime, _} = A <- Attachments, is_image_mime(Mime)],
    case ImageAttachments of
        [] ->
            send_formatted(ChatId, Content, State);
        _ ->
            Caption = truncate_caption(Content),
            lists:foreach(fun({_Mime, B64Data}) ->
                send_photo(ChatId, B64Data, Caption, State)
            end, ImageAttachments)
    end,
    {ok, State}.

send_typing(SessionId, State) ->
    ChatId = resolve_chat_id(SessionId, State),
    send_action(ChatId, <<"typing">>, State),
    ok.

update_draft(SessionId, DraftId, Content, State) ->
    ChatId = resolve_chat_id(SessionId, State),
    edit_message_html(ChatId, DraftId, Content, State),
    {ok, State}.

finalize_draft(_SessionId, _DraftId, State) ->
    {ok, State}.

%% gen_server callbacks

handle_cast({send_response, SessionId, Msg}, State) ->
    {ok, NewState} = send(SessionId, Msg, State),
    {noreply, NewState};
handle_cast({notify_typing, SessionId}, State) ->
    send_typing(SessionId, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(poll, State) ->
    Self   = self(),
    Offset = maps:get(offset, State, 0),
    spawn_link(fun() ->
        Updates = get_updates(Offset, State),
        Self ! {poll_result, Updates}
    end),
    {noreply, State};
handle_info({poll_result, Updates}, State) ->
    {NewOffset, Seen} = process_updates(Updates, State),
    erlang:send_after(1000, self(), poll),
    {noreply, State#{offset => NewOffset, seen_ids => Seen}};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) -> {reply, {error, unknown}, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _) -> {ok, State}.

%% Internal

delete_webhook(#{token := Token}) ->
    Url = make_api_url(Token, "/deleteWebhook"),
    case hackney:request(post, Url, [{<<"content-type">>, <<"application/json">>}],
                         <<"{}">> , [{recv_timeout, 10000}, with_body]) of
        {ok, 200, _, _} ->
            logger:info("[telegram] webhook deleted (long_poll mode)");
        {ok, Code, _, RBody} ->
            logger:warning("[telegram] deleteWebhook failed: ~p ~s",
                           [Code, RBody]);
        {error, Reason} ->
            logger:warning("[telegram] deleteWebhook error: ~p", [Reason])
    end.

get_updates(Offset, #{token := Token}) ->
    Base = make_api_url(Token, "/getUpdates"),
    Url = iolist_to_binary([Base, <<"?offset=">>,
                            integer_to_binary(Offset), <<"&timeout=30">>]),
    case hackney:request(get, Url, [], <<>>,
                         [{recv_timeout, 35000}, with_body]) of
        {ok, 200, _, Body} ->
            Decoded = jsx:decode(Body, [return_maps]),
            maps:get(<<"result">>, Decoded, []);
        {ok, Code, _, RBody} ->
            logger:warning("[telegram] getUpdates failed: ~p ~s", [Code, RBody]),
            [];
        {error, Reason} ->
            logger:warning("[telegram] getUpdates error: ~p", [Reason]),
            []
    end.

process_updates([], State) ->
    {maps:get(offset, State, 0), maps:get(seen_ids, State, sets:new())};
process_updates(Updates, State) ->
    SeenIds = maps:get(seen_ids, State, sets:new()),
    {NewSeen, MaxId} = lists:foldl(fun(Update, {Seen, Max}) ->
        UpdateId = maps:get(<<"update_id">>, Update, 0),
        Msg      = maps:get(<<"message">>,   Update, #{}),
        MsgId    = maps:get(<<"message_id">>, Msg, 0),
        case sets:is_element(MsgId, Seen) of
            true  -> {Seen, max(Max, UpdateId)};
            false ->
                dispatch_telegram_message(Update),
                {sets:add_element(MsgId, Seen), max(Max, UpdateId)}
        end
    end, {SeenIds, maps:get(offset, State, 0)}, Updates),
    {MaxId + 1, NewSeen}.

dispatch_telegram_message(Update) ->
    Msg    = maps:get(<<"message">>, Update, #{}),
    Text   = maps:get(<<"text">>,    Msg,    <<>>),
    From   = maps:get(<<"from">>,    Msg,    #{}),
    Chat   = maps:get(<<"chat">>,    Msg,    #{}),
    TgUserId = integer_to_binary(maps:get(<<"id">>, From, 0)),
    ChatId   = integer_to_binary(maps:get(<<"id">>, Chat, 0)),
    Username = maps:get(<<"username">>, From, <<>>),
    {Content, Attachments} = extract_content_and_attachments(Msg, Text),
    case bc_config:canonical_user_id() of
        Canonical when Canonical =/= undefined ->
            do_dispatch(Canonical, TgUserId, ChatId, Content, Msg, Attachments);
        undefined ->
            DmPolicy = get_dm_policy(),
            case DmPolicy of
                open ->
                    do_dispatch(<<"tg:", TgUserId/binary>>, TgUserId, ChatId, Content, Msg, Attachments);
                _ ->
                    case is_user_allowed(TgUserId) of
                        true ->
                            do_dispatch(<<"tg:", TgUserId/binary>>, TgUserId, ChatId, Content, Msg, Attachments);
                        false when DmPolicy =:= pairing ->
                            Meta = #{<<"username">> => Username},
                            {ok, Code, Status} = bc_pairing:request_pairing(telegram, TgUserId, Meta),
                            case Status of
                                created -> send_pairing_reply(ChatId, TgUserId, Code);
                                existing -> ok
                            end,
                            ok;
                        false ->
                            %% allowlist mode — silently drop
                            ok
                    end
            end
    end.

do_dispatch(UserId, TgUserId, ChatId, <<"/context", _/binary>>, _Msg, _Attachments) ->
    AgentId   = resolve_agent_id(TgUserId),
    SessionId = bc_session_registry:derive_session_id(UserId, AgentId, telegram),
    handle_context_command(SessionId, AgentId, ChatId),
    ok;
do_dispatch(UserId, TgUserId, ChatId, Content, Msg, Attachments) ->
    AgentId   = resolve_agent_id(TgUserId),
    SessionId = bc_session_registry:derive_session_id(UserId, AgentId, telegram),
    %% Map the derived SessionId to the Telegram ChatId for response routing.
    ets:insert(bc_telegram_chat_map, {SessionId, ChatId}),
    logger:debug("[telegram] dispatch message: chat_id=~s session_id=~s text=~s attachments=~B",
                 [ChatId, SessionId, Content, length(Attachments)]),
    ChannelMsg = #bc_channel_message{
        session_id  = SessionId,
        user_id     = UserId,
        agent_id    = AgentId,
        channel     = telegram,
        content     = Content,
        raw         = Msg,
        ts          = erlang:system_time(millisecond),
        attachments = Attachments
        %% reply_pid unset — responses routed via send_response/2
    },
    case bc_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            bc_session:dispatch_run(Pid, ChannelMsg);
        {error, not_found} ->
            Config = #{session_id  => SessionId,
                       user_id     => UserId,
                       channel_id  => ChatId,
                       channel_mod => bc_channel_telegram,
                       agent_id    => AgentId},
            {ok, _} = bc_sessions_sup:start_session(Config),
            %% bc_session_registry:register/2 is synchronous — no sleep needed.
            {ok, Pid} = bc_session_registry:lookup(SessionId),
            bc_session:dispatch_run(Pid, ChannelMsg)
    end.

handle_context_command(SessionId, AgentId, ChatId) ->
    Token = resolve_token(),
    TokenState = #{token => Token},
    case bc_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            History = bc_session:get_history(Pid),
            Info = bc_context:gather(#{agent_id => AgentId, history => History, session_id => SessionId}),
            Html = bc_context:format_telegram(Info),
            PlainFallback = bc_context:format_text(Info),
            send_message_html(binary_to_integer(ChatId), Html, PlainFallback, TokenState);
        {error, not_found} ->
            send_message_plain(binary_to_integer(ChatId),
                               <<"No active session.">>, TokenState)
    end.

extract_content_and_attachments(Msg, Text) ->
    case photo_enabled() of
        false ->
            maybe_extract_voice(Msg, Text);
        true ->
            case bc_telegram_photo:extract_photo(Msg) of
                {ok, FileId} ->
                    Caption = bc_telegram_photo:extract_caption(Msg),
                    Content = case {Caption, Text} of
                        {undefined, <<>>} -> <<"[Photo]">>;
                        {undefined, _}    -> Text;
                        {Cap, _}          -> Cap
                    end,
                    Attachments = try_download_photo(FileId),
                    {Content, Attachments};
                no_photo ->
                    maybe_extract_voice(Msg, Text)
            end
    end.

try_download_photo(FileId) ->
    Token = resolve_token(),
    case bc_telegram_photo:download(FileId, Token) of
        {ok, Mime, ImageBin} ->
            MaxSize = photo_max_size(),
            case bc_telegram_photo:validate_size(ImageBin, MaxSize) of
                ok ->
                    logger:debug("[telegram] photo downloaded: file_id=~s size=~B",
                                 [FileId, byte_size(ImageBin)]),
                    [bc_telegram_photo:to_attachment(Mime, ImageBin)];
                {error, too_large} ->
                    logger:warning("[telegram] photo too large: file_id=~s size=~B max=~B",
                                   [FileId, byte_size(ImageBin), photo_max_size()]),
                    []
            end;
        {error, Reason} ->
            logger:warning("[telegram] photo download failed: file_id=~s reason=~p",
                           [FileId, Reason]),
            []
    end.

maybe_extract_voice(Msg, Text) ->
    case voice_enabled() of
        false -> {Text, []};
        true ->
            case bc_telegram_audio:extract_voice(Msg) of
                {ok, FileId, Duration, MimeType} ->
                    try_transcribe_voice(FileId, Duration, MimeType, Text);
                no_voice ->
                    {Text, []}
            end
    end.

try_transcribe_voice(FileId, Duration, MimeType, Text) ->
    MaxDuration = voice_max_duration(),
    case Duration > MaxDuration of
        true ->
            logger:warning("[telegram] voice too long: duration=~Bs max=~Bs",
                           [Duration, MaxDuration]),
            FallbackText = <<"[Voice message too long for transcription]">>,
            Content = case Text of
                <<>> -> FallbackText;
                _    -> <<Text/binary, "\n", FallbackText/binary>>
            end,
            {Content, []};
        false ->
            Token = resolve_token(),
            case bc_telegram_audio:download(FileId, Token) of
                {ok, _DownloadMime, AudioBin} ->
                    SttConfig = voice_stt_config(),
                    case bc_stt:transcribe(AudioBin, SttConfig, #{mime_type => MimeType}) of
                        {ok, Transcript} ->
                            logger:info("[telegram] voice transcribed: duration=~Bs chars=~B",
                                        [Duration, byte_size(Transcript)]),
                            Content = case Text of
                                <<>> -> <<"[Voice] ", Transcript/binary>>;
                                _    -> <<Text/binary, "\n[Voice] ", Transcript/binary>>
                            end,
                            {Content, []};
                        {error, Reason} ->
                            logger:warning("[telegram] voice transcription failed: ~p", [Reason]),
                            FallbackText = <<"[Voice message — transcription failed]">>,
                            Content = case Text of
                                <<>> -> FallbackText;
                                _    -> <<Text/binary, "\n", FallbackText/binary>>
                            end,
                            {Content, []}
                    end;
                {error, Reason} ->
                    logger:warning("[telegram] voice download failed: file_id=~s reason=~p",
                                   [FileId, Reason]),
                    {Text, []}
            end
    end.

photo_enabled() ->
    Channels = bc_config:get(beamclaw_gateway, channels, []),
    TgConfig = proplists:get_value(telegram, Channels, #{}),
    PhotoCfg = maps:get(photo, TgConfig, #{}),
    maps:get(enabled, PhotoCfg, true).

photo_max_size() ->
    Channels = bc_config:get(beamclaw_gateway, channels, []),
    TgConfig = proplists:get_value(telegram, Channels, #{}),
    PhotoCfg = maps:get(photo, TgConfig, #{}),
    maps:get(max_size_bytes, PhotoCfg, 5242880).  %% 5 MB default

voice_enabled() ->
    Channels = bc_config:get(beamclaw_gateway, channels, []),
    TgConfig = proplists:get_value(telegram, Channels, #{}),
    VoiceCfg = maps:get(voice, TgConfig, #{}),
    maps:get(enabled, VoiceCfg, false).  %% opt-in: disabled by default

voice_max_duration() ->
    Channels = bc_config:get(beamclaw_gateway, channels, []),
    TgConfig = proplists:get_value(telegram, Channels, #{}),
    VoiceCfg = maps:get(voice, TgConfig, #{}),
    maps:get(max_duration_seconds, VoiceCfg, 120).  %% 2 min default

voice_stt_config() ->
    Channels = bc_config:get(beamclaw_gateway, channels, []),
    TgConfig = proplists:get_value(telegram, Channels, #{}),
    VoiceCfg = maps:get(voice, TgConfig, #{}),
    #{api_key  => bc_config:resolve(maps:get(stt_api_key, VoiceCfg, {env, "GROQ_API_KEY"})),
      base_url => maps:get(stt_base_url, VoiceCfg, "https://api.groq.com/openai/v1"),
      model    => maps:get(stt_model, VoiceCfg, "whisper-large-v3-turbo")}.

get_dm_policy() ->
    Channels = bc_config:get(beamclaw_gateway, channels, []),
    TgConfig = proplists:get_value(telegram, Channels, #{}),
    maps:get(dm_policy, TgConfig, pairing).

is_user_allowed(TgUserId) ->
    %% Check config allow_from list
    Channels = bc_config:get(beamclaw_gateway, channels, []),
    TgConfig = proplists:get_value(telegram, Channels, #{}),
    AllowFrom = maps:get(allow_from, TgConfig, []),
    ConfigAllowed = lists:member(TgUserId, [iolist_to_binary([Id]) || Id <- AllowFrom]),
    %% Check pairing store
    PairingAllowed = bc_pairing:is_allowed(telegram, TgUserId),
    ConfigAllowed orelse PairingAllowed.

send_pairing_reply(ChatId, TgUserId, Code) ->
    Text = iolist_to_binary([
        <<"BeamClaw: pairing required.\n\n">>,
        <<"Your ID: ">>, TgUserId, <<"\n">>,
        <<"Code: ">>, Code, <<"\n\n">>,
        <<"Run: beamclaw pair telegram ">>, Code
    ]),
    Token = resolve_token(),
    send_message_plain(binary_to_integer(ChatId), Text, #{token => Token}).

resolve_token() ->
    Channels = bc_config:get(beamclaw_gateway, channels, []),
    TgConfig = proplists:get_value(telegram, Channels, #{}),
    bc_config:resolve(maps:get(token, TgConfig, {env, "TELEGRAM_BOT_TOKEN"})).

resolve_agent_id(TgUserId) ->
    case bc_pairing:get_agent_id(telegram, TgUserId) of
        {ok, Id} -> Id;
        {error, not_found} ->
            bc_config:get(beamclaw_core, default_agent, <<"default">>)
    end.

send_formatted(ChatId, Content, State) ->
    Formatted = bc_telegram_format:format(Content),
    Chunks = bc_telegram_format:chunk(Formatted, 4096),
    lists:foreach(fun(Chunk) ->
        send_message_html(ChatId, Chunk, Content, State)
    end, Chunks).

send_message_html(ChatId, Html, OriginalText, State = #{token := Token}) ->
    Url  = make_api_url(Token, "/sendMessage"),
    Body = jsx:encode(#{chat_id => ChatId, text => Html,
                        parse_mode => <<"HTML">>}),
    case hackney:request(post, Url, [{<<"content-type">>, <<"application/json">>}],
                         Body, [with_body]) of
        {ok, 200, _, _} -> ok;
        {ok, 400, _, _} ->
            logger:warning("[telegram] HTML parse error, falling back to plain text"),
            send_message_plain(ChatId, OriginalText, State);
        {ok, Code, _, RBody} ->
            logger:warning("[telegram] sendMessage failed: ~p ~s", [Code, RBody]);
        {error, Reason} ->
            logger:warning("[telegram] sendMessage error: ~p", [Reason])
    end.

send_message_plain(ChatId, Text, #{token := Token}) ->
    Url  = make_api_url(Token, "/sendMessage"),
    Body = jsx:encode(#{chat_id => ChatId, text => Text}),
    case hackney:request(post, Url, [{<<"content-type">>, <<"application/json">>}],
                         Body, [with_body]) of
        {ok, 200, _, _} -> ok;
        {ok, Code, _, RBody} ->
            logger:warning("[telegram] sendMessage(plain) failed: ~p ~s", [Code, RBody]);
        {error, Reason} ->
            logger:warning("[telegram] sendMessage(plain) error: ~p", [Reason])
    end.

send_photo(ChatId, B64Data, Caption, State = #{token := Token}) ->
    PhotoBin = base64:decode(B64Data),
    Url = make_api_url(Token, "/sendPhoto"),
    Boundary = <<"----BeamClawBoundary">>,
    CType = <<"multipart/form-data; boundary=", Boundary/binary>>,
    %% Build multipart body
    Parts = [
        multipart_field(Boundary, <<"chat_id">>, integer_to_binary(ChatId)),
        multipart_file(Boundary, <<"photo">>, <<"image.png">>, <<"image/png">>, PhotoBin)
    ] ++ case Caption of
        <<>> -> [];
        _    -> [multipart_field(Boundary, <<"caption">>, Caption)]
    end,
    Body = iolist_to_binary(Parts ++ [<<"--", Boundary/binary, "--\r\n">>]),
    case hackney:request(post, Url, [{<<"content-type">>, CType}],
                         Body, [with_body]) of
        {ok, 200, _, _} ->
            logger:info("[telegram] photo sent to chat_id=~B", [ChatId]);
        {ok, Code, _, RBody} ->
            logger:warning("[telegram] sendPhoto failed: ~p ~s, falling back to text",
                           [Code, RBody]),
            %% Fall back to sending text description
            FallbackText = case Caption of
                <<>> -> <<"[Image could not be delivered]">>;
                _    -> Caption
            end,
            send_formatted(ChatId, FallbackText, State);
        {error, Reason} ->
            logger:warning("[telegram] sendPhoto error: ~p", [Reason])
    end.

multipart_field(Boundary, Name, Value) ->
    [<<"--", Boundary/binary, "\r\n">>,
     <<"Content-Disposition: form-data; name=\"", Name/binary, "\"\r\n\r\n">>,
     Value, <<"\r\n">>].

multipart_file(Boundary, FieldName, FileName, ContentType, Data) ->
    [<<"--", Boundary/binary, "\r\n">>,
     <<"Content-Disposition: form-data; name=\"", FieldName/binary,
       "\"; filename=\"", FileName/binary, "\"\r\n">>,
     <<"Content-Type: ", ContentType/binary, "\r\n\r\n">>,
     Data, <<"\r\n">>].

is_image_mime(<<"image/", _/binary>>) -> true;
is_image_mime(_) -> false.

truncate_caption(Content) when byte_size(Content) > 1024 ->
    Truncated = binary:part(Content, 0, 1021),
    <<Truncated/binary, "...">>;
truncate_caption(Content) ->
    Content.

send_action(ChatId, Action, #{token := Token}) ->
    Url  = make_api_url(Token, "/sendChatAction"),
    Body = jsx:encode(#{chat_id => ChatId, action => Action}),
    case hackney:request(post, Url, [{<<"content-type">>, <<"application/json">>}],
                         Body, [with_body]) of
        {ok, 200, _, _} -> ok;
        {ok, Code, _, RBody} ->
            logger:warning("[telegram] sendChatAction failed: ~p ~s", [Code, RBody]);
        {error, Reason} ->
            logger:warning("[telegram] sendChatAction error: ~p", [Reason])
    end.

edit_message_html(ChatId, MessageId, Content, State = #{token := Token}) ->
    Formatted = bc_telegram_format:format(Content),
    Url  = make_api_url(Token, "/editMessageText"),
    Body = jsx:encode(#{chat_id => ChatId, message_id => MessageId,
                        text => Formatted, parse_mode => <<"HTML">>}),
    case hackney:request(post, Url, [{<<"content-type">>, <<"application/json">>}],
                         Body, [with_body]) of
        {ok, 200, _, _} -> ok;
        {ok, 400, _, _} ->
            logger:warning("[telegram] editMessage HTML parse error, falling back to plain"),
            edit_message_plain(ChatId, MessageId, Content, State);
        {ok, Code, _, RBody} ->
            logger:warning("[telegram] editMessageText failed: ~p ~s", [Code, RBody]);
        {error, Reason} ->
            logger:warning("[telegram] editMessageText error: ~p", [Reason])
    end.

edit_message_plain(ChatId, MessageId, Text, #{token := Token}) ->
    Url  = make_api_url(Token, "/editMessageText"),
    Body = jsx:encode(#{chat_id => ChatId, message_id => MessageId, text => Text}),
    case hackney:request(post, Url, [{<<"content-type">>, <<"application/json">>}],
                         Body, [with_body]) of
        {ok, 200, _, _} -> ok;
        {ok, Code, _, RBody} ->
            logger:warning("[telegram] editMessageText(plain) failed: ~p ~s", [Code, RBody]);
        {error, Reason} ->
            logger:warning("[telegram] editMessageText(plain) error: ~p", [Reason])
    end.

make_api_url(Token, Path) ->
    list_to_binary("https://api.telegram.org/bot" ++ Token ++ Path).

resolve_chat_id(SessionId, _State) ->
    case ets:lookup(bc_telegram_chat_map, SessionId) of
        [{_, ChatId}] -> binary_to_integer(ChatId);
        []            ->
            %% Fallback: try interpreting SessionId as a raw chat ID (legacy)
            try binary_to_integer(SessionId)
            catch error:badarg -> 0
            end
    end.
