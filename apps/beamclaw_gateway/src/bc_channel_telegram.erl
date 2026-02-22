%% @doc Telegram channel — long-poll or webhook mode.
%% Implements bc_channel behaviour.
%%
%% bc_loop calls send_response/2 after each completed turn. The gen_server
%% handles {send_response, ...} casts and sends the reply via Telegram API.
-module(bc_channel_telegram).
-behaviour(gen_server).
%% Implements bc_channel callbacks.

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1, handle_webhook/1, send_response/2]).
-export([listen/1, send/3, send_typing/2, update_draft/4, finalize_draft/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Called by bc_loop to deliver a completed response to the Telegram user.
-spec send_response(SessionId :: binary(), Msg :: #bc_message{}) -> ok.
send_response(SessionId, Msg) ->
    gen_server:cast(?MODULE, {send_response, SessionId, Msg}).

%% @doc Entry point for bc_webhook_telegram_h.
handle_webhook(Update) ->
    dispatch_telegram_message(Update).

%% bc_channel callbacks

init(Config) ->
    Token = bc_config:resolve(maps:get(token, Config, {env, "TELEGRAM_BOT_TOKEN"})),
    Mode  = maps:get(mode, Config, long_poll),
    State = #{token => Token, mode => Mode, offset => 0, seen_ids => sets:new()},
    case Mode of
        long_poll -> self() ! poll;
        webhook   -> ok
    end,
    {ok, State}.

listen(State) ->
    {ok, State}.

send(SessionId, #bc_message{content = Content}, State) ->
    ChatId = resolve_chat_id(SessionId, State),
    send_message(ChatId, Content, State),
    {ok, State}.

send_typing(SessionId, State) ->
    ChatId = resolve_chat_id(SessionId, State),
    send_action(ChatId, <<"typing">>, State),
    ok.

update_draft(SessionId, DraftId, Content, State) ->
    ChatId = resolve_chat_id(SessionId, State),
    edit_message(ChatId, DraftId, Content, State),
    {ok, State}.

finalize_draft(_SessionId, _DraftId, State) ->
    {ok, State}.

%% gen_server callbacks

handle_cast({send_response, SessionId, Msg}, State) ->
    {ok, NewState} = send(SessionId, Msg, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(poll, State) ->
    Offset  = maps:get(offset, State, 0),
    Updates = get_updates(Offset, State),
    {NewOffset, Seen} = process_updates(Updates, State),
    erlang:send_after(1000, self(), poll),
    {noreply, State#{offset => NewOffset, seen_ids => Seen}};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) -> {reply, {error, unknown}, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _) -> {ok, State}.

%% Internal

get_updates(Offset, #{token := Token}) ->
    Url = "https://api.telegram.org/bot" ++ Token ++
          "/getUpdates?offset=" ++ integer_to_list(Offset) ++ "&timeout=30",
    case httpc:request(get, {Url, []}, [{timeout, 35000}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            Decoded = jsx:decode(iolist_to_binary(Body), [return_maps]),
            maps:get(<<"result">>, Decoded, []);
        {ok, {{_, Code, _}, _, RBody}} ->
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
    UserId = integer_to_binary(maps:get(<<"id">>, From, 0)),
    ChatId = integer_to_binary(maps:get(<<"id">>, Chat, 0)),
    ChannelMsg = #bc_channel_message{
        session_id = ChatId,
        user_id    = UserId,
        channel    = telegram,
        content    = Text,
        raw        = Msg,
        ts         = erlang:system_time(millisecond)
        %% reply_pid unset — responses routed via send_response/2
    },
    case bc_session_registry:lookup(ChatId) of
        {ok, Pid} ->
            bc_session:dispatch_run(Pid, ChannelMsg);
        {error, not_found} ->
            Config = #{session_id  => ChatId,
                       user_id     => UserId,
                       channel_id  => ChatId,
                       channel_mod => bc_channel_telegram},
            {ok, _} = bc_sessions_sup:start_session(Config),
            %% bc_session_registry:register/2 is synchronous — no sleep needed.
            {ok, Pid} = bc_session_registry:lookup(ChatId),
            bc_session:dispatch_run(Pid, ChannelMsg)
    end.

send_message(ChatId, Text, #{token := Token}) ->
    Url  = "https://api.telegram.org/bot" ++ Token ++ "/sendMessage",
    Body = jsx:encode(#{chat_id => ChatId, text => Text}),
    case httpc:request(post, {Url, [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, _}} -> ok;
        {ok, {{_, Code, _}, _, RBody}} ->
            logger:warning("[telegram] sendMessage failed: ~p ~s", [Code, RBody]);
        {error, Reason} ->
            logger:warning("[telegram] sendMessage error: ~p", [Reason])
    end.

send_action(ChatId, Action, #{token := Token}) ->
    Url  = "https://api.telegram.org/bot" ++ Token ++ "/sendChatAction",
    Body = jsx:encode(#{chat_id => ChatId, action => Action}),
    case httpc:request(post, {Url, [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, _}} -> ok;
        {ok, {{_, Code, _}, _, RBody}} ->
            logger:warning("[telegram] sendChatAction failed: ~p ~s", [Code, RBody]);
        {error, Reason} ->
            logger:warning("[telegram] sendChatAction error: ~p", [Reason])
    end.

edit_message(ChatId, MessageId, Text, #{token := Token}) ->
    Url  = "https://api.telegram.org/bot" ++ Token ++ "/editMessageText",
    Body = jsx:encode(#{chat_id => ChatId, message_id => MessageId, text => Text}),
    case httpc:request(post, {Url, [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, _}} -> ok;
        {ok, {{_, Code, _}, _, RBody}} ->
            logger:warning("[telegram] editMessageText failed: ~p ~s", [Code, RBody]);
        {error, Reason} ->
            logger:warning("[telegram] editMessageText error: ~p", [Reason])
    end.

resolve_chat_id(SessionId, _State) ->
    binary_to_integer(SessionId).
