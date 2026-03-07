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

-module(bc_telegram_integration_SUITE).
-moduledoc """
Telegram channel integration tests (Tier 2).

Uses bc_telegram_mock (a Cowboy-based mock Telegram Bot API) to exercise
bc_channel_telegram end-to-end. Validates request format, typing indicators,
empty message handling, bot command registration, and Unicode formatting.

Addresses the 15% of historical fix commits caused by Telegram integration
issues (binary encoding, typing indicators, empty responses, formatting).
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% CT callbacks
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([bot_commands_registered/1,
         send_message_integer_chat_id/1,
         send_message_html_format/1,
         send_message_unicode_content/1,
         typing_indicator_sent/1,
         empty_content_skipped/1,
         edit_message_on_draft_update/1,
         webhook_dispatch_creates_session/1]).

suite() ->
    [{timetrap, {seconds, 30}}].

groups() ->
    [{outgoing, [sequence],
      [bot_commands_registered,
       send_message_integer_chat_id,
       send_message_html_format,
       send_message_unicode_content,
       typing_indicator_sent,
       empty_content_skipped,
       edit_message_on_draft_update]},
     {incoming, [sequence],
      [webhook_dispatch_creates_session]}].

all() ->
    [{group, outgoing}, {group, incoming}].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    %% Pick random ports
    MockPort = 29000 + (erlang:unique_integer([positive]) rem 1000),
    GwPort   = 19000 + (erlang:unique_integer([positive]) rem 1000),
    MockBaseUrl = "http://127.0.0.1:" ++ integer_to_list(MockPort),

    %% Set dummy tokens before any app starts
    os:putenv("OPENROUTER_API_KEY", "test-dummy-key"),
    os:putenv("TELEGRAM_BOT_TOKEN", "test-token-123"),
    os:putenv("TELEGRAM_WEBHOOK_SECRET", "ct-test-secret"),

    %% Start cowboy/ranch (needed by the mock HTTP server)
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),

    %% Start the mock Telegram API (requires ranch/cowboy)
    {ok, MockPort} = bc_telegram_mock:start(#{port => MockPort}),

    %% Configure Telegram channel to use the mock
    application:set_env(beamclaw_gateway, channels, [
        {telegram, #{token => {env, "TELEGRAM_BOT_TOKEN"},
                     mode => webhook,  %% webhook mode avoids long-poll loop
                     api_base_url => MockBaseUrl,
                     dm_policy => open,
                     allow_from => [],
                     photo => #{enabled => false},
                     voice => #{enabled => false}}},
        {tui, #{enabled => false}}
    ]),
    application:set_env(beamclaw_gateway, http, #{port => GwPort}),

    %% Configure core
    application:set_env(beamclaw_core, session_persistence, false),
    application:set_env(beamclaw_core, autonomy_level, full),
    application:set_env(beamclaw_core, agentic_loop,
                        #{compaction_threshold_pct => 9999,
                          memory_flush => false}),

    %% Start the full application stack
    {ok, Started} = application:ensure_all_started(beamclaw_gateway),

    %% Wait for bot commands to be registered during init
    timer:sleep(1000),

    [{started_apps, Started},
     {mock_port, MockPort},
     {mock_base_url, MockBaseUrl},
     {gw_port, GwPort} | Config].

end_per_suite(Config) ->
    Started = proplists:get_value(started_apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Started)),
    bc_telegram_mock:stop(),
    os:unsetenv("OPENROUTER_API_KEY"),
    os:unsetenv("TELEGRAM_BOT_TOKEN"),
    os:unsetenv("TELEGRAM_WEBHOOK_SECRET"),
    ok.

init_per_testcase(bot_commands_registered, Config) ->
    %% Don't clear — setMyCommands was sent during init_per_suite
    Config;
init_per_testcase(_TC, Config) ->
    bc_telegram_mock:clear_requests(),
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% Outgoing tests
%% ---------------------------------------------------------------------------

%% Verify bc_channel_telegram calls setMyCommands on startup.
bot_commands_registered(_Config) ->
    Reqs = bc_telegram_mock:get_requests(<<"setMyCommands">>),
    ?assert(length(Reqs) >= 1),
    [Req | _] = Reqs,
    Body = maps:get(body, Req),
    Commands = maps:get(<<"commands">>, Body, []),
    %% Should include /new and /context
    Names = [maps:get(<<"command">>, C) || C <- Commands],
    ?assert(lists:member(<<"new">>, Names)),
    ?assert(lists:member(<<"context">>, Names)),
    ok.

%% Verify sendMessage sends chat_id as an integer (not a string).
send_message_integer_chat_id(_Config) ->
    %% Set up session → chat_id mapping
    SessionId = <<"ct-tg-int-", (integer_to_binary(
        erlang:unique_integer([positive])))/binary>>,
    ChatIdInt = 987654321,
    ets:insert(bc_telegram_chat_map, {SessionId, integer_to_binary(ChatIdInt)}),

    %% Send via the channel
    Msg = #bc_message{
        id      = <<"test-1">>,
        role    = assistant,
        content = <<"Hello from test">>,
        ts      = erlang:system_time(millisecond)
    },
    bc_channel_telegram:send_response(SessionId, Msg),
    timer:sleep(500),

    %% Check the mock received an integer chat_id
    Reqs = bc_telegram_mock:get_requests(<<"sendMessage">>),
    ?assert(length(Reqs) >= 1),
    [Req | _] = Reqs,
    Body = maps:get(body, Req),
    ReceivedChatId = maps:get(<<"chat_id">>, Body),
    ?assert(is_integer(ReceivedChatId)),
    ?assertEqual(ChatIdInt, ReceivedChatId),
    ok.

%% Verify sendMessage includes HTML parse_mode and properly formats markdown.
send_message_html_format(_Config) ->
    SessionId = <<"ct-tg-fmt-", (integer_to_binary(
        erlang:unique_integer([positive])))/binary>>,
    ets:insert(bc_telegram_chat_map, {SessionId, <<"111222333">>}),

    Msg = #bc_message{
        id      = <<"test-2">>,
        role    = assistant,
        content = <<"This is **bold** text">>,
        ts      = erlang:system_time(millisecond)
    },
    bc_channel_telegram:send_response(SessionId, Msg),
    timer:sleep(500),

    Reqs = bc_telegram_mock:get_requests(<<"sendMessage">>),
    ?assert(length(Reqs) >= 1),
    LastReq = lists:last(Reqs),
    Body = maps:get(body, LastReq),
    %% Should have parse_mode = HTML
    ParseMode = maps:get(<<"parse_mode">>, Body, <<>>),
    ?assertEqual(<<"HTML">>, ParseMode),
    %% Text should contain <b> tags
    Text = maps:get(<<"text">>, Body, <<>>),
    ?assertMatch({match, _}, re:run(Text, <<"<b>bold</b>">>)),
    ok.

%% Verify Unicode content goes through without corruption.
send_message_unicode_content(_Config) ->
    SessionId = <<"ct-tg-uni-", (integer_to_binary(
        erlang:unique_integer([positive])))/binary>>,
    ets:insert(bc_telegram_chat_map, {SessionId, <<"444555666">>}),

    UnicodeContent = <<"# Időjárás\n\nBudapest: 23°C 🌡️"/utf8>>,
    Msg = #bc_message{
        id      = <<"test-3">>,
        role    = assistant,
        content = UnicodeContent,
        ts      = erlang:system_time(millisecond)
    },
    bc_channel_telegram:send_response(SessionId, Msg),
    timer:sleep(500),

    Reqs = bc_telegram_mock:get_requests(<<"sendMessage">>),
    ?assert(length(Reqs) >= 1),
    LastReq = lists:last(Reqs),
    Body = maps:get(body, LastReq),
    Text = maps:get(<<"text">>, Body, <<>>),
    %% Should contain the header as bold and the Unicode content
    ?assertMatch({match, _}, re:run(Text, <<"Időjárás"/utf8>>)),
    ?assertMatch({match, _}, re:run(Text, <<"Budapest">>)),
    ok.

%% Verify typing indicator is sent via sendChatAction.
typing_indicator_sent(_Config) ->
    SessionId = <<"ct-tg-type-", (integer_to_binary(
        erlang:unique_integer([positive])))/binary>>,
    ets:insert(bc_telegram_chat_map, {SessionId, <<"777888999">>}),

    bc_channel_telegram:notify_typing(SessionId),
    timer:sleep(500),

    Reqs = bc_telegram_mock:get_requests(<<"sendChatAction">>),
    ?assert(length(Reqs) >= 1),
    [Req | _] = Reqs,
    Body = maps:get(body, Req),
    ?assertEqual(<<"typing">>, maps:get(<<"action">>, Body)),
    ok.

%% Verify empty/undefined content is not sent to Telegram.
empty_content_skipped(_Config) ->
    SessionId = <<"ct-tg-empty-", (integer_to_binary(
        erlang:unique_integer([positive])))/binary>>,
    ets:insert(bc_telegram_chat_map, {SessionId, <<"101010101">>}),
    bc_telegram_mock:clear_requests(),

    %% Empty binary content
    Msg1 = #bc_message{id = <<"e1">>, role = assistant,
                       content = <<>>,
                       ts = erlang:system_time(millisecond)},
    bc_channel_telegram:send_response(SessionId, Msg1),
    timer:sleep(200),

    %% Undefined content
    Msg2 = #bc_message{id = <<"e2">>, role = assistant,
                       content = undefined,
                       ts = erlang:system_time(millisecond)},
    bc_channel_telegram:send_response(SessionId, Msg2),
    timer:sleep(200),

    %% No sendMessage calls should have been made
    Reqs = bc_telegram_mock:get_requests(<<"sendMessage">>),
    ?assertEqual(0, length(Reqs)),
    ok.

%% Verify editMessageText is used for draft updates (progressive streaming).
edit_message_on_draft_update(_Config) ->
    SessionId = <<"ct-tg-edit-", (integer_to_binary(
        erlang:unique_integer([positive])))/binary>>,
    ets:insert(bc_telegram_chat_map, {SessionId, <<"202020202">>}),
    bc_telegram_mock:clear_requests(),

    %% update_draft through gen_server cast (needs token in State)
    gen_server:cast(bc_channel_telegram,
                    {update_draft, SessionId, 12345, <<"Updated text">>}),
    timer:sleep(500),

    Reqs = bc_telegram_mock:get_requests(<<"editMessageText">>),
    ?assert(length(Reqs) >= 1),
    ok.

%% ---------------------------------------------------------------------------
%% Incoming tests
%% ---------------------------------------------------------------------------

%% Verify that dispatching a webhook update creates a session.
webhook_dispatch_creates_session(Config) ->
    GwPort = proplists:get_value(gw_port, Config),

    %% Simulate an incoming webhook update
    Update = #{
        <<"update_id">> => erlang:unique_integer([positive]),
        <<"message">> => #{
            <<"message_id">> => erlang:unique_integer([positive]),
            <<"text">> => <<"Hello BeamClaw">>,
            <<"from">> => #{<<"id">> => 12345678, <<"username">> => <<"testuser">>},
            <<"chat">> => #{<<"id">> => 12345678, <<"type">> => <<"private">>},
            <<"date">> => erlang:system_time(second)
        }
    },

    %% Post to webhook endpoint with the secret token header
    WebhookUrl = "http://127.0.0.1:" ++ integer_to_list(GwPort) ++ "/webhook/telegram",
    ReqBody = binary_to_list(jsx:encode(Update)),
    {ok, {{_, StatusCode, _}, _, _}} =
        httpc:request(post,
            {WebhookUrl,
             [{"x-telegram-bot-api-secret-token", "ct-test-secret"}],
             "application/json", ReqBody},
            [{timeout, 5000}], []),
    ?assertEqual(200, StatusCode),

    %% The channel should have dispatched this to a session.
    %% Verify by checking that a sendMessage or sendChatAction was made
    %% (the session will trigger the mock provider and respond).
    timer:sleep(2000),

    %% At minimum, a typing indicator should have been sent
    AllReqs = bc_telegram_mock:get_requests(),
    Methods = [maps:get(method, R) || R <- AllReqs],
    ct:pal("Methods after webhook: ~p", [Methods]),
    %% Should have at least one outbound call (typing or response)
    ?assert(length(AllReqs) > 0),
    ok.
