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

-module(bc_context_integration_SUITE).
-moduledoc """
Integration tests for the /context command (Tier 2).

Exercises the full dispatch path through the TUI and Telegram channels:
- TUI:      /context input → session lookup → bc_context:gather → ANSI text → stdout
- Telegram: /context webhook → session lookup → bc_context:gather → PNG/text → API call

Verifies: no-session graceful fallback, history invariance (command is read-only),
output contains expected context information.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% CT callbacks
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% TUI test cases
-export([tui_context_no_session/1,
         tui_context_with_session/1,
         tui_context_with_history/1]).

%% Telegram test cases
-export([telegram_context_no_session/1,
         telegram_context_with_session/1,
         telegram_context_with_history/1]).

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [{group, tui_context}, {group, telegram_context}].

groups() ->
    [{tui_context, [sequence],
      [tui_context_no_session,
       tui_context_with_session,
       tui_context_with_history]},
     {telegram_context, [sequence],
      [telegram_context_no_session,
       telegram_context_with_session,
       telegram_context_with_history]}].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    Port = 19000 + (erlang:unique_integer([positive]) rem 1000),

    %% Canonical user ID — bypasses Telegram pairing
    os:putenv("BEAMCLAW_USER", "ct-context-user"),
    os:putenv("OPENROUTER_API_KEY", "test-dummy-key"),
    os:putenv("TELEGRAM_BOT_TOKEN", "000000000:fake-token-for-ct"),

    %% Gateway: HTTP on random port, TUI disabled, Telegram in webhook mode
    application:set_env(beamclaw_gateway, http, #{port => Port}),
    application:set_env(beamclaw_gateway, channels, [
        {telegram, #{token => {env, "TELEGRAM_BOT_TOKEN"},
                     mode => webhook,
                     dm_policy => open}},
        {tui, #{enabled => false}}
    ]),

    %% Core: no persistence, full autonomy, no compaction
    application:set_env(beamclaw_core, session_persistence, false),
    application:set_env(beamclaw_core, session_sharing, per_channel),
    application:set_env(beamclaw_core, autonomy_level, full),
    application:set_env(beamclaw_core, agentic_loop,
                        #{compaction_threshold => 9999,
                          memory_flush => false}),

    %% Start inets + gateway (brings up full app tree)
    {ok, _} = application:ensure_all_started(inets),
    {ok, Started} = application:ensure_all_started(beamclaw_gateway),

    %% Ensure default agent workspace exists for system prompt assembly
    bc_workspace:ensure_default_agent(),

    [{started_apps, Started}, {port, Port} | Config].

end_per_suite(Config) ->
    Started = proplists:get_value(started_apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Started)),
    os:unsetenv("BEAMCLAW_USER"),
    os:unsetenv("OPENROUTER_API_KEY"),
    os:unsetenv("TELEGRAM_BOT_TOKEN"),
    ok.

%% ---------------------------------------------------------------------------
%% Group lifecycle
%% ---------------------------------------------------------------------------

init_per_group(tui_context, Config) ->
    %% Start TUI with enabled=false to avoid start_io/read_line races.
    %% We'll send {line_result, ...} messages directly.
    GL = spawn_capture_gl(),
    TuiConfig = #{enabled => false},
    {ok, TuiPid} = bc_channel_tui:start_link(TuiConfig),
    %% Unlink so TUI survives when init_per_group process exits
    unlink(TuiPid),
    %% Redirect TUI's group leader to our capture process
    erlang:group_leader(GL, TuiPid),
    [{tui_pid, TuiPid}, {capture_gl, GL} | Config];
init_per_group(telegram_context, Config) ->
    Config.

end_per_group(tui_context, Config) ->
    GL = proplists:get_value(capture_gl, Config),
    GL ! stop,
    %% Unregister and stop TUI
    TuiPid = proplists:get_value(tui_pid, Config),
    catch gen_server:stop(TuiPid, normal, 1000),
    %% Unregister the named process if still registered
    catch unregister(bc_channel_tui),
    ok;
end_per_group(telegram_context, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% Per-testcase lifecycle
%% ---------------------------------------------------------------------------

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% TUI test cases
%% ---------------------------------------------------------------------------

tui_context_no_session(Config) ->
    GL = proplists:get_value(capture_gl, Config),
    TuiPid = proplists:get_value(tui_pid, Config),
    %% Clear any buffered output
    _ = get_captured(GL),
    %% Send /context as if user typed it
    TuiPid ! {line_result, <<"/context\n">>},
    timer:sleep(200),
    Output = get_captured(GL),
    %% Should say "No active session."
    ?assert(binary:match(Output, <<"No active session">>) =/= nomatch,
            {expected_no_session, Output}),
    ok.

tui_context_with_session(Config) ->
    GL = proplists:get_value(capture_gl, Config),
    %% Create a session matching what TUI would derive
    UserId = <<"ct-context-user">>,
    AgentId = bc_config:get(beamclaw_core, default_agent, <<"default">>),
    SessionId = bc_session_registry:derive_session_id(UserId, AgentId, tui),
    SessionConfig = #{
        session_id   => SessionId,
        user_id      => UserId,
        channel_id   => SessionId,
        channel_mod  => bc_channel_tui,
        agent_id     => AgentId,
        provider_mod => bc_provider_smoke_mock
    },
    {ok, _} = bc_sessions_sup:start_session(SessionConfig),
    %% Verify session exists
    {ok, _Pid} = bc_session_registry:lookup(SessionId),

    %% Clear buffer and send /context
    _ = get_captured(GL),
    TuiPid = proplists:get_value(tui_pid, Config),
    TuiPid ! {line_result, <<"/context\n">>},
    timer:sleep(300),
    Output = get_captured(GL),
    %% Should contain context grid output (model name, usage info)
    ?assert(binary:match(Output, <<"Context Usage">>) =/= nomatch,
            {expected_context_usage, Output}),
    ?assert(binary:match(Output, <<"tokens">>) =/= nomatch,
            {expected_tokens_info, Output}),
    ok.

tui_context_with_history(Config) ->
    GL = proplists:get_value(capture_gl, Config),
    UserId = <<"ct-context-user">>,
    AgentId = bc_config:get(beamclaw_core, default_agent, <<"default">>),
    SessionId = bc_session_registry:derive_session_id(UserId, AgentId, tui),
    {ok, Pid} = bc_session_registry:lookup(SessionId),

    %% Dispatch a user message to populate history
    ChannelMsg = #bc_channel_message{
        session_id = SessionId,
        user_id    = UserId,
        agent_id   = AgentId,
        channel    = tui,
        content    = <<"Hello from CT test">>,
        raw        = <<"Hello from CT test\n">>,
        ts         = erlang:system_time(millisecond)
    },
    bc_session:dispatch_run(Pid, ChannelMsg),
    %% Wait for the mock provider to respond
    ok = wait_for_history(Pid, fun(H) ->
        lists:any(fun(#bc_message{role = assistant}) -> true; (_) -> false end, H)
    end, 5000),

    HistoryBefore = bc_session:get_history(Pid),
    LenBefore = length(HistoryBefore),
    ?assert(LenBefore > 0, {expected_history, LenBefore}),

    %% Clear buffer and send /context
    _ = get_captured(GL),
    TuiPid = proplists:get_value(tui_pid, Config),
    TuiPid ! {line_result, <<"/context\n">>},
    timer:sleep(300),
    Output = get_captured(GL),

    %% Output should contain context info
    ?assert(binary:match(Output, <<"Context Usage">>) =/= nomatch,
            {expected_context_usage_with_history, Output}),

    %% History must be unchanged — /context is read-only
    HistoryAfter = bc_session:get_history(Pid),
    ?assertEqual(LenBefore, length(HistoryAfter)),
    ok.

%% ---------------------------------------------------------------------------
%% Telegram test cases
%% ---------------------------------------------------------------------------

telegram_context_no_session(Config) ->
    _Port = proplists:get_value(port, Config),
    %% Build a synthetic Telegram update with /context text
    Update = make_telegram_update(<<"/context">>),
    %% handle_webhook should return ok without crashing
    Result = bc_channel_telegram:handle_webhook(Update),
    ?assertEqual(ok, Result),
    %% No session should have been created by /context alone
    UserId = <<"ct-context-user">>,
    AgentId = bc_config:get(beamclaw_core, default_agent, <<"default">>),
    SessionId = bc_session_registry:derive_session_id(UserId, AgentId, telegram),
    ?assertEqual({error, not_found}, bc_session_registry:lookup(SessionId)),
    ok.

telegram_context_with_session(Config) ->
    _Port = proplists:get_value(port, Config),
    UserId = <<"ct-context-user">>,
    AgentId = bc_config:get(beamclaw_core, default_agent, <<"default">>),
    SessionId = bc_session_registry:derive_session_id(UserId, AgentId, telegram),

    %% Create session for Telegram channel
    SessionConfig = #{
        session_id   => SessionId,
        user_id      => UserId,
        channel_id   => <<"12345">>,
        channel_mod  => bc_channel_telegram,
        agent_id     => AgentId,
        provider_mod => bc_provider_smoke_mock
    },
    {ok, _} = bc_sessions_sup:start_session(SessionConfig),
    {ok, Pid} = bc_session_registry:lookup(SessionId),
    HistoryBefore = bc_session:get_history(Pid),

    %% Send /context via webhook
    Update = make_telegram_update(<<"/context">>),
    Result = bc_channel_telegram:handle_webhook(Update),
    ?assertEqual(ok, Result),

    %% History must be unchanged — /context is read-only
    timer:sleep(100),
    HistoryAfter = bc_session:get_history(Pid),
    ?assertEqual(length(HistoryBefore), length(HistoryAfter)),
    ok.

telegram_context_with_history(Config) ->
    _Port = proplists:get_value(port, Config),
    UserId = <<"ct-context-user">>,
    AgentId = bc_config:get(beamclaw_core, default_agent, <<"default">>),
    SessionId = bc_session_registry:derive_session_id(UserId, AgentId, telegram),
    {ok, Pid} = bc_session_registry:lookup(SessionId),

    %% Map session to chat ID for response routing
    ets:insert(bc_telegram_chat_map, {SessionId, <<"12345">>}),

    %% Dispatch a user message to populate history
    ChannelMsg = #bc_channel_message{
        session_id = SessionId,
        user_id    = UserId,
        agent_id   = AgentId,
        channel    = telegram,
        content    = <<"Hello from Telegram CT">>,
        raw        = #{},
        ts         = erlang:system_time(millisecond)
    },
    bc_session:dispatch_run(Pid, ChannelMsg),
    ok = wait_for_history(Pid, fun(H) ->
        lists:any(fun(#bc_message{role = assistant}) -> true; (_) -> false end, H)
    end, 5000),

    HistoryBefore = bc_session:get_history(Pid),
    LenBefore = length(HistoryBefore),
    ?assert(LenBefore > 0, {expected_telegram_history, LenBefore}),

    %% Send /context — hackney POST to fake token will fail gracefully
    Update = make_telegram_update(<<"/context">>),
    Result = bc_channel_telegram:handle_webhook(Update),
    ?assertEqual(ok, Result),

    %% History must be unchanged
    timer:sleep(100),
    HistoryAfter = bc_session:get_history(Pid),
    ?assertEqual(LenBefore, length(HistoryAfter)),
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

make_telegram_update(Text) ->
    #{<<"update_id">> => erlang:unique_integer([positive]),
      <<"message">> => #{
          <<"message_id">> => erlang:unique_integer([positive]),
          <<"text">> => Text,
          <<"from">> => #{<<"id">> => 99999,
                          <<"is_bot">> => false,
                          <<"first_name">> => <<"CT Test">>},
          <<"chat">> => #{<<"id">> => 12345,
                          <<"type">> => <<"private">>},
          <<"date">> => erlang:system_time(second)
      }}.

%% Group leader capture process — implements OTP IO protocol
spawn_capture_gl() ->
    spawn(fun() -> capture_gl_loop(<<>>) end).

capture_gl_loop(Acc) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _Enc, Mod, Func, Args}} ->
            %% io:format sends {put_chars, unicode, io_lib, format, [Fmt, FmtArgs]}
            Chars = apply(Mod, Func, Args),
            Bin = iolist_to_binary(Chars),
            From ! {io_reply, ReplyAs, ok},
            capture_gl_loop(<<Acc/binary, Bin/binary>>);
        {io_request, From, ReplyAs, {put_chars, _Enc, Chars}} ->
            Bin = iolist_to_binary(Chars),
            From ! {io_reply, ReplyAs, ok},
            capture_gl_loop(<<Acc/binary, Bin/binary>>);
        {io_request, From, ReplyAs, {put_chars, Chars}} ->
            Bin = iolist_to_binary(Chars),
            From ! {io_reply, ReplyAs, ok},
            capture_gl_loop(<<Acc/binary, Bin/binary>>);
        {io_request, From, ReplyAs, {get_line, _Prompt}} ->
            %% Return error so TUI enters dormant mode (no busy-loop).
            From ! {io_reply, ReplyAs, {error, enotsup}},
            capture_gl_loop(Acc);
        {io_request, From, ReplyAs, {get_line, _Enc, _Prompt}} ->
            %% io:get_line/1 sends {get_line, unicode, Prompt}
            From ! {io_reply, ReplyAs, {error, enotsup}},
            capture_gl_loop(Acc);
        {io_request, From, ReplyAs, _Other} ->
            From ! {io_reply, ReplyAs, ok},
            capture_gl_loop(Acc);
        {get_captured, From} ->
            From ! {captured, Acc},
            capture_gl_loop(<<>>);
        stop ->
            ok
    end.

get_captured(GL) ->
    GL ! {get_captured, self()},
    receive
        {captured, Data} -> Data
    after 2000 ->
        <<>>
    end.

wait_for_history(Pid, Predicate, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_history_loop(Pid, Predicate, Deadline).

wait_for_history_loop(Pid, Predicate, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= Deadline of
        true ->
            {error, timeout};
        false ->
            History = bc_session:get_history(Pid),
            case Predicate(History) of
                true -> ok;
                false ->
                    timer:sleep(50),
                    wait_for_history_loop(Pid, Predicate, Deadline)
            end
    end.
