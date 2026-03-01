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

-module(bc_compactor_tests).
-moduledoc """
EUnit tests for bc_compactor context compaction.

These tests cover the no-op path (history tokens within target) and the
split_by_tokens/2 function. The LLM-failure fallback path requires a live
HTTP endpoint; it is covered by integration/smoke tests rather than unit tests.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% ---- Mock session process ----
%%
%% Responds to the gen_server wire protocol used by bc_session:get_history/1
%% (gen_server:call) and bc_session:set_history/2 (gen_server:cast).

mock_session(History) ->
    spawn(fun() -> mock_session_loop(History) end).

mock_session_loop(History) ->
    receive
        {'$gen_call', From, get_history} ->
            gen_server:reply(From, History),
            mock_session_loop(History);
        {'$gen_call', From, get_provider_mod} ->
            gen_server:reply(From, bc_provider_openrouter),
            mock_session_loop(History);
        {'$gen_cast', {set_history, NewHistory}} ->
            mock_session_loop(NewHistory);
        _ ->
            mock_session_loop(History)
    after 5000 ->
        ok
    end.

make_msg(N) ->
    #bc_message{
        id      = integer_to_binary(N),
        role    = user,
        content = iolist_to_binary(["msg ", integer_to_list(N)]),
        ts      = N
    }.

%% Make a message with a specific content size (~tokens = byte_size div 4).
make_msg_with_size(N, SizeBytes) ->
    Padding = binary:copy(<<"x">>, SizeBytes),
    #bc_message{
        id      = integer_to_binary(N),
        role    = user,
        content = Padding,
        ts      = N
    }.

%% ---- Compact no-op tests ----

%% History tokens within target → compact is a no-op.
compact_small_history_test() ->
    %% Use a model with 200k window; 40% target = 80k tokens.
    %% 5 small messages << 80k tokens.
    application:set_env(beamclaw_core, default_provider, openrouter),
    application:set_env(beamclaw_core, providers,
                        [{openrouter, #{model => "anthropic/claude-sonnet-4-5"}}]),
    application:set_env(beamclaw_core, agentic_loop,
                        #{compaction_target_pct => 40}),
    History = [make_msg(I) || I <- lists:seq(1, 5)],
    Pid     = mock_session(History),
    Result  = bc_compactor:compact(Pid),
    exit(Pid, kill),
    ?assertEqual(ok, Result).

%% Very short history (2 messages) → always a no-op regardless of token count.
compact_minimal_history_test() ->
    application:set_env(beamclaw_core, default_provider, openrouter),
    application:set_env(beamclaw_core, providers,
                        [{openrouter, #{model => "anthropic/claude-sonnet-4-5"}}]),
    application:set_env(beamclaw_core, agentic_loop,
                        #{compaction_target_pct => 40}),
    History = [make_msg(1), make_msg(2)],
    Pid     = mock_session(History),
    Result  = bc_compactor:compact(Pid),
    exit(Pid, kill),
    ?assertEqual(ok, Result).

%% With no agentic_loop config, defaults are used — still a no-op for small history.
compact_default_config_test() ->
    application:set_env(beamclaw_core, default_provider, openrouter),
    application:set_env(beamclaw_core, providers,
                        [{openrouter, #{model => "anthropic/claude-sonnet-4-5"}}]),
    application:unset_env(beamclaw_core, agentic_loop),
    History = [make_msg(I) || I <- lists:seq(1, 3)],
    Pid     = mock_session(History),
    Result  = bc_compactor:compact(Pid),
    exit(Pid, kill),
    ?assertEqual(ok, Result).

%% ---- split_by_tokens/2 tests ----

%% All messages fit within budget → OldMsgs is empty.
split_all_fit_test() ->
    Msgs = [make_msg(I) || I <- lists:seq(1, 5)],
    %% Each msg is ~6 bytes ("msg N") = ~1 token. Budget = 100 tokens.
    {Old, Keep} = bc_compactor:split_by_tokens(Msgs, 100),
    ?assertEqual([], Old),
    ?assertEqual(5, length(Keep)),
    ?assertEqual(Msgs, Keep).

%% Budget fits only the last message → first 4 are old.
split_keeps_at_least_one_test() ->
    %% 5 messages, each ~1000 bytes = ~250 tokens. Budget = 300 tokens.
    Msgs = [make_msg_with_size(I, 1000) || I <- lists:seq(1, 5)],
    {Old, Keep} = bc_compactor:split_by_tokens(Msgs, 300),
    ?assertEqual(4, length(Old)),
    ?assertEqual(1, length(Keep)),
    %% Keep should be the last message
    ?assertEqual(<<"5">>, (hd(Keep))#bc_message.id).

%% Budget = 0 → keeps at least one message (the most recent).
split_zero_budget_test() ->
    Msgs = [make_msg(I) || I <- lists:seq(1, 3)],
    {Old, Keep} = bc_compactor:split_by_tokens(Msgs, 0),
    ?assertEqual(2, length(Old)),
    ?assertEqual(1, length(Keep)),
    ?assertEqual(<<"3">>, (hd(Keep))#bc_message.id).

%% Empty history → both empty.
split_empty_history_test() ->
    {Old, Keep} = bc_compactor:split_by_tokens([], 100),
    ?assertEqual([], Old),
    ?assertEqual([], Keep).

%% Partial fit: budget fits last 2 of 5 messages.
split_partial_fit_test() ->
    %% 5 messages: first 3 are 2000 bytes (~500 tokens), last 2 are 400 bytes (~100 tokens).
    Big  = [make_msg_with_size(I, 2000) || I <- lists:seq(1, 3)],
    Small = [make_msg_with_size(I, 400) || I <- lists:seq(4, 5)],
    Msgs = Big ++ Small,
    %% Budget = 250 tokens → fits last 2 (100+100 = 200 < 250)
    {Old, Keep} = bc_compactor:split_by_tokens(Msgs, 250),
    ?assertEqual(3, length(Old)),
    ?assertEqual(2, length(Keep)),
    %% Keep should be messages 4 and 5
    ?assertEqual(<<"4">>, (hd(Keep))#bc_message.id).

%% Single message → always in Keep.
split_single_message_test() ->
    Msgs = [make_msg(1)],
    {Old, Keep} = bc_compactor:split_by_tokens(Msgs, 100),
    ?assertEqual([], Old),
    ?assertEqual(1, length(Keep)).

%% Preserves message order.
split_preserves_order_test() ->
    Msgs = [make_msg(I) || I <- lists:seq(1, 10)],
    {Old, Keep} = bc_compactor:split_by_tokens(Msgs, 20),
    %% All messages together are small enough to fit, so all should be kept
    OldIds = [M#bc_message.id || M <- Old],
    KeepIds = [M#bc_message.id || M <- Keep],
    %% Recombined should equal original
    ?assertEqual([M#bc_message.id || M <- Msgs], OldIds ++ KeepIds).
