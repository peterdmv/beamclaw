%% @doc Tests for bc_session_store — Mnesia-backed session persistence.
-module(bc_session_store_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").
-include_lib("beamclaw_core/include/bc_session_store.hrl").

%% ---- Fixtures ----

setup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    ok = bc_session_store:init_table(),
    ok.

teardown(_) ->
    mnesia:clear_table(bc_session_stored),
    ok.

%% ---- Test suite ----

session_store_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun save_and_load_roundtrip/0,
        fun load_not_found/0,
        fun save_updates_existing/0,
        fun delete_session/0,
        fun delete_expired/0,
        fun history_serialization_preserves_records/0
    ]}.

save_and_load_roundtrip() ->
    SessionId = <<"test-session-1">>,
    History = [#bc_message{id = <<"m1">>, role = user, content = <<"hello">>, ts = 1000}],
    ok = bc_session_store:save(SessionId, #{
        user_id  => <<"alice">>,
        agent_id => <<"default">>,
        autonomy => supervised,
        history  => History,
        config   => #{foo => bar}
    }),
    {ok, Stored} = bc_session_store:load(SessionId),
    ?assertEqual(SessionId, Stored#bc_session_stored.session_id),
    ?assertEqual(<<"alice">>, Stored#bc_session_stored.user_id),
    ?assertEqual(<<"default">>, Stored#bc_session_stored.agent_id),
    ?assertEqual(supervised, Stored#bc_session_stored.autonomy),
    %% History should be deserialized back to original records
    [Msg] = Stored#bc_session_stored.history,
    ?assertEqual(<<"m1">>, Msg#bc_message.id),
    ?assertEqual(user, Msg#bc_message.role),
    ?assertEqual(<<"hello">>, Msg#bc_message.content).

load_not_found() ->
    ?assertEqual({error, not_found}, bc_session_store:load(<<"nonexistent">>)).

save_updates_existing() ->
    SessionId = <<"test-session-2">>,
    H1 = [#bc_message{id = <<"m1">>, role = user, content = <<"first">>, ts = 1}],
    ok = bc_session_store:save(SessionId, #{
        user_id  => <<"bob">>,
        agent_id => <<"default">>,
        history  => H1,
        config   => #{}
    }),
    H2 = H1 ++ [#bc_message{id = <<"m2">>, role = assistant, content = <<"reply">>, ts = 2}],
    ok = bc_session_store:save(SessionId, #{history => H2, config => #{}}),
    {ok, Stored} = bc_session_store:load(SessionId),
    ?assertEqual(2, length(Stored#bc_session_stored.history)),
    %% user_id should be preserved from first save
    ?assertEqual(<<"bob">>, Stored#bc_session_stored.user_id).

delete_session() ->
    SessionId = <<"test-session-3">>,
    ok = bc_session_store:save(SessionId, #{history => [], config => #{}}),
    {ok, _} = bc_session_store:load(SessionId),
    ok = bc_session_store:delete(SessionId),
    ?assertEqual({error, not_found}, bc_session_store:load(SessionId)).

delete_expired() ->
    %% Create a session with an old timestamp by writing directly to Mnesia
    SessionId = <<"test-session-4">>,
    OldTime = erlang:system_time(second) - 7200,  %% 2 hours ago
    Entry = #bc_session_stored{
        session_id = SessionId,
        user_id    = <<"old_user">>,
        agent_id   = <<"default">>,
        autonomy   = supervised,
        history    = term_to_binary({1, []}),
        created_at = OldTime,
        updated_at = OldTime,
        config     = #{}
    },
    ok = mnesia:dirty_write(bc_session_stored, Entry),
    %% Also create a recent session
    RecentId = <<"test-session-5">>,
    ok = bc_session_store:save(RecentId, #{history => [], config => #{}}),
    %% Delete sessions older than 1 hour
    Count = bc_session_store:delete_expired(3600),
    ?assertEqual(1, Count),
    ?assertEqual({error, not_found}, bc_session_store:load(SessionId)),
    {ok, _} = bc_session_store:load(RecentId).

history_serialization_preserves_records() ->
    SessionId = <<"test-session-6">>,
    History = [
        #bc_message{id = <<"m1">>, role = system, content = <<"system prompt">>,
                    tool_calls = [], ts = 100},
        #bc_message{id = <<"m2">>, role = user, content = <<"hello">>, ts = 200},
        #bc_message{id = <<"m3">>, role = assistant, content = <<"hi there">>,
                    tool_calls = [{name, <<"test">>}], ts = 300},
        #bc_message{id = <<"m4">>, role = tool, content = <<"result">>,
                    tool_call_id = <<"tc1">>, name = <<"test">>, ts = 400}
    ],
    ok = bc_session_store:save(SessionId, #{history => History, config => #{}}),
    {ok, Stored} = bc_session_store:load(SessionId),
    Restored = Stored#bc_session_stored.history,
    ?assertEqual(4, length(Restored)),
    [M1, M2, M3, M4] = Restored,
    ?assertEqual(system, M1#bc_message.role),
    ?assertEqual(user, M2#bc_message.role),
    ?assertEqual(assistant, M3#bc_message.role),
    ?assertEqual([{name, <<"test">>}], M3#bc_message.tool_calls),
    ?assertEqual(tool, M4#bc_message.role),
    ?assertEqual(<<"tc1">>, M4#bc_message.tool_call_id).
