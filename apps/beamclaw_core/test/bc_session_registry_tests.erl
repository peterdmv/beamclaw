%% @doc Tests for bc_session_registry — derive_session_id/2,3 + isolation modes.
-module(bc_session_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- Fixtures ----

setup() ->
    application:set_env(beamclaw_core, session_sharing, shared, [{persistent, true}]),
    ok.

teardown(_) ->
    ok.

%% ---- Test suite ----

session_registry_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun deterministic/0,
        fun same_user_agent_same_id/0,
        fun different_user_different_id/0,
        fun different_agent_different_id/0,
        fun session_id_format/0,
        fun shared_mode_ignores_channel/0,
        fun per_channel_mode_differs/0
    ]}.

deterministic() ->
    Id1 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>),
    Id2 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>),
    ?assertEqual(Id1, Id2).

same_user_agent_same_id() ->
    Id1 = bc_session_registry:derive_session_id(<<"local:bob">>, <<"mybot">>),
    Id2 = bc_session_registry:derive_session_id(<<"local:bob">>, <<"mybot">>),
    ?assertEqual(Id1, Id2).

different_user_different_id() ->
    IdA = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>),
    IdB = bc_session_registry:derive_session_id(<<"bob">>,   <<"default">>),
    ?assertNotEqual(IdA, IdB).

different_agent_different_id() ->
    IdA = bc_session_registry:derive_session_id(<<"alice">>, <<"agent1">>),
    IdB = bc_session_registry:derive_session_id(<<"alice">>, <<"agent2">>),
    ?assertNotEqual(IdA, IdB).

session_id_format() ->
    Id = bc_session_registry:derive_session_id(<<"test">>, <<"default">>),
    ?assert(is_binary(Id)),
    ?assertMatch(<<"session-", _/binary>>, Id),
    %% 16 bytes hex = 32 chars
    <<"session-", Hex/binary>> = Id,
    ?assertEqual(32, byte_size(Hex)).

shared_mode_ignores_channel() ->
    application:set_env(beamclaw_core, session_sharing, shared, [{persistent, true}]),
    Id1 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>, tui),
    Id2 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>, telegram),
    Id3 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>, http),
    ?assertEqual(Id1, Id2),
    ?assertEqual(Id2, Id3).

per_channel_mode_differs() ->
    application:set_env(beamclaw_core, session_sharing, per_channel, [{persistent, true}]),
    Id1 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>, tui),
    Id2 = bc_session_registry:derive_session_id(<<"alice">>, <<"default">>, telegram),
    ?assertNotEqual(Id1, Id2),
    %% Reset to shared for other tests
    application:set_env(beamclaw_core, session_sharing, shared, [{persistent, true}]).
