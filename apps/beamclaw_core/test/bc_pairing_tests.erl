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

-module(bc_pairing_tests).
-moduledoc "Tests for bc_pairing — channel access control via pairing codes.".

-include_lib("eunit/include/eunit.hrl").

%% ---- Fixtures ----

setup() ->
    %% Use a temp dir to avoid polluting real pairing state
    TmpDir = filename:join("/tmp", "bc_pairing_test_" ++
                           integer_to_list(erlang:unique_integer([positive]))),
    os:putenv("BEAMCLAW_HOME", TmpDir),
    %% Set default_agent for tests
    application:set_env(beamclaw_core, default_agent, <<"default">>),
    TmpDir.

teardown(TmpDir) ->
    os:unsetenv("BEAMCLAW_HOME"),
    %% Clean up temp files
    os:cmd("rm -rf " ++ TmpDir),
    ok.

%% ---- Test suite ----

pairing_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun empty_allowed_list/1,
        fun request_creates_code/1,
        fun idempotent_request/1,
        fun approve_moves_to_allowed/1,
        fun approve_not_found/1,
        fun revoke_removes_user/1,
        fun revoke_not_found/1,
        fun code_format_valid/1,
        fun max_pending_evicts_oldest/1,
        fun list_pending_prunes_expired/1,
        fun approve_expired_code/1,
        fun is_allowed_after_approve/1,
        %% v2 agent_id tests
        fun approve_default_agent/1,
        fun approve_with_agent/1,
        fun get_agent_id_found/1,
        fun get_agent_id_not_found/1,
        fun v1_migration/1,
        fun list_allowed_returns_maps/1,
        fun revoke_v2_format/1,
        fun is_allowed_v2_format/1
    ]}.

%% ---- Tests ----

empty_allowed_list(_TmpDir) ->
    fun() ->
        ?assertEqual(false, bc_pairing:is_allowed(telegram, <<"12345">>)),
        ?assertEqual([], bc_pairing:list_allowed(telegram)),
        ?assertEqual([], bc_pairing:list_pending(telegram))
    end.

request_creates_code(_TmpDir) ->
    fun() ->
        {ok, Code, created} = bc_pairing:request_pairing(
            telegram, <<"12345">>, #{<<"username">> => <<"alice">>}),
        ?assertEqual(8, byte_size(Code)),
        Pending = bc_pairing:list_pending(telegram),
        ?assertEqual(1, length(Pending)),
        ?assertEqual(<<"12345">>, maps:get(<<"id">>, hd(Pending)))
    end.

idempotent_request(_TmpDir) ->
    fun() ->
        {ok, Code1, created} = bc_pairing:request_pairing(
            telegram, <<"12345">>, #{<<"username">> => <<"alice">>}),
        {ok, Code2, existing} = bc_pairing:request_pairing(
            telegram, <<"12345">>, #{<<"username">> => <<"alice">>}),
        ?assertEqual(Code1, Code2),
        ?assertEqual(1, length(bc_pairing:list_pending(telegram)))
    end.

approve_moves_to_allowed(_TmpDir) ->
    fun() ->
        {ok, Code, created} = bc_pairing:request_pairing(
            telegram, <<"12345">>, #{}),
        {ok, <<"12345">>} = bc_pairing:approve(telegram, Code),
        ?assertEqual([], bc_pairing:list_pending(telegram)),
        Allowed = bc_pairing:list_allowed(telegram),
        ?assertEqual(1, length(Allowed)),
        ?assertEqual(<<"12345">>, maps:get(<<"id">>, hd(Allowed))),
        ?assertEqual(true, bc_pairing:is_allowed(telegram, <<"12345">>))
    end.

approve_not_found(_TmpDir) ->
    fun() ->
        ?assertEqual({error, not_found}, bc_pairing:approve(telegram, <<"INVALID1">>))
    end.

revoke_removes_user(_TmpDir) ->
    fun() ->
        {ok, Code, created} = bc_pairing:request_pairing(
            telegram, <<"12345">>, #{}),
        {ok, <<"12345">>} = bc_pairing:approve(telegram, Code),
        ?assertEqual(true, bc_pairing:is_allowed(telegram, <<"12345">>)),
        ok = bc_pairing:revoke(telegram, <<"12345">>),
        ?assertEqual(false, bc_pairing:is_allowed(telegram, <<"12345">>)),
        ?assertEqual([], bc_pairing:list_allowed(telegram))
    end.

revoke_not_found(_TmpDir) ->
    fun() ->
        ?assertEqual({error, not_found}, bc_pairing:revoke(telegram, <<"99999">>))
    end.

code_format_valid(_TmpDir) ->
    fun() ->
        {ok, Code, created} = bc_pairing:request_pairing(
            telegram, <<"12345">>, #{}),
        ?assertEqual(8, byte_size(Code)),
        %% All chars should be in the valid alphabet (no 0/O/1/I)
        ValidChars = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789",
        lists:foreach(fun(C) ->
            ?assert(lists:member(C, ValidChars))
        end, binary_to_list(Code))
    end.

max_pending_evicts_oldest(_TmpDir) ->
    fun() ->
        {ok, _C1, created} = bc_pairing:request_pairing(
            telegram, <<"user1">>, #{}),
        {ok, _C2, created} = bc_pairing:request_pairing(
            telegram, <<"user2">>, #{}),
        {ok, _C3, created} = bc_pairing:request_pairing(
            telegram, <<"user3">>, #{}),
        ?assertEqual(3, length(bc_pairing:list_pending(telegram))),
        %% 4th request should evict the oldest (user1)
        {ok, _C4, created} = bc_pairing:request_pairing(
            telegram, <<"user4">>, #{}),
        Pending = bc_pairing:list_pending(telegram),
        ?assertEqual(3, length(Pending)),
        Ids = [maps:get(<<"id">>, R) || R <- Pending],
        ?assertNot(lists:member(<<"user1">>, Ids)),
        ?assert(lists:member(<<"user4">>, Ids))
    end.

list_pending_prunes_expired(_TmpDir) ->
    fun() ->
        %% Manually write an expired entry
        Dir = bc_pairing:pairing_dir(),
        ok = filelib:ensure_dir(filename:join(Dir, "x")),
        Expired = #{<<"id">> => <<"old">>,
                    <<"code">> => <<"AAAAAAAA">>,
                    <<"created_at">> => 0,  %% epoch — always expired
                    <<"meta">> => #{}},
        Data = jsx:encode(#{<<"version">> => 1, <<"requests">> => [Expired]}),
        ok = file:write_file(filename:join(Dir, "telegram-pending.json"), Data),
        %% list_pending should return empty (expired pruned)
        ?assertEqual([], bc_pairing:list_pending(telegram))
    end.

approve_expired_code(_TmpDir) ->
    fun() ->
        %% Manually write an expired entry with a known code
        Dir = bc_pairing:pairing_dir(),
        ok = filelib:ensure_dir(filename:join(Dir, "x")),
        Expired = #{<<"id">> => <<"old">>,
                    <<"code">> => <<"BBBBBBBB">>,
                    <<"created_at">> => 0,
                    <<"meta">> => #{}},
        Data = jsx:encode(#{<<"version">> => 1, <<"requests">> => [Expired]}),
        ok = file:write_file(filename:join(Dir, "telegram-pending.json"), Data),
        ?assertEqual({error, expired}, bc_pairing:approve(telegram, <<"BBBBBBBB">>))
    end.

is_allowed_after_approve(_TmpDir) ->
    fun() ->
        ?assertEqual(false, bc_pairing:is_allowed(telegram, <<"99">>)),
        {ok, Code, created} = bc_pairing:request_pairing(
            telegram, <<"99">>, #{}),
        ?assertEqual(false, bc_pairing:is_allowed(telegram, <<"99">>)),
        {ok, <<"99">>} = bc_pairing:approve(telegram, Code),
        ?assertEqual(true, bc_pairing:is_allowed(telegram, <<"99">>))
    end.

%% ---- v2 agent_id tests ----

approve_default_agent(_TmpDir) ->
    fun() ->
        {ok, Code, created} = bc_pairing:request_pairing(
            telegram, <<"u1">>, #{}),
        {ok, <<"u1">>} = bc_pairing:approve(telegram, Code),
        [Entry] = bc_pairing:list_allowed(telegram),
        ?assertEqual(<<"u1">>, maps:get(<<"id">>, Entry)),
        ?assertEqual(<<"default">>, maps:get(<<"agent_id">>, Entry))
    end.

approve_with_agent(_TmpDir) ->
    fun() ->
        {ok, Code, created} = bc_pairing:request_pairing(
            telegram, <<"u2">>, #{}),
        {ok, <<"u2">>} = bc_pairing:approve(telegram, Code, <<"mom">>),
        [Entry] = bc_pairing:list_allowed(telegram),
        ?assertEqual(<<"u2">>, maps:get(<<"id">>, Entry)),
        ?assertEqual(<<"mom">>, maps:get(<<"agent_id">>, Entry))
    end.

get_agent_id_found(_TmpDir) ->
    fun() ->
        {ok, Code, created} = bc_pairing:request_pairing(
            telegram, <<"u3">>, #{}),
        {ok, <<"u3">>} = bc_pairing:approve(telegram, Code, <<"helper">>),
        ?assertEqual({ok, <<"helper">>},
                     bc_pairing:get_agent_id(telegram, <<"u3">>))
    end.

get_agent_id_not_found(_TmpDir) ->
    fun() ->
        ?assertEqual({error, not_found},
                     bc_pairing:get_agent_id(telegram, <<"nonexistent">>))
    end.

v1_migration(_TmpDir) ->
    fun() ->
        %% Manually write v1 format (bare binary IDs)
        Dir = bc_pairing:pairing_dir(),
        ok = filelib:ensure_dir(filename:join(Dir, "x")),
        V1Data = jsx:encode(#{<<"version">> => 1,
                              <<"allowed">> => [<<"alice">>, <<"bob">>]}),
        ok = file:write_file(filename:join(Dir, "telegram-allowed.json"), V1Data),
        %% read_allowed should auto-migrate to v2 maps
        Allowed = bc_pairing:list_allowed(telegram),
        ?assertEqual(2, length(Allowed)),
        [A, B] = Allowed,
        ?assertEqual(<<"alice">>, maps:get(<<"id">>, A)),
        ?assertEqual(<<"default">>, maps:get(<<"agent_id">>, A)),
        ?assertEqual(<<"bob">>, maps:get(<<"id">>, B)),
        ?assertEqual(<<"default">>, maps:get(<<"agent_id">>, B)),
        %% is_allowed should work with migrated data
        ?assert(bc_pairing:is_allowed(telegram, <<"alice">>)),
        ?assert(bc_pairing:is_allowed(telegram, <<"bob">>)),
        ?assertNot(bc_pairing:is_allowed(telegram, <<"charlie">>))
    end.

list_allowed_returns_maps(_TmpDir) ->
    fun() ->
        {ok, Code1, created} = bc_pairing:request_pairing(
            telegram, <<"m1">>, #{}),
        {ok, <<"m1">>} = bc_pairing:approve(telegram, Code1, <<"agent_a">>),
        {ok, Code2, created} = bc_pairing:request_pairing(
            telegram, <<"m2">>, #{}),
        {ok, <<"m2">>} = bc_pairing:approve(telegram, Code2),
        Allowed = bc_pairing:list_allowed(telegram),
        ?assertEqual(2, length(Allowed)),
        %% All entries should be maps with id and agent_id
        lists:foreach(fun(Entry) ->
            ?assert(is_map(Entry)),
            ?assert(maps:is_key(<<"id">>, Entry)),
            ?assert(maps:is_key(<<"agent_id">>, Entry))
        end, Allowed)
    end.

revoke_v2_format(_TmpDir) ->
    fun() ->
        {ok, Code1, created} = bc_pairing:request_pairing(
            telegram, <<"r1">>, #{}),
        {ok, <<"r1">>} = bc_pairing:approve(telegram, Code1, <<"agent_x">>),
        {ok, Code2, created} = bc_pairing:request_pairing(
            telegram, <<"r2">>, #{}),
        {ok, <<"r2">>} = bc_pairing:approve(telegram, Code2, <<"agent_y">>),
        ?assertEqual(2, length(bc_pairing:list_allowed(telegram))),
        ok = bc_pairing:revoke(telegram, <<"r1">>),
        Remaining = bc_pairing:list_allowed(telegram),
        ?assertEqual(1, length(Remaining)),
        ?assertEqual(<<"r2">>, maps:get(<<"id">>, hd(Remaining)))
    end.

is_allowed_v2_format(_TmpDir) ->
    fun() ->
        {ok, Code, created} = bc_pairing:request_pairing(
            telegram, <<"v2user">>, #{}),
        {ok, <<"v2user">>} = bc_pairing:approve(telegram, Code, <<"custom">>),
        ?assert(bc_pairing:is_allowed(telegram, <<"v2user">>)),
        ?assertNot(bc_pairing:is_allowed(telegram, <<"nobody">>))
    end.
