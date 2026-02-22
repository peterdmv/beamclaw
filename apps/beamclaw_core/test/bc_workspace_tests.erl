%% @doc EUnit tests for bc_workspace agent workspace management.
-module(bc_workspace_tests).

-include_lib("eunit/include/eunit.hrl").

%% Use a temp directory so tests don't touch ~/.beamclaw.
-define(setup(F), {setup, fun setup/0, fun cleanup/1, fun F/1}).

setup() ->
    TmpDir = filename:join("/tmp", "beamclaw_ws_test_" ++
                            integer_to_list(erlang:unique_integer([positive]))),
    os:putenv("BEAMCLAW_HOME", TmpDir),
    TmpDir.

cleanup(TmpDir) ->
    os:unsetenv("BEAMCLAW_HOME"),
    os:cmd("rm -rf " ++ TmpDir).

%% ---- validate_agent_id ----

validate_valid_test() ->
    ?assertEqual(ok, bc_workspace:validate_agent_id(<<"default">>)).

validate_with_hyphens_test() ->
    ?assertEqual(ok, bc_workspace:validate_agent_id(<<"my-agent">>)).

validate_with_underscores_test() ->
    ?assertEqual(ok, bc_workspace:validate_agent_id(<<"my_agent_1">>)).

validate_empty_test() ->
    ?assertEqual({error, invalid_agent_id}, bc_workspace:validate_agent_id(<<>>)).

validate_uppercase_test() ->
    ?assertEqual({error, invalid_agent_id}, bc_workspace:validate_agent_id(<<"MyAgent">>)).

validate_spaces_test() ->
    ?assertEqual({error, invalid_agent_id}, bc_workspace:validate_agent_id(<<"my agent">>)).

validate_path_traversal_test() ->
    ?assertEqual({error, invalid_agent_id}, bc_workspace:validate_agent_id(<<"../evil">>)).

validate_non_binary_test() ->
    ?assertEqual({error, invalid_agent_id}, bc_workspace:validate_agent_id(42)).

%% ---- create_agent ----

create_agent_test_() -> ?setup(create_agent_t).
create_agent_t(_TmpDir) ->
    [?_assertEqual(ok, bc_workspace:create_agent(<<"test-agent">>)),
     ?_assert(bc_workspace:agent_exists(<<"test-agent">>)),
     %% All 6 files should exist
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"SOUL.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"IDENTITY.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"USER.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"TOOLS.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"MEMORY.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"AGENTS.md">>))].

create_duplicate_test_() -> ?setup(create_duplicate_t).
create_duplicate_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"dup-agent">>),
    [?_assertEqual({error, exists}, bc_workspace:create_agent(<<"dup-agent">>))].

create_invalid_test() ->
    ?assertEqual({error, invalid_agent_id}, bc_workspace:create_agent(<<"Bad Name">>)).

%% ---- delete_agent ----

delete_agent_test_() -> ?setup(delete_agent_t).
delete_agent_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"del-agent">>),
    [?_assertEqual(ok, bc_workspace:delete_agent(<<"del-agent">>)),
     ?_assertNot(bc_workspace:agent_exists(<<"del-agent">>))].

delete_default_test() ->
    ?assertEqual({error, cannot_delete_default}, bc_workspace:delete_agent(<<"default">>)).

delete_nonexistent_test_() -> ?setup(delete_nonexistent_t).
delete_nonexistent_t(_TmpDir) ->
    [?_assertEqual({error, not_found}, bc_workspace:delete_agent(<<"ghost">>))].

%% ---- list_agents ----

list_agents_test_() -> ?setup(list_agents_t).
list_agents_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"alpha">>),
    ok = bc_workspace:create_agent(<<"beta">>),
    [?_assertEqual([<<"alpha">>, <<"beta">>], bc_workspace:list_agents())].

%% ---- ensure_default_agent ----

ensure_default_test_() -> ?setup(ensure_default_t).
ensure_default_t(_TmpDir) ->
    ok = bc_workspace:ensure_default_agent(),
    [?_assert(bc_workspace:agent_exists(<<"default">>)),
     %% Idempotent — second call succeeds
     ?_assertEqual(ok, bc_workspace:ensure_default_agent())].

%% ---- read truncation ----

truncation_test_() -> ?setup(truncation_t).
truncation_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"trunc-agent">>),
    %% Write a file larger than 20KB
    BigContent = binary:copy(<<"x">>, 25000),
    ok = bc_workspace:write_bootstrap_file(<<"trunc-agent">>, <<"SOUL.md">>, BigContent),
    {ok, Read} = bc_workspace:read_bootstrap_file(<<"trunc-agent">>, <<"SOUL.md">>),
    [?_assert(byte_size(Read) < 25000),
     ?_assert(binary:match(Read, <<"[...truncated at 20KB...]">>) =/= nomatch)].
