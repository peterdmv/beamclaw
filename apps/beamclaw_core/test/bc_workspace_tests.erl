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
create_agent_t(TmpDir) ->
    [?_assertEqual(ok, bc_workspace:create_agent(<<"test-agent">>)),
     ?_assert(bc_workspace:agent_exists(<<"test-agent">>)),
     %% All 7 files should exist
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"SOUL.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"IDENTITY.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"USER.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"TOOLS.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"MEMORY.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"AGENTS.md">>)),
     ?_assertMatch({ok, _}, bc_workspace:read_bootstrap_file(<<"test-agent">>, <<"BOOTSTRAP.md">>)),
     %% memory/ subdirectory should exist
     ?_assert(filelib:is_dir(filename:join([TmpDir, "agents", "test-agent", "memory"])))].

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

%% ---- daily logs ----

read_daily_log_test_() -> ?setup(read_daily_log_t).
read_daily_log_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"log-agent">>),
    Dir = filename:join([bc_workspace:agent_dir(<<"log-agent">>), "memory"]),
    ok = file:write_file(filename:join(Dir, "2026-02-22.md"),
                         <<"## Notes\nDiscussed architecture.">>),
    {ok, Content} = bc_workspace:read_daily_log(<<"log-agent">>, <<"2026-02-22">>),
    [?_assert(binary:match(Content, <<"Discussed architecture">>) =/= nomatch)].

read_daily_log_missing_test_() -> ?setup(read_daily_log_missing_t).
read_daily_log_missing_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"log-agent2">>),
    [?_assertEqual({error, not_found},
                   bc_workspace:read_daily_log(<<"log-agent2">>, <<"2026-01-01">>))].

list_daily_logs_test_() -> ?setup(list_daily_logs_t).
list_daily_logs_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"log-agent3">>),
    Dir = filename:join([bc_workspace:agent_dir(<<"log-agent3">>), "memory"]),
    ok = file:write_file(filename:join(Dir, "2026-02-20.md"), <<"day 1">>),
    ok = file:write_file(filename:join(Dir, "2026-02-21.md"), <<"day 2">>),
    ok = file:write_file(filename:join(Dir, "2026-02-22.md"), <<"day 3">>),
    Logs = bc_workspace:list_daily_logs(<<"log-agent3">>),
    %% Newest first
    [?_assertEqual([<<"2026-02-22.md">>, <<"2026-02-21.md">>, <<"2026-02-20.md">>], Logs)].

list_daily_logs_empty_test_() -> ?setup(list_daily_logs_empty_t).
list_daily_logs_empty_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"log-agent4">>),
    [?_assertEqual([], bc_workspace:list_daily_logs(<<"log-agent4">>))].

%% ---- rehatch_agent ----

rehatch_agent_test_() -> ?setup(rehatch_t).
rehatch_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"rehatch-agent">>),
    %% Modify IDENTITY.md and write a daily log
    ok = bc_workspace:write_bootstrap_file(<<"rehatch-agent">>,
                                           <<"IDENTITY.md">>, <<"custom identity">>),
    Dir = bc_workspace:agent_dir(<<"rehatch-agent">>),
    MemDir = filename:join(Dir, "memory"),
    ok = file:write_file(filename:join(MemDir, "2026-02-22.md"), <<"daily log">>),
    %% Rehatch
    ok = bc_workspace:rehatch_agent(<<"rehatch-agent">>),
    %% All 7 files should be restored to template defaults
    Templates = bc_workspace_templates:all_templates(),
    FileChecks = lists:map(fun({Filename, Expected}) ->
        {ok, Actual} = bc_workspace:read_bootstrap_file(<<"rehatch-agent">>, Filename),
        ?_assertEqual(Expected, Actual)
    end, Templates),
    %% Daily log should be wiped
    FileChecks ++ [?_assertEqual([], bc_workspace:list_daily_logs(<<"rehatch-agent">>))].

rehatch_not_found_test_() -> ?setup(rehatch_not_found_t).
rehatch_not_found_t(_TmpDir) ->
    [?_assertEqual({error, not_found},
                   bc_workspace:rehatch_agent(<<"nonexistent">>))].

rehatch_preserves_skills_test_() -> ?setup(rehatch_preserves_skills_t).
rehatch_preserves_skills_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"rehatch-skills">>),
    Dir = bc_workspace:agent_dir(<<"rehatch-skills">>),
    %% Create a per-agent skills/ subdirectory with a file
    SkillsDir = filename:join([Dir, "skills", "my-skill"]),
    ok = filelib:ensure_dir(filename:join(SkillsDir, "dummy")),
    ok = file:write_file(filename:join(SkillsDir, "SKILL.md"), <<"skill content">>),
    %% Rehatch
    ok = bc_workspace:rehatch_agent(<<"rehatch-skills">>),
    %% skills/ dir and file should still be present
    [?_assert(filelib:is_dir(SkillsDir)),
     ?_assertEqual({ok, <<"skill content">>},
                   file:read_file(filename:join(SkillsDir, "SKILL.md")))].
