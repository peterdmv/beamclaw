%% @doc EUnit tests for bc_skill_discovery.
-module(bc_skill_discovery_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

-define(setup(F), {setup, fun setup/0, fun cleanup/1, fun F/1}).

setup() ->
    TmpDir = filename:join("/tmp", "beamclaw_sd_test_" ++
                            integer_to_list(erlang:unique_integer([positive]))),
    os:putenv("BEAMCLAW_HOME", TmpDir),
    %% Create agent workspace
    AgentDir = filename:join([TmpDir, "agents", "test-agent"]),
    ok = filelib:ensure_dir(filename:join(AgentDir, "dummy")),
    TmpDir.

cleanup(TmpDir) ->
    os:unsetenv("BEAMCLAW_HOME"),
    os:cmd("rm -rf " ++ TmpDir).

%% Helper to write a SKILL.md in a skills directory.
write_skill(BaseDir, SkillName, Content) ->
    Dir = filename:join(BaseDir, SkillName),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    ok = file:write_file(filename:join(Dir, "SKILL.md"), Content).

%% Count bundled skills so tests are stable regardless of priv/skills/ contents.
bundled_count() ->
    case code:priv_dir(beamclaw_core) of
        {error, _} -> 0;
        PrivDir ->
            Dir = filename:join(PrivDir, "skills"),
            case file:list_dir(Dir) of
                {ok, Entries} ->
                    length([E || E <- Entries,
                            filelib:is_file(filename:join([Dir, E, "SKILL.md"]))]);
                _ -> 0
            end
    end.

%% ---- discover from global dir ----

global_discovery_test_() -> ?setup(global_discovery_t).
global_discovery_t(TmpDir) ->
    GlobalDir = filename:join(TmpDir, "skills"),
    write_skill(GlobalDir, "git-helper",
        <<"---\nname: git-helper\ndescription: Git assistance\n---\n"
          "Help with git commands.\n">>),
    Skills = bc_skill_discovery:discover(<<"test-agent">>),
    BC = bundled_count(),
    HasGitHelper = lists:any(fun(S) -> S#bc_skill.name =:= <<"git-helper">> end, Skills),
    [?_assertEqual(1 + BC, length(Skills)),
     ?_assert(HasGitHelper)].

%% ---- discover from agent dir ----

agent_discovery_test_() -> ?setup(agent_discovery_t).
agent_discovery_t(TmpDir) ->
    AgentSkillDir = filename:join([TmpDir, "agents", "test-agent", "skills"]),
    write_skill(AgentSkillDir, "my-tool",
        <<"---\nname: my-tool\ndescription: Agent-specific\n---\n"
          "Agent tool docs.\n">>),
    Skills = bc_skill_discovery:discover(<<"test-agent">>),
    BC = bundled_count(),
    MyTool = [S || S <- Skills, S#bc_skill.name =:= <<"my-tool">>],
    [?_assertEqual(1 + BC, length(Skills)),
     ?_assertEqual({agent, <<"test-agent">>}, (hd(MyTool))#bc_skill.source)].

%% ---- agent overrides global ----

override_test_() -> ?setup(override_t).
override_t(TmpDir) ->
    %% Same skill name in both global and agent dirs
    GlobalDir = filename:join(TmpDir, "skills"),
    write_skill(GlobalDir, "shared-skill",
        <<"---\nname: shared-skill\ndescription: Global version\n---\nGlobal.\n">>),
    AgentSkillDir = filename:join([TmpDir, "agents", "test-agent", "skills"]),
    write_skill(AgentSkillDir, "shared-skill",
        <<"---\nname: shared-skill\ndescription: Agent version\n---\nAgent.\n">>),
    Skills = bc_skill_discovery:discover(<<"test-agent">>),
    BC = bundled_count(),
    Shared = [S || S <- Skills, S#bc_skill.name =:= <<"shared-skill">>],
    [?_assertEqual(1 + BC, length(Skills)),
     %% Agent version should win
     ?_assertEqual(<<"Agent version">>, (hd(Shared))#bc_skill.description)].

%% ---- empty dirs (only bundled skills) ----

empty_dirs_test_() -> ?setup(empty_dirs_t).
empty_dirs_t(_TmpDir) ->
    Skills = bc_skill_discovery:discover(<<"test-agent">>),
    BC = bundled_count(),
    [?_assertEqual(BC, length(Skills))].

%% ---- missing dirs (only bundled skills) ----

missing_dirs_test() ->
    os:putenv("BEAMCLAW_HOME", "/tmp/nonexistent_beamclaw_path"),
    Skills = bc_skill_discovery:discover(<<"nonexistent-agent">>),
    os:unsetenv("BEAMCLAW_HOME"),
    BC = bundled_count(),
    ?assertEqual(BC, length(Skills)).

%% ---- fallback name from directory ----

fallback_name_test_() -> ?setup(fallback_name_t).
fallback_name_t(TmpDir) ->
    GlobalDir = filename:join(TmpDir, "skills"),
    %% SKILL.md with no name in frontmatter
    write_skill(GlobalDir, "nameless-dir",
        <<"---\ndescription: No name field\n---\nContent.\n">>),
    Skills = bc_skill_discovery:discover(<<"test-agent">>),
    Nameless = [S || S <- Skills, S#bc_skill.name =:= <<"nameless-dir">>],
    [?_assertEqual(1, length(Nameless)),
     ?_assertEqual(<<"nameless-dir">>, (hd(Nameless))#bc_skill.name)].
