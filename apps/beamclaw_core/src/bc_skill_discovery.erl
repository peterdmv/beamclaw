%% @doc Skill discovery — scans filesystem for SKILL.md files.
%%
%% Discovery sources (lowest to highest precedence):
%% 1. Bundled:   <app_priv>/skills/*/SKILL.md
%% 2. Global:    ~/.beamclaw/skills/*/SKILL.md
%% 3. Per-agent: ~/.beamclaw/agents/<agent-id>/skills/*/SKILL.md
%%
%% Per-agent skills override global/bundled skills with the same name.
-module(bc_skill_discovery).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([discover/1, global_skills_dir/0, agent_skills_dir/1]).

%% @doc Discover all skills for an agent. Per-agent overrides global by name.
-spec discover(binary()) -> [#bc_skill{}].
discover(AgentId) ->
    Bundled = scan_dir(bundled_skills_dir(), global),
    Global  = scan_dir(global_skills_dir(), global),
    Agent   = scan_dir(agent_skills_dir(AgentId), {agent, AgentId}),
    %% Merge: later entries override earlier ones with the same name
    merge_skills(Bundled ++ Global ++ Agent).

%% @doc Return the global skills directory.
-spec global_skills_dir() -> string().
global_skills_dir() ->
    case os:getenv("BEAMCLAW_HOME") of
        false ->
            Home = os:getenv("HOME"),
            filename:join([Home, ".beamclaw", "skills"]);
        Dir ->
            filename:join([Dir, "skills"])
    end.

%% @doc Return the per-agent skills directory.
-spec agent_skills_dir(binary()) -> string().
agent_skills_dir(AgentId) ->
    case os:getenv("BEAMCLAW_HOME") of
        false ->
            Home = os:getenv("HOME"),
            filename:join([Home, ".beamclaw", "agents",
                           binary_to_list(AgentId), "skills"]);
        Dir ->
            filename:join([Dir, "agents",
                           binary_to_list(AgentId), "skills"])
    end.

%% Internal

bundled_skills_dir() ->
    case code:priv_dir(beamclaw_core) of
        {error, _} -> "";
        PrivDir    -> filename:join(PrivDir, "skills")
    end.

%% Scan a skills directory for subdirectories containing SKILL.md.
scan_dir("", _Source) -> [];
scan_dir(Dir, Source) ->
    case file:list_dir(Dir) of
        {ok, Entries} ->
            lists:filtermap(fun(Entry) ->
                SubDir = filename:join(Dir, Entry),
                SkillFile = filename:join(SubDir, "SKILL.md"),
                case filelib:is_dir(SubDir) andalso filelib:is_file(SkillFile) of
                    true ->
                        case file:read_file(SkillFile) of
                            {ok, Bin} ->
                                case bc_skill_parser:parse(Bin, Source, SkillFile) of
                                    {ok, Skill} ->
                                        {true, Skill};
                                    {error, missing_name} ->
                                        %% Use directory name as fallback
                                        FallbackSkill = #bc_skill{
                                            name = list_to_binary(Entry),
                                            description = undefined,
                                            homepage = undefined,
                                            emoji = undefined,
                                            content = Bin,
                                            source = Source,
                                            metadata = #{},
                                            path = SkillFile
                                        },
                                        {true, FallbackSkill};
                                    {error, _} ->
                                        false
                                end;
                            {error, _} ->
                                false
                        end;
                    false ->
                        false
                end
            end, lists:sort(Entries));
        {error, _} ->
            []
    end.

%% Merge skills: later entries override earlier ones with the same name.
merge_skills(Skills) ->
    Map = lists:foldl(fun(Skill, Acc) ->
        maps:put(Skill#bc_skill.name, Skill, Acc)
    end, #{}, Skills),
    maps:values(Map).
