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

-module(bc_system_prompt).
-moduledoc """
System prompt assembly from agent workspace bootstrap files.

Reads the eight markdown files from the agent's workspace directory and
converts them into system-role messages prepended to the conversation
history before each LLM call. Also loads daily logs (today + yesterday)
and eligible skill files. Assembled fresh every call so that MEMORY.md
and daily log updates mid-session are picked up immediately.
""".

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([assemble/1, assemble/2, resolve_base_dir/2]).

-doc """
Assemble system messages from an agent's bootstrap files.
Returns messages in order:
  IDENTITY → SOUL → USER → TOOLS → AGENTS → HEARTBEAT → BOOTSTRAP → MEMORY
  → memory/yesterday.md → memory/today.md
  → skill messages (if skills config provided via assemble/2)
Files that are missing or empty/whitespace-only are skipped.
If the agent doesn't exist, returns a single fallback system message.
""".
-spec assemble(binary()) -> [#bc_message{}].
assemble(AgentId) ->
    assemble(AgentId, #{}).

-spec assemble(binary(), map()) -> [#bc_message{}].
assemble(AgentId, Config) ->
    case bc_workspace:agent_exists(AgentId) of
        false ->
            [fallback_message()];
        true ->
            Files = bc_workspace:read_all_bootstrap_files(AgentId),
            %% Ordered: IDENTITY → SOUL → USER → TOOLS → AGENTS → HEARTBEAT → BOOTSTRAP → MEMORY
            Order = [<<"IDENTITY.md">>, <<"SOUL.md">>, <<"USER.md">>,
                     <<"TOOLS.md">>, <<"AGENTS.md">>, <<"HEARTBEAT.md">>,
                     <<"BOOTSTRAP.md">>, <<"MEMORY.md">>],
            BootstrapMsgs = lists:filtermap(fun(Filename) ->
                case maps:get(Filename, Files, undefined) of
                    undefined -> false;
                    Content   ->
                        case is_blank(Content) of
                            true  -> false;
                            false ->
                                MsgContent = <<"[", Filename/binary, "]\n", Content/binary>>,
                                {true, #bc_message{
                                    id      = generate_id(),
                                    role    = system,
                                    content = MsgContent,
                                    ts      = 0
                                }}
                        end
                end
            end, Order),
            DailyMsgs = load_daily_logs(AgentId),
            SkillMsgs = load_skills(AgentId, Config),
            BaseMsgs = case BootstrapMsgs of
                [] -> [fallback_message()];
                _  -> BootstrapMsgs
            end,
            BaseMsgs ++ DailyMsgs ++ SkillMsgs
    end.

%% Internal

fallback_message() ->
    #bc_message{
        id      = generate_id(),
        role    = system,
        content = <<"You are a helpful assistant.">>,
        ts      = 0
    }.

-doc "Load today and yesterday daily logs as system messages.".
load_daily_logs(AgentId) ->
    Today = today_date(),
    Yesterday = yesterday_date(),
    %% Yesterday first, today second (most recent closest to conversation)
    Dates = [Yesterday, Today],
    lists:filtermap(fun(Date) ->
        case bc_workspace:read_daily_log(AgentId, Date) of
            {ok, Content} ->
                case is_blank(Content) of
                    true -> false;
                    false ->
                        Filename = <<"memory/", Date/binary, ".md">>,
                        MsgContent = <<"[", Filename/binary, "]\n", Content/binary>>,
                        {true, #bc_message{
                            id      = generate_id(),
                            role    = system,
                            content = MsgContent,
                            ts      = 0
                        }}
                end;
            {error, _} -> false
        end
    end, Dates).

-doc """
Load eligible skills as system messages using a two-tier strategy:

- **Always skills** (`metadata.beamclaw.always = true`): full content injected
  as individual system messages (same as before). These contain reference
  instructions the agent should always have available.
- **On-demand skills**: a single compact summary message listing skill names,
  descriptions, and read_file paths. The agent reads the full SKILL.md only
  when it decides to use a skill, saving tokens on every turn.
""".
load_skills(AgentId, Config) ->
    case code:ensure_loaded(bc_skill_discovery) of
        {module, _} ->
            try
                Skills = bc_skill_discovery:discover(AgentId),
                Eligible = lists:filter(fun(S) ->
                    case bc_skill_eligibility:check(S) of
                        ok -> true;
                        {missing, Details} ->
                            logger:debug("Skill ~s ineligible: ~p",
                                         [skill_name(S), Details]),
                            false
                    end
                end, Skills),
                Filtered = filter_by_allowlist(Eligible, Config),
                {Always, OnDemand} = lists:partition(fun is_always_skill/1, Filtered),
                AlwaysMsgs = lists:map(fun(Skill) ->
                    Name = skill_name(Skill),
                    RawContent = skill_content(Skill),
                    Content = resolve_base_dir(RawContent, skill_path(Skill)),
                    MsgContent = <<"[skill:", Name/binary, "]\n", Content/binary>>,
                    #bc_message{
                        id      = generate_id(),
                        role    = system,
                        content = MsgContent,
                        ts      = 0
                    }
                end, Always),
                SummaryMsgs = build_skill_summary(OnDemand),
                AlwaysMsgs ++ SummaryMsgs
            catch _:_ -> []
            end;
        _ -> []
    end.

is_always_skill(Skill) ->
    Meta = skill_metadata(Skill),
    case Meta of
        #{<<"beamclaw">> := #{<<"always">> := true}} -> true;
        _ -> false
    end.

build_skill_summary([]) -> [];
build_skill_summary(Skills) ->
    Lines = [format_skill_summary(S) || S <- Skills],
    SkillList = iolist_to_binary(lists:join(<<"\n">>, Lines)),
    Content = <<"[skills:available]\n"
                "## Available Skills\n\n"
                "Before replying, scan the skills below. If one clearly applies to the user's\n"
                "request, read its full content with the read_file tool, then follow its\n"
                "instructions.\n\n"
                "<available_skills>\n",
                SkillList/binary, "\n"
                "</available_skills>\n\n"
                "Rules:\n"
                "- Select at most ONE skill per turn.\n"
                "- Do NOT read a skill unless it clearly matches the user's request.\n"
                "- After reading, follow the skill's instructions exactly.">>,
    [#bc_message{
        id      = generate_id(),
        role    = system,
        content = Content,
        ts      = 0
    }].

format_skill_summary(Skill) ->
    Name = skill_name(Skill),
    Desc = case skill_description(Skill) of
               undefined -> <<"(no description)">>;
               D -> D
           end,
    Path = skill_path(Skill),
    PathBin = case Path of
                  P when is_list(P), P =/= "" -> list_to_binary(P);
                  _ -> <<"unknown">>
              end,
    BaseDirNote = case has_base_dir(Skill) of
                      true ->
                          Dir = list_to_binary(filename:dirname(Path)),
                          <<" (baseDir: ", Dir/binary, ")">>;
                      false -> <<>>
                  end,
    <<"- ", Name/binary, ": ", Desc/binary, " [read: ", PathBin/binary, "]",
      BaseDirNote/binary>>.

has_base_dir(Skill) ->
    Content = skill_content(Skill),
    binary:match(Content, <<"{baseDir}">>) =/= nomatch.

filter_by_allowlist(Skills, #{skills := #{entries := Entries}}) ->
    [S || S <- Skills,
     case maps:get(skill_name(S), Entries, #{enabled => true}) of
         #{enabled := false} -> false;
         _ -> true
     end];
filter_by_allowlist(Skills, _Config) ->
    Skills.

%% Access skill fields via element/2 to avoid compile-time record dependency.
%% #bc_skill{name, description, homepage, emoji, content, source, metadata, path}
%% = {bc_skill, Name, Description, Homepage, Emoji, Content, Source, Metadata, Path}
skill_name(Skill) -> element(2, Skill).
skill_description(Skill) -> element(3, Skill).
skill_content(Skill) -> element(6, Skill).
skill_metadata(Skill) -> element(8, Skill).
skill_path(Skill) -> element(9, Skill).

-doc """
Replace `{baseDir}` in skill content with the directory containing the SKILL.md file.
This allows bundled skills to reference sibling files (e.g., scripts/) via a
path that resolves correctly at runtime regardless of installation location.
""".
-spec resolve_base_dir(binary(), string()) -> binary().
resolve_base_dir(Content, Path) when is_list(Path), Path =/= "" ->
    BaseDir = list_to_binary(filename:dirname(Path)),
    binary:replace(Content, <<"{baseDir}">>, BaseDir, [global]);
resolve_base_dir(Content, _) ->
    Content.

today_date() ->
    {Y, M, D} = date(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

yesterday_date() ->
    Today = calendar:date_to_gregorian_days(date()),
    {Y, M, D} = calendar:gregorian_days_to_date(Today - 1),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

is_blank(Bin) ->
    Trimmed = string:trim(Bin),
    Trimmed =:= <<>> orelse Trimmed =:= "".

generate_id() ->
    <<N:128>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("sys-~32.16.0b", [N])).
