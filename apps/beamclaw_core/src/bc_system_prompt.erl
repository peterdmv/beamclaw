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

-export([assemble/1, assemble/2]).

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
Load eligible skills as system messages.
Skills are represented as maps with keys: name, content, description, etc.
This avoids a compile-time dependency on the #bc_skill{} record which lives
in bc_types.hrl and may not be defined until M16.
""".
load_skills(AgentId, Config) ->
    case code:ensure_loaded(bc_skill_discovery) of
        {module, _} ->
            try
                Skills = bc_skill_discovery:discover(AgentId),
                Eligible = [S || S <- Skills,
                            bc_skill_eligibility:is_eligible(S)],
                Filtered = filter_by_allowlist(Eligible, Config),
                lists:map(fun(Skill) ->
                    Name = skill_name(Skill),
                    Content = skill_content(Skill),
                    MsgContent = <<"[skill:", Name/binary, "]\n", Content/binary>>,
                    #bc_message{
                        id      = generate_id(),
                        role    = system,
                        content = MsgContent,
                        ts      = 0
                    }
                end, Filtered)
            catch _:_ -> []
            end;
        _ -> []
    end.

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
skill_content(Skill) -> element(6, Skill).

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
