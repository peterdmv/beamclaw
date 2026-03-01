%%
%% Copyright Peter Dimitrov 2026, All Rights Reserved.
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

-module(bc_memory_flush).
-moduledoc """
Pre-compaction memory flush.

Asks the LLM to save durable memories before history is compacted or discarded.
Extracted from bc_loop to allow reuse by bc_session_maintenance for idle and
nightly maintenance flushes.

Creates a fresh provider instance per call — no long-lived state.
""".

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([run/1, run/2]).

-define(FLUSH_PROMPT,
    <<"Session history is about to be compacted. Before the older messages "
      "are summarized and trimmed, save any durable facts, observations, or "
      "context worth preserving. Use the workspace_memory tool:\n"
      "- Identity info -> update_bootstrap (file: IDENTITY.md)\n"
      "- User info -> update_bootstrap (file: USER.md)\n"
      "- Session observations -> append_daily\n"
      "- Long-term facts -> append (to MEMORY.md)\n"
      "If there is nothing worth saving, respond with just 'ok'.">>).

-doc """
Run a memory flush for the given session. Gathers all info from the session
pid, initializes a fresh provider, and executes a hidden LLM turn.
""".
-spec run(pid()) -> ok | {error, term()}.
run(SessionPid) ->
    run(SessionPid, #{}).

-doc """
Run a memory flush with optional overrides.
Supported options:
  provider_mod => module()  — skip the session query for provider module
""".
-spec run(pid(), map()) -> ok | {error, term()}.
run(SessionPid, Opts) ->
    try
        History   = bc_session:get_history(SessionPid),
        AgentId   = bc_session:get_agent_id(SessionPid),
        ProvMod   = maps:get(provider_mod, Opts,
                             bc_session:get_provider_mod(SessionPid)),
        SessionId = bc_session:get_session_id(SessionPid),
        do_flush(SessionId, SessionPid, History, AgentId, ProvMod)
    catch
        Class:Reason:Stack ->
            logger:warning("[memory_flush] failed: ~p:~p ~p",
                           [Class, Reason, Stack]),
            {error, {Class, Reason}}
    end.

%% Internal

do_flush(_SessionId, _SessionPid, History, _AgentId, _ProvMod)
  when length(History) < 2 ->
    ok;
do_flush(SessionId, _SessionPid, History, AgentId, ProvMod) ->
    SystemMsgs = bc_system_prompt:assemble(AgentId),
    FlushMsg   = #bc_message{
        id      = bc_compactor:generate_id(),
        role    = system,
        content = ?FLUSH_PROMPT,
        ts      = erlang:system_time(millisecond)
    },
    FullHistory = SystemMsgs ++ History ++ [FlushMsg],
    Tools    = bc_tool_registry:list(),
    ToolDefs = [Def || {_Name, _Mod, Def} <- Tools],
    Options  = #{tools => ToolDefs},
    ProvConfig = bc_compactor:get_provider_config(ProvMod),
    case ProvMod:init(ProvConfig) of
        {ok, ProvState} ->
            case ProvMod:complete(FullHistory, Options, ProvState) of
                {ok, ResponseMsg, _NewProvState} ->
                    ToolCalls = bc_tool_parser:parse(ResponseMsg),
                    execute_flush_tool_calls(ToolCalls, SessionId, AgentId),
                    ok;
                {error, Reason, _NewProvState} ->
                    logger:warning("[memory_flush] LLM call failed: ~p "
                                   "session=~s", [Reason, SessionId]),
                    {error, Reason}
            end;
        {error, Reason} ->
            logger:warning("[memory_flush] provider init failed: ~p "
                           "session=~s", [Reason, SessionId]),
            {error, Reason}
    end.

execute_flush_tool_calls([], _SessionId, _AgentId) -> ok;
execute_flush_tool_calls(Calls, SessionId, AgentId) ->
    SessionRef = #bc_session_ref{
        session_id = SessionId,
        user_id    = <<"maintenance">>,
        session_pid = undefined,
        autonomy   = full,
        agent_id   = AgentId
    },
    lists:foreach(fun(TC) ->
        logger:debug("[memory_flush] tool call: ~s session=~s",
                     [TC#bc_tool_call.name, SessionId]),
        try
            case bc_tool_registry:lookup(TC#bc_tool_call.name) of
                {ok, {Mod, _Def}} ->
                    Mod:execute(TC#bc_tool_call.args, SessionRef, #{});
                {error, not_found} ->
                    logger:warning("[memory_flush] tool not found: ~s",
                                   [TC#bc_tool_call.name])
            end
        catch Class:Reason ->
            logger:warning("[memory_flush] tool ~s failed: ~p:~p",
                           [TC#bc_tool_call.name, Class, Reason])
        end
    end, Calls).
