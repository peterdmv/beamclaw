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
-module(bc_channel_a2a).
-moduledoc """
A2A channel — stateless response routing for A2A protocol sessions.

Called by `bc_loop` via dynamic dispatch (`channel_mod_for(a2a)`) when the
agentic loop completes a turn for an A2A-originated session. Looks up the
task ID from the session-to-task mapping and transitions the task to completed.
""".

-include("bc_a2a_types.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

-export([send_response/2, notify_typing/1]).

-spec send_response(binary(), #bc_message{}) -> ok.
send_response(SessionId, #bc_message{content = Content}) ->
    case bc_a2a_task_manager:task_id_for_session(SessionId) of
        {ok, TaskId} ->
            AgentMsg = #a2a_message{
                role = agent,
                parts = [#{type => text, text => Content}]
            },
            bc_a2a_task_manager:update_task(TaskId, completed, AgentMsg),
            ok;
        {error, not_found} ->
            ok
    end;
send_response(_SessionId, _Msg) ->
    ok.

-spec notify_typing(binary()) -> ok.
notify_typing(_SessionId) ->
    ok.
