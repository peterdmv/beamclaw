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

-module(beamclaw_a2a_app).
-moduledoc """
BeamClaw A2A (Agent2Agent) protocol application.

This application implements Google's A2A protocol for agent-to-agent communication,
providing a JSON-RPC 2.0 over HTTP interface for:
- Task management (create, get, cancel, list)
- Message passing between agents
- Agent capability discovery via Agent Cards
""".

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    bc_a2a_task:init(),
    beamclaw_a2a_sup:start_link().

stop(_State) ->
    ok.