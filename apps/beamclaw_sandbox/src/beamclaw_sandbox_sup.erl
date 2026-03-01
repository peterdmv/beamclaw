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

-module(beamclaw_sandbox_sup).
-moduledoc """
Top-level supervisor for beamclaw_sandbox.

Children:
  - bc_sandbox_registry: ETS-backed {SessionId, Scope} -> Pid mapping
  - bc_sandbox_reaper: periodic orphan container cleanup
  - bc_sandbox_sup: simple_one_for_one for per-sandbox bc_sandbox processes
""".
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = [
        #{id       => bc_sandbox_registry,
          start    => {bc_sandbox_registry, start_link, []},
          restart  => permanent,
          shutdown => 5000,
          type     => worker,
          modules  => [bc_sandbox_registry]},
        #{id       => bc_sandbox_reaper,
          start    => {bc_sandbox_reaper, start_link, []},
          restart  => permanent,
          shutdown => 5000,
          type     => worker,
          modules  => [bc_sandbox_reaper]},
        #{id       => bc_sandbox_sup,
          start    => {bc_sandbox_sup, start_link, []},
          restart  => permanent,
          shutdown => infinity,
          type     => supervisor,
          modules  => [bc_sandbox_sup]}
    ],
    {ok, {SupFlags, Children}}.
