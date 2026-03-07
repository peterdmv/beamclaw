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

-module(bc_tool_noop_mock).
-moduledoc "No-op mock tool. Always succeeds with a fixed response.".
-behaviour(bc_tool).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #bc_tool_def{
        name        = <<"noop_tool">>,
        description = <<"A no-op tool for testing">>,
        parameters  = #{},
        source      = builtin
    }.

execute(_Args, _SessionRef, _Context) ->
    {ok, <<"noop: ok">>}.

requires_approval() -> false.

min_autonomy() -> read_only.
