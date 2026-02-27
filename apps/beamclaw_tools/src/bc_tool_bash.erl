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

-module(bc_tool_bash).
-moduledoc "Built-in bash tool — runs a bash script (multi-line supported).".
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"bash">>,
      description => <<"Execute a bash script. Supports multi-line scripts.">>,
      parameters  => #{
          type       => object,
          properties => #{
              script => #{type => string, description => <<"Bash script content">>}
          },
          required   => [<<"script">>]
      },
      source => builtin}.

execute(#{<<"script">> := Script}, _Session, _Context) ->
    %% Write script to temp file, execute with bash, return output
    TmpFile = "/tmp/bc_bash_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:write_file(TmpFile, Script),
    Output = os:cmd("bash " ++ TmpFile ++ " 2>&1"),
    _ = file:delete(TmpFile),
    {ok, unicode:characters_to_binary(Output)}.

requires_approval() -> true.

min_autonomy() -> supervised.
