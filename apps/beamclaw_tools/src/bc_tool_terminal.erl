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

-module(bc_tool_terminal).
-moduledoc "Built-in terminal tool — runs a single shell command.".
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"terminal">>,
      description => <<"Run a single shell command and return stdout+stderr.">>,
      parameters  => #{
          type       => object,
          properties => #{
              command => #{type => string, description => <<"Shell command to execute">>}
          },
          required   => [<<"command">>]
      },
      source => builtin}.

execute(#{<<"command">> := Cmd}, _Session, _Context) ->
    SafeCmd = binary_to_list(Cmd),
    case catch os:cmd(SafeCmd) of
        {'EXIT', Reason} ->
            {error, iolist_to_binary(io_lib:format("exit: ~p", [Reason]))};
        Output ->
            {ok, unicode:characters_to_binary(Output)}
    end.

requires_approval() -> true.

min_autonomy() -> supervised.
