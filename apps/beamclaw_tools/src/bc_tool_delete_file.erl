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

-module(bc_tool_delete_file).
-moduledoc "Built-in delete_file tool — deletes a file at the given path.".
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"delete_file">>,
      description => <<"Delete a file at the given path.">>,
      parameters  => #{
          type       => object,
          properties => #{
              path => #{type => string,
                        description => <<"Absolute or relative path to the file to delete">>}
          },
          required   => [<<"path">>]
      },
      source => builtin}.

execute(#{<<"path">> := Path}, _Session, _Context) ->
    case file:delete(binary_to_list(Path)) of
        ok -> {ok, <<"ok">>};
        {error, enoent} -> {ok, <<"File not found (already deleted)">>};
        {error, Reason} -> {error, iolist_to_binary(atom_to_list(Reason))}
    end.

requires_approval() -> true.

min_autonomy() -> supervised.
