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

-module(bc_tool_jq).
-moduledoc "Built-in jq tool — runs jq filter on JSON input.".
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"jq">>,
      description => <<"Apply a jq filter to a JSON string. Requires jq on PATH.">>,
      parameters  => #{
          type       => object,
          properties => #{
              filter => #{type => string, description => <<"jq filter expression">>},
              input  => #{type => string, description => <<"JSON string to process">>}
          },
          required   => [<<"filter">>, <<"input">>]
      },
      source => builtin}.

execute(#{<<"filter">> := Filter, <<"input">> := Input}, _Session, _Context) ->
    TmpIn = "/tmp/bc_jq_in_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:write_file(TmpIn, Input),
    Cmd = io_lib:format("jq ~p ~s 2>&1", [binary_to_list(Filter), TmpIn]),
    Output = os:cmd(lists:flatten(Cmd)),
    _ = file:delete(TmpIn),
    {ok, unicode:characters_to_binary(Output)}.

requires_approval() -> false.

min_autonomy() -> read_only.
