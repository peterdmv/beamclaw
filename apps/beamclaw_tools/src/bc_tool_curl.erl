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

-module(bc_tool_curl).
-moduledoc "Built-in curl tool — makes HTTP requests.".
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"curl">>,
      description => <<"Make an HTTP request. Returns status code and body.">>,
      parameters  => #{
          type       => object,
          properties => #{
              url     => #{type => string},
              method  => #{type => string,
                           enum => [<<"GET">>, <<"POST">>, <<"PUT">>,
                                    <<"DELETE">>, <<"PATCH">>]},
              headers => #{type => object},
              body    => #{type => string}
          },
          required   => [<<"url">>]
      },
      source => builtin}.

execute(#{<<"url">> := Url} = Args, _Session, _Context) ->
    Method  = maps:get(<<"method">>,  Args, <<"GET">>),
    Headers = maps:get(<<"headers">>, Args, #{}),
    Body    = maps:get(<<"body">>,    Args, <<>>),
    HList   = [{K, V} || {K, V} <- maps:to_list(Headers)],
    MethodAtom = list_to_atom(string:lowercase(binary_to_list(Method))),
    case hackney:request(MethodAtom, Url, HList, Body,
                         [{recv_timeout, 30000}, {connect_timeout, 10000},
                          with_body]) of
        {ok, StatusCode, _RespHeaders, RespBody} ->
            Result = iolist_to_binary(io_lib:format("~p\n~s", [StatusCode, RespBody])),
            {ok, Result};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

requires_approval() -> true.

min_autonomy() -> supervised.
