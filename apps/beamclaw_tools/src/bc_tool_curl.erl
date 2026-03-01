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
-export([expand_env_vars/1]).

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

execute(#{<<"url">> := RawUrl} = Args, _Session, _Context) ->
    Url     = expand_env_vars(RawUrl),
    Method  = maps:get(<<"method">>,  Args, <<"GET">>),
    Headers = maps:get(<<"headers">>, Args, #{}),
    Body    = maps:get(<<"body">>,    Args, <<>>),
    HList   = [{K, expand_env_vars(V)} || {K, V} <- maps:to_list(Headers)],
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

%% @doc Expand $VAR and ${VAR} patterns from os:getenv.
%% Unset vars are left unexpanded (preserves debuggability).
-spec expand_env_vars(binary()) -> binary().
expand_env_vars(Bin) ->
    Pat = <<"\\$\\{([A-Za-z_][A-Za-z0-9_]*)\\}|\\$([A-Za-z_][A-Za-z0-9_]*)">>,
    case re:run(Bin, Pat, [global, {capture, all, index}]) of
        {match, Matches} ->
            expand_matches(Bin, lists:reverse(Matches), Bin);
        nomatch ->
            Bin
    end.

%% Process matches right-to-left so byte offsets remain valid after replacement.
expand_matches(_OrigBin, [], Acc) ->
    Acc;
expand_matches(OrigBin, [Match | Rest], Acc) ->
    {FullStart, FullLen, VarName} = extract_var(OrigBin, Match),
    case os:getenv(binary_to_list(VarName)) of
        false ->
            expand_matches(OrigBin, Rest, Acc);
        Value ->
            Prefix = binary:part(Acc, 0, FullStart),
            Suffix = binary:part(Acc, FullStart + FullLen, byte_size(Acc) - FullStart - FullLen),
            expand_matches(OrigBin, Rest, <<Prefix/binary, (list_to_binary(Value))/binary, Suffix/binary>>)
    end.

%% Extract the full match position and the variable name from capture groups.
%% Group 1 captures ${VAR}, group 2 captures $VAR — exactly one will match.
%% Erlang's re:run truncates trailing non-participating groups, so ${VAR}
%% matches return a 2-element list (group 2 omitted).
extract_var(Bin, [{FullStart, FullLen}, {G1Start, G1Len} | _]) when G1Start >= 0 ->
    {FullStart, FullLen, binary:part(Bin, G1Start, G1Len)};
extract_var(Bin, [{FullStart, FullLen}, _, {G2Start, G2Len}]) when G2Start >= 0 ->
    {FullStart, FullLen, binary:part(Bin, G2Start, G2Len)}.
