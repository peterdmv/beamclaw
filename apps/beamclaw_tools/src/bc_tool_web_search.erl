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

-module(bc_tool_web_search).
-moduledoc """
Built-in web search tool — queries the Brave Search API.

Read-only tool (no approval required, works at read_only autonomy).
API key is resolved from the BRAVE_API_KEY environment variable at
execute time. Returns a clear error when the key is not configured.
""".
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).
-export([format_results/2]).

definition() ->
    #{name        => <<"web_search">>,
      description => <<"Search the web using Brave Search. Returns titles, URLs, and snippets.">>,
      parameters  => #{
          type       => object,
          properties => #{
              query   => #{type => string,
                           description => <<"Search query">>},
              count   => #{type => integer,
                           description => <<"Number of results (1-10, default 5)">>},
              freshness => #{type => string,
                             description => <<"Freshness filter: pd (past day), pw (past week), pm (past month), py (past year), or YYYY-MM-DDtoYYYY-MM-DD">>},
              country => #{type => string,
                           description => <<"2-letter country code (e.g., US, GB, DE)">>}
          },
          required   => [<<"query">>]
      },
      source => builtin}.

execute(#{<<"query">> := Query} = Args, _Session, _Context) ->
    case get_api_key() of
        {error, Reason} ->
            {error, Reason};
        {ok, ApiKey} ->
            Count = clamp_count(maps:get(<<"count">>, Args, 5)),
            Freshness = maps:get(<<"freshness">>, Args, undefined),
            Country = maps:get(<<"country">>, Args, undefined),
            Url = list_to_binary(build_url(Query, Count, Freshness, Country)),
            Headers = [{<<"Accept">>, <<"application/json">>},
                       {<<"X-Subscription-Token">>, list_to_binary(ApiKey)}],
            case hackney:request(get, Url, Headers, <<>>,
                                 [{recv_timeout, 30000}, {connect_timeout, 10000},
                                  with_body]) of
                {ok, 200, _RespHeaders, RespBody} ->
                    case jsx:decode(RespBody, [return_maps]) of
                        #{<<"web">> := #{<<"results">> := Results}} ->
                            {ok, format_results(Query, Results)};
                        _ ->
                            {ok, format_results(Query, [])}
                    end;
                {ok, StatusCode, _RespHeaders, RespBody} ->
                    {error, iolist_to_binary(
                        io_lib:format("Brave Search API returned HTTP ~p: ~s",
                                      [StatusCode, truncate(RespBody, 500)]))};
                {error, Reason} ->
                    {error, iolist_to_binary(
                        io_lib:format("Web search request failed: ~p", [Reason]))}
            end
    end;
execute(_Args, _Session, _Context) ->
    {error, <<"Missing required parameter: query">>}.

requires_approval() -> false.

min_autonomy() -> read_only.

%% -------------------------------------------------------------------
%% Internal
%% -------------------------------------------------------------------

get_api_key() ->
    try bc_config:get(beamclaw_tools, web_search) of
        #{api_key := Key} when is_list(Key), Key =/= [] ->
            {ok, Key};
        #{api_key := Key} when is_binary(Key), Key =/= <<>> ->
            {ok, binary_to_list(Key)};
        _ ->
            {error, no_api_key_msg()}
    catch
        error:{missing_config, _, _} ->
            {error, no_api_key_msg()};
        error:{missing_env_var, _} ->
            {error, no_api_key_msg()}
    end.

no_api_key_msg() ->
    <<"Web search is not configured. Set the BRAVE_API_KEY environment variable "
      "with your Brave Search API key. Get one at https://brave.com/search/api/">>.

clamp_count(N) when is_integer(N), N >= 1, N =< 10 -> N;
clamp_count(N) when is_integer(N), N < 1 -> 1;
clamp_count(N) when is_integer(N) -> 10;
clamp_count(_) -> 5.

build_url(Query, Count, Freshness, Country) ->
    QParams0 = [{<<"q">>, Query}, {<<"count">>, integer_to_binary(Count)}],
    QParams1 = maybe_add(<<"freshness">>, Freshness, QParams0),
    QParams2 = maybe_add(<<"country">>, Country, QParams1),
    QueryString = uri_string:compose_query(QParams2),
    binary_to_list(<<"https://api.search.brave.com/res/v1/web/search?", QueryString/binary>>).

maybe_add(_Key, undefined, Acc) -> Acc;
maybe_add(Key, Val, Acc) -> Acc ++ [{Key, Val}].

-doc "Format Brave Search API results for LLM consumption.".
-spec format_results(binary(), list()) -> binary().
format_results(Query, []) ->
    iolist_to_binary(
        io_lib:format("Web search results for \"~s\":\n\nNo results found.\n", [Query]));
format_results(Query, Results) ->
    Header = io_lib:format("Web search results for \"~s\":\n\n", [Query]),
    {_, Lines} = lists:foldl(fun(Result, {N, Acc}) ->
        Title = maps:get(<<"title">>, Result, <<"(no title)">>),
        Url = maps:get(<<"url">>, Result, <<>>),
        Desc = maps:get(<<"description">>, Result, <<>>),
        Line = io_lib:format("~p. ~s\n   ~s\n   ~s\n\n", [N, Title, Url, Desc]),
        {N + 1, [Line | Acc]}
    end, {1, []}, Results),
    Count = length(Results),
    Footer = io_lib:format("(~p result~s)\n", [Count, plural(Count)]),
    iolist_to_binary([Header, lists:reverse(Lines), Footer]).

plural(1) -> "";
plural(_) -> "s".

truncate(Bin, MaxLen) when byte_size(Bin) =< MaxLen -> Bin;
truncate(Bin, MaxLen) -> binary:part(Bin, 0, MaxLen).
