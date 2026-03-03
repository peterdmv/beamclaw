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

-module(bc_env_context).
-moduledoc """
Dynamic environment context — injects current time, weather, news headlines,
and interaction timing into the LLM system prompt.

Singleton gen_server under `beamclaw_core_sup`. Caches weather and news data
with configurable TTLs (default 1 hour). Fetches weather via Brave Search API
and news via Finnhub API. Current time and time-since-last-interaction are
computed fresh on every call.

The output is a compact text block (~100-150 tokens) intended for injection
as a single system message. It is never stored in session history.

Configuration is under `{beamclaw_core, [{agentic_loop, #{environment_context => #{...}}}]}`.
Feature is opt-in (`enabled => false` by default).
""".

-behaviour(gen_server).

%% Public API
-export([start_link/0, get_context/2, invalidate/0, invalidate/1]).

%% Pure-function formatter (exported for testing)
-export([format_context/1, format_time_ago/1, parse_user_location/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TTL, 3600).       %% 1 hour
-define(FETCH_TIMEOUT, 15000).    %% 15s HTTP timeout
-define(MAX_HEADLINES, 5).
-define(MAX_WEATHER_LEN, 200).    %% truncate weather snippet

-record(state, {
    weather_cache  = undefined :: {binary(), non_neg_integer()} | undefined,
    news_cache     = undefined :: {[binary()], non_neg_integer()} | undefined,
    weather_ttl    = ?DEFAULT_TTL :: pos_integer(),
    news_ttl       = ?DEFAULT_TTL :: pos_integer(),
    news_category  = <<"general">> :: binary()
}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-doc """
Build the environment context text for the given agent.

`LastActivity` is the Unix-seconds timestamp of the session's last activity
(0 or undefined for new sessions). The agent's USER.md is parsed for location.

Returns `{ok, Binary}` with the formatted context block, or `disabled` if
the feature is turned off.
""".
-spec get_context(binary(), non_neg_integer() | undefined) ->
    {ok, binary()} | disabled.
get_context(AgentId, LastActivity) ->
    case is_enabled() of
        false -> disabled;
        true  -> gen_server:call(?SERVER, {get_context, AgentId, LastActivity}, 30000)
    end.

-doc "Invalidate all cached data (forces re-fetch on next call).".
-spec invalidate() -> ok.
invalidate() ->
    gen_server:cast(?SERVER, invalidate_all).

-doc "Invalidate a specific cache source (weather | news).".
-spec invalidate(weather | news) -> ok.
invalidate(Source) ->
    gen_server:cast(?SERVER, {invalidate, Source}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    Cfg = get_config(),
    State = #state{
        weather_ttl   = maps:get(weather_ttl_seconds, Cfg, ?DEFAULT_TTL),
        news_ttl      = maps:get(news_ttl_seconds, Cfg, ?DEFAULT_TTL),
        news_category = maps:get(news_category, Cfg, <<"general">>)
    },
    {ok, State}.

handle_call({get_context, AgentId, LastActivity}, _From, State0) ->
    Now = erlang:system_time(second),
    Location = resolve_location(AgentId),
    {Weather, State1} = maybe_refresh_weather(Location, Now, State0),
    {News, State2}    = maybe_refresh_news(Now, State1),
    Context = format_context(#{
        now            => Now,
        last_activity  => LastActivity,
        weather        => Weather,
        news           => News,
        location       => Location
    }),
    {reply, {ok, Context}, State2};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(invalidate_all, State) ->
    {noreply, State#state{weather_cache = undefined, news_cache = undefined}};
handle_cast({invalidate, weather}, State) ->
    {noreply, State#state{weather_cache = undefined}};
handle_cast({invalidate, news}, State) ->
    {noreply, State#state{news_cache = undefined}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% ===================================================================
%% Pure-function formatter (no side effects, testable)
%% ===================================================================

-doc """
Format the environment context block from a map of gathered data.

Input map keys:
- `now`           — current Unix timestamp (seconds)
- `last_activity` — last interaction Unix timestamp (0 or undefined for new)
- `weather`       — weather summary binary or `undefined`
- `news`          — list of headline binaries or `[]`
- `location`      — location string binary or `undefined`
""".
-spec format_context(map()) -> binary().
format_context(#{now := Now} = Data) ->
    LastActivity = maps:get(last_activity, Data, undefined),
    Weather      = maps:get(weather, Data, undefined),
    News         = maps:get(news, Data, []),
    Location     = maps:get(location, Data, undefined),

    %% Time section (always present)
    {{Y, Mo, D}, {H, Mi, _S}} = calendar:system_time_to_universal_time(Now, second),
    DayName = day_name(calendar:day_of_the_week({Y, Mo, D})),
    MonthName = month_name(Mo),
    TimeLine = iolist_to_binary(io_lib:format(
        "~s, ~s ~B, ~B, ~2..0B:~2..0B UTC",
        [DayName, MonthName, D, Y, H, Mi])),

    %% Time since last interaction
    TimeSinceLine = case LastActivity of
        undefined -> undefined;
        0         -> undefined;
        LA when is_integer(LA), LA > 0 ->
            Elapsed = max(0, Now - LA),
            case Elapsed of
                0 -> undefined;
                _ -> format_time_ago(Elapsed)
            end;
        _ -> undefined
    end,

    %% Build sections
    Sections = [
        <<"## Current Environment\n">>,
        <<"**Time:** ", TimeLine/binary, "\n">>
    ] ++
    case TimeSinceLine of
        undefined -> [];
        TSL -> [<<"**Since last interaction:** ", TSL/binary, "\n">>]
    end ++
    case {Weather, Location} of
        {undefined, _} -> [];
        {W, undefined} -> [<<"**Weather:** ", W/binary, "\n">>];
        {W, Loc}       -> [<<"**Weather:** ", Loc/binary, " — ", W/binary, "\n">>]
    end ++
    case News of
        [] -> [];
        Headlines ->
            HeadlineLines = [<<"- ", HL/binary>> || HL <- Headlines],
            [<<"**Headlines:**\n">>,
             iolist_to_binary(lists:join(<<"\n">>, HeadlineLines)),
             <<"\n">>]
    end ++
    [<<"\n---\n"
       "Use this context naturally. Greet appropriately for the time of day.\n"
       "Reference weather or news only when relevant to the conversation.\n"
       "Do not force these topics — let them arise organically.">>],

    iolist_to_binary(Sections).

-doc "Format elapsed seconds into a human-readable duration string.".
-spec format_time_ago(non_neg_integer()) -> binary().
format_time_ago(0) -> <<"just now">>;
format_time_ago(Seconds) when Seconds < 60 ->
    iolist_to_binary(io_lib:format("~B seconds ago", [Seconds]));
format_time_ago(Seconds) when Seconds < 3600 ->
    Minutes = Seconds div 60,
    case Minutes of
        1 -> <<"1 minute ago">>;
        _ -> iolist_to_binary(io_lib:format("~B minutes ago", [Minutes]))
    end;
format_time_ago(Seconds) when Seconds < 86400 ->
    Hours = Seconds div 3600,
    Mins  = (Seconds rem 3600) div 60,
    case {Hours, Mins} of
        {1, 0} -> <<"1 hour ago">>;
        {H, 0} -> iolist_to_binary(io_lib:format("~B hours ago", [H]));
        {1, M} -> iolist_to_binary(io_lib:format("1 hour ~B min ago", [M]));
        {H, M} -> iolist_to_binary(io_lib:format("~B hours ~B min ago", [H, M]))
    end;
format_time_ago(Seconds) ->
    Days = Seconds div 86400,
    case Days of
        1 -> <<"1 day ago">>;
        _ -> iolist_to_binary(io_lib:format("~B days ago", [Days]))
    end.

-doc """
Parse USER.md content for location/timezone information.

Looks for lines matching:
- `**Timezone:** <value>`
- `**Location:** <value>`
- `**City:** <value>`

Returns the first found value as a binary, or `undefined`.
""".
-spec parse_user_location(binary() | undefined) -> binary() | undefined.
parse_user_location(undefined) -> undefined;
parse_user_location(<<>>) -> undefined;
parse_user_location(Content) when is_binary(Content) ->
    Patterns = [
        <<"\\*\\*Location:\\*\\*\\s*(.+)">>,
        <<"\\*\\*City:\\*\\*\\s*(.+)">>,
        <<"\\*\\*Timezone:\\*\\*\\s*(.+)">>
    ],
    try_patterns(Patterns, Content).

%% ===================================================================
%% Internal: Cache management
%% ===================================================================

maybe_refresh_weather(_Location, Now, #state{weather_cache = {Data, CachedAt},
                                              weather_ttl = TTL} = State)
  when Now - CachedAt < TTL ->
    {Data, State};
maybe_refresh_weather(Location, Now, State) ->
    case fetch_weather(Location) of
        {ok, WeatherData} ->
            {WeatherData, State#state{weather_cache = {WeatherData, Now}}};
        {error, _Reason} ->
            %% Use stale cache if available; otherwise undefined
            case State#state.weather_cache of
                {StaleData, _} -> {StaleData, State};
                undefined      -> {undefined, State}
            end
    end.

maybe_refresh_news(Now, #state{news_cache = {Data, CachedAt},
                               news_ttl = TTL} = State)
  when Now - CachedAt < TTL ->
    {Data, State};
maybe_refresh_news(Now, State) ->
    case fetch_news(State#state.news_category) of
        {ok, Headlines} ->
            {Headlines, State#state{news_cache = {Headlines, Now}}};
        {error, _Reason} ->
            case State#state.news_cache of
                {StaleData, _} -> {StaleData, State};
                undefined      -> {[], State}
            end
    end.

%% ===================================================================
%% Internal: Weather fetch (Brave Search)
%% ===================================================================

fetch_weather(undefined) -> {error, no_location};
fetch_weather(Location) ->
    case get_brave_api_key() of
        {error, _} = Err -> Err;
        {ok, ApiKey} ->
            Query = <<"current weather in ", Location/binary>>,
            Url = build_brave_url(Query),
            Headers = [{<<"Accept">>, <<"application/json">>},
                       {<<"X-Subscription-Token">>, list_to_binary(ApiKey)}],
            case hackney:request(get, Url, Headers, <<>>,
                                 [{recv_timeout, ?FETCH_TIMEOUT},
                                  {connect_timeout, 5000}, with_body]) of
                {ok, 200, _RH, Body} ->
                    parse_brave_weather(Body);
                {ok, Status, _RH, _Body} ->
                    {error, {http_status, Status}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

parse_brave_weather(Body) ->
    try
        Decoded = jsx:decode(Body, [return_maps]),
        case Decoded of
            #{<<"web">> := #{<<"results">> := Results}} when is_list(Results), Results =/= [] ->
                %% Extract the first result's description as weather summary
                First = hd(Results),
                Desc = maps:get(<<"description">>, First, <<>>),
                Trimmed = truncate_binary(Desc, ?MAX_WEATHER_LEN),
                {ok, Trimmed};
            _ ->
                {error, no_results}
        end
    catch
        _:_ -> {error, parse_error}
    end.

build_brave_url(Query) ->
    QS = uri_string:compose_query([{<<"q">>, Query}, {<<"count">>, <<"1">>}]),
    <<"https://api.search.brave.com/res/v1/web/search?", QS/binary>>.

%% ===================================================================
%% Internal: News fetch (Finnhub)
%% ===================================================================

fetch_news(Category) ->
    case get_finnhub_token() of
        {error, _} = Err -> Err;
        {ok, Token} ->
            Url = iolist_to_binary([
                <<"https://finnhub.io/api/v1/news?category=">>,
                Category,
                <<"&token=">>,
                Token
            ]),
            case hackney:request(get, Url, [{<<"Accept">>, <<"application/json">>}],
                                 <<>>, [{recv_timeout, ?FETCH_TIMEOUT},
                                        {connect_timeout, 5000}, with_body]) of
                {ok, 200, _RH, Body} ->
                    parse_finnhub_news(Body);
                {ok, Status, _RH, _Body} ->
                    {error, {http_status, Status}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

parse_finnhub_news(Body) ->
    try
        case jsx:decode(Body, [return_maps]) of
            Items when is_list(Items) ->
                Headlines = lists:sublist(
                    [maps:get(<<"headline">>, Item, <<"(untitled)">>)
                     || Item <- Items,
                        is_map(Item),
                        maps:is_key(<<"headline">>, Item)],
                    ?MAX_HEADLINES),
                {ok, Headlines};
            _ ->
                {error, unexpected_format}
        end
    catch
        _:_ -> {error, parse_error}
    end.

%% ===================================================================
%% Internal: Location resolution
%% ===================================================================

resolve_location(AgentId) ->
    %% Try config first, then USER.md
    Cfg = get_config(),
    case maps:get(location, Cfg, undefined) of
        undefined ->
            case code:ensure_loaded(bc_workspace) of
                {module, _} ->
                    case bc_workspace:read_bootstrap_file(AgentId, <<"USER.md">>) of
                        {ok, Content} -> parse_user_location(Content);
                        _             -> undefined
                    end;
                _ -> undefined
            end;
        Loc when is_binary(Loc) -> Loc;
        Loc when is_list(Loc) -> list_to_binary(Loc);
        _ -> undefined
    end.

%% ===================================================================
%% Internal: Config & API key helpers
%% ===================================================================

get_config() ->
    try
        LoopCfg = bc_config:get(beamclaw_core, agentic_loop, #{}),
        maps:get(environment_context, LoopCfg, #{})
    catch
        _:_ -> #{}
    end.

is_enabled() ->
    Cfg = get_config(),
    maps:get(enabled, Cfg, false).

get_brave_api_key() ->
    try bc_config:get(beamclaw_tools, web_search) of
        #{api_key := Key} when is_list(Key), Key =/= [] -> {ok, Key};
        #{api_key := Key} when is_binary(Key), Key =/= <<>> ->
            {ok, binary_to_list(Key)};
        _ -> {error, no_api_key}
    catch
        _:_ -> {error, no_api_key}
    end.

get_finnhub_token() ->
    try
        case os:getenv("FINNHUB_TOKEN") of
            false -> {error, no_token};
            ""    -> {error, no_token};
            Token -> {ok, list_to_binary(Token)}
        end
    catch
        _:_ -> {error, no_token}
    end.

%% ===================================================================
%% Internal: Helpers
%% ===================================================================

try_patterns([], _Content) -> undefined;
try_patterns([Pattern | Rest], Content) ->
    case re:run(Content, Pattern, [{capture, [1], binary}, multiline]) of
        {match, [Value]} ->
            Trimmed = string:trim(Value),
            case Trimmed of
                <<>> -> try_patterns(Rest, Content);
                _    -> strip_markdown(Trimmed)
            end;
        _ -> try_patterns(Rest, Content)
    end.

%% Strip surrounding markdown formatting like parenthetical notes
strip_markdown(Bin) ->
    %% Remove trailing parenthetical like "(useful for ...)"
    case re:run(Bin, <<"^(.+?)\\s*\\(.*\\)\\s*$">>, [{capture, [1], binary}]) of
        {match, [Clean]} ->
            Trimmed = string:trim(Clean),
            case Trimmed of
                <<>> -> Bin;
                _    -> Trimmed
            end;
        _ -> Bin
    end.

truncate_binary(Bin, MaxLen) when byte_size(Bin) =< MaxLen -> Bin;
truncate_binary(Bin, MaxLen) ->
    <<Prefix:MaxLen/binary, _/binary>> = Bin,
    <<Prefix/binary, "...">>.

day_name(1) -> "Monday";
day_name(2) -> "Tuesday";
day_name(3) -> "Wednesday";
day_name(4) -> "Thursday";
day_name(5) -> "Friday";
day_name(6) -> "Saturday";
day_name(7) -> "Sunday".

month_name(1)  -> "January";
month_name(2)  -> "February";
month_name(3)  -> "March";
month_name(4)  -> "April";
month_name(5)  -> "May";
month_name(6)  -> "June";
month_name(7)  -> "July";
month_name(8)  -> "August";
month_name(9)  -> "September";
month_name(10) -> "October";
month_name(11) -> "November";
month_name(12) -> "December".
