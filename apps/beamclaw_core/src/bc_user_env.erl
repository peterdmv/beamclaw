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

-module(bc_user_env).
-moduledoc """
User environment context injection — gen_server.

Singleton gen_server that builds an ephemeral [user:environment] system message
injected into every LLM call. Contains current time, idle duration, weather,
and news headlines. Weather and news are fetched asynchronously on a periodic
refresh timer (never in handle_call) and cached in state.

Weather data comes from Open-Meteo (free, no API key). News headlines come from
Finnhub (requires FINNHUB_TOKEN).

The message is never stored in session history — it is assembled fresh for each
LLM request and prepended alongside system prompt and auto-context messages.
""".
-behaviour(gen_server).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/0, get_env_message/2]).
%% Exported for testing
-export([format_idle_time/1, format_time_section/3,
         parse_weather_json/1, parse_news_json/1,
         parse_timezone_from_user_md/1, parse_location_from_user_md/1,
         tz_offset/1, build_env_block/1, wmo_description/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-record(state, {
    weather_cache :: #{binary() => binary()},  %% LocKey => WeatherText
    news_text     :: binary() | undefined,
    worker_ref    :: reference() | undefined  %% monitor ref for async worker
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Build the [user:environment] system message for injection into the LLM call.
Returns [] when disabled or on failure. Returns [#bc_message{}] otherwise.
Never blocks on HTTP — reads cached weather/news from state.
""".
-spec get_env_message(AgentId :: binary(), SessionPid :: pid()) -> [#bc_message{}].
get_env_message(AgentId, SessionPid) ->
    try gen_server:call(?MODULE, {get_env_message, AgentId, SessionPid}, 5000)
    catch _:_ -> []
    end.

%% ---- gen_server callbacks ----

init([]) ->
    Config = bc_config:get(beamclaw_core, user_env, #{enabled => true}),
    case maps:get(enabled, Config, true) of
        true  -> self() ! refresh;
        false -> ok
    end,
    {ok, #state{weather_cache = #{}}}.

handle_call({get_env_message, AgentId, SessionPid}, _From, State) ->
    Config = bc_config:get(beamclaw_core, user_env, #{enabled => true}),
    case maps:get(enabled, Config, true) of
        false ->
            {reply, [], State};
        true ->
            Sections = build_sections(AgentId, SessionPid, Config, State),
            case [S || S <- Sections, S =/= <<>>] of
                [] ->
                    {reply, [], State};
                NonEmpty ->
                    Block = iolist_to_binary([
                        <<"[user:environment]\n">>,
                        lists:join(<<"\n\n">>, NonEmpty)
                    ]),
                    Msg = #bc_message{
                        id      = generate_id(),
                        role    = system,
                        content = Block,
                        ts      = erlang:system_time(millisecond)
                    },
                    {reply, [Msg], State}
            end
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    Config = bc_config:get(beamclaw_core, user_env, #{enabled => true}),
    case maps:get(enabled, Config, true) of
        false ->
            {noreply, State};
        true ->
            %% Schedule next refresh
            Interval = maps:get(refresh_interval_ms, Config, 1800000),
            erlang:send_after(Interval, self(), refresh),
            Self = self(),
            WeatherCfg = maps:get(weather, Config, #{enabled => true}),
            NewsCfg = maps:get(news, Config, #{enabled => true}),
            WeatherEnabled = maps:get(enabled, WeatherCfg, true),
            NewsEnabled = maps:get(enabled, NewsCfg, true),
            %% Collect all locations: default config + any already cached
            DefaultLat = maps:get(latitude, Config, 59.33),
            DefaultLon = maps:get(longitude, Config, 18.07),
            DefaultLocName = maps:get(location_name, Config, "Stockholm"),
            DefaultKey = loc_key(DefaultLat, DefaultLon),
            CachedLocs = lists:filtermap(fun(K) ->
                case K =:= DefaultKey of
                    true  -> false;  %% already included as default
                    false ->
                        case parse_loc_key(K) of
                            {Lat, Lon, LN} -> {true, {K, Lat, Lon, LN}};
                            error -> false
                        end
                end
            end, maps:keys(State#state.weather_cache)),
            AllLocs = [{DefaultKey, DefaultLat, DefaultLon, DefaultLocName} | CachedLocs],
            Token = resolve_finnhub_token(Config),
            NeedNews = NewsEnabled andalso needs_refresh(State#state.news_text, undefined),
            case WeatherEnabled orelse NeedNews of
                false ->
                    {noreply, State};
                true ->
                    {_Pid, Ref} = spawn_monitor(fun() ->
                        WeatherMap = case WeatherEnabled of
                            true ->
                                lists:foldl(fun({LK, Lat, Lon, LN}, Acc) ->
                                    case fetch_weather(Lat, Lon, LN) of
                                        {ok, Text} -> Acc#{LK => Text};
                                        {error, _} -> Acc
                                    end
                                end, #{}, AllLocs);
                            false ->
                                #{}
                        end,
                        News = case NeedNews andalso Token =/= undefined of
                            true ->
                                case fetch_news(Token) of
                                    {ok, Text2} -> Text2;
                                    {error, _} -> undefined
                                end;
                            false -> keep
                        end,
                        Self ! {refresh_result, WeatherMap, News}
                    end),
                    {noreply, State#state{worker_ref = Ref}}
            end
    end;
handle_info({refresh_result, WeatherMap, News}, State) when is_map(WeatherMap) ->
    %% Merge fetched weather into cache (new values overwrite old)
    NewCache = maps:merge(State#state.weather_cache, WeatherMap),
    State1 = State#state{weather_cache = NewCache},
    State2 = case News of
        keep      -> State1;
        undefined -> State1;
        _         -> State1#state{news_text = News}
    end,
    {noreply, State2#state{worker_ref = undefined}};
handle_info({fetch_location, LocKey, Lat, Lon, LocName}, State) ->
    %% On-demand async fetch for a new location (cache miss)
    Self = self(),
    spawn(fun() ->
        case fetch_weather(Lat, Lon, LocName) of
            {ok, Text} -> Self ! {location_result, LocKey, Text};
            {error, _} -> ok
        end
    end),
    {noreply, State};
handle_info({location_result, LocKey, Text}, State) ->
    NewCache = (State#state.weather_cache)#{LocKey => Text},
    {noreply, State#state{weather_cache = NewCache}};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{worker_ref = Ref} = State) ->
    %% Worker crashed — clear ref, caches remain unchanged
    {noreply, State#state{worker_ref = undefined}};
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% ---- Section builders ----

build_sections(AgentId, SessionPid, Config, State) ->
    %% Time section (always fresh, pure computation)
    {Timezone, UtcOffset} = resolve_timezone(AgentId, Config),
    TimeSection = format_time_section(Timezone, UtcOffset, erlang:universaltime()),

    %% Idle section (pure computation)
    IdleSection = build_idle_section(SessionPid),

    %% Weather from per-location cache
    {LocName, Lat, Lon} = resolve_location(AgentId, Config),
    LocKey = loc_key(Lat, Lon),
    WeatherSection = case maps:get(LocKey, State#state.weather_cache, undefined) of
        undefined ->
            %% Cache miss — trigger async fetch for this location
            self() ! {fetch_location, LocKey, Lat, Lon, LocName},
            <<>>;
        W -> W
    end,

    %% News from cached state (global, not per-agent)
    NewsSection = case State#state.news_text of
        undefined -> <<>>;
        N         -> N
    end,

    [TimeSection, IdleSection, WeatherSection, NewsSection].

resolve_timezone(AgentId, Config) ->
    %% Config overrides take precedence
    CfgTz = maps:get(timezone, Config, undefined),
    CfgOffset = maps:get(utc_offset_hours, Config, undefined),
    case {CfgTz, CfgOffset} of
        {Tz, Off} when Tz =/= undefined, Off =/= undefined ->
            {iolist_to_binary(io_lib:format("~s", [Tz])), Off};
        {Tz, undefined} when Tz =/= undefined ->
            {iolist_to_binary(io_lib:format("~s", [Tz])), tz_offset(Tz)};
        _ ->
            %% Try to parse from USER.md
            case bc_workspace:read_bootstrap_file(AgentId, <<"USER.md">>) of
                {ok, UserMd} ->
                    case parse_timezone_from_user_md(UserMd) of
                        undefined -> {<<"UTC">>, 0};
                        Tz ->
                            TzBin = iolist_to_binary(io_lib:format("~s", [Tz])),
                            {TzBin, tz_offset(Tz)}
                    end;
                _ ->
                    {<<"UTC">>, 0}
            end
    end.

resolve_location(AgentId, Config) ->
    %% Try agent's USER.md first, fall back to global config
    case bc_workspace:read_bootstrap_file(AgentId, <<"USER.md">>) of
        {ok, UserMd} ->
            case parse_location_from_user_md(UserMd) of
                {LocName, Lat, Lon} -> {LocName, Lat, Lon};
                undefined -> default_location(Config)
            end;
        _ ->
            default_location(Config)
    end.

default_location(Config) ->
    Lat = maps:get(latitude, Config, 59.33),
    Lon = maps:get(longitude, Config, 18.07),
    LocName = to_bin(maps:get(location_name, Config, "Stockholm")),
    {LocName, Lat, Lon}.

-doc "Extract location from USER.md. Returns {LocName, Lat, Lon} or undefined.".
-spec parse_location_from_user_md(binary()) -> {binary(), float(), float()} | undefined.
parse_location_from_user_md(Content) ->
    %% Match **Location:** or **Helyszín:** (Hungarian)
    Pattern = <<"\\*\\*(Location|Helyszín):\\*\\*\\s*(.+)"/utf8>>,
    case re:run(Content, Pattern, [{capture, [2], binary}, unicode]) of
        {match, [Raw]} ->
            parse_location_value(string:trim(Raw));
        nomatch ->
            undefined
    end.

parse_location_value(<<>>) -> undefined;
parse_location_value(<<"(", _/binary>>) -> undefined;  %% placeholder
parse_location_value(Value) ->
    %% Expected format: "CityName, LAT, LON"
    Parts = binary:split(Value, <<",">>, [global]),
    case Parts of
        [CityRaw, LatRaw, LonRaw] ->
            City = string:trim(CityRaw),
            case {parse_float(string:trim(LatRaw)), parse_float(string:trim(LonRaw))} of
                {{ok, Lat}, {ok, Lon}} -> {City, Lat, Lon};
                _ -> undefined
            end;
        _ -> undefined
    end.

parse_float(Bin) ->
    Str = binary_to_list(Bin),
    case string:to_float(Str) of
        {F, []} when is_float(F) -> {ok, F};
        {error, no_float} ->
            %% Try integer
            case string:to_integer(Str) of
                {I, []} when is_integer(I) -> {ok, float(I)};
                _ -> error
            end;
        _ -> error
    end.

loc_key(Lat, Lon) ->
    iolist_to_binary(io_lib:format("~.2f,~.2f", [float(Lat), float(Lon)])).

parse_loc_key(Key) ->
    case binary:split(Key, <<",">>) of
        [LatBin, LonBin] ->
            case {parse_float(LatBin), parse_float(LonBin)} of
                {{ok, Lat}, {ok, Lon}} ->
                    %% Use lat/lon as location name fallback
                    {Lat, Lon, Key};
                _ -> error
            end;
        _ -> error
    end.

build_idle_section(SessionPid) ->
    try
        LastActivity = bc_session:get_last_activity(SessionPid),
        Now = erlang:system_time(second),
        Delta = Now - LastActivity,
        iolist_to_binary([<<"Time since last message: ">>, format_idle_time(Delta)])
    catch _:_ -> <<>>
    end.

resolve_finnhub_token(Config) ->
    case maps:get(finnhub_token, Config, undefined) of
        {env, Var} ->
            case os:getenv(Var) of
                false -> undefined;
                ""    -> undefined;
                Val   -> Val
            end;
        undefined ->
            case os:getenv("FINNHUB_TOKEN") of
                false -> undefined;
                ""    -> undefined;
                Val   -> Val
            end;
        Val when is_list(Val), Val =/= [] -> Val;
        _ -> undefined
    end.

%% ---- HTTP fetchers ----

fetch_weather(Lat, Lon, LocName) ->
    Url = iolist_to_binary([
        <<"https://api.open-meteo.com/v1/forecast?latitude=">>,
        float_to_bin(Lat), <<"&longitude=">>, float_to_bin(Lon),
        <<"&current=temperature_2m,relative_humidity_2m,weather_code&timezone=auto">>
    ]),
    case hackney:request(get, Url,
                         [{<<"User-Agent">>, <<"BeamClaw/0.1">>}],
                         <<>>,
                         [{recv_timeout, 5000}, {connect_timeout, 5000},
                          with_body]) of
        {ok, 200, _Headers, Body} ->
            case parse_weather_json(Body) of
                {ok, Text} ->
                    LocBin = to_bin(LocName),
                    {ok, iolist_to_binary([
                        <<"Weather in ">>, LocBin, <<": ">>, Text
                    ])};
                {error, _} = Err -> Err
            end;
        {ok, Status, _Headers, _Body} ->
            {error, {http_status, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

fetch_news(Token) ->
    Url = iolist_to_binary([
        <<"https://finnhub.io/api/v1/news?category=general&token=">>,
        list_to_binary(Token)
    ]),
    case hackney:request(get, Url,
                         [{<<"Accept">>, <<"application/json">>}],
                         <<>>,
                         [{recv_timeout, 5000}, {connect_timeout, 5000},
                          with_body]) of
        {ok, 200, _Headers, Body} ->
            case parse_news_json(Body) of
                {ok, Text} when Text =/= <<>> -> {ok, Text};
                {ok, <<>>} -> {error, no_news};
                {error, _} = Err -> Err
            end;
        {ok, Status, _Headers, _Body} ->
            {error, {http_status, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

%% ---- Pure functions (exported for testing) ----

-doc "Format seconds-since-last-activity as a human-readable string.".
-spec format_idle_time(non_neg_integer()) -> binary().
format_idle_time(Seconds) when Seconds < 60 ->
    <<"just now">>;
format_idle_time(Seconds) when Seconds < 3600 ->
    Minutes = Seconds div 60,
    case Minutes of
        1 -> <<"1 minute ago">>;
        N -> iolist_to_binary(io_lib:format("~B minutes ago", [N]))
    end;
format_idle_time(Seconds) when Seconds < 86400 ->
    Hours = Seconds div 3600,
    case Hours of
        1 -> <<"1 hour ago">>;
        N -> iolist_to_binary(io_lib:format("~B hours ago", [N]))
    end;
format_idle_time(Seconds) ->
    Days = Seconds div 86400,
    case Days of
        1 -> <<"1 day ago">>;
        N -> iolist_to_binary(io_lib:format("~B days ago", [N]))
    end.

-doc "Format the time section with day-of-week, date, time, and timezone.".
-spec format_time_section(binary(), integer(), calendar:datetime()) -> binary().
format_time_section(TzLabel, UtcOffset, {{Year, Month, Day}, {Hour, Min, _Sec}}) ->
    %% Apply UTC offset
    TotalMinutes = Hour * 60 + Min + UtcOffset * 60,
    %% Handle day rollover (simplified — doesn't adjust date for overflow)
    LocalHour = ((TotalMinutes div 60) rem 24 + 24) rem 24,
    LocalMin  = ((TotalMinutes rem 60) + 60) rem 60,
    DayOfWeek = day_name(calendar:day_of_the_week({Year, Month, Day})),
    MonthName = month_name(Month),
    TzSuffix = format_tz_suffix(TzLabel, UtcOffset),
    iolist_to_binary(io_lib:format(
        "~s, ~s ~B, ~4..0B ~2..0B:~2..0B ~s",
        [DayOfWeek, MonthName, Day, Year, LocalHour, LocalMin, TzSuffix]
    )).

format_tz_suffix(TzLabel, _Offset) when TzLabel =:= <<"UTC">>; TzLabel =:= <<"GMT">> ->
    <<"(UTC)">>;
format_tz_suffix(TzLabel, Offset) ->
    OffsetStr = format_utc_offset(Offset),
    iolist_to_binary([<<"(">>, TzLabel, <<", ">>, OffsetStr, <<")">>]).

format_utc_offset(0) -> <<"UTC+0">>;
format_utc_offset(N) when N > 0 ->
    iolist_to_binary(io_lib:format("UTC+~B", [N]));
format_utc_offset(N) ->
    iolist_to_binary(io_lib:format("UTC~B", [N])).

-doc "Parse Open-Meteo JSON response into a weather summary string.".
-spec parse_weather_json(binary()) -> {ok, binary()} | {error, term()}.
parse_weather_json(Json) ->
    try
        Data = jsx:decode(Json, [return_maps]),
        #{<<"current">> := Current} = Data,
        Temp = maps:get(<<"temperature_2m">>, Current, null),
        Humidity = maps:get(<<"relative_humidity_2m">>, Current, null),
        Code = maps:get(<<"weather_code">>, Current, -1),
        Desc = wmo_description(Code),
        TempStr = case Temp of
            null -> <<"??">>;
            T when is_float(T) -> iolist_to_binary(io_lib:format("~.1f", [T]));
            T when is_integer(T) -> integer_to_binary(T)
        end,
        HumStr = case Humidity of
            null -> <<"??">>;
            H when is_integer(H) -> integer_to_binary(H);
            H when is_float(H) -> integer_to_binary(round(H))
        end,
        {ok, iolist_to_binary(io_lib:format(
            "~s\302\260C, ~s, ~s% humidity",
            [TempStr, Desc, HumStr]
        ))}
    catch _:_ ->
        {error, bad_json}
    end.

-doc "Map WMO weather interpretation codes to human-readable descriptions.".
-spec wmo_description(integer()) -> binary().
wmo_description(0)  -> <<"clear sky">>;
wmo_description(1)  -> <<"mainly clear">>;
wmo_description(2)  -> <<"partly cloudy">>;
wmo_description(3)  -> <<"overcast">>;
wmo_description(45) -> <<"foggy">>;
wmo_description(48) -> <<"foggy">>;
wmo_description(51) -> <<"light drizzle">>;
wmo_description(53) -> <<"drizzle">>;
wmo_description(55) -> <<"heavy drizzle">>;
wmo_description(56) -> <<"freezing drizzle">>;
wmo_description(57) -> <<"freezing drizzle">>;
wmo_description(61) -> <<"light rain">>;
wmo_description(63) -> <<"rain">>;
wmo_description(65) -> <<"heavy rain">>;
wmo_description(66) -> <<"freezing rain">>;
wmo_description(67) -> <<"freezing rain">>;
wmo_description(71) -> <<"light snow">>;
wmo_description(73) -> <<"snow">>;
wmo_description(75) -> <<"heavy snow">>;
wmo_description(77) -> <<"snow grains">>;
wmo_description(80) -> <<"light rain showers">>;
wmo_description(81) -> <<"rain showers">>;
wmo_description(82) -> <<"heavy rain showers">>;
wmo_description(85) -> <<"light snow showers">>;
wmo_description(86) -> <<"heavy snow showers">>;
wmo_description(95) -> <<"thunderstorm">>;
wmo_description(96) -> <<"thunderstorm with hail">>;
wmo_description(99) -> <<"thunderstorm with heavy hail">>;
wmo_description(_)  -> <<"unknown">>.

-doc "Parse Finnhub news JSON response into headline bullet points.".
-spec parse_news_json(binary()) -> {ok, binary()} | {error, term()}.
parse_news_json(Json) ->
    try
        Articles = jsx:decode(Json, [return_maps]),
        case Articles of
            List when is_list(List), length(List) > 0 ->
                Top5 = lists:sublist(List, 5),
                Lines = lists:filtermap(fun(Article) ->
                    case maps:get(<<"headline">>, Article, undefined) of
                        undefined -> false;
                        <<>>      -> false;
                        H         -> {true, iolist_to_binary([<<"- ">>, H])}
                    end
                end, Top5),
                case Lines of
                    [] -> {ok, <<>>};
                    _  -> {ok, iolist_to_binary([
                               <<"Headlines:\n">>,
                               lists:join(<<"\n">>, Lines)
                           ])}
                end;
            _ ->
                {ok, <<>>}
        end
    catch _:_ ->
        {error, bad_json}
    end.

-doc "Extract timezone from USER.md content. Returns undefined if not found.".
-spec parse_timezone_from_user_md(binary()) -> binary() | undefined.
parse_timezone_from_user_md(Content) ->
    case re:run(Content, <<"\\*\\*(Timezone|Időzóna):\\*\\*\\s*(.+)"/utf8>>,
                [{capture, [2], binary}, unicode]) of
        {match, [Tz]} ->
            Trimmed = string:trim(Tz),
            case Trimmed of
                <<>> -> undefined;
                <<"(", _/binary>> -> undefined;  %% placeholder like "(useful for...)"
                _ -> strip_parenthetical(Trimmed)
            end;
        nomatch ->
            undefined
    end.

-doc "Strip trailing parenthetical like 'CET (Central European Time)' → 'CET'.".
strip_parenthetical(Bin) ->
    case re:run(Bin, <<"^(.+?)\\s*\\(.*\\)\\s*$">>, [{capture, [1], binary}]) of
        {match, [Core]} -> string:trim(Core);
        nomatch         -> Bin
    end.

-doc "Map IANA timezone names to UTC offsets (integer hours). Simplified, no DST.".
-spec tz_offset(binary() | string()) -> integer().
tz_offset(Tz) when is_binary(Tz) -> tz_offset(binary_to_list(Tz));
tz_offset("UTC") -> 0;
tz_offset("GMT") -> 0;
tz_offset("UTC+" ++ N) -> safe_int(N, 0);
tz_offset("UTC-" ++ N) -> -safe_int(N, 0);
tz_offset("GMT+" ++ N) -> safe_int(N, 0);
tz_offset("GMT-" ++ N) -> -safe_int(N, 0);
%% Common abbreviations
tz_offset("CET")  -> 1;    tz_offset("CEST") -> 2;
tz_offset("WET")  -> 0;    tz_offset("WEST") -> 1;
tz_offset("EET")  -> 2;    tz_offset("EEST") -> 3;
tz_offset("BST")  -> 1;    tz_offset("IST")  -> 1;
tz_offset("MSK")  -> 3;
tz_offset("EST")  -> -5;   tz_offset("EDT")  -> -4;
tz_offset("CST")  -> -6;   tz_offset("CDT")  -> -5;
tz_offset("MST")  -> -7;   tz_offset("MDT")  -> -6;
tz_offset("PST")  -> -8;   tz_offset("PDT")  -> -7;
tz_offset("AKST") -> -9;   tz_offset("AKDT") -> -8;
tz_offset("HST")  -> -10;  tz_offset("AST")  -> -4;
tz_offset("BRT")  -> -3;
tz_offset("JST")  -> 9;    tz_offset("KST")  -> 9;
tz_offset("HKT")  -> 8;    tz_offset("SGT")  -> 8;
tz_offset("ICT")  -> 7;    tz_offset("WIB")  -> 7;
tz_offset("GST")  -> 4;
tz_offset("AEST") -> 10;   tz_offset("AWST") -> 8;
tz_offset("NZST") -> 12;
%% Europe
tz_offset("Europe/London")     -> 0;
tz_offset("Europe/Dublin")     -> 0;
tz_offset("Europe/Paris")      -> 1;
tz_offset("Europe/Berlin")     -> 1;
tz_offset("Europe/Rome")       -> 1;
tz_offset("Europe/Madrid")     -> 1;
tz_offset("Europe/Amsterdam")  -> 1;
tz_offset("Europe/Brussels")   -> 1;
tz_offset("Europe/Vienna")     -> 1;
tz_offset("Europe/Zurich")     -> 1;
tz_offset("Europe/Warsaw")     -> 1;
tz_offset("Europe/Budapest")   -> 1;
tz_offset("Europe/Prague")     -> 1;
tz_offset("Europe/Stockholm")  -> 1;
tz_offset("Europe/Oslo")       -> 1;
tz_offset("Europe/Copenhagen") -> 1;
tz_offset("Europe/Helsinki")   -> 2;
tz_offset("Europe/Bucharest")  -> 2;
tz_offset("Europe/Athens")     -> 2;
tz_offset("Europe/Istanbul")   -> 3;
tz_offset("Europe/Moscow")     -> 3;
%% Americas
tz_offset("America/New_York")    -> -5;
tz_offset("America/Chicago")     -> -6;
tz_offset("America/Denver")      -> -7;
tz_offset("America/Los_Angeles") -> -8;
tz_offset("America/Anchorage")   -> -9;
tz_offset("America/Sao_Paulo")   -> -3;
tz_offset("America/Toronto")     -> -5;
tz_offset("America/Vancouver")   -> -8;
%% Asia-Pacific
tz_offset("Asia/Tokyo")      -> 9;
tz_offset("Asia/Shanghai")   -> 8;
tz_offset("Asia/Hong_Kong")  -> 8;
tz_offset("Asia/Singapore")  -> 8;
tz_offset("Asia/Seoul")      -> 9;
tz_offset("Asia/Kolkata")    -> 5;  %% actually +5:30, rounded
tz_offset("Asia/Dubai")      -> 4;
tz_offset("Asia/Jerusalem")  -> 2;
tz_offset("Asia/Bangkok")    -> 7;
tz_offset("Asia/Jakarta")    -> 7;
%% Oceania
tz_offset("Australia/Sydney")    -> 10;  %% AEST (no DST)
tz_offset("Australia/Melbourne") -> 10;
tz_offset("Australia/Perth")     -> 8;
tz_offset("Pacific/Auckland")    -> 12;
tz_offset("Pacific/Honolulu")    -> -10;
%% Fallback
tz_offset(_) -> 0.

-doc "Assemble non-empty sections into the final environment block.".
-spec build_env_block([binary()]) -> binary().
build_env_block(Sections) ->
    NonEmpty = [S || S <- Sections, S =/= <<>>],
    case NonEmpty of
        [] -> <<>>;
        _  -> iolist_to_binary([
                  <<"[user:environment]\n">>,
                  lists:join(<<"\n\n">>, NonEmpty)
              ])
    end.

%% ---- Internal helpers ----

needs_refresh(undefined, _) -> true;
needs_refresh(_, _) -> false.

float_to_bin(F) when is_float(F) ->
    iolist_to_binary(io_lib:format("~.2f", [F]));
float_to_bin(I) when is_integer(I) ->
    integer_to_binary(I).

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L)   -> list_to_binary(L);
to_bin(A) when is_atom(A)   -> atom_to_binary(A, utf8).

day_name(1) -> <<"Monday">>;
day_name(2) -> <<"Tuesday">>;
day_name(3) -> <<"Wednesday">>;
day_name(4) -> <<"Thursday">>;
day_name(5) -> <<"Friday">>;
day_name(6) -> <<"Saturday">>;
day_name(7) -> <<"Sunday">>.

month_name(1)  -> <<"January">>;
month_name(2)  -> <<"February">>;
month_name(3)  -> <<"March">>;
month_name(4)  -> <<"April">>;
month_name(5)  -> <<"May">>;
month_name(6)  -> <<"June">>;
month_name(7)  -> <<"July">>;
month_name(8)  -> <<"August">>;
month_name(9)  -> <<"September">>;
month_name(10) -> <<"October">>;
month_name(11) -> <<"November">>;
month_name(12) -> <<"December">>.

generate_id() ->
    <<N:128>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~32.16.0b", [N])).

safe_int(Str, Default) ->
    try list_to_integer(string:trim(Str))
    catch _:_ -> Default
    end.
