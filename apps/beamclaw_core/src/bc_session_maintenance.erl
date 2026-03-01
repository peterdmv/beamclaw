%%
%% Copyright Peter Dimitrov 2026, All Rights Reserved.
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

-module(bc_session_maintenance).
-moduledoc """
Periodic session maintenance gen_server.

Three maintenance tasks on active sessions:
  1. Idle compaction — compact sessions idle > N minutes above a token threshold
  2. Nightly maintenance — aggressive flush + compact during quiet hours (once/day)
  3. Pre-expiry flush — extract memories before TTL deletes a session

All operations are opt-in via the `maintenance` config key in `beamclaw_core`.
Each session operation is wrapped in try/catch — a failure on one session
doesn't prevent processing others. Sequential processing limits API concurrency
to 1 concurrent LLM call.
""".
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Exported for testing
-export([get_config/0, scan_idle_sessions/1, scan_pre_expiry/1,
         should_run_nightly/2]).

-define(DEFAULT_SCAN_INTERVAL_MS, 300000).  %% 5 minutes

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Cfg = get_config(),
    case maps:get(enabled, Cfg, false) of
        true ->
            IntervalMs = maps:get(scan_interval_ms, Cfg, ?DEFAULT_SCAN_INTERVAL_MS),
            erlang:send_after(IntervalMs, self(), scan_tick),
            logger:info("[maintenance] started: interval=~Bms", [IntervalMs]),
            {ok, #{scan_interval_ms => IntervalMs,
                   nightly_done_date => undefined}};
        false ->
            logger:debug("[maintenance] disabled"),
            {ok, #{enabled => false}}
    end.

handle_info(scan_tick, #{enabled := false} = State) ->
    {noreply, State};
handle_info(scan_tick, State) ->
    Cfg = get_config(),
    IntervalMs = maps:get(scan_interval_ms, Cfg, ?DEFAULT_SCAN_INTERVAL_MS),
    %% Run maintenance tasks
    scan_idle_sessions(Cfg),
    NewState = maybe_run_nightly(Cfg, State),
    scan_pre_expiry(Cfg),
    erlang:send_after(IntervalMs, self(), scan_tick),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---- Configuration ----

-spec get_config() -> map().
get_config() ->
    bc_config:get(beamclaw_core, maintenance, #{}).

%% ---- Idle Session Compaction ----

-spec scan_idle_sessions(map()) -> ok.
scan_idle_sessions(Cfg) ->
    IdleMinutes = maps:get(idle_compaction_minutes, Cfg, 15),
    ThresholdPct = maps:get(idle_compaction_threshold_pct, Cfg, 20),
    TargetPct = maps:get(idle_compaction_target_pct, Cfg, 10),
    Now = erlang:system_time(second),
    Sessions = bc_session_registry:all(),
    lists:foreach(fun({_SId, Pid}) ->
        try
            Summary = bc_session:get_state_summary(Pid),
            IdleSec = Now - maps:get(last_activity, Summary, Now),
            case IdleSec > IdleMinutes * 60
                 andalso not maps:get(loop_busy, Summary, true)
                 andalso maps:get(history_len, Summary, 0) > 2 of
                true ->
                    maybe_compact_idle(Pid, Summary, ThresholdPct, TargetPct);
                false ->
                    ok
            end
        catch _:_ -> ok
        end
    end, Sessions).

maybe_compact_idle(Pid, Summary, ThresholdPct, TargetPct) ->
    History = bc_session:get_history(Pid),
    ProvMod = maps:get(provider_mod, Summary, bc_provider_openrouter),
    Model = bc_context:get_model_name(ProvMod),
    Window = bc_context:context_window(Model),
    Tokens = bc_context:estimate_history_tokens(History),
    Threshold = Window * ThresholdPct div 100,
    SessionId = maps:get(session_id, Summary, <<"unknown">>),
    case Tokens > Threshold of
        true ->
            logger:info("[maintenance] idle compaction: session=~s tokens=~B "
                        "threshold=~B", [SessionId, Tokens, Threshold]),
            bc_memory_flush:run(Pid),
            bc_compactor:compact(Pid, #{target_pct => TargetPct}),
            bc_obs:emit(maintenance_compact_complete,
                        #{session_id => SessionId, reason => idle});
        false ->
            ok
    end.

%% ---- Nightly Maintenance ----

maybe_run_nightly(Cfg, State) ->
    QuietHours = maps:get(quiet_hours, Cfg, {2, 4}),
    NightlyDone = maps:get(nightly_done_date, State, undefined),
    case should_run_nightly(QuietHours, NightlyDone) of
        true ->
            run_nightly(Cfg),
            Today = today_date(),
            State#{nightly_done_date => Today};
        false ->
            State
    end.

-spec should_run_nightly({non_neg_integer(), non_neg_integer()},
                         calendar:date() | undefined) -> boolean().
should_run_nightly({StartHour, EndHour}, DoneDate) ->
    {{Y, M, D}, {Hour, _, _}} = calendar:universal_time(),
    Today = {Y, M, D},
    InRange = Hour >= StartHour andalso Hour < EndHour,
    NotDoneToday = DoneDate =/= Today,
    InRange andalso NotDoneToday.

run_nightly(Cfg) ->
    MinMessages = maps:get(nightly_min_messages, Cfg, 10),
    TargetPct = maps:get(idle_compaction_target_pct, Cfg, 10),
    Sessions = bc_session_registry:all(),
    bc_obs:emit(maintenance_nightly_start,
                #{session_count => length(Sessions)}),
    FlushedCount = lists:foldl(fun({_SId, Pid}, Acc) ->
        try
            case bc_session:is_busy(Pid) of
                true -> Acc;
                false ->
                    History = bc_session:get_history(Pid),
                    case length(History) >= MinMessages of
                        true ->
                            SessionId = bc_session:get_session_id(Pid),
                            logger:info("[maintenance] nightly flush: session=~s",
                                        [SessionId]),
                            bc_memory_flush:run(Pid),
                            bc_compactor:compact(Pid, #{target_pct => TargetPct}),
                            bc_obs:emit(maintenance_compact_complete,
                                        #{session_id => SessionId,
                                          reason => nightly}),
                            Acc + 1;
                        false ->
                            Acc
                    end
            end
        catch _:_ -> Acc
        end
    end, 0, Sessions),
    bc_obs:emit(maintenance_nightly_complete,
                #{flushed_count => FlushedCount}),
    ok.

%% ---- Pre-Expiry Memory Extraction ----

-spec scan_pre_expiry(map()) -> ok.
scan_pre_expiry(Cfg) ->
    TTL = bc_config:get(beamclaw_core, session_ttl_seconds, 3600),
    PreExpiryMinutes = maps:get(pre_expiry_minutes, Cfg, 10),
    PreExpiryWindow = PreExpiryMinutes * 60,
    Now = erlang:system_time(second),
    Sessions = bc_session_registry:all(),
    lists:foreach(fun({_SId, Pid}) ->
        try
            Summary = bc_session:get_state_summary(Pid),
            IdleSec = Now - maps:get(last_activity, Summary, Now),
            case IdleSec > (TTL - PreExpiryWindow)
                 andalso not maps:get(loop_busy, Summary, true)
                 andalso maps:get(history_len, Summary, 0) >= 5 of
                true ->
                    SessionId = maps:get(session_id, Summary, <<"unknown">>),
                    logger:info("[maintenance] pre-expiry flush: session=~s "
                                "idle=~Bs ttl=~Bs", [SessionId, IdleSec, TTL]),
                    bc_memory_flush:run(Pid),
                    bc_obs:emit(maintenance_pre_expiry_flush,
                                #{session_id => SessionId});
                false ->
                    ok
            end
        catch _:_ -> ok
        end
    end, Sessions).

%% ---- Helpers ----

today_date() ->
    {Date, _Time} = calendar:universal_time(),
    Date.
