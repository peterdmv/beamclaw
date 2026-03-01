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

-module(bc_tool_scheduler).
-moduledoc """
Built-in tool: create and manage scheduled tasks and heartbeat check-ins.

The agent can create one-shot, recurring, or randomly-timed scheduled jobs
that fire prompts to the LLM and deliver results via Telegram, TUI, webhook,
or silently.

Actions: create, list, cancel, pause, resume.

Requires beamclaw_scheduler to be enabled in configuration.
""".
-behaviour(bc_tool).

-include("bc_sched_job.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"scheduler">>,
      description => <<"Create and manage scheduled tasks and heartbeat check-ins. "
                       "Actions: create (new job), list (show jobs), "
                       "cancel (stop a job), pause, resume.">>,
      parameters  => #{
          type       => object,
          properties => #{
              action => #{type => string,
                         description => <<"Action to perform: create, list, cancel, "
                                         "pause, resume">>},
              schedule_type => #{type => string,
                                description => <<"Schedule type: 'at' (one-shot at datetime), "
                                                "'every' (fixed interval), 'random_in' "
                                                "(N random times per interval)">>},
              datetime => #{type => string,
                           description => <<"ISO 8601 datetime for 'at' type "
                                           "(e.g., '2026-03-01T14:00:00Z')">>},
              interval => #{type => string,
                           description => <<"Human-friendly interval for 'every' or "
                                           "'random_in' type (e.g., '30m', '2h', '24h', '7d')">>},
              count => #{type => integer,
                        description => <<"Number of random firings per interval "
                                        "(for 'random_in' type)">>},
              prompt => #{type => string,
                         description => <<"The message/instruction for the LLM when the "
                                         "job fires">>},
              session_mode => #{type => string,
                               description => <<"'shared' (reuse current session with full "
                                               "history) or 'isolated' (clean slate each time). "
                                               "Default: shared">>},
              channel => #{type => string,
                          description => <<"Delivery channel: 'telegram', 'tui', 'webhook', "
                                          "'silent'. Default: inferred from current channel">>},
              webhook_url => #{type => string,
                              description => <<"URL for webhook delivery (required when "
                                              "channel is 'webhook')">>},
              heartbeat => #{type => boolean,
                            description => <<"If true, this is a heartbeat check-in. "
                                            "The LLM can respond with HEARTBEAT_OK to suppress "
                                            "delivery. Default: false">>},
              active_hours => #{type => string,
                               description => <<"UTC hour range when the job should fire "
                                               "(e.g., '8-22'). Outside this range, firings "
                                               "are silently skipped. Default: always active">>},
              job_id => #{type => string,
                         description => <<"Job ID for cancel, pause, resume actions">>}
          },
          required => [<<"action">>]
      },
      source => builtin}.

execute(Args, SessionRef, _Context) ->
    case application:get_env(beamclaw_scheduler, enabled, false) of
        false ->
            {error, <<"Scheduler is not enabled. Set {beamclaw_scheduler, [{enabled, true}]} "
                      "in config.">>};
        true ->
            Action = maps:get(<<"action">>, Args, <<>>),
            do_action(Action, Args, SessionRef)
    end.

requires_approval() -> true.

min_autonomy() -> supervised.

%% ---------------------------------------------------------------------------
%% Actions
%% ---------------------------------------------------------------------------

do_action(<<"create">>, Args, SessionRef) ->
    create_job(Args, SessionRef);
do_action(<<"list">>, _Args, SessionRef) ->
    list_jobs(SessionRef);
do_action(<<"cancel">>, Args, _SessionRef) ->
    cancel_job(Args);
do_action(<<"pause">>, Args, _SessionRef) ->
    pause_job(Args);
do_action(<<"resume">>, Args, _SessionRef) ->
    resume_job(Args);
do_action(<<>>, _, _) ->
    {error, <<"'action' parameter is required">>};
do_action(Other, _, _) ->
    {error, <<"Unknown action: '", Other/binary, "'. "
              "Valid actions: create, list, cancel, pause, resume">>}.

%% ---------------------------------------------------------------------------
%% Create
%% ---------------------------------------------------------------------------

create_job(Args, SessionRef) ->
    case validate_create(Args) of
        {error, _} = Err ->
            Err;
        {ok, ScheduleType, ScheduleSpec} ->
            SessionId = element(2, SessionRef),  %% #bc_session_ref.session_id
            UserId    = element(3, SessionRef),   %% #bc_session_ref.user_id
            AgentId   = element(6, SessionRef),   %% #bc_session_ref.agent_id

            Prompt       = maps:get(<<"prompt">>, Args, <<"Check in with the user.">>),
            SessionMode  = parse_session_mode(maps:get(<<"session_mode">>, Args, <<"shared">>)),
            Heartbeat    = maps:get(<<"heartbeat">>, Args, false),
            ActiveHours  = parse_active_hours(maps:get(<<"active_hours">>, Args, undefined)),
            Delivery     = build_delivery(Args, SessionRef),

            DefaultMaxErrors = application:get_env(beamclaw_scheduler, max_errors, 3),
            DefaultAutonomy  = application:get_env(beamclaw_scheduler, default_autonomy,
                                                    supervised),

            %% Check job limit
            MaxJobs = application:get_env(beamclaw_scheduler, max_jobs_per_agent, 50),
            Existing = bc_sched_store:list_by_agent(AgentId),
            case length(Existing) >= MaxJobs of
                true ->
                    {error, iolist_to_binary(
                        io_lib:format("Maximum jobs per agent (~B) reached", [MaxJobs]))};
                false ->
                    JobId = generate_job_id(),
                    Now = erlang:system_time(second),

                    %% For heartbeat jobs, apply defaults from config
                    {SuppressOk, AH} = case Heartbeat of
                        true ->
                            HBConfig = application:get_env(beamclaw_scheduler, heartbeat, #{}),
                            SO = maps:get(suppress_ok, HBConfig, true),
                            DefaultAH = maps:get(active_hours, HBConfig, undefined),
                            AHFinal = case ActiveHours of
                                undefined -> DefaultAH;
                                _ -> ActiveHours
                            end,
                            {SO, AHFinal};
                        false ->
                            {false, ActiveHours}
                    end,

                    Job = #bc_sched_job{
                        job_id        = JobId,
                        agent_id      = AgentId,
                        user_id       = UserId,
                        session_id    = case SessionMode of
                                            shared -> SessionId;
                                            isolated -> undefined
                                        end,
                        schedule_type = ScheduleType,
                        schedule_spec = ScheduleSpec,
                        prompt        = Prompt,
                        session_mode  = SessionMode,
                        autonomy      = DefaultAutonomy,
                        delivery      = Delivery,
                        status        = active,
                        created_at    = Now,
                        updated_at    = Now,
                        last_fired_at = undefined,
                        fire_count    = 0,
                        error_count   = 0,
                        max_errors    = DefaultMaxErrors,
                        heartbeat     = Heartbeat,
                        suppress_ok   = SuppressOk,
                        active_hours  = AH
                    },
                    ok = bc_sched_store:save(Job),
                    bc_sched_runner:schedule(Job),
                    bc_obs:emit(sched_job_created, #{
                        job_id        => JobId,
                        schedule_type => ScheduleType,
                        agent_id      => AgentId
                    }),
                    {ok, iolist_to_binary(io_lib:format(
                        "Scheduled job created.\n"
                        "Job ID: ~s\n"
                        "Type: ~p\n"
                        "Heartbeat: ~p\n"
                        "Mode: ~p\n"
                        "Channel: ~p",
                        [JobId, ScheduleType, Heartbeat, SessionMode,
                         maps:get(channel, Delivery, unknown)]))}
            end
    end.

validate_create(Args) ->
    case maps:get(<<"schedule_type">>, Args, undefined) of
        undefined ->
            {error, <<"'schedule_type' is required: at, every, or random_in">>};
        <<"at">> ->
            validate_at(Args);
        <<"every">> ->
            validate_every(Args);
        <<"random_in">> ->
            validate_random_in(Args);
        Other ->
            {error, <<"Unknown schedule_type: '", Other/binary, "'. "
                      "Use: at, every, or random_in">>}
    end.

validate_at(Args) ->
    case maps:get(<<"datetime">>, Args, undefined) of
        undefined ->
            {error, <<"'datetime' is required for 'at' schedule type">>};
        DateStr when is_binary(DateStr) ->
            case parse_iso8601(DateStr) of
                {ok, EpochSec} ->
                    {ok, at, #{epoch_seconds => EpochSec}};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, <<"'datetime' must be an ISO 8601 string">>}
    end.

validate_every(Args) ->
    case maps:get(<<"interval">>, Args, undefined) of
        undefined ->
            {error, <<"'interval' is required for 'every' schedule type">>};
        Interval ->
            case bc_sched_interval:parse(Interval) of
                {ok, Ms} ->
                    {ok, every, #{interval_ms => Ms}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

validate_random_in(Args) ->
    case maps:get(<<"interval">>, Args, undefined) of
        undefined ->
            {error, <<"'interval' is required for 'random_in' schedule type">>};
        Interval ->
            case bc_sched_interval:parse(Interval) of
                {ok, Ms} ->
                    Count = maps:get(<<"count">>, Args, 1),
                    case is_integer(Count) andalso Count > 0 of
                        true ->
                            {ok, random_in, #{interval_ms => Ms, count => Count}};
                        false ->
                            {error, <<"'count' must be a positive integer">>}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% ---------------------------------------------------------------------------
%% List
%% ---------------------------------------------------------------------------

list_jobs(SessionRef) ->
    AgentId = element(6, SessionRef),
    Jobs = bc_sched_store:list_by_agent(AgentId),
    case Jobs of
        [] ->
            {ok, <<"No scheduled jobs found.">>};
        _ ->
            Lines = lists:map(fun format_job/1, Jobs),
            {ok, iolist_to_binary(lists:join(<<"\n\n">>, Lines))}
    end.

format_job(#bc_sched_job{} = J) ->
    iolist_to_binary(io_lib:format(
        "**~s**\n"
        "  Type: ~p | Status: ~p | Heartbeat: ~p\n"
        "  Prompt: ~s\n"
        "  Fires: ~B | Errors: ~B | Last: ~s",
        [J#bc_sched_job.job_id,
         J#bc_sched_job.schedule_type,
         J#bc_sched_job.status,
         J#bc_sched_job.heartbeat,
         truncate(J#bc_sched_job.prompt, 60),
         J#bc_sched_job.fire_count,
         J#bc_sched_job.error_count,
         format_timestamp(J#bc_sched_job.last_fired_at)])).

truncate(Bin, MaxLen) when byte_size(Bin) > MaxLen ->
    <<Prefix:MaxLen/binary, _/binary>> = Bin,
    <<Prefix/binary, "...">>;
truncate(Bin, _) ->
    Bin.

format_timestamp(undefined) -> <<"never">>;
format_timestamp(Epoch) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(
        Epoch + 62167219200),  %% Unix epoch offset
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC",
                                   [Y, Mo, D, H, Mi, S])).

%% ---------------------------------------------------------------------------
%% Cancel / Pause / Resume
%% ---------------------------------------------------------------------------

cancel_job(Args) ->
    case maps:get(<<"job_id">>, Args, undefined) of
        undefined ->
            {error, <<"'job_id' is required for cancel action">>};
        JobId ->
            bc_sched_runner:cancel(JobId),
            case bc_sched_store:update_status(JobId, completed) of
                ok ->
                    bc_obs:emit(sched_job_completed, #{job_id => JobId}),
                    {ok, <<"Job ", JobId/binary, " cancelled.">>};
                {error, not_found} ->
                    {error, <<"Job not found: ", JobId/binary>>}
            end
    end.

pause_job(Args) ->
    case maps:get(<<"job_id">>, Args, undefined) of
        undefined ->
            {error, <<"'job_id' is required for pause action">>};
        JobId ->
            case bc_sched_runner:pause(JobId) of
                ok ->
                    {ok, <<"Job ", JobId/binary, " paused.">>};
                {error, not_found} ->
                    {error, <<"Job not found: ", JobId/binary>>};
                {error, _} ->
                    {error, <<"Job ", JobId/binary, " is not active">>}
            end
    end.

resume_job(Args) ->
    case maps:get(<<"job_id">>, Args, undefined) of
        undefined ->
            {error, <<"'job_id' is required for resume action">>};
        JobId ->
            case bc_sched_runner:resume(JobId) of
                ok ->
                    {ok, <<"Job ", JobId/binary, " resumed.">>};
                {error, not_found} ->
                    {error, <<"Job not found: ", JobId/binary>>};
                {error, _} ->
                    {error, <<"Job ", JobId/binary, " is not paused">>}
            end
    end.

%% ---------------------------------------------------------------------------
%% Internal — helpers
%% ---------------------------------------------------------------------------

parse_session_mode(<<"isolated">>) -> isolated;
parse_session_mode(_) -> shared.

parse_active_hours(undefined) -> undefined;
parse_active_hours(Bin) when is_binary(Bin) ->
    case binary:split(Bin, <<"-">>) of
        [StartBin, EndBin] ->
            try
                Start = binary_to_integer(StartBin),
                End = binary_to_integer(EndBin),
                case Start >= 0 andalso Start =< 23 andalso
                     End >= 0 andalso End =< 23 of
                    true -> {Start, End};
                    false -> undefined
                end
            catch _:_ -> undefined
            end;
        _ -> undefined
    end;
parse_active_hours(_) -> undefined.

build_delivery(Args, SessionRef) ->
    Channel = case maps:get(<<"channel">>, Args, undefined) of
        undefined -> infer_channel(SessionRef);
        <<"telegram">> -> telegram;
        <<"tui">>      -> tui;
        <<"webhook">>  -> webhook;
        <<"silent">>   -> silent;
        _              -> infer_channel(SessionRef)
    end,
    Base = #{channel => Channel},
    D1 = case Channel of
        telegram ->
            %% Try to get chat_id from the Telegram ETS mapping
            SessionId = element(2, SessionRef),
            ChatId = case catch ets:lookup(bc_telegram_chat_map, SessionId) of
                [{_, CId}] -> binary_to_integer(CId);
                _ -> 0
            end,
            Base#{chat_id => ChatId};
        webhook ->
            Base#{webhook_url => maps:get(<<"webhook_url">>, Args, <<>>)};
        _ ->
            Base
    end,
    D1.

infer_channel(_SessionRef) ->
    %% Default to silent — the agent should explicitly choose
    silent.

parse_iso8601(Bin) ->
    %% Parse ISO 8601 datetime string to Unix epoch seconds
    %% Supports: "2026-03-01T14:00:00Z" and "2026-03-01T14:00:00"
    try
        Str = binary_to_list(Bin),
        %% Remove trailing Z if present
        Clean = case lists:last(Str) of
            $Z -> lists:droplast(Str);
            _ -> Str
        end,
        case string:split(Clean, "T") of
            [DateStr, TimeStr] ->
                [YS, MoS, DS] = string:split(DateStr, "-", all),
                TimeParts = string:split(TimeStr, ":", all),
                {HS, MiS, SecS} = case TimeParts of
                    [H0, Mi0, S0] -> {H0, Mi0, S0};
                    [H0, Mi0]     -> {H0, Mi0, "0"};
                    [H0]          -> {H0, "0", "0"}
                end,
                Y  = list_to_integer(YS),
                Mo = list_to_integer(MoS),
                D  = list_to_integer(DS),
                H  = list_to_integer(HS),
                Mi = list_to_integer(MiS),
                Sec = list_to_integer(SecS),
                GregSec = calendar:datetime_to_gregorian_seconds({{Y, Mo, D}, {H, Mi, Sec}}),
                EpochSec = GregSec - 62167219200,  %% Unix epoch offset
                {ok, EpochSec};
            _ ->
                {error, <<"Invalid datetime format. Use ISO 8601: YYYY-MM-DDTHH:MM:SSZ">>}
        end
    catch
        _:_ ->
            {error, <<"Failed to parse datetime. Use ISO 8601: YYYY-MM-DDTHH:MM:SSZ">>}
    end.

generate_job_id() ->
    <<N:128>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~32.16.0b", [N])).
