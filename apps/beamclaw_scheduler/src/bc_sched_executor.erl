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

-module(bc_sched_executor).
-moduledoc """
Session dispatch and delivery gen_server for the scheduler.

Receives {execute, Job} from bc_sched_runner. Creates or reuses sessions
based on session_mode. Dispatches prompts via bc_session:dispatch/2 with
reply_pid=self(). Receives {bc_done, SessionId, Msg} and routes to the
configured delivery channel.

Handles error counting, auto-pause on max_errors, and heartbeat suppression.
""".
-behaviour(gen_server).

-include("bc_sched_job.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/0]).
-export([execute/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    pending = #{} :: #{binary() => #bc_sched_job{}}  %% session_id => job (awaiting response)
}).

%% ---------------------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc "Execute a scheduled job (called by bc_sched_runner).".
-spec execute(#bc_sched_job{}) -> ok.
execute(Job) ->
    gen_server:cast(?MODULE, {execute, Job}).

%% ---------------------------------------------------------------------------
%% gen_server callbacks
%% ---------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast({execute, Job}, State) ->
    NewState = do_execute(Job, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({bc_done, SessionId, Msg}, State) ->
    case maps:get(SessionId, State#state.pending, undefined) of
        undefined ->
            {noreply, State};
        Job ->
            handle_response(Job, Msg),
            NewPending = maps:remove(SessionId, State#state.pending),
            {noreply, State#state{pending = NewPending}}
    end;

handle_info({bc_turn_complete, _SessionId}, State) ->
    %% Turn complete — isolated sessions cleaned up by session_cleaner TTL
    {noreply, State};

handle_info({bc_chunk, _SessionId, _Chunk}, State) ->
    %% Ignore streaming chunks — we only care about final response
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal — execution
%% ---------------------------------------------------------------------------

do_execute(#bc_sched_job{job_id = JobId} = Job, State) ->
    NowEpoch = erlang:system_time(second),
    try
        SessionId = get_session_id(Job),
        ensure_session(SessionId, Job),
        %% Build the channel message
        ChannelMsg = #bc_channel_message{
            session_id  = SessionId,
            user_id     = Job#bc_sched_job.user_id,
            agent_id    = Job#bc_sched_job.agent_id,
            channel     = scheduler,
            content     = build_prompt(Job),
            raw         = #{scheduler => true, job_id => JobId},
            ts          = NowEpoch,
            reply_pid   = self()
        },
        bc_obs:emit(sched_job_fired, #{
            job_id     => JobId,
            session_id => SessionId,
            heartbeat  => Job#bc_sched_job.heartbeat
        }),
        {ok, SessionPid} = bc_session_registry:lookup(SessionId),
        bc_session:dispatch_run(SessionPid, ChannelMsg),
        ok = bc_sched_store:update_after_fire(JobId, NowEpoch),
        State#state{pending = maps:put(SessionId, Job, State#state.pending)}
    catch
        Class:Reason:Stack ->
            logger:error("[sched_executor] job ~s failed: ~p:~p~n~p",
                         [JobId, Class, Reason, Stack]),
            handle_execution_error(Job, NowEpoch),
            State
    end.

get_session_id(#bc_sched_job{session_mode = shared, session_id = SId})
  when is_binary(SId) ->
    SId;
get_session_id(#bc_sched_job{session_mode = isolated, job_id = JobId}) ->
    %% Generate fresh session ID for isolated mode
    <<N:64>> = crypto:strong_rand_bytes(8),
    Hex = iolist_to_binary(io_lib:format("~16.16.0b", [N])),
    <<"sched-", (binary:part(JobId, 0, 8))/binary, "-", Hex/binary>>;
get_session_id(#bc_sched_job{session_id = undefined} = Job) ->
    %% Shared mode but no session_id stored — use agent default
    get_session_id(Job#bc_sched_job{session_mode = isolated}).

ensure_session(SessionId, Job) ->
    case bc_session_registry:lookup(SessionId) of
        {ok, _Pid} ->
            ok;
        {error, not_found} ->
            Config = #{
                session_id  => SessionId,
                user_id     => Job#bc_sched_job.user_id,
                channel_id  => SessionId,
                channel_mod => undefined,
                agent_id    => Job#bc_sched_job.agent_id
            },
            {ok, _} = bc_sessions_sup:start_session(Config),
            ok
    end.

build_prompt(#bc_sched_job{heartbeat = true, prompt = Prompt}) ->
    <<"[HEARTBEAT CHECK-IN]\n\n", Prompt/binary,
      "\n\nIf everything is fine and there's nothing important to report, "
      "respond with exactly: HEARTBEAT_OK">>;
build_prompt(#bc_sched_job{prompt = Prompt}) ->
    Prompt.

%% ---------------------------------------------------------------------------
%% Internal — response handling
%% ---------------------------------------------------------------------------

handle_response(#bc_sched_job{heartbeat = true, suppress_ok = true} = Job,
                #bc_message{content = Content}) ->
    case is_heartbeat_ok(Content) of
        true ->
            bc_obs:emit(sched_suppressed, #{
                job_id     => Job#bc_sched_job.job_id,
                session_id => get_session_id(Job)
            }),
            ok;
        false ->
            deliver(Job, Content)
    end;
handle_response(Job, #bc_message{content = Content}) ->
    deliver(Job, Content).

is_heartbeat_ok(Content) when is_binary(Content) ->
    Trimmed = string:trim(Content),
    Trimmed =:= <<"HEARTBEAT_OK">> orelse
    Trimmed =:= <<"HEARTBEAT_OK.">>;
is_heartbeat_ok(_) ->
    false.

%% ---------------------------------------------------------------------------
%% Internal — delivery
%% ---------------------------------------------------------------------------

deliver(#bc_sched_job{delivery = #{channel := silent}}, _Content) ->
    ok;

deliver(#bc_sched_job{delivery = #{channel := telegram, chat_id := ChatId}} = Job,
        Content) ->
    try
        SessionId = get_session_id(Job),
        Msg = #bc_message{
            id      = generate_id(),
            role    = assistant,
            content = Content,
            ts      = erlang:system_time(second)
        },
        %% Ensure the session_id → chat_id mapping exists for Telegram routing
        catch ets:insert(bc_telegram_chat_map,
                         {SessionId, integer_to_binary(ChatId)}),
        case whereis(bc_channel_telegram) of
            undefined ->
                logger:warning("[sched_executor] telegram channel not running for job ~s",
                              [Job#bc_sched_job.job_id]);
            _Pid ->
                bc_channel_telegram:send_response(SessionId, Msg)
        end
    catch
        _:Err ->
            logger:error("[sched_executor] telegram delivery failed for job ~s: ~p",
                        [Job#bc_sched_job.job_id, Err])
    end;

deliver(#bc_sched_job{delivery = #{channel := tui}} = Job, Content) ->
    try
        SessionId = get_session_id(Job),
        Msg = #bc_message{
            id      = generate_id(),
            role    = assistant,
            content = Content,
            ts      = erlang:system_time(second)
        },
        case whereis(bc_channel_tui) of
            undefined ->
                logger:warning("[sched_executor] tui channel not running for job ~s",
                              [Job#bc_sched_job.job_id]);
            _Pid ->
                bc_channel_tui:send_response(SessionId, Msg)
        end
    catch
        _:Err ->
            logger:error("[sched_executor] tui delivery failed for job ~s: ~p",
                        [Job#bc_sched_job.job_id, Err])
    end;

deliver(#bc_sched_job{delivery = #{channel := webhook, webhook_url := Url}} = Job,
        Content) ->
    try
        Payload = jsx:encode(#{
            job_id    => Job#bc_sched_job.job_id,
            agent_id  => Job#bc_sched_job.agent_id,
            heartbeat => Job#bc_sched_job.heartbeat,
            content   => Content,
            timestamp => erlang:system_time(second)
        }),
        Headers = [{<<"Content-Type">>, <<"application/json">>}],
        case hackney:request(post, Url, Headers, Payload, [{recv_timeout, 10000}]) of
            {ok, Status, _, _} when Status >= 200, Status < 300 ->
                ok;
            {ok, Status, _, _} ->
                logger:warning("[sched_executor] webhook returned ~p for job ~s",
                              [Status, Job#bc_sched_job.job_id]);
            {error, HttpErr} ->
                logger:error("[sched_executor] webhook failed for job ~s: ~p",
                            [Job#bc_sched_job.job_id, HttpErr])
        end
    catch
        _:WebhookErr ->
            logger:error("[sched_executor] webhook delivery error for job ~s: ~p",
                        [Job#bc_sched_job.job_id, WebhookErr])
    end;

deliver(Job, _Content) ->
    logger:warning("[sched_executor] unknown delivery channel for job ~s: ~p",
                  [Job#bc_sched_job.job_id, Job#bc_sched_job.delivery]).

%% ---------------------------------------------------------------------------
%% Internal — error handling
%% ---------------------------------------------------------------------------

handle_execution_error(#bc_sched_job{job_id = JobId} = _Job, NowEpoch) ->
    case bc_sched_store:update_error(JobId, NowEpoch) of
        {ok, paused} ->
            bc_sched_runner:cancel(JobId),
            bc_obs:emit(sched_job_paused, #{job_id => JobId, reason => auto}),
            bc_obs:emit(sched_job_failed, #{job_id => JobId, error => auto_paused});
        {ok, active} ->
            bc_obs:emit(sched_job_failed, #{job_id => JobId, error => execution_error});
        {error, not_found} ->
            ok
    end.

generate_id() ->
    <<N:128>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("sched-~32.16.0b", [N])).
