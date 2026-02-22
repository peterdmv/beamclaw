%% @doc Session gen_server — the "lane."
%%
%% Permanent process. Holds conversation history and serializes runs via a
%% pending_runs queue. When the loop is busy, incoming runs are enqueued.
%% When the loop completes (turn_complete), the next run is dispatched.
%%
%% History survives loop crashes because bc_session is permanent and bc_loop
%% is transient. On bc_loop restart, bc_loop:init/1 looks up this process via
%% bc_session_registry, fetches history, and announces its new PID here.
-module(bc_session).
-behaviour(gen_server).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/1,
         dispatch_run/2,
         get_history/1,
         set_history/2,
         get_channel_mod/1,
         get_agent_id/1,
         append_message/2,
         set_loop_pid/2,
         turn_complete/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    session_id    :: binary(),
    user_id       :: binary(),
    channel_id    :: binary(),
    agent_id      :: binary(),
    autonomy      :: autonomy_level(),
    loop_pid      :: pid() | undefined,
    loop_busy     :: boolean(),          %% true while a run is in progress
    channel_mod   :: module() | undefined,
    provider_mod  :: module(),
    memory_mod    :: module(),
    history       :: [#bc_message{}],
    pending_runs  :: queue:queue(),
    config        :: map()
}).

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Dispatch a new user run (message) to this session.
-spec dispatch_run(Pid :: pid(), Message :: #bc_channel_message{}) -> ok.
dispatch_run(Pid, Message) ->
    gen_server:cast(Pid, {dispatch_run, Message}).

%% @doc Retrieve the full conversation history.
-spec get_history(Pid :: pid()) -> [#bc_message{}].
get_history(Pid) ->
    gen_server:call(Pid, get_history).

%% @doc Replace the full conversation history (called by bc_compactor).
-spec set_history(Pid :: pid(), History :: [#bc_message{}]) -> ok.
set_history(Pid, History) ->
    gen_server:cast(Pid, {set_history, History}).

%% @doc Return the channel module for this session (called by bc_loop on init).
-spec get_channel_mod(Pid :: pid()) -> module() | undefined.
get_channel_mod(Pid) ->
    gen_server:call(Pid, get_channel_mod).

%% @doc Return the agent ID for this session.
-spec get_agent_id(Pid :: pid()) -> binary().
get_agent_id(Pid) ->
    gen_server:call(Pid, get_agent_id).

%% @doc Append a single message to history (called by bc_loop).
-spec append_message(Pid :: pid(), Message :: #bc_message{}) -> ok.
append_message(Pid, Message) ->
    gen_server:cast(Pid, {append_message, Message}).

%% @doc Update the loop pid reference. Called by bc_loop:init/1.
%% Drains the pending_runs queue if messages arrived before the loop started.
-spec set_loop_pid(Pid :: pid(), LoopPid :: pid()) -> ok.
set_loop_pid(Pid, LoopPid) ->
    gen_server:cast(Pid, {set_loop_pid, LoopPid}).

%% @doc Called by bc_loop when a turn completes. Dequeues next run if any.
-spec turn_complete(Pid :: pid(), Result :: term()) -> ok.
turn_complete(Pid, Result) ->
    gen_server:cast(Pid, {turn_complete, Result}).

init(Config) ->
    SessionId = maps:get(session_id, Config, generate_id()),
    bc_session_registry:register(SessionId, self()),
    bc_obs:emit(session_start, #{session_id => SessionId}),
    DefaultAgent = bc_config:get(beamclaw_core, default_agent, <<"default">>),
    State = #state{
        session_id   = SessionId,
        user_id      = maps:get(user_id,      Config, <<"anonymous">>),
        channel_id   = maps:get(channel_id,   Config, <<"default">>),
        agent_id     = maps:get(agent_id,     Config, DefaultAgent),
        autonomy     = maps:get(autonomy,      Config,
                           bc_config:get(beamclaw_core, autonomy_level, supervised)),
        loop_pid     = undefined,
        loop_busy    = false,
        channel_mod  = maps:get(channel_mod,  Config, undefined),
        provider_mod = maps:get(provider_mod, Config, bc_provider_openrouter),
        memory_mod   = maps:get(memory_mod,   Config, bc_memory_ets),
        history      = [],
        pending_runs = queue:new(),
        config       = Config
    },
    {ok, State}.

%% Loop not yet started (startup race) — enqueue for safety.
handle_cast({dispatch_run, Message}, #state{loop_pid = undefined} = State) ->
    NewQueue = queue:in(Message, State#state.pending_runs),
    {noreply, State#state{pending_runs = NewQueue}};

%% Loop idle — dispatch directly and mark busy.
handle_cast({dispatch_run, Message}, #state{loop_busy = false} = State) ->
    gen_statem:cast(State#state.loop_pid, {run, Message}),
    {noreply, State#state{loop_busy = true}};

%% Loop busy — enqueue.
handle_cast({dispatch_run, Message}, State) ->
    NewQueue = queue:in(Message, State#state.pending_runs),
    {noreply, State#state{pending_runs = NewQueue}};

%% Loop announced its PID — drain any queued messages that arrived during startup.
handle_cast({set_loop_pid, Pid}, State) ->
    NewState = State#state{loop_pid = Pid},
    case queue:out(NewState#state.pending_runs) of
        {{value, Msg}, Rest} ->
            gen_statem:cast(Pid, {run, Msg}),
            {noreply, NewState#state{pending_runs = Rest, loop_busy = true}};
        {empty, _} ->
            {noreply, NewState#state{loop_busy = false}}
    end;

handle_cast({set_history, NewHistory}, State) ->
    {noreply, State#state{history = NewHistory}};

handle_cast({append_message, Msg}, State) ->
    {noreply, State#state{history = State#state.history ++ [Msg]}};

handle_cast({turn_complete, _Result}, State) ->
    case queue:out(State#state.pending_runs) of
        {{value, NextMsg}, Rest} ->
            %% Keep loop_pid; dispatch next queued run. Loop stays busy.
            gen_statem:cast(State#state.loop_pid, {run, NextMsg}),
            {noreply, State#state{pending_runs = Rest}};
        {empty, _} ->
            %% No pending runs — loop becomes idle (pid retained for next run).
            {noreply, State#state{loop_busy = false}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_history, _From, State) ->
    {reply, State#state.history, State};
handle_call(get_channel_mod, _From, State) ->
    {reply, State#state.channel_mod, State};
handle_call(get_agent_id, _From, State) ->
    {reply, State#state.agent_id, State};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    bc_obs:emit(session_end, #{session_id => State#state.session_id,
                               reason => Reason}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

generate_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format(
        "~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
        [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E])).
