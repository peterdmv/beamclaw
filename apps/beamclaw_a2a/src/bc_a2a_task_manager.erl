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
-module(bc_a2a_task_manager).
-moduledoc """
A2A Task Manager — ETS-backed task store with agent loop bridge.

Each inbound A2A message creates a task which is dispatched to a BeamClaw
session via bc_session_registry. Task state is updated as the agentic loop
processes the request.
""".
-behaviour(gen_server).

-include("bc_a2a_types.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

-export([start_link/0,
         create_task/1, create_task/2,
         get_task/1,
         cancel_task/1,
         update_task/2, update_task/3,
         add_artifact/2,
         list_tasks/0, list_tasks/1,
         task_id_for_session/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(TABLE, bc_a2a_tasks).
-define(SESSION_TABLE, bc_a2a_sessions).

%% --- Public API ---

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec create_task(#a2a_message{}) -> {ok, #a2a_task{}} | {error, term()}.
create_task(Message) ->
    create_task(Message, #{}).

-spec create_task(#a2a_message{}, map()) -> {ok, #a2a_task{}} | {error, term()}.
create_task(Message, Metadata) ->
    gen_server:call(?MODULE, {create_task, Message, Metadata}).

-spec get_task(binary()) -> {ok, #a2a_task{}} | {error, not_found}.
get_task(TaskId) ->
    case ets:lookup(?TABLE, TaskId) of
        [{_, Task}] -> {ok, Task};
        []          -> {error, not_found}
    end.

-spec cancel_task(binary()) -> {ok, #a2a_task{}} | {error, term()}.
cancel_task(TaskId) ->
    gen_server:call(?MODULE, {cancel_task, TaskId}).

-spec update_task(binary(), a2a_task_state()) -> {ok, #a2a_task{}} | {error, term()}.
update_task(TaskId, NewState) ->
    update_task(TaskId, NewState, undefined).

-spec update_task(binary(), a2a_task_state(), #a2a_message{} | undefined) ->
    {ok, #a2a_task{}} | {error, term()}.
update_task(TaskId, NewState, Message) ->
    gen_server:call(?MODULE, {update_task, TaskId, NewState, Message}).

-spec add_artifact(binary(), #a2a_artifact{}) -> {ok, #a2a_task{}} | {error, term()}.
add_artifact(TaskId, Artifact) ->
    gen_server:call(?MODULE, {add_artifact, TaskId, Artifact}).

-spec list_tasks() -> [#a2a_task{}].
list_tasks() ->
    list_tasks(#{}).

-spec list_tasks(map()) -> [#a2a_task{}].
list_tasks(Opts) ->
    ContextId = maps:get(context_id, Opts, undefined),
    Status = maps:get(status, Opts, undefined),
    Limit = maps:get(limit, Opts, 50),
    Tasks = [T || {_, T} <- ets:tab2list(?TABLE)],
    Filtered = filter_context(filter_status(Tasks, Status), ContextId),
    Sorted = lists:sort(fun(A, B) ->
        (A#a2a_task.status)#a2a_status.timestamp >=
        (B#a2a_task.status)#a2a_status.timestamp
    end, Filtered),
    lists:sublist(Sorted, Limit).

-spec task_id_for_session(binary()) -> {ok, binary()} | {error, not_found}.
task_id_for_session(SessionId) ->
    case ets:lookup(?SESSION_TABLE, SessionId) of
        [{_, TaskId}] -> {ok, TaskId};
        []            -> {error, not_found}
    end.

%% --- gen_server callbacks ---

init([]) ->
    ets:new(?TABLE, [named_table, set, protected, {read_concurrency, true}]),
    ets:new(?SESSION_TABLE, [named_table, set, protected, {read_concurrency, true}]),
    {ok, #{}}.

handle_call({create_task, Message, Metadata}, _From, State) ->
    Task = bc_a2a_task:new_with_metadata(Message, Metadata),
    ets:insert(?TABLE, {Task#a2a_task.id, Task}),
    case bc_a2a_task:transition(Task, working) of
        {ok, Working} ->
            ets:insert(?TABLE, {Working#a2a_task.id, Working}),
            SessionId = derive_session_id(Working),
            ets:insert(?SESSION_TABLE, {SessionId, Working#a2a_task.id}),
            bc_obs:emit(a2a_task_created, #{
                task_id => Working#a2a_task.id,
                session_id => SessionId
            }),
            try dispatch_to_session(Working) catch _:_ -> ok end,
            {reply, {ok, Working}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({cancel_task, TaskId}, _From, State) ->
    case ets:lookup(?TABLE, TaskId) of
        [{_, Task}] ->
            case bc_a2a_task:transition(Task, canceled) of
                {ok, Canceled} ->
                    ets:insert(?TABLE, {TaskId, Canceled}),
                    bc_obs:emit(a2a_task_updated, #{
                        task_id => TaskId,
                        state => canceled
                    }),
                    {reply, {ok, Canceled}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_task, TaskId, NewState, Message}, _From, State) ->
    case ets:lookup(?TABLE, TaskId) of
        [{_, Task}] ->
            case bc_a2a_task:transition(Task, NewState, Message) of
                {ok, Updated} ->
                    ets:insert(?TABLE, {TaskId, Updated}),
                    bc_obs:emit(a2a_task_updated, #{
                        task_id => TaskId,
                        state => NewState
                    }),
                    {reply, {ok, Updated}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({add_artifact, TaskId, Artifact}, _From, State) ->
    case ets:lookup(?TABLE, TaskId) of
        [{_, Task}] ->
            Updated = bc_a2a_task:add_artifact(Task, Artifact),
            ets:insert(?TABLE, {TaskId, Updated}),
            {reply, {ok, Updated}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% --- Internal ---

dispatch_to_session(#a2a_task{} = Task) ->
    Text = extract_text(Task),
    case Text of
        undefined -> ok;
        _ ->
            SessionId = derive_session_id(Task),
            UserId = <<"a2a:", (Task#a2a_task.id)/binary>>,
            AgentId = maps:get(<<"agentId">>, Task#a2a_task.metadata,
                               bc_config:get(beamclaw_core, default_agent, <<"default">>)),
            Msg = #bc_channel_message{
                session_id = SessionId,
                user_id    = UserId,
                channel    = a2a,
                content    = Text,
                raw        = Task,
                ts         = erlang:system_time(millisecond)
            },
            ensure_session_and_dispatch(SessionId, Msg, UserId, AgentId)
    end.

ensure_session_and_dispatch(SessionId, Msg, UserId, AgentId) ->
    case bc_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            bc_session:dispatch_run(Pid, Msg);
        {error, not_found} ->
            Config = #{session_id  => SessionId,
                       user_id     => UserId,
                       channel_id  => SessionId,
                       channel_mod => bc_channel_a2a,
                       agent_id    => AgentId},
            {ok, _} = bc_sessions_sup:start_session(Config),
            {ok, Pid} = bc_session_registry:lookup(SessionId),
            bc_session:dispatch_run(Pid, Msg)
    end.

extract_text(#a2a_task{history = [Latest | _]}) ->
    case Latest of
        #a2a_message{parts = Parts} ->
            case [T || #{type := text, text := T} <- Parts] of
                [First | _] -> First;
                []          -> undefined
            end;
        _ ->
            undefined
    end;
extract_text(#a2a_task{history = []}) ->
    undefined.

derive_session_id(#a2a_task{context_id = undefined, id = Id}) ->
    bc_session_registry:derive_session_id(<<"a2a:", Id/binary>>, <<"default">>);
derive_session_id(#a2a_task{context_id = CtxId}) ->
    bc_session_registry:derive_session_id(<<"a2a:", CtxId/binary>>, <<"default">>).

filter_context(Tasks, undefined) -> Tasks;
filter_context(Tasks, CtxId) ->
    [T || T <- Tasks, T#a2a_task.context_id =:= CtxId].

filter_status(Tasks, undefined) -> Tasks;
filter_status(Tasks, Status) ->
    [T || T <- Tasks, (T#a2a_task.status)#a2a_status.state =:= Status].
