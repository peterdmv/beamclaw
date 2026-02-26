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

-module(bc_a2a_task).
-moduledoc """
A2A task state management.

Manages task lifecycle and state using an ETS table. Tasks have the following
states: submitted, working, completed, failed, canceled.

Each task contains:
- id: unique task identifier
- contextId: optional context identifier for grouping
- status: current task status (state + optional message)
- artifacts: list of generated artifacts
- history: message history for the task
""".

-export([init/0,
         create/1, create/2,
         get/1,
         update_status/2, update_status/3,
         add_artifact/2,
         add_message/2,
         cancel/1,
         list/1,
         task_to_map/1]).

-define(TABLE, bc_a2a_tasks).

-record(task, {
    id :: binary(),
    context_id :: binary() | undefined,
    status :: map(),
    artifacts = [] :: [map()],
    history = [] :: [map()],
    created_at :: integer(),
    updated_at :: integer()
}).

%% @doc Initialize the ETS table for task storage.
init() ->
    case ets:info(?TABLE, name) of
        undefined ->
            ets:new(?TABLE, [named_table, public, {keypos, #task.id}]);
        _ ->
            ok
    end.

%% @doc Create a new task with generated ID.
create(InitialMessage) ->
    create(InitialMessage, undefined).

%% @doc Create a new task with optional context ID.
create(InitialMessage, ContextId) ->
    TaskId = generate_id(),
    Now = erlang:system_time(second),
    Task = #task{
        id = TaskId,
        context_id = ContextId,
        status = #{state => <<"submitted">>, message => <<"Task created">>},
        history = [InitialMessage],
        created_at = Now,
        updated_at = Now
    },
    ets:insert(?TABLE, Task),
    TaskId.

%% @doc Get a task by ID.
get(TaskId) ->
    case ets:lookup(?TABLE, TaskId) of
        [Task] -> {ok, Task};
        [] -> {error, not_found}
    end.

%% @doc Update task status with state only.
update_status(TaskId, State) ->
    update_status(TaskId, State, undefined).

%% @doc Update task status with state and optional message.
update_status(TaskId, State, Message) ->
    case ?MODULE:get(TaskId) of
        {ok, Task} ->
            Status = case Message of
                undefined -> #{state => State};
                _ -> #{state => State, message => Message}
            end,
            UpdatedTask = Task#task{
                status = Status,
                updated_at = erlang:system_time(second)
            },
            ets:insert(?TABLE, UpdatedTask),
            ok;
        Error ->
            Error
    end.

%% @doc Add an artifact to a task.
add_artifact(TaskId, Artifact) ->
    case ?MODULE:get(TaskId) of
        {ok, Task} ->
            UpdatedTask = Task#task{
                artifacts = [Artifact | Task#task.artifacts],
                updated_at = erlang:system_time(second)
            },
            ets:insert(?TABLE, UpdatedTask),
            ok;
        Error ->
            Error
    end.

%% @doc Add a message to task history.
add_message(TaskId, Message) ->
    case ?MODULE:get(TaskId) of
        {ok, Task} ->
            UpdatedTask = Task#task{
                history = [Message | Task#task.history],
                updated_at = erlang:system_time(second)
            },
            ets:insert(?TABLE, UpdatedTask),
            ok;
        Error ->
            Error
    end.

%% @doc Cancel a task.
cancel(TaskId) ->
    update_status(TaskId, <<"canceled">>, <<"Task canceled by request">>).

%% @doc List tasks, optionally filtered by context ID.
list(undefined) ->
    Tasks = ets:tab2list(?TABLE),
    [task_to_map(Task) || Task <- Tasks];
list(ContextId) ->
    Tasks = ets:match_object(?TABLE, #task{context_id = ContextId, _ = '_'}),
    [task_to_map(Task) || Task <- Tasks].

%% Internal functions

generate_id() ->
    Bytes = crypto:strong_rand_bytes(16),
    base64:encode(Bytes).

task_to_map(#task{} = Task) ->
    #{
        id => Task#task.id,
        contextId => Task#task.context_id,
        status => Task#task.status,
        artifacts => lists:reverse(Task#task.artifacts),
        history => lists:reverse(Task#task.history),
        createdAt => Task#task.created_at,
        updatedAt => Task#task.updated_at
    }.