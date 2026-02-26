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

-module(bc_a2a_task_tests).

-include_lib("eunit/include/eunit.hrl").

% Helper function to convert task record to map for testing
% This mirrors the internal task_to_map function from bc_a2a_task
task_record_to_map(Task) when is_tuple(Task) ->
    [task, Id, ContextId, Status, Artifacts, History, CreatedAt, UpdatedAt] = tuple_to_list(Task),
    #{
        id => Id,
        contextId => ContextId,
        status => Status,
        artifacts => lists:reverse(Artifacts),
        history => lists:reverse(History),
        createdAt => CreatedAt,
        updatedAt => UpdatedAt
    }.

setup() ->
    bc_a2a_task:init().

cleanup(_) ->
    case ets:info(bc_a2a_tasks, name) of
        undefined -> ok;
        _ -> 
            ets:delete_all_objects(bc_a2a_tasks),
            ok
    end.

task_lifecycle_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        fun test_create_task/0,
        fun test_get_task/0,
        fun test_update_status/0,
        fun test_add_message/0,
        fun test_add_artifact/0,
        fun test_cancel_task/0,
        fun test_list_tasks/0
    ]}.

test_create_task() ->
    cleanup(unused),
    Message = #{role => <<"user">>, parts => [#{type => <<"text">>, text => <<"Hello">>}]},
    TaskId = bc_a2a_task:create(Message),
    
    ?assert(is_binary(TaskId)),
    ?assert(byte_size(TaskId) > 0),
    
    {ok, TaskRecord} = bc_a2a_task:get(TaskId),
    TaskMap = task_record_to_map(TaskRecord),
    
    ?assertEqual(TaskId, maps:get(id, TaskMap)),
    ?assertEqual(undefined, maps:get(contextId, TaskMap)),
    ?assertEqual([Message], maps:get(history, TaskMap)),
    ?assertEqual([], maps:get(artifacts, TaskMap)),
    
    Status = maps:get(status, TaskMap),
    ?assertEqual(<<"submitted">>, maps:get(state, Status)).

test_get_task() ->
    cleanup(unused),
    Message = #{role => <<"user">>, parts => [#{type => <<"text">>, text => <<"Test">>}]},
    TaskId = bc_a2a_task:create(Message),
    
    {ok, _Task} = bc_a2a_task:get(TaskId),
    ?assertEqual({error, not_found}, bc_a2a_task:get(<<"nonexistent">>)).

test_update_status() ->
    cleanup(unused),
    Message = #{role => <<"user">>, parts => [#{type => <<"text">>, text => <<"Test">>}]},
    TaskId = bc_a2a_task:create(Message),
    
    ?assertEqual(ok, bc_a2a_task:update_status(TaskId, <<"working">>)),
    {ok, TaskRecord} = bc_a2a_task:get(TaskId),
    TaskMap = task_record_to_map(TaskRecord),
    Status = maps:get(status, TaskMap),
    ?assertEqual(<<"working">>, maps:get(state, Status)),
    
    ?assertEqual(ok, bc_a2a_task:update_status(TaskId, <<"completed">>, <<"Done">>)),
    {ok, TaskRecord2} = bc_a2a_task:get(TaskId),
    TaskMap2 = task_record_to_map(TaskRecord2),
    Status2 = maps:get(status, TaskMap2),
    ?assertEqual(<<"completed">>, maps:get(state, Status2)),
    ?assertEqual(<<"Done">>, maps:get(message, Status2)).

test_add_message() ->
    cleanup(unused),
    Message1 = #{role => <<"user">>, parts => [#{type => <<"text">>, text => <<"Hello">>}]},
    TaskId = bc_a2a_task:create(Message1),
    
    Message2 = #{role => <<"agent">>, parts => [#{type => <<"text">>, text => <<"Hi there">>}]},
    ?assertEqual(ok, bc_a2a_task:add_message(TaskId, Message2)),
    
    {ok, TaskRecord} = bc_a2a_task:get(TaskId),
    TaskMap = task_record_to_map(TaskRecord),
    History = maps:get(history, TaskMap),
    
    ?assertEqual(2, length(History)),
    ?assertEqual([Message1, Message2], History).

test_add_artifact() ->
    cleanup(unused),
    Message = #{role => <<"user">>, parts => [#{type => <<"text">>, text => <<"Generate file">>}]},
    TaskId = bc_a2a_task:create(Message),
    
    Artifact = #{name => <<"output.txt">>, parts => [#{type => <<"text">>, text => <<"Content">>}]},
    ?assertEqual(ok, bc_a2a_task:add_artifact(TaskId, Artifact)),
    
    {ok, TaskRecord} = bc_a2a_task:get(TaskId),
    TaskMap = task_record_to_map(TaskRecord),
    Artifacts = maps:get(artifacts, TaskMap),
    
    ?assertEqual(1, length(Artifacts)),
    ?assertEqual([Artifact], Artifacts).

test_cancel_task() ->
    cleanup(unused),
    Message = #{role => <<"user">>, parts => [#{type => <<"text">>, text => <<"Test">>}]},
    TaskId = bc_a2a_task:create(Message),
    
    ?assertEqual(ok, bc_a2a_task:cancel(TaskId)),
    
    {ok, TaskRecord} = bc_a2a_task:get(TaskId),
    TaskMap = task_record_to_map(TaskRecord),
    Status = maps:get(status, TaskMap),
    ?assertEqual(<<"canceled">>, maps:get(state, Status)).

test_list_tasks() ->
    % Clear any existing tasks for this test
    cleanup(unused),
    
    Message1 = #{role => <<"user">>, parts => [#{type => <<"text">>, text => <<"Task 1">>}]},
    Message2 = #{role => <<"user">>, parts => [#{type => <<"text">>, text => <<"Task 2">>}]},
    
    _TaskId1 = bc_a2a_task:create(Message1, <<"context1">>),
    _TaskId2 = bc_a2a_task:create(Message2, <<"context2">>),
    _TaskId3 = bc_a2a_task:create(Message2, <<"context1">>),
    
    AllTasks = bc_a2a_task:list(undefined),
    ?assertEqual(3, length(AllTasks)),
    
    Context1Tasks = bc_a2a_task:list(<<"context1">>),
    ?assertEqual(2, length(Context1Tasks)),
    
    Context2Tasks = bc_a2a_task:list(<<"context2">>),
    ?assertEqual(1, length(Context2Tasks)).