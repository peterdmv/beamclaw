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
-module(bc_a2a_task_manager_tests).
-include_lib("eunit/include/eunit.hrl").
-include("bc_a2a_types.hrl").

%% Fixture: start/stop task manager for each test
manager_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun create_and_get_task/0,
      fun get_nonexistent_task/0,
      fun cancel_task/0,
      fun cancel_terminal_task/0,
      fun update_task_state/0,
      fun list_tasks_returns_list/0,
      fun task_id_for_session_not_found/0]}.

setup() ->
    %% Start pg (required by bc_obs)
    case pg:start_link(bc_obs_backends) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end,
    {ok, MgrPid} = bc_a2a_task_manager:start_link(),
    MgrPid.

cleanup(MgrPid) ->
    gen_server:stop(MgrPid).

msg() ->
    #a2a_message{role = user, parts = [#{type => text, text => <<"hello">>}]}.

create_and_get_task() ->
    {ok, Task} = bc_a2a_task_manager:create_task(msg()),
    ?assertEqual(working, (Task#a2a_task.status)#a2a_status.state),
    {ok, Got} = bc_a2a_task_manager:get_task(Task#a2a_task.id),
    ?assertEqual(Task#a2a_task.id, Got#a2a_task.id).

get_nonexistent_task() ->
    ?assertEqual({error, not_found}, bc_a2a_task_manager:get_task(<<"nonexistent">>)).

cancel_task() ->
    {ok, Task} = bc_a2a_task_manager:create_task(msg()),
    {ok, Canceled} = bc_a2a_task_manager:cancel_task(Task#a2a_task.id),
    ?assertEqual(canceled, (Canceled#a2a_task.status)#a2a_status.state).

cancel_terminal_task() ->
    {ok, Task} = bc_a2a_task_manager:create_task(msg()),
    {ok, _} = bc_a2a_task_manager:cancel_task(Task#a2a_task.id),
    %% Canceling an already-canceled task should fail
    ?assertMatch({error, _}, bc_a2a_task_manager:cancel_task(Task#a2a_task.id)).

update_task_state() ->
    {ok, Task} = bc_a2a_task_manager:create_task(msg()),
    AgentMsg = #a2a_message{role = agent, parts = [#{type => text, text => <<"done">>}]},
    {ok, Completed} = bc_a2a_task_manager:update_task(Task#a2a_task.id, completed, AgentMsg),
    ?assertEqual(completed, (Completed#a2a_task.status)#a2a_status.state).

list_tasks_returns_list() ->
    {ok, _} = bc_a2a_task_manager:create_task(msg()),
    Tasks = bc_a2a_task_manager:list_tasks(),
    ?assert(is_list(Tasks)),
    ?assert(length(Tasks) >= 1).

task_id_for_session_not_found() ->
    ?assertEqual({error, not_found},
                 bc_a2a_task_manager:task_id_for_session(<<"no-such-session">>)).
