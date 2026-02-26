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

-module(bc_a2a_types).
-moduledoc """
Type constructors for A2A protocol objects.

Provides functions to create well-formed A2A protocol messages, parts,
tasks, artifacts, and status objects according to the A2A specification.
""".

-export([text_part/1,
         file_part/2,
         data_part/1,
         message/2,
         task/1,
         artifact/2,
         task_status/1, task_status/2]).

%% @doc Create a text part.
text_part(Text) when is_binary(Text) ->
    #{type => <<"text">>, text => Text};
text_part(Text) when is_list(Text) ->
    text_part(list_to_binary(Text)).

%% @doc Create a file part.
file_part(Name, Data) when is_binary(Name), is_binary(Data) ->
    #{type => <<"file">>, name => Name, data => Data};
file_part(Name, Data) when is_list(Name) ->
    file_part(list_to_binary(Name), Data).

%% @doc Create a data part.
data_part(Data) when is_binary(Data) ->
    #{type => <<"data">>, data => Data}.

%% @doc Create a message with role and parts.
message(Role, Parts) when is_binary(Role), is_list(Parts) ->
    #{role => Role, parts => Parts};
message(Role, Parts) when is_list(Role) ->
    message(list_to_binary(Role), Parts).

%% @doc Create a task object from stored task state.
task(TaskMap) when is_map(TaskMap) ->
    #{
        id => maps:get(id, TaskMap),
        contextId => maps:get(contextId, TaskMap, null),
        status => maps:get(status, TaskMap),
        artifacts => maps:get(artifacts, TaskMap, []),
        history => maps:get(history, TaskMap, [])
    }.

%% @doc Create an artifact.
artifact(Name, Parts) when is_binary(Name), is_list(Parts) ->
    #{name => Name, parts => Parts};
artifact(Name, Parts) when is_list(Name) ->
    artifact(list_to_binary(Name), Parts).

%% @doc Create a task status with state only.
task_status(State) when is_binary(State) ->
    #{state => State};
task_status(State) when is_list(State) ->
    task_status(list_to_binary(State)).

%% @doc Create a task status with state and message.
task_status(State, Message) when is_binary(State), is_binary(Message) ->
    #{state => State, message => Message};
task_status(State, Message) when is_list(State) ->
    task_status(list_to_binary(State), Message);
task_status(State, Message) when is_list(Message) ->
    task_status(State, list_to_binary(Message));
task_status(State, Message) when is_list(State), is_list(Message) ->
    task_status(list_to_binary(State), list_to_binary(Message)).