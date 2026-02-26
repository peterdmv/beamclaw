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

-module(bc_a2a_handler_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    bc_a2a_task:init().

cleanup(_) ->
    case ets:info(bc_a2a_tasks, name) of
        undefined -> ok;
        _ -> ets:delete(bc_a2a_tasks)
    end.

handler_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        fun test_agent_card_generation/0,
        fun test_task_workflow/0,
        fun test_types_construction/0,
        fun test_json_encoding/0
    ]}.

test_agent_card_generation() ->
    % Test the agent card generation which is used by the handler
    Card = bc_a2a_agent_card:generate(),
    Json = jsx:encode(Card),
    
    ?assert(is_binary(Json)),
    ?assert(byte_size(Json) > 0),
    
    % Verify it can be decoded back
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(<<"0.3.0">>, maps:get(<<"version">>, Decoded)),
    ?assert(maps:is_key(<<"capabilities">>, Decoded)),
    ?assert(maps:is_key(<<"skills">>, Decoded)).

test_task_workflow() ->
    % Test the task workflow that the handler would use
    Message = bc_a2a_types:message(<<"user">>, [bc_a2a_types:text_part(<<"Hello">>)]),
    
    % Create task
    TaskId = bc_a2a_task:create(Message),
    ?assert(is_binary(TaskId)),
    
    % Update status
    ok = bc_a2a_task:update_status(TaskId, <<"working">>, <<"Processing">>),
    
    % Get task
    {ok, _TaskRecord} = bc_a2a_task:get(TaskId),
    
    % Add message
    Response = bc_a2a_types:message(<<"agent">>, [bc_a2a_types:text_part(<<"Hi there">>)]),
    ok = bc_a2a_task:add_message(TaskId, Response),
    
    % Add artifact
    Artifact = bc_a2a_types:artifact(<<"output.txt">>, [bc_a2a_types:text_part(<<"Result">>)]),
    ok = bc_a2a_task:add_artifact(TaskId, Artifact),
    
    % Complete task
    ok = bc_a2a_task:update_status(TaskId, <<"completed">>, <<"Done">>),
    
    % List tasks
    Tasks = bc_a2a_task:list(undefined),
    ?assert(length(Tasks) >= 1).

test_types_construction() ->
    % Test the A2A type constructors
    TextPart = bc_a2a_types:text_part(<<"Hello world">>),
    ?assertEqual(#{type => <<"text">>, text => <<"Hello world">>}, TextPart),
    
    FilePart = bc_a2a_types:file_part(<<"test.txt">>, <<"content">>),
    ?assertEqual(#{type => <<"file">>, name => <<"test.txt">>, data => <<"content">>}, FilePart),
    
    DataPart = bc_a2a_types:data_part(<<"binary data">>),
    ?assertEqual(#{type => <<"data">>, data => <<"binary data">>}, DataPart),
    
    Message = bc_a2a_types:message(<<"user">>, [TextPart]),
    ?assertEqual(#{role => <<"user">>, parts => [TextPart]}, Message),
    
    Status = bc_a2a_types:task_status(<<"completed">>, <<"Success">>),
    ?assertEqual(#{state => <<"completed">>, message => <<"Success">>}, Status),
    
    Artifact = bc_a2a_types:artifact(<<"result.json">>, [DataPart]),
    ?assertEqual(#{name => <<"result.json">>, parts => [DataPart]}, Artifact).

test_json_encoding() ->
    % Test that our types can be properly JSON encoded
    Message = bc_a2a_types:message(<<"user">>, [
        bc_a2a_types:text_part(<<"Hello">>),
        bc_a2a_types:file_part(<<"data.txt">>, <<"file content">>)
    ]),
    
    Json = jsx:encode(Message),
    ?assert(is_binary(Json)),
    
    Decoded = jsx:decode(Json, [return_maps]),
    ?assertEqual(<<"user">>, maps:get(<<"role">>, Decoded)),
    
    Parts = maps:get(<<"parts">>, Decoded),
    ?assertEqual(2, length(Parts)),
    
    [FirstPart, SecondPart] = Parts,
    ?assertEqual(<<"text">>, maps:get(<<"type">>, FirstPart)),
    ?assertEqual(<<"Hello">>, maps:get(<<"text">>, FirstPart)),
    ?assertEqual(<<"file">>, maps:get(<<"type">>, SecondPart)),
    ?assertEqual(<<"data.txt">>, maps:get(<<"name">>, SecondPart)).