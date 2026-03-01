%%
%% Copyright The BeamClaw Authors 2026, All Rights Reserved.
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
-include("bc_a2a_types.hrl").

new_task_test() ->
    Msg = #a2a_message{role = user, parts = [#{type => text, text => <<"hello">>}]},
    Task = bc_a2a_task:new(Msg),
    ?assertEqual(submitted, (Task#a2a_task.status)#a2a_status.state),
    ?assertEqual(1, length(Task#a2a_task.history)),
    ?assertEqual([], Task#a2a_task.artifacts),
    ?assert(is_binary(Task#a2a_task.id)).

transition_submitted_to_working_test() ->
    Msg = #a2a_message{role = user, parts = [#{type => text, text => <<"hi">>}]},
    Task = bc_a2a_task:new(Msg),
    {ok, Working} = bc_a2a_task:transition(Task, working),
    ?assertEqual(working, (Working#a2a_task.status)#a2a_status.state).

transition_working_to_completed_test() ->
    Msg = #a2a_message{role = user, parts = [#{type => text, text => <<"hi">>}]},
    Task = bc_a2a_task:new(Msg),
    {ok, Working} = bc_a2a_task:transition(Task, working),
    AgentMsg = #a2a_message{role = agent, parts = [#{type => text, text => <<"done">>}]},
    {ok, Completed} = bc_a2a_task:transition(Working, completed, AgentMsg),
    ?assertEqual(completed, (Completed#a2a_task.status)#a2a_status.state),
    ?assertEqual(2, length(Completed#a2a_task.history)).

invalid_transition_test() ->
    Msg = #a2a_message{role = user, parts = [#{type => text, text => <<"hi">>}]},
    Task = bc_a2a_task:new(Msg),
    ?assertEqual({error, invalid_transition}, bc_a2a_task:transition(Task, completed)).

terminal_states_test() ->
    Msg = #a2a_message{role = user, parts = [#{type => text, text => <<"hi">>}]},
    Task = bc_a2a_task:new(Msg),
    ?assertNot(bc_a2a_task:is_terminal(Task)),
    {ok, Working} = bc_a2a_task:transition(Task, working),
    ?assertNot(bc_a2a_task:is_terminal(Working)),
    {ok, Failed} = bc_a2a_task:transition(Working, failed),
    ?assert(bc_a2a_task:is_terminal(Failed)).

add_artifact_test() ->
    Msg = #a2a_message{role = user, parts = [#{type => text, text => <<"hi">>}]},
    Task = bc_a2a_task:new(Msg),
    Art = #a2a_artifact{name = <<"result.txt">>, parts = [#{type => text, text => <<"data">>}], index = 0},
    Updated = bc_a2a_task:add_artifact(Task, Art),
    ?assertEqual(1, length(Updated#a2a_task.artifacts)),
    [A] = Updated#a2a_task.artifacts,
    ?assertEqual(0, A#a2a_artifact.index).

serialization_roundtrip_test() ->
    Msg = #a2a_message{role = user, parts = [#{type => text, text => <<"hello">>}]},
    Task = bc_a2a_task:new(<<"test-id-123">>, Msg, #{}),
    Json = bc_a2a_task:to_json(Task),
    ?assert(is_map(Json)),
    ?assertEqual(<<"test-id-123">>, maps:get(<<"id">>, Json)),
    ?assertEqual(<<"submitted">>, maps:get(<<"state">>, maps:get(<<"status">>, Json))),
    %% Roundtrip
    Restored = bc_a2a_task:from_json(Json),
    ?assertEqual(Task#a2a_task.id, Restored#a2a_task.id),
    ?assertEqual((Task#a2a_task.status)#a2a_status.state,
                 (Restored#a2a_task.status)#a2a_status.state).

state_machine_input_required_test() ->
    Msg = #a2a_message{role = user, parts = [#{type => text, text => <<"hi">>}]},
    Task = bc_a2a_task:new(Msg),
    {ok, Working} = bc_a2a_task:transition(Task, working),
    AskMsg = #a2a_message{role = agent, parts = [#{type => text, text => <<"need more info">>}]},
    {ok, InputReq} = bc_a2a_task:transition(Working, input_required, AskMsg),
    ?assertEqual(input_required, (InputReq#a2a_task.status)#a2a_status.state),
    %% Can go back to working
    UserReply = #a2a_message{role = user, parts = [#{type => text, text => <<"here you go">>}]},
    {ok, BackWorking} = bc_a2a_task:transition(InputReq, working, UserReply),
    ?assertEqual(working, (BackWorking#a2a_task.status)#a2a_status.state),
    ?assertEqual(3, length(BackWorking#a2a_task.history)).

%% --- New tests for review fixes ---

iso8601_returns_binary_test() ->
    Msg = #a2a_message{role = user, parts = [#{type => text, text => <<"hi">>}]},
    Task = bc_a2a_task:new(Msg),
    Ts = (Task#a2a_task.status)#a2a_status.timestamp,
    ?assert(is_binary(Ts)).

new_with_metadata_test() ->
    Msg = #a2a_message{role = user, parts = [#{type => text, text => <<"hi">>}]},
    Meta = #{<<"contextId">> => <<"ctx-1">>},
    Task = bc_a2a_task:new_with_metadata(Msg, Meta),
    ?assertEqual(<<"ctx-1">>, Task#a2a_task.context_id),
    ?assert(is_binary(Task#a2a_task.id)).

parse_invalid_state_test() ->
    %% from_json with unknown state should default to submitted
    Json = #{<<"id">> => <<"test">>,
             <<"status">> => #{<<"state">> => <<"bogus_state">>}},
    Task = bc_a2a_task:from_json(Json),
    ?assertEqual(submitted, (Task#a2a_task.status)#a2a_status.state).

reject_undefined_test() ->
    Map = #{a => 1, b => undefined, c => 3},
    Filtered = bc_a2a_task:reject_undefined(Map),
    ?assertEqual(#{a => 1, c => 3}, Filtered).

history_ordering_test() ->
    %% Verify chronological order is preserved through serialization
    Msg1 = #a2a_message{role = user, parts = [#{type => text, text => <<"first">>}]},
    Task0 = bc_a2a_task:new(Msg1),
    {ok, Working} = bc_a2a_task:transition(Task0, working),
    Msg2 = #a2a_message{role = agent, parts = [#{type => text, text => <<"second">>}]},
    {ok, Task1} = bc_a2a_task:transition(Working, completed, Msg2),
    Json = bc_a2a_task:to_json(Task1),
    History = maps:get(<<"history">>, Json),
    %% First message should be the user's, second should be agent's
    [First, Second] = History,
    ?assertEqual(<<"user">>, maps:get(<<"role">>, First)),
    ?assertEqual(<<"agent">>, maps:get(<<"role">>, Second)).
