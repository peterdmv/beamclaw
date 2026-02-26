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
    ?assertEqual(4, length(BackWorking#a2a_task.history)).
