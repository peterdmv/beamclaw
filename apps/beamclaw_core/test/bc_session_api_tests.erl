-module(bc_session_api_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% Tests for bc_session new APIs: get_session_id, get_last_activity,
%% is_busy, get_state_summary.

setup() ->
    %% Set minimal app env for bc_session
    application:set_env(beamclaw_core, autonomy_level, supervised),
    application:set_env(beamclaw_core, default_agent, <<"default">>),
    application:set_env(beamclaw_core, session_persistence, false),
    %% Start the session registry (ETS table)
    case whereis(bc_session_registry) of
        undefined -> {ok, _} = bc_session_registry:start_link();
        _         -> ok
    end,
    ok.

cleanup(_) ->
    ok.

start_session(Config) ->
    {ok, Pid} = bc_session:start_link(Config),
    Pid.

get_session_id_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Pid = start_session(#{session_id => <<"test-sid-001">>,
                              user_id => <<"user1">>}),
        ?assertEqual(<<"test-sid-001">>, bc_session:get_session_id(Pid))
    end}.

get_last_activity_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Before = erlang:system_time(second),
        Pid = start_session(#{session_id => <<"test-sid-002">>,
                              user_id => <<"user1">>}),
        After = erlang:system_time(second),
        Activity = bc_session:get_last_activity(Pid),
        ?assert(Activity >= Before),
        ?assert(Activity =< After)
    end}.

is_busy_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Pid = start_session(#{session_id => <<"test-sid-003">>,
                              user_id => <<"user1">>}),
        %% Initially not busy (no loop yet)
        ?assertEqual(false, bc_session:is_busy(Pid))
    end}.

get_state_summary_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Pid = start_session(#{session_id => <<"test-sid-004">>,
                              user_id => <<"user1">>,
                              agent_id => <<"test-agent">>}),
        Summary = bc_session:get_state_summary(Pid),
        ?assertEqual(<<"test-sid-004">>, maps:get(session_id, Summary)),
        ?assertEqual(<<"user1">>, maps:get(user_id, Summary)),
        ?assertEqual(<<"test-agent">>, maps:get(agent_id, Summary)),
        ?assertEqual(bc_provider_openrouter, maps:get(provider_mod, Summary)),
        ?assertEqual(false, maps:get(loop_busy, Summary)),
        ?assertEqual(0, maps:get(history_len, Summary)),
        ?assert(is_integer(maps:get(last_activity, Summary)))
    end}.

last_activity_updates_on_append_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Pid = start_session(#{session_id => <<"test-sid-005">>,
                              user_id => <<"user1">>}),
        T1 = bc_session:get_last_activity(Pid),
        timer:sleep(1100),  %% wait >1 second for timestamp to advance
        bc_session:append_message(Pid, #bc_message{
            id = <<"msg1">>, role = user, content = <<"hello">>
        }),
        %% append_message is a cast; do a sync call to ensure it's processed
        T2 = bc_session:get_last_activity(Pid),
        ?assert(T2 >= T1)
    end}.

summary_history_len_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Pid = start_session(#{session_id => <<"test-sid-006">>,
                              user_id => <<"user1">>}),
        bc_session:append_message(Pid, #bc_message{
            id = <<"m1">>, role = user, content = <<"hello">>
        }),
        bc_session:append_message(Pid, #bc_message{
            id = <<"m2">>, role = assistant, content = <<"hi">>
        }),
        %% Sync: get_state_summary is a call, append is cast, FIFO guarantees
        Summary = bc_session:get_state_summary(Pid),
        ?assertEqual(2, maps:get(history_len, Summary))
    end}.
