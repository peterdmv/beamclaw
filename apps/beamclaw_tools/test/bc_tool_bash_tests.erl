-module(bc_tool_bash_tests).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% execute/3 — arg name handling tests
%%--------------------------------------------------------------------

script_key_works_test() ->
    {ok, Output} = bc_tool_bash:execute(
        #{<<"script">> => <<"echo hello">>}, undefined, #{}),
    ?assertEqual(<<"hello\n">>, Output).

command_key_fallback_test() ->
    %% LLM sometimes sends "command" instead of "script" — should work
    {ok, Output} = bc_tool_bash:execute(
        #{<<"command">> => <<"echo fallback">>}, undefined, #{}),
    ?assertEqual(<<"fallback\n">>, Output).

missing_key_returns_error_test() ->
    {error, Msg} = bc_tool_bash:execute(
        #{<<"code">> => <<"echo bad">>}, undefined, #{}),
    ?assertMatch(<<"Missing required parameter 'script'", _/binary>>, Msg).

empty_args_returns_error_test() ->
    {error, Msg} = bc_tool_bash:execute(#{}, undefined, #{}),
    ?assertMatch(<<"Missing required parameter 'script'", _/binary>>, Msg).

%%--------------------------------------------------------------------
%% definition/0
%%--------------------------------------------------------------------

definition_has_script_param_test() ->
    Def = bc_tool_bash:definition(),
    ?assertEqual(<<"bash">>, maps:get(name, Def)),
    Props = maps:get(properties, maps:get(parameters, Def)),
    ?assert(maps:is_key(script, Props)).
