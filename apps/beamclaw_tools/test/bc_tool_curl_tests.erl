-module(bc_tool_curl_tests).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% expand_env_vars/1 tests
%%--------------------------------------------------------------------

expand_no_vars_test() ->
    Url = <<"https://example.com/api?key=123">>,
    ?assertEqual(Url, bc_tool_curl:expand_env_vars(Url)).

expand_dollar_var_test() ->
    os:putenv("BC_TEST_TOKEN", "abc123"),
    ?assertEqual(<<"https://api.example.com?token=abc123">>,
                 bc_tool_curl:expand_env_vars(<<"https://api.example.com?token=$BC_TEST_TOKEN">>)),
    os:unsetenv("BC_TEST_TOKEN").

expand_braced_var_test() ->
    os:putenv("BC_TEST_TOKEN", "xyz789"),
    ?assertEqual(<<"https://api.example.com?token=xyz789">>,
                 bc_tool_curl:expand_env_vars(<<"https://api.example.com?token=${BC_TEST_TOKEN}">>)),
    os:unsetenv("BC_TEST_TOKEN").

expand_unset_var_test() ->
    os:unsetenv("BC_TEST_UNSET_VAR"),
    Input = <<"https://api.example.com?token=$BC_TEST_UNSET_VAR">>,
    ?assertEqual(Input, bc_tool_curl:expand_env_vars(Input)).

expand_unset_braced_var_test() ->
    os:unsetenv("BC_TEST_UNSET_VAR"),
    Input = <<"https://api.example.com?token=${BC_TEST_UNSET_VAR}">>,
    ?assertEqual(Input, bc_tool_curl:expand_env_vars(Input)).

expand_multiple_vars_test() ->
    os:putenv("BC_TEST_HOST", "api.finn.com"),
    os:putenv("BC_TEST_KEY", "secret"),
    ?assertEqual(<<"https://api.finn.com/v1?key=secret">>,
                 bc_tool_curl:expand_env_vars(<<"https://$BC_TEST_HOST/v1?key=$BC_TEST_KEY">>)),
    os:unsetenv("BC_TEST_HOST"),
    os:unsetenv("BC_TEST_KEY").

expand_mixed_set_unset_test() ->
    os:putenv("BC_TEST_SET", "hello"),
    os:unsetenv("BC_TEST_MISSING"),
    ?assertEqual(<<"hello-$BC_TEST_MISSING">>,
                 bc_tool_curl:expand_env_vars(<<"$BC_TEST_SET-$BC_TEST_MISSING">>)),
    os:unsetenv("BC_TEST_SET").

expand_adjacent_vars_test() ->
    os:putenv("BC_TEST_A", "foo"),
    os:putenv("BC_TEST_B", "bar"),
    ?assertEqual(<<"foobar">>,
                 bc_tool_curl:expand_env_vars(<<"${BC_TEST_A}${BC_TEST_B}">>)),
    os:unsetenv("BC_TEST_A"),
    os:unsetenv("BC_TEST_B").

expand_empty_string_test() ->
    ?assertEqual(<<>>, bc_tool_curl:expand_env_vars(<<>>)).

expand_var_with_underscores_test() ->
    os:putenv("_BC_TEST_123_VAR", "val"),
    ?assertEqual(<<"prefix-val-suffix">>,
                 bc_tool_curl:expand_env_vars(<<"prefix-$_BC_TEST_123_VAR-suffix">>)),
    os:unsetenv("_BC_TEST_123_VAR").

expand_dollar_not_a_var_test() ->
    %% Dollar followed by non-identifier chars should pass through.
    Input = <<"price is $5.00">>,
    ?assertEqual(Input, bc_tool_curl:expand_env_vars(Input)).
