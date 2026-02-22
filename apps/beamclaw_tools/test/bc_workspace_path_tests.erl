%% @doc EUnit tests for bc_workspace_path.
-module(bc_workspace_path_tests).

-include_lib("eunit/include/eunit.hrl").

base_dir_default_test() ->
    %% When BEAMCLAW_HOME is not set, base_dir uses $HOME
    os:unsetenv("BEAMCLAW_HOME"),
    Home = os:getenv("HOME"),
    Expected = filename:join([Home, ".beamclaw", "agents"]),
    ?assertEqual(Expected, bc_workspace_path:base_dir()).

base_dir_override_test() ->
    os:putenv("BEAMCLAW_HOME", "/tmp/custom"),
    ?assertEqual("/tmp/custom/agents", bc_workspace_path:base_dir()),
    os:unsetenv("BEAMCLAW_HOME").

agent_dir_test() ->
    os:putenv("BEAMCLAW_HOME", "/tmp/test"),
    ?assertEqual("/tmp/test/agents/my-agent",
                 bc_workspace_path:agent_dir(<<"my-agent">>)),
    os:unsetenv("BEAMCLAW_HOME").

bootstrap_file_test() ->
    os:putenv("BEAMCLAW_HOME", "/tmp/test"),
    ?assertEqual("/tmp/test/agents/my-agent/SOUL.md",
                 bc_workspace_path:bootstrap_file(<<"my-agent">>, <<"SOUL.md">>)),
    os:unsetenv("BEAMCLAW_HOME").

memory_dir_test() ->
    os:putenv("BEAMCLAW_HOME", "/tmp/test"),
    ?assertEqual("/tmp/test/agents/my-agent/memory",
                 bc_workspace_path:memory_dir(<<"my-agent">>)),
    os:unsetenv("BEAMCLAW_HOME").

daily_log_file_test() ->
    os:putenv("BEAMCLAW_HOME", "/tmp/test"),
    ?assertEqual("/tmp/test/agents/my-agent/memory/2026-02-22.md",
                 bc_workspace_path:daily_log_file(<<"my-agent">>, <<"2026-02-22">>)),
    os:unsetenv("BEAMCLAW_HOME").
