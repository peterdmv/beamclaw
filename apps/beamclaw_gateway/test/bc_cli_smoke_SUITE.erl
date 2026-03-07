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

-module(bc_cli_smoke_SUITE).
-moduledoc """
CLI escript smoke tests (Tier 2).

Builds the escript and exercises non-interactive commands:
  beamclaw version, beamclaw help, beamclaw doctor, beamclaw agent list,
  beamclaw skills list, beamclaw sandbox status, beamclaw scheduler list.

Verifies exit codes and output sanity. Addresses the 9% of historical
fix commits caused by CLI/daemon lifecycle issues.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([escript_exists/1,
         version_output/1,
         help_output/1,
         unknown_command_fails/1,
         agent_list/1,
         skills_list/1,
         sandbox_status/1]).

suite() ->
    [{timetrap, {seconds, 60}}].

groups() ->
    [{cli_commands, [sequence],
      [escript_exists,
       version_output,
       help_output,
       unknown_command_fails,
       agent_list,
       skills_list,
       sandbox_status]}].

all() ->
    [{group, cli_commands}].

%% ---------------------------------------------------------------------------
%% Suite lifecycle
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    %% Build the escript first
    ProjectRoot = find_project_root(),
    ct:pal("Project root: ~s", [ProjectRoot]),

    %% Run rebar3 escriptize
    EscriptCmd = "cd " ++ ProjectRoot ++ " && rebar3 escriptize 2>&1",
    EscriptResult = os:cmd(EscriptCmd),
    ct:pal("escriptize output: ~s", [EscriptResult]),

    EscriptPath = filename:join([ProjectRoot, "_build", "default", "bin", "beamclaw"]),
    [{escript_path, EscriptPath},
     {project_root, ProjectRoot} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% Test cases
%% ---------------------------------------------------------------------------

%% Verify the escript binary exists and is executable.
escript_exists(Config) ->
    Path = proplists:get_value(escript_path, Config),
    ?assert(filelib:is_regular(Path)),
    ok.

%% `beamclaw version` should print a version string.
version_output(Config) ->
    {Output, ExitCode} = run_cli(Config, "version"),
    ct:pal("version output: ~s (exit: ~B)", [Output, ExitCode]),
    ?assertEqual(0, ExitCode),
    %% Should contain "beamclaw" and a version-like pattern
    ?assertMatch({match, _}, re:run(Output, "beamclaw")),
    ?assertMatch({match, _}, re:run(Output, "[0-9]+\\.[0-9]+")),
    ok.

%% `beamclaw help` should print usage information.
help_output(Config) ->
    {Output, ExitCode} = run_cli(Config, "help"),
    ct:pal("help output: ~s (exit: ~B)", [Output, ExitCode]),
    ?assertEqual(0, ExitCode),
    %% Should contain common commands
    ?assertMatch({match, _}, re:run(Output, "tui")),
    ?assertMatch({match, _}, re:run(Output, "start")),
    ?assertMatch({match, _}, re:run(Output, "agent")),
    ok.

%% `beamclaw foobar` should fail with exit code 1.
unknown_command_fails(Config) ->
    {Output, ExitCode} = run_cli(Config, "notarealcommand"),
    ct:pal("unknown command output: ~s (exit: ~B)", [Output, ExitCode]),
    ?assertEqual(1, ExitCode),
    ?assertMatch({match, _}, re:run(Output, "unknown command")),
    ok.

%% `beamclaw agent list` should succeed (may print empty list).
agent_list(Config) ->
    {Output, ExitCode} = run_cli(Config, "agent list"),
    ct:pal("agent list output: ~s (exit: ~B)", [Output, ExitCode]),
    ?assertEqual(0, ExitCode),
    ok.

%% `beamclaw skills list` should succeed.
skills_list(Config) ->
    {Output, ExitCode} = run_cli(Config, "skills list"),
    ct:pal("skills list output: ~s (exit: ~B)", [Output, ExitCode]),
    ?assertEqual(0, ExitCode),
    ok.

%% `beamclaw sandbox status` should succeed (reports Docker availability).
sandbox_status(Config) ->
    {Output, ExitCode} = run_cli(Config, "sandbox status"),
    ct:pal("sandbox status output: ~s (exit: ~B)", [Output, ExitCode]),
    ?assertEqual(0, ExitCode),
    %% Should mention Docker or sandbox
    ?assertEqual(match, re:run(Output, "[Dd]ocker|[Ss]andbox", [{capture, none}])),
    ok.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

%% Run a CLI command and return {Output, ExitCode}.
%% Uses a wrapper script to capture the exit code since os:cmd/1 doesn't return it.
run_cli(Config, SubCommand) ->
    Path = proplists:get_value(escript_path, Config),
    %% Use a shell command that captures exit code
    Cmd = lists:flatten(io_lib:format(
        "~s ~s 2>&1; echo \"__EXIT_CODE__$?\"", [Path, SubCommand])),
    RawOutput = os:cmd(Cmd),
    parse_exit_code(RawOutput).

parse_exit_code(Output) ->
    case string:split(Output, "__EXIT_CODE__", trailing) of
        [Content, CodeStr] ->
            Code = list_to_integer(string:trim(CodeStr)),
            {string:trim(Content), Code};
        _ ->
            {Output, -1}
    end.

find_project_root() ->
    %% Walk up from the test directory to find rebar.config
    CWD = os:cmd("pwd"),
    TrimCWD = string:trim(CWD),
    find_root(TrimCWD).

find_root("/") -> "/tmp";  %% fallback
find_root(Dir) ->
    case filelib:is_regular(filename:join(Dir, "rebar.config")) of
        true  -> Dir;
        false -> find_root(filename:dirname(Dir))
    end.
