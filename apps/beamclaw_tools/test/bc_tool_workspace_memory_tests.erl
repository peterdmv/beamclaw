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

-module(bc_tool_workspace_memory_tests).
-moduledoc "EUnit tests for bc_tool_workspace_memory.".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

-define(setup(F), {setup, fun setup/0, fun cleanup/1, fun F/1}).

setup() ->
    TmpDir = filename:join("/tmp", "beamclaw_wm_test_" ++
                            integer_to_list(erlang:unique_integer([positive]))),
    os:putenv("BEAMCLAW_HOME", TmpDir),
    %% Create agent directory with MEMORY.md and memory/ subdir
    AgentDir = filename:join([TmpDir, "agents", "test-agent"]),
    ok = filelib:ensure_dir(filename:join(AgentDir, "dummy")),
    ok = file:write_file(filename:join(AgentDir, "MEMORY.md"),
                         <<"# Memory\n\nInitial content.\n">>),
    MemDir = filename:join(AgentDir, "memory"),
    ok = filelib:ensure_dir(filename:join(MemDir, "dummy")),
    TmpDir.

cleanup(TmpDir) ->
    os:unsetenv("BEAMCLAW_HOME"),
    os:cmd("rm -rf " ++ TmpDir).

session_ref() ->
    #bc_session_ref{
        session_id  = <<"test-session">>,
        user_id     = <<"test-user">>,
        session_pid = self(),
        autonomy    = supervised,
        agent_id    = <<"test-agent">>
    }.

%% ---- read ----

read_test_() -> ?setup(read_t).
read_t(_TmpDir) ->
    Ref = session_ref(),
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read">>}, Ref, #{}),
    [?_assert(binary:match(Content, <<"Initial content">>) =/= nomatch)].

%% ---- append ----

append_test_() -> ?setup(append_t).
append_t(_TmpDir) ->
    Ref = session_ref(),
    {ok, _} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"append">>, <<"content">> => <<"New fact.">>}, Ref, #{}),
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read">>}, Ref, #{}),
    [?_assert(binary:match(Content, <<"Initial content">>) =/= nomatch),
     ?_assert(binary:match(Content, <<"New fact.">>) =/= nomatch)].

%% ---- replace ----

replace_test_() -> ?setup(replace_t).
replace_t(_TmpDir) ->
    Ref = session_ref(),
    {ok, _} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"replace">>, <<"content">> => <<"Replaced.">>}, Ref, #{}),
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read">>}, Ref, #{}),
    [?_assertEqual(<<"Replaced.">>, Content)].

%% ---- missing file ----

missing_file_test_() -> ?setup(missing_file_t).
missing_file_t(TmpDir) ->
    %% Create agent dir without MEMORY.md
    MissingDir = filename:join([TmpDir, "agents", "empty-agent"]),
    ok = filelib:ensure_dir(filename:join(MissingDir, "dummy")),
    Ref = session_ref(),
    Ref2 = Ref#bc_session_ref{agent_id = <<"empty-agent">>},
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read">>}, Ref2, #{}),
    [?_assert(binary:match(Content, <<"does not exist">>) =/= nomatch)].

%% ---- truncation ----

truncation_test_() -> ?setup(truncation_t).
truncation_t(_TmpDir) ->
    Ref = session_ref(),
    BigContent = binary:copy(<<"x">>, 25000),
    {ok, _} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"replace">>, <<"content">> => BigContent}, Ref, #{}),
    {ok, Read} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read">>}, Ref, #{}),
    [?_assert(byte_size(Read) < 25000),
     ?_assert(binary:match(Read, <<"[...truncated at 20KB...]">>) =/= nomatch)].

%% ---- missing content for append/replace ----

append_no_content_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"append">>}, Ref, #{})).

replace_no_content_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"replace">>}, Ref, #{})).

invalid_action_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"delete">>}, Ref, #{})).

%% ---- daily log: append_daily ----

append_daily_test_() -> ?setup(append_daily_t).
append_daily_t(_TmpDir) ->
    Ref = session_ref(),
    {ok, Msg} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"append_daily">>,
          <<"content">> => <<"Discussed architecture today.">>}, Ref, #{}),
    %% Now read it back
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read_daily">>}, Ref, #{}),
    [?_assertEqual(<<"Daily log updated.">>, Msg),
     ?_assert(binary:match(Content, <<"Discussed architecture">>) =/= nomatch)].

%% ---- daily log: read_daily with specific date ----

read_daily_specific_date_test_() -> ?setup(read_daily_specific_t).
read_daily_specific_t(TmpDir) ->
    Ref = session_ref(),
    %% Write a log for a specific past date
    MemDir = filename:join([TmpDir, "agents", "test-agent", "memory"]),
    ok = file:write_file(filename:join(MemDir, "2026-01-15.md"),
                         <<"Past log entry.">>),
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read_daily">>, <<"date">> => <<"2026-01-15">>}, Ref, #{}),
    [?_assert(binary:match(Content, <<"Past log entry">>) =/= nomatch)].

%% ---- daily log: read_daily for missing date ----

read_daily_missing_test_() -> ?setup(read_daily_missing_t).
read_daily_missing_t(_TmpDir) ->
    Ref = session_ref(),
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read_daily">>, <<"date">> => <<"1999-01-01">>}, Ref, #{}),
    [?_assert(binary:match(Content, <<"No daily log">>) =/= nomatch)].

%% ---- daily log: list_daily ----

list_daily_test_() -> ?setup(list_daily_t).
list_daily_t(TmpDir) ->
    Ref = session_ref(),
    %% Write a couple of daily logs
    MemDir = filename:join([TmpDir, "agents", "test-agent", "memory"]),
    ok = file:write_file(filename:join(MemDir, "2026-02-20.md"), <<"day 1">>),
    ok = file:write_file(filename:join(MemDir, "2026-02-21.md"), <<"day 2">>),
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"list_daily">>}, Ref, #{}),
    [?_assert(binary:match(Content, <<"2026-02-21.md">>) =/= nomatch),
     ?_assert(binary:match(Content, <<"2026-02-20.md">>) =/= nomatch)].

%% ---- daily log: list_daily empty ----

list_daily_empty_test_() -> ?setup(list_daily_empty_t).
list_daily_empty_t(TmpDir) ->
    %% Create agent with empty memory dir
    AgentDir = filename:join([TmpDir, "agents", "no-logs"]),
    MemDir = filename:join(AgentDir, "memory"),
    ok = filelib:ensure_dir(filename:join(MemDir, "dummy")),
    Ref = (session_ref())#bc_session_ref{agent_id = <<"no-logs">>},
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"list_daily">>}, Ref, #{}),
    [?_assert(binary:match(Content, <<"No daily logs">>) =/= nomatch)].

%% ---- append_daily missing content ----

append_daily_no_content_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"append_daily">>}, Ref, #{})).

%% ---- read_bootstrap ----

read_bootstrap_test_() -> ?setup(read_bootstrap_t).
read_bootstrap_t(TmpDir) ->
    Ref = session_ref(),
    %% Write IDENTITY.md for the test agent
    AgentDir = filename:join([TmpDir, "agents", "test-agent"]),
    ok = file:write_file(filename:join(AgentDir, "IDENTITY.md"),
                         <<"# Identity\n\n- **Name:** TestBot\n">>),
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read_bootstrap">>, <<"file">> => <<"IDENTITY.md">>}, Ref, #{}),
    [?_assert(binary:match(Content, <<"TestBot">>) =/= nomatch)].

%% ---- read_bootstrap missing file ----

read_bootstrap_missing_test_() -> ?setup(read_bootstrap_missing_t).
read_bootstrap_missing_t(_TmpDir) ->
    Ref = session_ref(),
    {ok, Content} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read_bootstrap">>, <<"file">> => <<"SOUL.md">>}, Ref, #{}),
    [?_assert(binary:match(Content, <<"does not exist">>) =/= nomatch)].

%% ---- update_bootstrap ----

update_bootstrap_test_() -> ?setup(update_bootstrap_t).
update_bootstrap_t(_TmpDir) ->
    Ref = session_ref(),
    NewContent = <<"# Identity\n\n- **Name:** Sparky\n- **Creature:** Fox\n">>,
    {ok, Msg} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"update_bootstrap">>,
          <<"file">> => <<"IDENTITY.md">>,
          <<"content">> => NewContent}, Ref, #{}),
    %% Read it back
    {ok, ReadBack} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read_bootstrap">>, <<"file">> => <<"IDENTITY.md">>}, Ref, #{}),
    [?_assertEqual(<<"IDENTITY.md updated.">>, Msg),
     ?_assertEqual(NewContent, ReadBack)].

%% ---- update_bootstrap all allowed files ----

update_bootstrap_allowed_files_test_() -> ?setup(update_bootstrap_allowed_t).
update_bootstrap_allowed_t(_TmpDir) ->
    Ref = session_ref(),
    Files = [<<"IDENTITY.md">>, <<"USER.md">>, <<"SOUL.md">>,
             <<"TOOLS.md">>, <<"AGENTS.md">>],
    Results = lists:map(fun(F) ->
        {ok, Msg} = bc_tool_workspace_memory:execute(
            #{<<"action">> => <<"update_bootstrap">>,
              <<"file">> => F,
              <<"content">> => <<"test content for ", F/binary>>}, Ref, #{}),
        Msg
    end, Files),
    [?_assertEqual(5, length(Results))].

%% ---- update_bootstrap rejects disallowed files ----

update_bootstrap_reject_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    %% MEMORY.md should be rejected (use read/append/replace instead)
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"update_bootstrap">>,
          <<"file">> => <<"MEMORY.md">>,
          <<"content">> => <<"test">>}, Ref, #{})),
    %% BOOTSTRAP.md should be rejected
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"update_bootstrap">>,
          <<"file">> => <<"BOOTSTRAP.md">>,
          <<"content">> => <<"test">>}, Ref, #{})),
    %% Random file should be rejected
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"update_bootstrap">>,
          <<"file">> => <<"../../etc/passwd">>,
          <<"content">> => <<"test">>}, Ref, #{})).

%% ---- read_bootstrap missing file param ----

read_bootstrap_no_file_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"read_bootstrap">>}, Ref, #{})).

%% ---- update_bootstrap missing content ----

update_bootstrap_no_content_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"update_bootstrap">>,
          <<"file">> => <<"IDENTITY.md">>}, Ref, #{})).

%% ---- delete_bootstrap ----

delete_bootstrap_test_() -> ?setup(delete_bootstrap_t).
delete_bootstrap_t(TmpDir) ->
    Ref = session_ref(),
    %% Write BOOTSTRAP.md for the test agent
    AgentDir = filename:join([TmpDir, "agents", "test-agent"]),
    BootPath = filename:join(AgentDir, "BOOTSTRAP.md"),
    ok = file:write_file(BootPath, <<"# Bootstrap\n\nDiscovery...">>),
    ?assert(filelib:is_regular(BootPath)),
    {ok, Msg} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"delete_bootstrap">>,
          <<"file">> => <<"BOOTSTRAP.md">>}, Ref, #{}),
    [?_assertEqual(<<"Deleted BOOTSTRAP.md">>, Msg),
     ?_assertNot(filelib:is_regular(BootPath))].

delete_bootstrap_already_absent_test_() -> ?setup(delete_bootstrap_absent_t).
delete_bootstrap_absent_t(_TmpDir) ->
    Ref = session_ref(),
    {ok, Msg} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"delete_bootstrap">>,
          <<"file">> => <<"BOOTSTRAP.md">>}, Ref, #{}),
    [?_assert(binary:match(Msg, <<"already absent">>) =/= nomatch)].

delete_bootstrap_reject_memory_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"delete_bootstrap">>,
          <<"file">> => <<"MEMORY.md">>}, Ref, #{})).

delete_bootstrap_no_file_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"delete_bootstrap">>}, Ref, #{})).

%% ---- search ----

search_memory_test_() -> ?setup(search_memory_t).
search_memory_t(_TmpDir) ->
    Ref = session_ref(),
    %% Replace MEMORY.md with searchable content
    {ok, _} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"replace">>,
          <<"content">> => <<"# Memory\n\nErlang OTP is great for concurrency.\n\n"
                             "Python is good for machine learning.\n\n"
                             "Rust provides memory safety without garbage collection.">>},
        Ref, #{}),
    {ok, Result} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"search">>, <<"query">> => <<"erlang concurrency">>}, Ref, #{}),
    [?_assert(binary:match(Result, <<"MEMORY.md">>) =/= nomatch),
     ?_assert(binary:match(Result, <<"Erlang">>) =/= nomatch)].

search_no_results_test_() -> ?setup(search_no_results_t).
search_no_results_t(_TmpDir) ->
    Ref = session_ref(),
    {ok, Result} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"search">>,
          <<"query">> => <<"quantum computing superconductor">>}, Ref, #{}),
    [?_assert(binary:match(Result, <<"No results">>) =/= nomatch)].

search_daily_logs_test_() -> ?setup(search_daily_t).
search_daily_t(TmpDir) ->
    Ref = session_ref(),
    MemDir = filename:join([TmpDir, "agents", "test-agent", "memory"]),
    ok = file:write_file(filename:join(MemDir, "2026-02-20.md"),
                         <<"Discussed database architecture and schema design today.">>),
    ok = file:write_file(filename:join(MemDir, "2026-02-21.md"),
                         <<"Fixed authentication bug in the login flow.">>),
    {ok, Result} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"search">>, <<"query">> => <<"database schema">>}, Ref, #{}),
    [?_assert(binary:match(Result, <<"2026-02-20">>) =/= nomatch)].

search_missing_query_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"search">>}, Ref, #{})).

%% ---- search_all ----

search_all_test_() -> ?setup(search_all_t).
search_all_t(TmpDir) ->
    Ref = session_ref(),
    %% Write some bootstrap files
    AgentDir = filename:join([TmpDir, "agents", "test-agent"]),
    ok = file:write_file(filename:join(AgentDir, "IDENTITY.md"),
                         <<"# Identity\n\nI am a helpful Erlang coding assistant.">>),
    ok = file:write_file(filename:join(AgentDir, "TOOLS.md"),
                         <<"# Tools\n\nI can use terminal, bash, and curl.">>),
    {ok, Result} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"search_all">>, <<"query">> => <<"erlang coding">>}, Ref, #{}),
    [?_assert(binary:match(Result, <<"IDENTITY.md">>) =/= nomatch)].

search_all_with_daily_test_() -> ?setup(search_all_daily_t).
search_all_daily_t(TmpDir) ->
    Ref = session_ref(),
    MemDir = filename:join([TmpDir, "agents", "test-agent", "memory"]),
    ok = file:write_file(filename:join(MemDir, "2026-02-22.md"),
                         <<"Explored the supervisor tree structure today.">>),
    {ok, Result} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"search_all">>, <<"query">> => <<"supervisor tree">>}, Ref, #{}),
    [?_assert(binary:match(Result, <<"2026-02-22">>) =/= nomatch)].

search_all_missing_query_test() ->
    Ref = #bc_session_ref{session_id = <<"x">>, user_id = <<"u">>,
                          session_pid = self(), autonomy = supervised,
                          agent_id = <<"any">>},
    ?assertMatch({error, _}, bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"search_all">>}, Ref, #{})).

%% ---- search with keyword mode (explicit) ----

search_keyword_mode_test_() -> ?setup(search_keyword_mode_t).
search_keyword_mode_t(_TmpDir) ->
    Ref = session_ref(),
    {ok, _} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"replace">>,
          <<"content">> => <<"# Memory\n\nErlang OTP beam virtual machine.\n\n"
                             "Python Django web framework.">>},
        Ref, #{}),
    {ok, Result} = bc_tool_workspace_memory:execute(
        #{<<"action">> => <<"search">>,
          <<"query">> => <<"erlang beam">>,
          <<"mode">> => <<"keyword">>}, Ref, #{}),
    [?_assert(binary:match(Result, <<"MEMORY.md">>) =/= nomatch)].
