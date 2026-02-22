%% @doc EUnit tests for bc_tool_workspace_memory.
-module(bc_tool_workspace_memory_tests).

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
