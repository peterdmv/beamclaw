%% @doc EUnit tests for bc_tool_workspace_memory.
-module(bc_tool_workspace_memory_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

-define(setup(F), {setup, fun setup/0, fun cleanup/1, fun F/1}).

setup() ->
    TmpDir = filename:join("/tmp", "beamclaw_wm_test_" ++
                            integer_to_list(erlang:unique_integer([positive]))),
    os:putenv("BEAMCLAW_HOME", TmpDir),
    %% Create agent directory with MEMORY.md
    AgentDir = filename:join([TmpDir, "agents", "test-agent"]),
    ok = filelib:ensure_dir(filename:join(AgentDir, "dummy")),
    ok = file:write_file(filename:join(AgentDir, "MEMORY.md"),
                         <<"# Memory\n\nInitial content.\n">>),
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
