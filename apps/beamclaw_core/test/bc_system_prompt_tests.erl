%% @doc EUnit tests for bc_system_prompt assembly.
-module(bc_system_prompt_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

-define(setup(F), {setup, fun setup/0, fun cleanup/1, fun F/1}).

setup() ->
    TmpDir = filename:join("/tmp", "beamclaw_sp_test_" ++
                            integer_to_list(erlang:unique_integer([positive]))),
    os:putenv("BEAMCLAW_HOME", TmpDir),
    TmpDir.

cleanup(TmpDir) ->
    os:unsetenv("BEAMCLAW_HOME"),
    os:cmd("rm -rf " ++ TmpDir).

%% ---- fallback for missing agent ----

fallback_test() ->
    %% Nonexistent agent returns a single fallback system message
    Msgs = bc_system_prompt:assemble(<<"nonexistent-agent-xyz">>),
    ?assertEqual(1, length(Msgs)),
    [Msg] = Msgs,
    ?assertEqual(system, Msg#bc_message.role),
    ?assert(binary:match(Msg#bc_message.content, <<"helpful assistant">>) =/= nomatch).

%% ---- assembly ordering ----

ordering_test_() -> ?setup(ordering_t).
ordering_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"order-agent">>),
    Msgs = bc_system_prompt:assemble(<<"order-agent">>),
    %% At least 7 messages (bootstrap files) + possible skills
    Contents = [M#bc_message.content || M <- Msgs],
    %% Check ordering: IDENTITY first among bootstrap messages
    FirstContent = hd(Contents),
    %% Find the last bootstrap message (MEMORY.md)
    BootstrapContents = [C || C <- Contents,
        binary:match(C, <<"[IDENTITY.md]">>) =/= nomatch orelse
        binary:match(C, <<"[SOUL.md]">>) =/= nomatch orelse
        binary:match(C, <<"[USER.md]">>) =/= nomatch orelse
        binary:match(C, <<"[TOOLS.md]">>) =/= nomatch orelse
        binary:match(C, <<"[AGENTS.md]">>) =/= nomatch orelse
        binary:match(C, <<"[BOOTSTRAP.md]">>) =/= nomatch orelse
        binary:match(C, <<"[MEMORY.md]">>) =/= nomatch],
    LastBootstrap = lists:last(BootstrapContents),
    [?_assert(length(Msgs) >= 7),
     ?_assert(binary:match(FirstContent, <<"[IDENTITY.md]">>) =/= nomatch),
     ?_assert(binary:match(LastBootstrap, <<"[MEMORY.md]">>) =/= nomatch),
     %% BOOTSTRAP.md should be present (before MEMORY.md)
     ?_assert(lists:any(fun(C) ->
         binary:match(C, <<"[BOOTSTRAP.md]">>) =/= nomatch end, Contents)),
     %% All messages are system role
     ?_assert(lists:all(fun(M) -> M#bc_message.role =:= system end, Msgs))].

%% ---- BOOTSTRAP.md ordering ----

bootstrap_before_memory_test_() -> ?setup(bootstrap_before_memory_t).
bootstrap_before_memory_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"boot-agent">>),
    Msgs = bc_system_prompt:assemble(<<"boot-agent">>),
    Contents = [M#bc_message.content || M <- Msgs],
    BootIdx = find_index(fun(C) ->
        binary:match(C, <<"[BOOTSTRAP.md]">>) =/= nomatch end, Contents),
    MemIdx = find_index(fun(C) ->
        binary:match(C, <<"[MEMORY.md]">>) =/= nomatch end, Contents),
    [?_assert(BootIdx < MemIdx)].

%% ---- BOOTSTRAP.md skipped when missing ----

bootstrap_skipped_when_deleted_test_() -> ?setup(bootstrap_skipped_t).
bootstrap_skipped_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"no-boot-agent">>),
    %% Delete BOOTSTRAP.md
    Path = filename:join(bc_workspace:agent_dir(<<"no-boot-agent">>), "BOOTSTRAP.md"),
    file:delete(Path),
    Msgs = bc_system_prompt:assemble(<<"no-boot-agent">>),
    Contents = [M#bc_message.content || M <- Msgs],
    %% 6 bootstrap messages (7 - deleted BOOTSTRAP.md) + possible skills
    [?_assert(length(Msgs) >= 6),
     ?_assertNot(lists:any(fun(C) ->
         binary:match(C, <<"[BOOTSTRAP.md]">>) =/= nomatch end, Contents))].

%% ---- skip empty files ----

skip_empty_test_() -> ?setup(skip_empty_t).
skip_empty_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"empty-agent">>),
    %% Clear TOOLS.md and AGENTS.md to empty
    ok = bc_workspace:write_bootstrap_file(<<"empty-agent">>, <<"TOOLS.md">>, <<>>),
    ok = bc_workspace:write_bootstrap_file(<<"empty-agent">>, <<"AGENTS.md">>, <<"   \n  ">>),
    Msgs = bc_system_prompt:assemble(<<"empty-agent">>),
    Contents = [M#bc_message.content || M <- Msgs],
    %% 5 bootstrap messages (7 total - 2 empty) + possible skill messages
    [?_assert(length(Msgs) >= 5),
     %% TOOLS and AGENTS should be absent
     ?_assertNot(lists:any(fun(C) -> binary:match(C, <<"[TOOLS.md]">>) =/= nomatch end, Contents)),
     ?_assertNot(lists:any(fun(C) -> binary:match(C, <<"[AGENTS.md]">>) =/= nomatch end, Contents))].

%% ---- all empty returns fallback ----

all_empty_test_() -> ?setup(all_empty_t).
all_empty_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"blank-agent">>),
    Files = [<<"SOUL.md">>, <<"IDENTITY.md">>, <<"USER.md">>,
             <<"TOOLS.md">>, <<"MEMORY.md">>, <<"AGENTS.md">>,
             <<"BOOTSTRAP.md">>],
    lists:foreach(fun(F) ->
        ok = bc_workspace:write_bootstrap_file(<<"blank-agent">>, F, <<>>)
    end, Files),
    Msgs = bc_system_prompt:assemble(<<"blank-agent">>),
    %% First message should be the fallback (bootstrap files all empty)
    %% Additional skill messages may follow from bundled skills
    [?_assert(length(Msgs) >= 1),
     ?_assert(binary:match((hd(Msgs))#bc_message.content, <<"helpful assistant">>) =/= nomatch)].

%% ---- daily logs in system prompt ----

daily_logs_test_() -> ?setup(daily_logs_t).
daily_logs_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"daily-agent">>),
    %% Write a daily log for today
    {Y, M, D} = date(),
    Today = iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])),
    Dir = filename:join([bc_workspace:agent_dir(<<"daily-agent">>), "memory"]),
    ok = file:write_file(
        filename:join(Dir, binary_to_list(<<Today/binary, ".md">>)),
        <<"## Today\nWorked on testing.">>),
    Msgs = bc_system_prompt:assemble(<<"daily-agent">>),
    Contents = [Msg#bc_message.content || Msg <- Msgs],
    ExpectedTag = <<"[memory/", Today/binary, ".md]">>,
    [%% Should have bootstrap files + today's daily log + possible skills
     ?_assert(length(Msgs) >= 8),
     ?_assert(lists:any(fun(C) ->
         binary:match(C, ExpectedTag) =/= nomatch end, Contents)),
     %% Daily log content should be there
     ?_assert(lists:any(fun(C) ->
         binary:match(C, <<"Worked on testing">>) =/= nomatch end, Contents))].

%% ---- Helpers ----

find_index(Pred, List) ->
    find_index(Pred, List, 1).
find_index(_Pred, [], _N) -> 0;
find_index(Pred, [H|T], N) ->
    case Pred(H) of
        true  -> N;
        false -> find_index(Pred, T, N + 1)
    end.
