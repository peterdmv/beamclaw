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
    %% All 6 template files are non-empty, so we should get 6 messages
    Contents = [M#bc_message.content || M <- Msgs],
    %% Check ordering: IDENTITY first, MEMORY last
    FirstContent = hd(Contents),
    LastContent  = lists:last(Contents),
    [?_assert(length(Msgs) =:= 6),
     ?_assert(binary:match(FirstContent, <<"[IDENTITY.md]">>) =/= nomatch),
     ?_assert(binary:match(LastContent,  <<"[MEMORY.md]">>)   =/= nomatch),
     %% All messages are system role
     ?_assert(lists:all(fun(M) -> M#bc_message.role =:= system end, Msgs))].

%% ---- skip empty files ----

skip_empty_test_() -> ?setup(skip_empty_t).
skip_empty_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"empty-agent">>),
    %% Clear TOOLS.md and AGENTS.md to empty
    ok = bc_workspace:write_bootstrap_file(<<"empty-agent">>, <<"TOOLS.md">>, <<>>),
    ok = bc_workspace:write_bootstrap_file(<<"empty-agent">>, <<"AGENTS.md">>, <<"   \n  ">>),
    Msgs = bc_system_prompt:assemble(<<"empty-agent">>),
    Contents = [M#bc_message.content || M <- Msgs],
    [?_assertEqual(4, length(Msgs)),
     %% TOOLS and AGENTS should be absent
     ?_assertNot(lists:any(fun(C) -> binary:match(C, <<"[TOOLS.md]">>) =/= nomatch end, Contents)),
     ?_assertNot(lists:any(fun(C) -> binary:match(C, <<"[AGENTS.md]">>) =/= nomatch end, Contents))].

%% ---- all empty returns fallback ----

all_empty_test_() -> ?setup(all_empty_t).
all_empty_t(_TmpDir) ->
    ok = bc_workspace:create_agent(<<"blank-agent">>),
    Files = [<<"SOUL.md">>, <<"IDENTITY.md">>, <<"USER.md">>,
             <<"TOOLS.md">>, <<"MEMORY.md">>, <<"AGENTS.md">>],
    lists:foreach(fun(F) ->
        ok = bc_workspace:write_bootstrap_file(<<"blank-agent">>, F, <<>>)
    end, Files),
    Msgs = bc_system_prompt:assemble(<<"blank-agent">>),
    [?_assertEqual(1, length(Msgs)),
     ?_assert(binary:match((hd(Msgs))#bc_message.content, <<"helpful assistant">>) =/= nomatch)].
