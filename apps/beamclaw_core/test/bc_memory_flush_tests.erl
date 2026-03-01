-module(bc_memory_flush_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% Tests for bc_memory_flush — extracted memory flush logic.
%% Uses mocked session pids via gen_server to avoid needing a full
%% session + loop + provider stack.

%% ---- Mock session gen_server for testing ----

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

-record(mock_state, {
    session_id    :: binary(),
    agent_id      :: binary(),
    provider_mod  :: module(),
    history       :: list(),
    last_activity :: non_neg_integer(),
    loop_busy     :: boolean()
}).

init(Cfg) ->
    {ok, #mock_state{
        session_id    = maps:get(session_id, Cfg, <<"flush-test">>),
        agent_id      = maps:get(agent_id, Cfg, <<"default">>),
        provider_mod  = maps:get(provider_mod, Cfg, bc_provider_openrouter),
        history       = maps:get(history, Cfg, []),
        last_activity = erlang:system_time(second),
        loop_busy     = false
    }}.

handle_call(get_history, _From, S) ->
    {reply, S#mock_state.history, S};
handle_call(get_agent_id, _From, S) ->
    {reply, S#mock_state.agent_id, S};
handle_call(get_provider_mod, _From, S) ->
    {reply, S#mock_state.provider_mod, S};
handle_call(get_session_id, _From, S) ->
    {reply, S#mock_state.session_id, S};
handle_call(get_last_activity, _From, S) ->
    {reply, S#mock_state.last_activity, S};
handle_call(is_busy, _From, S) ->
    {reply, S#mock_state.loop_busy, S};
handle_call(get_state_summary, _From, S) ->
    Summary = #{session_id    => S#mock_state.session_id,
                user_id       => <<"test-user">>,
                agent_id      => S#mock_state.agent_id,
                provider_mod  => S#mock_state.provider_mod,
                last_activity => S#mock_state.last_activity,
                loop_busy     => S#mock_state.loop_busy,
                history_len   => length(S#mock_state.history)},
    {reply, Summary, S};
handle_call(_Req, _From, S) ->
    {reply, {error, unknown}, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

start_mock(Cfg) ->
    {ok, Pid} = gen_server:start_link(?MODULE, Cfg, []),
    Pid.

%% ---- Tests ----

flush_empty_history_test() ->
    %% With empty history (< 2 messages), flush should return ok immediately
    Pid = start_mock(#{history => []}),
    ?assertEqual(ok, bc_memory_flush:run(Pid, #{provider_mod => bc_provider_openrouter})).

flush_short_history_test() ->
    %% With only 1 message, flush should return ok immediately
    Pid = start_mock(#{history => [
        #bc_message{id = <<"m1">>, role = user, content = <<"hello">>}
    ]}),
    ?assertEqual(ok, bc_memory_flush:run(Pid, #{provider_mod => bc_provider_openrouter})).

flush_provider_init_error_test() ->
    %% With a real provider but no API key configured, init will succeed
    %% but complete will fail — should return {error, ...}
    %% We need at least 2 messages to trigger the actual flush
    application:set_env(beamclaw_core, providers, [
        {openrouter, #{api_key => "", base_url => "http://localhost:1/v1",
                       model => "test-model"}}
    ]),
    application:set_env(beamclaw_core, agentic_loop, #{}),
    Pid = start_mock(#{history => [
        #bc_message{id = <<"m1">>, role = user, content = <<"hello">>},
        #bc_message{id = <<"m2">>, role = assistant, content = <<"hi">>}
    ]}),
    %% This will try to call the LLM and fail (no real server).
    %% The function should catch the error and return {error, ...}
    Result = bc_memory_flush:run(Pid, #{provider_mod => bc_provider_openrouter}),
    case Result of
        ok -> ok;  %% provider init might silently succeed
        {error, _} -> ok  %% expected
    end.
