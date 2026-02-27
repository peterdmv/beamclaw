-module(bc_provider_websearch_mock).
-moduledoc """
Mock LLM provider that returns a web_search tool call on first stream,
then a normal assistant response. Used to test bc_loop web_search
tool execution path in bc_agentic_loop_SUITE.
""".
-behaviour(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([init/1, complete/3, stream/4, capabilities/1, terminate/2]).

init(_Config) ->
    {ok, #{call_count => 0}}.

complete(_Messages, _Options, State) ->
    Msg = #bc_message{id = <<"ws-mock-complete">>, role = assistant,
                      content = <<"done">>, ts = 0},
    {ok, Msg, State}.

stream(_Messages, _Options, CallerPid, #{call_count := N} = State) ->
    case N of
        0 ->
            ToolCall = #{<<"id">> => <<"ws-tc-1">>,
                         <<"name">> => <<"web_search">>,
                         <<"args">> => #{<<"query">> => <<"erlang otp 28">>}},
            Msg = #bc_message{
                id         = <<"ws-mock-1">>,
                role       = assistant,
                content    = <<"Searching the web...">>,
                tool_calls = [ToolCall],
                ts         = 0
            },
            CallerPid ! {stream_chunk, self(), <<"Searching the web...">>},
            CallerPid ! {stream_done, self(), Msg},
            {ok, State#{call_count := 1}};
        _ ->
            Msg = #bc_message{
                id      = <<"ws-mock-2">>,
                role    = assistant,
                content = <<"Here are the search results.">>,
                ts      = 0
            },
            CallerPid ! {stream_chunk, self(), <<"Here are the search results.">>},
            CallerPid ! {stream_done, self(), Msg},
            {ok, State#{call_count := N + 1}}
    end.

capabilities(_State) ->
    #{supports_streaming => true, supports_tools => true}.

terminate(_Reason, _State) ->
    ok.
