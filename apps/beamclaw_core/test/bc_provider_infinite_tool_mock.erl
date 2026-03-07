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

-module(bc_provider_infinite_tool_mock).
-moduledoc """
Mock LLM provider that always returns tool calls (never a final response).

Used to test bc_loop's max_tool_iterations safety limit. Every stream call
returns a tool call for the `noop_tool`.
""".
-behaviour(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([init/1, complete/3, stream/4, capabilities/1, terminate/2]).

init(_Config) ->
    {ok, #{call_count => 0}}.

complete(_Messages, _Options, State) ->
    {ok, #bc_message{role = assistant, content = <<"loop">>}, State}.

stream(_Messages, _Options, CallerPid, #{call_count := N} = State) ->
    Id = iolist_to_binary(io_lib:format("tc-inf-~B", [N])),
    ToolCall = #{<<"id">> => Id,
                 <<"name">> => <<"noop_tool">>,
                 <<"args">> => #{}},
    Msg = #bc_message{
        id         = Id,
        role       = assistant,
        content    = <<"Calling noop_tool again...">>,
        tool_calls = [ToolCall],
        ts         = 0
    },
    CallerPid ! {stream_chunk, self(), <<"Calling noop_tool...">>},
    CallerPid ! {stream_done, self(), Msg},
    {ok, State#{call_count := N + 1}}.

capabilities(_State) ->
    #{supports_streaming => true, supports_tools => true}.

terminate(_Reason, _State) ->
    ok.
