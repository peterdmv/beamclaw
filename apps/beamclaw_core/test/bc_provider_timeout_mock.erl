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

-module(bc_provider_timeout_mock).
-moduledoc """
Mock LLM provider that never responds (simulates stream timeout).

stream/4 returns {ok, State} but sends nothing to CallerPid, triggering
bc_loop's receive_stream 60s timeout. For tests, the timetrap should be
set accordingly.
""".
-behaviour(bc_provider).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([init/1, complete/3, stream/4, capabilities/1, terminate/2]).

init(_Config) ->
    {ok, #{call_count => 0}}.

complete(_Messages, _Options, State) ->
    {error, timeout_simulated, State}.

stream(_Messages, _Options, CallerPid, #{call_count := N} = State) ->
    case N of
        0 ->
            %% First call: never send stream_done — triggers bc_loop's 60s timeout
            {ok, State#{call_count := 1}};
        _ ->
            %% Subsequent calls: succeed normally (proves loop recovered)
            Msg = #bc_message{
                id      = <<"timeout-recovery-1">>,
                role    = assistant,
                content = <<"Recovered after timeout">>,
                ts      = 0
            },
            CallerPid ! {stream_chunk, self(), <<"Recovered">>},
            CallerPid ! {stream_done, self(), Msg},
            {ok, State#{call_count := N + 1}}
    end.

capabilities(_State) ->
    #{supports_streaming => true, supports_tools => false}.

terminate(_Reason, _State) ->
    ok.
