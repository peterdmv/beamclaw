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

-module(bc_scrubber).
-moduledoc """
Credential scrubbing.

Applied to every tool result before it enters history or is sent to the LLM.
Uses regex patterns to redact known credential formats.
""".

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([scrub/1, scrub_message/1, scrub_result/1, scrub_map/1]).

-define(PATTERNS, [
    %% Generic key=value patterns (negative lookahead skips $VAR references)
    "api[_-]?key\\s*[:=]\\s*(?!\\$)\\S+",
    "token\\s*[:=]\\s*(?!\\$)\\S+",
    "password\\s*[:=]\\s*(?!\\$)\\S+",
    "secret\\s*[:=]\\s*(?!\\$)\\S+",
    %% Bearer tokens
    "Bearer\\s+\\S+",
    %% OpenAI keys
    "sk-[A-Za-z0-9_-]{20,}",
    %% GitHub PATs
    "ghp_[A-Za-z0-9]{36}",
    "ghs_[A-Za-z0-9]{36}",
    %% AWS keys (rough pattern)
    "AKIA[0-9A-Z]{16}"
]).

-doc "Scrub a binary string.".
-spec scrub(binary()) -> binary().
scrub(Text) when is_binary(Text) ->
    lists:foldl(fun(Pattern, Acc) ->
        re:replace(Acc, Pattern, <<"[REDACTED]">>,
                   [global, caseless, {return, binary}])
    end, Text, ?PATTERNS);
scrub(Other) ->
    Other.

-doc "Scrub the content field of a bc_message.".
-spec scrub_message(#bc_message{}) -> #bc_message{}.
scrub_message(#bc_message{content = Content} = Msg) when is_binary(Content) ->
    Msg#bc_message{content = scrub(Content)};
scrub_message(Msg) ->
    Msg.

-doc "Scrub the content field of a bc_tool_result.".
-spec scrub_result(#bc_tool_result{}) -> #bc_tool_result{}.
scrub_result(#bc_tool_result{content = Content} = Result) ->
    Result#bc_tool_result{content = scrub(Content)}.

-doc "Scrub binary values in a map (e.g. tool call args before obs logging).".
-spec scrub_map(map()) -> map().
scrub_map(Map) when is_map(Map) ->
    maps:map(fun(_K, V) when is_binary(V) -> scrub(V);
                (_K, V) -> V
             end, Map);
scrub_map(Other) ->
    Other.
