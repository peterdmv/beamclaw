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

-module(bc_tool_parser).
-moduledoc """
Tool call parsing fallback chain.

Tries in order:
  1. OpenAI native tool_calls field (non-empty list)
  2. XML tags: <tool_call>, <toolcall>, <invoke> with <name>/<args>
  3. Markdown code block: ```json\n{"tool":"foo","args":{...}}\n```
  4. Empty — return []

Security: only match structured delimiters, never extract arbitrary JSON from free text.
""".

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([parse/1]).

-doc "Parse tool calls from a bc_message. Returns list of bc_tool_call records.".
-spec parse(#bc_message{}) -> [#bc_tool_call{}].
parse(#bc_message{tool_calls = [_ | _] = Calls}) ->
    %% 1. Native OpenAI tool_calls
    [native_to_record(C) || C <- Calls];
parse(#bc_message{content = Content}) when is_binary(Content) ->
    case parse_xml(Content) of
        [_ | _] = Results -> Results;
        [] ->
            case parse_markdown(Content) of
                [_ | _] = Results -> Results;
                [] -> []
            end
    end;
parse(_) ->
    [].

%% ---- Native ----

native_to_record(#{<<"id">> := Id, <<"function">> := #{<<"name">> := Name,
                                                        <<"arguments">> := ArgsJson}}) ->
    Args = decode_args(ArgsJson),
    %% Some providers emit an empty-string id for a tool call; falling through
    %% with id = <<>> would get baked into history as an invalid tool_call_id
    %% and permanently break the session on the next request (the provider
    %% rejects tool messages with a non-empty-string tool_call_id requirement).
    SafeId = case Id of <<>> -> generate_id(); _ -> Id end,
    #bc_tool_call{id = SafeId, name = Name, args = Args, source = native};
native_to_record(Map) when is_map(Map) ->
    #bc_tool_call{
        id     = maps:get(<<"id">>,   Map, generate_id()),
        name   = maps:get(<<"name">>, Map, <<>>),
        args   = maps:get(<<"args">>, Map, #{}),
        source = native
    }.

%% ---- XML tags ----
%% Matches: <tool_call>, <toolcall>, <invoke>

parse_xml(Content) ->
    Patterns = [
        <<"<tool_call>">>,
        <<"<toolcall>">>,
        <<"<invoke>">>
    ],
    lists:flatmap(fun(Tag) -> parse_xml_tag(Content, Tag) end, Patterns).

parse_xml_tag(Content, OpenTag) ->
    CloseTag = close_tag(OpenTag),
    case binary:split(Content, OpenTag, [global]) of
        [_] -> [];
        [_ | Parts] ->
            lists:filtermap(fun(Part) ->
                case binary:split(Part, CloseTag) of
                    [Body | _] -> extract_xml_fields(Body);
                    _          -> false
                end
            end, Parts)
    end.

close_tag(<<"<tool_call>">>) -> <<"</tool_call>">>;
close_tag(<<"<toolcall>">>)  -> <<"</toolcall>">>;
close_tag(<<"<invoke>">>)    -> <<"</invoke>">>.

extract_xml_fields(Body) ->
    case {xml_field(Body, <<"name">>), xml_field(Body, <<"args">>)} of
        {{ok, Name}, {ok, ArgsStr}} ->
            Args = decode_args(ArgsStr),
            {true, #bc_tool_call{id = generate_id(), name = Name,
                                  args = Args, source = xml}};
        _ -> false
    end.

xml_field(Body, Field) ->
    Open  = <<"<", Field/binary, ">">>,
    Close = <<"</", Field/binary, ">">>,
    case binary:split(Body, Open) of
        [_, Rest] ->
            case binary:split(Rest, Close) of
                [Value | _] -> {ok, string:trim(Value)};
                _            -> error
            end;
        _ -> error
    end.

%% ---- Markdown code blocks ----
%% Matches: ```json\n{...}\n```

parse_markdown(Content) ->
    case binary:split(Content, <<"```json">>, [global]) of
        [_] -> [];
        [_ | Parts] ->
            lists:filtermap(fun(Part) ->
                case binary:split(Part, <<"```">>) of
                    [JsonStr | _] ->
                        Trimmed = string:trim(JsonStr),
                        parse_json_tool(Trimmed);
                    _ -> false
                end
            end, Parts)
    end.

parse_json_tool(Json) ->
    try jsx:decode(Json, [return_maps]) of
        #{<<"tool">> := Name, <<"args">> := Args} ->
            {true, #bc_tool_call{id = generate_id(), name = Name,
                                  args = Args, source = markdown}};
        #{<<"name">> := Name, <<"args">> := Args} ->
            {true, #bc_tool_call{id = generate_id(), name = Name,
                                  args = Args, source = markdown}};
        _ -> false
    catch _:_ -> false
    end.

%% ---- Helpers ----

decode_args(ArgsJson) when is_binary(ArgsJson) ->
    try jsx:decode(ArgsJson, [return_maps])
    catch _:_ -> #{<<"raw">> => ArgsJson}
    end;
decode_args(Args) when is_map(Args) -> Args;
decode_args(_) -> #{}.

generate_id() ->
    <<N:64>> = crypto:strong_rand_bytes(8),
    iolist_to_binary(io_lib:format("tc_~16.16.0b", [N])).
