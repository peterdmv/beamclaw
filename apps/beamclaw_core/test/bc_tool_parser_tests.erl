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

-module(bc_tool_parser_tests).
-moduledoc "EUnit tests for bc_tool_parser fallback chain.".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% Helper: build an assistant message with just content.
msg(Content) ->
    #bc_message{id = <<"t">>, role = assistant, content = Content}.

%% ---- Fallback: empty / no match ----

no_match_plain_text_test() ->
    ?assertEqual([], bc_tool_parser:parse(msg(<<"just plain text">>))).

%% Security rule: free-text JSON must NOT be extracted.
no_match_free_text_json_test() ->
    Content = <<"{\"tool\":\"bash\",\"args\":{\"cmd\":\"ls\"}}">>,
    ?assertEqual([], bc_tool_parser:parse(msg(Content))).

no_content_field_test() ->
    ?assertEqual([], bc_tool_parser:parse(#bc_message{role = user})).

%% ---- Native OpenAI tool_calls ----

native_tool_calls_test() ->
    Call = #{<<"id">>       => <<"tc_1">>,
             <<"function">> => #{<<"name">>      => <<"bash">>,
                                 <<"arguments">> => <<"{\"cmd\":\"ls\"}">>}},
    Msg  = #bc_message{id = <<"1">>, role = assistant,
                       content = <<>>, tool_calls = [Call]},
    [TC] = bc_tool_parser:parse(Msg),
    ?assertEqual(<<"bash">>,             TC#bc_tool_call.name),
    ?assertEqual(#{<<"cmd">> => <<"ls">>}, TC#bc_tool_call.args),
    ?assertEqual(native,                 TC#bc_tool_call.source).

native_multiple_calls_test() ->
    MkCall = fun(Id, Name) ->
        #{<<"id">>       => Id,
          <<"function">> => #{<<"name">> => Name, <<"arguments">> => <<"{}">>}}
    end,
    Msg = #bc_message{id = <<"2">>, role = assistant, content = <<>>,
                      tool_calls = [MkCall(<<"id1">>, <<"bash">>),
                                    MkCall(<<"id2">>, <<"jq">>)]},
    TCs = bc_tool_parser:parse(Msg),
    ?assertEqual(2, length(TCs)),
    Names = [TC#bc_tool_call.name || TC <- TCs],
    ?assert(lists:member(<<"bash">>, Names)),
    ?assert(lists:member(<<"jq">>, Names)).

%% ---- Native: empty id falls back to a generated id ----
%% Regression: some providers emit "id": "" for a tool call. Trusting it
%% verbatim bakes an empty tool_call_id into session history, which the
%% provider then permanently rejects on every subsequent request.

native_empty_id_falls_back_test() ->
    Call = #{<<"id">>       => <<>>,
             <<"function">> => #{<<"name">>      => <<"bash">>,
                                 <<"arguments">> => <<"{}">>}},
    Msg  = #bc_message{id = <<"4">>, role = assistant,
                       content = <<>>, tool_calls = [Call]},
    [TC] = bc_tool_parser:parse(Msg),
    ?assertEqual(<<"bash">>, TC#bc_tool_call.name),
    ?assertNotEqual(<<>>, TC#bc_tool_call.id).

%% ---- XML <tool_call> tag ----

xml_tool_call_tag_test() ->
    Content = <<"text <tool_call><name>read_file</name>"
                "<args>{\"path\":\"/etc/hosts\"}</args></tool_call> tail">>,
    [TC] = bc_tool_parser:parse(msg(Content)),
    ?assertEqual(<<"read_file">>,                TC#bc_tool_call.name),
    ?assertEqual(#{<<"path">> => <<"/etc/hosts">>}, TC#bc_tool_call.args),
    ?assertEqual(xml,                            TC#bc_tool_call.source).

%% ---- XML <toolcall> tag ----

xml_toolcall_tag_test() ->
    Content = <<"<toolcall><name>bash</name>"
                "<args>{\"cmd\":\"pwd\"}</args></toolcall>">>,
    [TC] = bc_tool_parser:parse(msg(Content)),
    ?assertEqual(<<"bash">>, TC#bc_tool_call.name),
    ?assertEqual(xml,        TC#bc_tool_call.source).

%% ---- XML <invoke> tag ----

xml_invoke_tag_test() ->
    Content = <<"<invoke><name>curl</name>"
                "<args>{\"url\":\"http://example.com\"}</args></invoke>">>,
    [TC] = bc_tool_parser:parse(msg(Content)),
    ?assertEqual(<<"curl">>, TC#bc_tool_call.name).

%% ---- XML: multiple calls in one message ----

xml_multiple_calls_test() ->
    Content = <<"<tool_call><name>a</name><args>{}</args></tool_call>"
                "<tool_call><name>b</name><args>{}</args></tool_call>">>,
    TCs = bc_tool_parser:parse(msg(Content)),
    ?assertEqual(2, length(TCs)),
    Names = [TC#bc_tool_call.name || TC <- TCs],
    ?assert(lists:member(<<"a">>, Names)),
    ?assert(lists:member(<<"b">>, Names)).

%% ---- Markdown code block with "tool" key ----

markdown_tool_key_test() ->
    Content = <<"prefix\n```json\n"
                "{\"tool\":\"jq\",\"args\":{\"filter\":\".x\"}}"
                "\n```\nsuffix">>,
    [TC] = bc_tool_parser:parse(msg(Content)),
    ?assertEqual(<<"jq">>,                    TC#bc_tool_call.name),
    ?assertEqual(#{<<"filter">> => <<".x">>}, TC#bc_tool_call.args),
    ?assertEqual(markdown,                    TC#bc_tool_call.source).

%% ---- Markdown code block with "name" key ----

markdown_name_key_test() ->
    Content = <<"```json\n"
                "{\"name\":\"terminal\",\"args\":{\"cmd\":\"date\"}}"
                "\n```">>,
    [TC] = bc_tool_parser:parse(msg(Content)),
    ?assertEqual(<<"terminal">>, TC#bc_tool_call.name),
    ?assertEqual(markdown,       TC#bc_tool_call.source).

%% ---- Markdown: malformed JSON returns empty ----

markdown_invalid_json_test() ->
    Content = <<"```json\nnot valid json\n```">>,
    ?assertEqual([], bc_tool_parser:parse(msg(Content))).

%% ---- Native takes priority over XML ----

native_takes_priority_over_xml_test() ->
    Call = #{<<"id">>       => <<"tc_1">>,
             <<"function">> => #{<<"name">> => <<"bash">>,
                                 <<"arguments">> => <<"{}">>}},
    %% Message has BOTH native tool_calls AND XML content.
    %% Native wins (step 1 of fallback chain).
    Msg = #bc_message{id = <<"3">>, role = assistant,
                      content = <<"<tool_call><name>jq</name><args>{}</args></tool_call>">>,
                      tool_calls = [Call]},
    [TC] = bc_tool_parser:parse(Msg),
    ?assertEqual(<<"bash">>, TC#bc_tool_call.name),
    ?assertEqual(native,     TC#bc_tool_call.source).
