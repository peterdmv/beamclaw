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

-module(bc_loop_attachment_tests).
-moduledoc "EUnit tests for bc_loop:strip_old_attachments/2.".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% Helper to create a user message with attachments.
user_msg(Content, Attachments) ->
    #bc_message{id = Content, role = user, content = Content,
                attachments = Attachments}.

%% Helper to create a user message without attachments.
user_msg(Content) ->
    #bc_message{id = Content, role = user, content = Content}.

%% Helper to create an assistant message.
assistant_msg(Content) ->
    #bc_message{id = Content, role = assistant, content = Content}.

%% Helper to create a system message.
system_msg(Content) ->
    #bc_message{id = Content, role = system, content = Content}.

%% Helper to create a tool message.
tool_msg(Content) ->
    #bc_message{id = Content, role = tool, content = Content}.

img() -> [{<<"image/jpeg">>, <<"base64data">>}].

%% ---- No attachments at all ----

no_attachments_untouched_test() ->
    Msgs = [system_msg(<<"sys">>), user_msg(<<"hello">>), assistant_msg(<<"hi">>)],
    ?assertEqual(Msgs, bc_loop:strip_old_attachments(Msgs, 5)).

empty_list_test() ->
    ?assertEqual([], bc_loop:strip_old_attachments([], 5)).

%% ---- Fewer than N user messages — all kept ----

fewer_than_n_keeps_all_test() ->
    Msgs = [user_msg(<<"u1">>, img()),
            assistant_msg(<<"a1">>),
            user_msg(<<"u2">>, img()),
            assistant_msg(<<"a2">>)],
    Result = bc_loop:strip_old_attachments(Msgs, 5),
    %% Both user messages should retain attachments
    [U1, _, U2, _] = Result,
    ?assertEqual(img(), U1#bc_message.attachments),
    ?assertEqual(img(), U2#bc_message.attachments).

%% ---- Exactly N user messages — all kept ----

exactly_n_keeps_all_test() ->
    Msgs = [user_msg(<<"u1">>, img()),
            user_msg(<<"u2">>, img()),
            user_msg(<<"u3">>, img())],
    Result = bc_loop:strip_old_attachments(Msgs, 3),
    lists:foreach(fun(M) ->
        ?assertEqual(img(), M#bc_message.attachments)
    end, Result).

%% ---- More than N user messages — old ones stripped ----

old_attachments_stripped_test() ->
    %% 6 user messages with attachments, keep=3
    Msgs = [user_msg(<<"u1">>, img()),     %% old → strip
            assistant_msg(<<"a1">>),
            user_msg(<<"u2">>, img()),     %% old → strip
            assistant_msg(<<"a2">>),
            user_msg(<<"u3">>, img()),     %% old → strip
            user_msg(<<"u4">>, img()),     %% recent → keep
            user_msg(<<"u5">>, img()),     %% recent → keep
            user_msg(<<"u6">>, img())],    %% recent → keep
    Result = bc_loop:strip_old_attachments(Msgs, 3),
    [R1, _, R2, _, R3, R4, R5, R6] = Result,
    %% Old messages stripped
    ?assertEqual([], R1#bc_message.attachments),
    ?assertEqual([], R2#bc_message.attachments),
    ?assertEqual([], R3#bc_message.attachments),
    %% Recent messages kept
    ?assertEqual(img(), R4#bc_message.attachments),
    ?assertEqual(img(), R5#bc_message.attachments),
    ?assertEqual(img(), R6#bc_message.attachments).

%% ---- Non-user messages never stripped ----

non_user_messages_untouched_test() ->
    Msgs = [system_msg(<<"sys">>),
            user_msg(<<"u1">>, img()),
            assistant_msg(<<"a1">>),
            tool_msg(<<"t1">>),
            user_msg(<<"u2">>, img())],
    Result = bc_loop:strip_old_attachments(Msgs, 1),
    [Sys, U1, Asst, Tool, U2] = Result,
    %% System, assistant, tool messages unchanged
    ?assertEqual(system_msg(<<"sys">>), Sys),
    ?assertEqual(assistant_msg(<<"a1">>), Asst),
    ?assertEqual(tool_msg(<<"t1">>), Tool),
    %% Old user message stripped, recent kept
    ?assertEqual([], U1#bc_message.attachments),
    ?assertEqual(img(), U2#bc_message.attachments).

%% ---- User messages without attachments count toward limit ----

user_without_attachments_counts_test() ->
    %% 4 user messages: u1 has img, u2 plain, u3 plain, u4 has img. keep=2
    %% From end: u4 (recent, count 1), u3 (recent, count 2), u2 (old), u1 (old)
    Msgs = [user_msg(<<"u1">>, img()),
            user_msg(<<"u2">>),
            user_msg(<<"u3">>),
            user_msg(<<"u4">>, img())],
    Result = bc_loop:strip_old_attachments(Msgs, 2),
    [R1, R2, R3, R4] = Result,
    ?assertEqual([], R1#bc_message.attachments),         %% old → stripped
    ?assertEqual(<<"u2">>, R2#bc_message.content),       %% plain → unchanged
    ?assertEqual(<<"u3">>, R3#bc_message.content),       %% plain → unchanged
    ?assertEqual(img(), R4#bc_message.attachments).      %% recent → kept

%% ---- Content preserved when attachments stripped ----

content_preserved_on_strip_test() ->
    Msgs = [user_msg(<<"[Attached image saved to /tmp/bc_attach_123.jpg]\nPlease edit this">>, img()),
            user_msg(<<"another message">>),
            user_msg(<<"third">>),
            user_msg(<<"fourth">>),
            user_msg(<<"fifth">>),
            user_msg(<<"sixth">>)],
    Result = bc_loop:strip_old_attachments(Msgs, 5),
    [R1 | _] = Result,
    ?assertEqual([], R1#bc_message.attachments),
    ?assertEqual(<<"[Attached image saved to /tmp/bc_attach_123.jpg]\nPlease edit this">>,
                 R1#bc_message.content).

%% ---- keep=0 strips all ----

keep_zero_strips_all_test() ->
    Msgs = [user_msg(<<"u1">>, img()),
            user_msg(<<"u2">>, img())],
    Result = bc_loop:strip_old_attachments(Msgs, 0),
    [R1, R2] = Result,
    ?assertEqual([], R1#bc_message.attachments),
    ?assertEqual([], R2#bc_message.attachments).

%% ---- Interleaved system/assistant messages don't affect count ----

interleaved_non_user_test() ->
    Msgs = [system_msg(<<"s1">>),
            user_msg(<<"u1">>, img()),
            assistant_msg(<<"a1">>),
            system_msg(<<"s2">>),
            user_msg(<<"u2">>, img()),
            assistant_msg(<<"a2">>),
            user_msg(<<"u3">>, img())],
    Result = bc_loop:strip_old_attachments(Msgs, 2),
    [_, R1, _, _, R2, _, R3] = Result,
    ?assertEqual([], R1#bc_message.attachments),     %% old
    ?assertEqual(img(), R2#bc_message.attachments),  %% recent (2nd from end)
    ?assertEqual(img(), R3#bc_message.attachments).  %% recent (1st from end)
