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

-module(bc_scrubber_tests).
-moduledoc "EUnit tests for bc_scrubber credential redaction.".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% ---- scrub/1 — passthrough ----

passthrough_test() ->
    ?assertEqual(<<"hello world">>, bc_scrubber:scrub(<<"hello world">>)).

non_binary_integer_test() ->
    ?assertEqual(42, bc_scrubber:scrub(42)).

non_binary_atom_test() ->
    ?assertEqual(foo, bc_scrubber:scrub(foo)).

%% ---- scrub/1 — api_key patterns ----

api_key_equals_test() ->
    R = bc_scrubber:scrub(<<"api_key=supersecret">>),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch),
    ?assert(binary:match(R, <<"supersecret">>) =:= nomatch).

api_key_colon_test() ->
    R = bc_scrubber:scrub(<<"api_key: supersecret">>),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch).

apikey_no_separator_test() ->
    %% "apikey" variant (no underscore)
    R = bc_scrubber:scrub(<<"apikey=supersecret">>),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch).

%% ---- scrub/1 — token pattern ----

token_test() ->
    R = bc_scrubber:scrub(<<"token=abc123xyz">>),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch),
    ?assert(binary:match(R, <<"abc123xyz">>) =:= nomatch).

%% ---- scrub/1 — password pattern ----

password_test() ->
    R = bc_scrubber:scrub(<<"password: hunter2">>),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch).

%% ---- scrub/1 — secret pattern ----

secret_test() ->
    R = bc_scrubber:scrub(<<"secret=mysecretvalue">>),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch).

%% ---- scrub/1 — Bearer token ----

bearer_test() ->
    Input = <<"Authorization: Bearer eyJhbGciOiJSUzI1NiJ9.token">>,
    R = bc_scrubber:scrub(Input),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch),
    ?assert(binary:match(R, <<"eyJhbGciOiJSUzI1NiJ9.token">>) =:= nomatch).

%% ---- scrub/1 — OpenAI key (sk-) ----

openai_key_test() ->
    %% Must be >= 20 alnum chars after "sk-"
    R = bc_scrubber:scrub(<<"sk-abcdefghijklmnopqrstuvwxyz1234">>),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch).

openai_key_too_short_not_matched_test() ->
    %% < 20 chars — should NOT match
    R = bc_scrubber:scrub(<<"sk-tooshort">>),
    ?assertEqual(<<"sk-tooshort">>, R).

%% ---- scrub/1 — GitHub PAT (ghp_) ----

github_ghp_test() ->
    Pat = <<"ghp_", (binary:copy(<<"A">>, 36))/binary>>,
    R   = bc_scrubber:scrub(Pat),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch).

%% ---- scrub/1 — GitHub server token (ghs_) ----

github_ghs_test() ->
    Pat = <<"ghs_", (binary:copy(<<"B">>, 36))/binary>>,
    R   = bc_scrubber:scrub(Pat),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch).

%% ---- scrub/1 — AWS access key ----

aws_key_test() ->
    R = bc_scrubber:scrub(<<"AKIAIOSFODNN7EXAMPLE1234">>),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch).

%% ---- scrub/1 — multiple secrets in one string ----

multiple_secrets_test() ->
    Input = <<"token=abc123 and Bearer xyz789abc">>,
    R = bc_scrubber:scrub(Input),
    ?assert(binary:match(R, <<"abc123">>) =:= nomatch),
    ?assert(binary:match(R, <<"xyz789abc">>) =:= nomatch).

%% ---- scrub_message/1 ----

scrub_message_binary_content_test() ->
    Msg = #bc_message{id = <<"1">>, role = user,
                      content = <<"Bearer eyJhbGciOiJSUzI1NiJ9.rest">>},
    Scrubbed = bc_scrubber:scrub_message(Msg),
    ?assert(binary:match(Scrubbed#bc_message.content, <<"[REDACTED]">>) =/= nomatch).

scrub_message_preserves_other_fields_test() ->
    Msg = #bc_message{id = <<"42">>, role = assistant,
                      content = <<"clean text">>, ts = 1000},
    Scrubbed = bc_scrubber:scrub_message(Msg),
    ?assertEqual(<<"42">>,      Scrubbed#bc_message.id),
    ?assertEqual(assistant,     Scrubbed#bc_message.role),
    ?assertEqual(<<"clean text">>, Scrubbed#bc_message.content),
    ?assertEqual(1000,          Scrubbed#bc_message.ts).

scrub_message_undefined_content_test() ->
    %% Non-binary content → message returned unchanged
    Msg = #bc_message{id = <<"2">>, role = tool, content = undefined},
    ?assertEqual(Msg, bc_scrubber:scrub_message(Msg)).

%% ---- scrub_result/1 ----

scrub_result_test() ->
    R = #bc_tool_result{tool_call_id = <<"x">>, name = <<"bash">>,
                        content = <<"api_key=topsecret">>},
    Scrubbed = bc_scrubber:scrub_result(R),
    ?assert(binary:match(Scrubbed#bc_tool_result.content, <<"[REDACTED]">>) =/= nomatch),
    ?assert(binary:match(Scrubbed#bc_tool_result.content, <<"topsecret">>) =:= nomatch).

scrub_result_clean_content_test() ->
    R = #bc_tool_result{tool_call_id = <<"y">>, name = <<"read_file">>,
                        content = <<"some file content">>},
    Scrubbed = bc_scrubber:scrub_result(R),
    ?assertEqual(<<"some file content">>, Scrubbed#bc_tool_result.content).

%% ---- scrub/1 — env var references preserved ----

token_env_var_passthrough_test() ->
    ?assertEqual(<<"token=$FINNHUB_TOKEN">>,
                 bc_scrubber:scrub(<<"token=$FINNHUB_TOKEN">>)).

api_key_env_var_passthrough_test() ->
    ?assertEqual(<<"api_key=$MY_KEY">>,
                 bc_scrubber:scrub(<<"api_key=$MY_KEY">>)).

password_env_var_passthrough_test() ->
    ?assertEqual(<<"password=$DB_PASS">>,
                 bc_scrubber:scrub(<<"password=$DB_PASS">>)).

secret_env_var_passthrough_test() ->
    ?assertEqual(<<"secret=$MY_SECRET">>,
                 bc_scrubber:scrub(<<"secret=$MY_SECRET">>)).

token_real_value_still_scrubbed_test() ->
    R = bc_scrubber:scrub(<<"token=abc123xyz">>),
    ?assert(binary:match(R, <<"[REDACTED]">>) =/= nomatch),
    ?assert(binary:match(R, <<"abc123xyz">>) =:= nomatch).

%% ---- scrub_map/1 ----

scrub_map_test() ->
    M = #{<<"command">> => <<"curl -H 'token=real_secret' http://example.com">>},
    R = bc_scrubber:scrub_map(M),
    ?assert(binary:match(maps:get(<<"command">>, R), <<"real_secret">>) =:= nomatch).

scrub_map_passthrough_test() ->
    M = #{<<"command">> => <<"ls -la">>},
    ?assertEqual(M, bc_scrubber:scrub_map(M)).

scrub_map_non_binary_values_test() ->
    M = #{<<"count">> => 42, <<"flag">> => true},
    ?assertEqual(M, bc_scrubber:scrub_map(M)).
