%%
%% Copyright The BeamClaw Authors 2026, All Rights Reserved.
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
-module(bc_a2a_auth_tests).
-include_lib("eunit/include/eunit.hrl").

%% --- verify_bearer/2 ---

verify_matching_token_test() ->
    ?assertEqual(ok, bc_a2a_http_h:verify_bearer(<<"my-secret">>, <<"my-secret">>)).

verify_mismatched_token_test() ->
    ?assertMatch({error, _}, bc_a2a_http_h:verify_bearer(<<"wrong">>, <<"my-secret">>)).

verify_different_length_token_test() ->
    ?assertMatch({error, _}, bc_a2a_http_h:verify_bearer(<<"short">>, <<"much-longer-token">>)).

%% --- resolve_bearer_token/0 ---

resolve_token_unset_test() ->
    os:unsetenv("A2A_BEARER_TOKEN"),
    ?assertEqual(undefined, bc_a2a_http_h:resolve_bearer_token()).

resolve_token_empty_test() ->
    os:putenv("A2A_BEARER_TOKEN", ""),
    ?assertEqual(undefined, bc_a2a_http_h:resolve_bearer_token()),
    os:unsetenv("A2A_BEARER_TOKEN").

resolve_token_set_test() ->
    os:putenv("A2A_BEARER_TOKEN", "test-token-123"),
    ?assertEqual(<<"test-token-123">>, bc_a2a_http_h:resolve_bearer_token()),
    os:unsetenv("A2A_BEARER_TOKEN").
