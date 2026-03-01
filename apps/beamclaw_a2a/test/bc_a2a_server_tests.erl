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
-module(bc_a2a_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include("bc_a2a_types.hrl").

%% Test JSON-RPC dispatch (stateless, no task manager needed for error paths)

invalid_request_test() ->
    Resp = bc_a2a_server:handle_request(#{<<"foo">> => <<"bar">>}),
    ?assertMatch(#{<<"error">> := #{<<"code">> := -32600}}, Resp).

method_not_found_test() ->
    Req = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"unknown/method">>},
    Resp = bc_a2a_server:handle_request(Req),
    ?assertMatch(#{<<"error">> := #{<<"code">> := -32601}}, Resp).

missing_id_param_test() ->
    Req = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
            <<"method">> => <<"tasks/get">>, <<"params">> => #{}},
    Resp = bc_a2a_server:handle_request(Req),
    ?assertMatch(#{<<"error">> := #{<<"code">> := -32602}}, Resp).

notification_returns_undefined_test() ->
    Req = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"message/send">>},
    ?assertEqual(undefined, bc_a2a_server:handle_request(Req)).

agent_card_test() ->
    Card = bc_a2a_server:agent_card(),
    ?assert(is_map(Card)),
    ?assert(maps:is_key(<<"name">>, Card)),
    ?assert(maps:is_key(<<"capabilities">>, Card)),
    ?assert(maps:is_key(<<"skills">>, Card)).

agent_card_has_version_test() ->
    Card = bc_a2a_server:agent_card(),
    Version = maps:get(<<"version">>, Card),
    ?assert(is_binary(Version)).
