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
-module(bc_a2a_http_h).
-moduledoc """
Cowboy handler for A2A protocol HTTP endpoints.

Routes (mounted in bc_gateway_cowboy):
  GET  /.well-known/agent.json  -> Agent Card discovery
  POST /a2a                     -> JSON-RPC 2.0 A2A requests
""".

-export([init/2]).
%% Exported for testing
-export([verify_bearer/2, resolve_bearer_token/0]).

init(Req0, State) ->
    ClientIp = peer_ip(Req0),
    Path = cowboy_req:path(Req0),
    case bc_rate_limiter:check(ClientIp, Path) of
        {error, rate_limited} ->
            Req = cowboy_req:reply(429,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{error => <<"rate limited">>}), Req0),
            {ok, Req, State};
        ok ->
            Method = cowboy_req:method(Req0),
            handle(Method, Path, Req0, State)
    end.

%% Agent Card discovery
handle(<<"GET">>, <<"/.well-known/agent.json">>, Req0, State) ->
    Card = bc_a2a_server:agent_card(),
    Body = jsx:encode(Card),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req0),
    bc_obs:emit(a2a_request, #{method => <<"GET">>, path => <<"/.well-known/agent.json">>}),
    {ok, Req, State};

%% A2A JSON-RPC endpoint (Bearer token auth on POST only)
handle(<<"POST">>, <<"/a2a">>, Req0, State) ->
    case authenticate(Req0) of
        ok ->
            handle_jsonrpc(Req0, State);
        {error, Reason} ->
            bc_obs:emit(a2a_auth_failed, #{client_ip => peer_ip(Req0),
                                           reason => Reason}),
            Req = cowboy_req:reply(401,
                #{<<"content-type">> => <<"application/json">>,
                  <<"www-authenticate">> => <<"Bearer">>},
                jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                             <<"id">> => null,
                             <<"error">> => #{<<"code">> => -32000,
                                              <<"message">> => <<"Authentication required">>}}),
                Req0),
            {ok, Req, State}
    end;

handle(_, _, Req0, State) ->
    Req = cowboy_req:reply(404, #{}, <<"Not Found">>, Req0),
    {ok, Req, State}.

%% --- Internal ---

handle_jsonrpc(Req0, State) ->
    {ok, RawBody, Req1} = cowboy_req:read_body(Req0),
    case catch jsx:decode(RawBody, [return_maps]) of
        Request when is_map(Request) ->
            case bc_a2a_server:handle_request(Request) of
                undefined ->
                    %% Notification — no response
                    Req = cowboy_req:reply(204, #{}, <<>>, Req1),
                    bc_obs:emit(a2a_request, #{method => <<"POST">>, path => <<"/a2a">>,
                                               rpc_method => maps:get(<<"method">>, Request, undefined)}),
                    {ok, Req, State};
                Response ->
                    Body = jsx:encode(Response),
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Body, Req1),
                    bc_obs:emit(a2a_request, #{method => <<"POST">>, path => <<"/a2a">>,
                                               rpc_method => maps:get(<<"method">>, Request, undefined)}),
                    {ok, Req, State}
            end;
        _ ->
            Error = jsx:encode(#{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">>      => null,
                <<"error">>   => #{<<"code">> => -32700, <<"message">> => <<"Parse error">>}
            }),
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                Error, Req1),
            {ok, Req, State}
    end.

%% Bearer token authentication — only on POST /a2a, not GET agent card
authenticate(Req) ->
    case resolve_bearer_token() of
        undefined -> ok;  %% No token configured = open access
        Configured ->
            case cowboy_req:header(<<"authorization">>, Req) of
                undefined ->
                    {error, <<"missing authorization header">>};
                <<"Bearer ", Token/binary>> ->
                    verify_bearer(Token, Configured);
                <<"bearer ", Token/binary>> ->
                    verify_bearer(Token, Configured);
                _ ->
                    {error, <<"invalid authorization scheme">>}
            end
    end.

-spec verify_bearer(binary(), binary()) -> ok | {error, binary()}.
verify_bearer(Provided, Configured) ->
    ProvBin = iolist_to_binary(Provided),
    ConfBin = iolist_to_binary(Configured),
    case constant_time_equals(ProvBin, ConfBin) of
        true  -> ok;
        false -> {error, <<"invalid bearer token">>}
    end.

constant_time_equals(A, B) when byte_size(A) =:= byte_size(B) ->
    crypto:hash_equals(A, B);
constant_time_equals(_, _) ->
    false.

-spec resolve_bearer_token() -> binary() | undefined.
resolve_bearer_token() ->
    case os:getenv("A2A_BEARER_TOKEN") of
        false -> undefined;
        ""    -> undefined;
        Val   -> list_to_binary(Val)
    end.

peer_ip(Req) ->
    {IP, _Port} = cowboy_req:peer(Req),
    list_to_binary(inet:ntoa(IP)).
