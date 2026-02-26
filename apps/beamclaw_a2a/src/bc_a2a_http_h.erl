%% @doc Cowboy handler for A2A protocol HTTP endpoints.
%%
%% Routes (to be mounted in bc_gateway_cowboy):
%%   GET  /.well-known/agent.json  → Agent Card discovery
%%   POST /a2a                     → JSON-RPC 2.0 A2A requests
-module(bc_a2a_http_h).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    handle(Method, Path, Req0, State).

%% Agent Card discovery
handle(<<"GET">>, <<"/.well-known/agent.json">>, Req0, State) ->
    Card = bc_a2a_server:agent_card(),
    Body = jsx:encode(Card),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Body, Req0),
    {ok, Req, State};

%% A2A JSON-RPC endpoint
handle(<<"POST">>, <<"/a2a">>, Req0, State) ->
    {ok, RawBody, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(RawBody, [return_maps]) of
        Request when is_map(Request) ->
            case bc_a2a_server:handle_request(Request) of
                undefined ->
                    %% Notification — no response
                    Req = cowboy_req:reply(204, #{}, <<>>, Req1),
                    {ok, Req, State};
                Response ->
                    Body = jsx:encode(Response),
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Body, Req1),
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
    end;

handle(_, _, Req0, State) ->
    Req = cowboy_req:reply(404, #{}, <<"Not Found">>, Req0),
    {ok, Req, State}.
