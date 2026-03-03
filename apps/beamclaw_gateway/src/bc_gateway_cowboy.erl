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

-module(bc_gateway_cowboy).
-moduledoc """
Cowboy HTTP listener wrapper.

Runs as a gen_server child of bc_gateway_http_sup. The Cowboy/Ranch listener
is started in init/1 and stopped in terminate/2, so the supervisor owns our
gen_server pid (not Ranch's) and shutdown is clean.

Routes:
  GET  /health                 → bc_http_health_h
  GET  /metrics                → bc_http_metrics_h
  POST /v1/chat/completions    → bc_http_completions_h
  GET  /ws                     → bc_ws_h
  POST /webhook/telegram       → bc_webhook_telegram_h
  GET  /.well-known/agent.json → bc_a2a_http_h  (A2A Agent Card)
  POST /a2a                    → bc_a2a_http_h  (A2A JSON-RPC)
""".

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ConfigPort = maps:get(port, bc_config:get(beamclaw_gateway, http, #{port => 18800}), 18800),
    Port = case os:getenv("BEAMCLAW_PORT") of
        false -> ConfigPort;
        P     -> list_to_integer(P)
    end,
    Routes = cowboy_router:compile([
        {'_', [
            {"/health",                    bc_http_health_h,      []},
            {"/metrics",                   bc_http_metrics_h,     []},
            {"/v1/chat/completions",       bc_http_completions_h, []},
            {"/ws",                        bc_ws_h,               []},
            {"/webhook/telegram",          bc_webhook_telegram_h, []},
            {"/.well-known/agent.json",    bc_a2a_http_h,         []},
            {"/a2a",                       bc_a2a_http_h,         []}
        ]}
    ]),
    case cowboy:start_clear(bc_http_listener,
            [{port, Port}],
            #{env => #{dispatch => Routes}}) of
        {ok, _ListenerPid} ->
            {ok, #{}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(bc_http_listener),
    ok.
