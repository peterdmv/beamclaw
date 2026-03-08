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

-module(bc_webhook_h).
-moduledoc """
Generic webhook ingestion handler — POST /webhook/:source

Receives webhooks from arbitrary external services (TradingView, GitHub,
Stripe, etc.) and dispatches them to the agent session as user messages.

Authentication: per-source secret checked in three locations (first match wins):
  1. X-Webhook-Secret header (works for GitHub, Stripe, etc.)
  2. ?secret= query parameter in the URL
  3. "secret" field in a JSON body (works for TradingView)

The secret is checked against WEBHOOK_SECRET_<SOURCE> env var (uppercased).
If no env var is set for the source, the endpoint is open (no auth required).

The "secret" field is automatically stripped from JSON bodies before forwarding
to the agent session, so it never enters the conversation history.

Session routing: uses bc_config:canonical_user_id() (BEAMCLAW_USER) to route
into the user's existing shared session. With session_sharing=shared (default),
webhook messages arrive in the same session as Telegram/TUI.

Fire-and-forget: returns 200 immediately without waiting for the agent response.
""".

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([init/2]).
%% Exported for testing
-export([format_content/3, resolve_source_secret/1, authenticate/3,
         extract_secret/2]).

init(Req0, State) ->
    ClientIp = peer_ip(Req0),
    Source = cowboy_req:binding(source, Req0),
    Route = <<"/webhook/", Source/binary>>,
    case bc_rate_limiter:check(ClientIp, Route) of
        {error, rate_limited} ->
            reply_json(429, #{error => <<"rate limited">>}, Req0, State);
        ok ->
            handle_webhook(Source, Req0, State)
    end.

handle_webhook(Source, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case authenticate(Source, Req1, Body) of
        ok ->
            Content = format_content(Source, Body, Req1),
            AgentId = agent_id(Req1),
            UserId = resolve_user_id(Source),
            SessionId = bc_session_registry:derive_session_id(UserId, AgentId),
            SessionPid = get_or_create_session(SessionId, UserId, AgentId),
            ChannelMsg = #bc_channel_message{
                session_id = SessionId,
                user_id    = UserId,
                agent_id   = AgentId,
                channel    = webhook,
                content    = Content,
                raw        = Body,
                ts         = erlang:system_time(millisecond)
            },
            bc_session:dispatch_run(SessionPid, ChannelMsg),
            bc_obs:emit(webhook_received, #{source => Source,
                                             session_id => SessionId}),
            reply_json(200, #{status => <<"accepted">>}, Req1, State);
        {error, Reason} ->
            bc_obs:emit(webhook_auth_failed, #{source => Source,
                                                reason => Reason,
                                                client_ip => peer_ip(Req1)}),
            reply_json(401, #{error => <<"unauthorized">>}, Req1, State)
    end.

%% --- Authentication ---

-spec authenticate(binary(), cowboy_req:req(), binary()) -> ok | {error, binary()}.
authenticate(Source, Req, Body) ->
    case resolve_source_secret(Source) of
        undefined ->
            %% No secret configured = open access for this source
            ok;
        Configured ->
            case extract_secret(Req, Body) of
                undefined ->
                    {error, <<"missing webhook secret">>};
                Provided ->
                    verify_secret(Provided, Configured)
            end
    end.

%% Extract secret from header, query param, or JSON body (first match wins).
-spec extract_secret(cowboy_req:req(), binary()) -> binary() | undefined.
extract_secret(Req, Body) ->
    case cowboy_req:header(<<"x-webhook-secret">>, Req) of
        undefined ->
            case cowboy_req:match_qs([{secret, [], undefined}], Req) of
                #{secret := undefined} ->
                    extract_body_secret(Body);
                #{secret := QS} ->
                    QS
            end;
        Header ->
            Header
    end.

extract_body_secret(Body) ->
    case catch jsx:decode(Body, [return_maps]) of
        Map when is_map(Map) -> maps:get(<<"secret">>, Map, undefined);
        _ -> undefined
    end.

-spec resolve_source_secret(binary()) -> binary() | undefined.
resolve_source_secret(Source) ->
    EnvVar = "WEBHOOK_SECRET_" ++ string:uppercase(binary_to_list(Source)),
    case os:getenv(EnvVar) of
        false -> undefined;
        ""    -> undefined;
        Val   -> list_to_binary(Val)
    end.

verify_secret(Provided, Configured) ->
    ProvBin = iolist_to_binary(Provided),
    ConfBin = iolist_to_binary(Configured),
    case constant_time_equals(ProvBin, ConfBin) of
        true  -> ok;
        false -> {error, <<"invalid webhook secret">>}
    end.

constant_time_equals(A, B) when byte_size(A) =:= byte_size(B) ->
    crypto:hash_equals(A, B);
constant_time_equals(_, _) ->
    false.

%% --- Content formatting ---

-spec format_content(binary(), binary(), cowboy_req:req()) -> binary().
format_content(Source, Body, _Req) ->
    Prefix = <<"[Webhook: ", Source/binary, "]\n">>,
    Payload = try_format_json(Body),
    <<Prefix/binary, Payload/binary>>.

try_format_json(Body) ->
    case catch jsx:decode(Body, [return_maps]) of
        Decoded when is_map(Decoded) ->
            %% Strip "secret" field before forwarding to agent
            Stripped = maps:remove(<<"secret">>, Decoded),
            jsx:encode(Stripped);
        _ ->
            Body
    end.

%% --- Session routing ---

resolve_user_id(Source) ->
    case bc_config:canonical_user_id() of
        undefined -> <<"webhook:", Source/binary>>;
        Canonical -> Canonical
    end.

agent_id(Req) ->
    case cowboy_req:match_qs([{agent, [], undefined}], Req) of
        #{agent := undefined} ->
            bc_config:get(beamclaw_core, default_agent, <<"default">>);
        #{agent := AgentBin} when is_binary(AgentBin) ->
            AgentBin
    end.

get_or_create_session(SessionId, UserId, AgentId) ->
    case bc_session_registry:lookup(SessionId) of
        {ok, Pid} ->
            Pid;
        {error, not_found} ->
            Config = #{session_id  => SessionId,
                       user_id     => UserId,
                       channel_id  => SessionId,
                       channel_mod => undefined,
                       agent_id    => AgentId},
            {ok, _} = bc_sessions_sup:start_session(Config),
            {ok, Pid} = bc_session_registry:lookup(SessionId),
            Pid
    end.

%% --- Helpers ---

reply_json(StatusCode, Body, Req0, State) ->
    Req = cowboy_req:reply(StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body), Req0),
    {ok, Req, State}.

peer_ip(Req) ->
    {Ip, _Port} = cowboy_req:peer(Req),
    Ip.
