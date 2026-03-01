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

-module(bc_webhook_telegram_h).
-moduledoc "Telegram webhook handler — POST /webhook/telegram".

-export([init/2]).
%% Exported for testing
-export([verify_token/2]).

init(Req, State) ->
    case validate_secret(Req) of
        ok ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            Update = jsx:decode(Body, [return_maps]),
            bc_channel_telegram:handle_webhook(Update),
            Req3 = cowboy_req:reply(200, #{}, <<>>, Req2),
            {ok, Req3, State};
        {error, Reason} ->
            logger:warning("[webhook_telegram] rejected: ~s", [Reason]),
            Req2 = cowboy_req:reply(401, #{}, <<>>, Req),
            {ok, Req2, State}
    end.

%% Internal

validate_secret(Req) ->
    Header = cowboy_req:header(<<"x-telegram-bot-api-secret-token">>, Req),
    Secret = resolve_webhook_secret(),
    verify_token(Header, Secret).

-spec verify_token(Provided :: binary() | undefined,
                   Configured :: binary() | undefined) ->
    ok | {error, binary()}.
verify_token(undefined, _) ->
    {error, <<"missing header">>};
verify_token(_, undefined) ->
    {error, <<"no secret configured">>};
verify_token(Provided, Configured) ->
    ProvBin = iolist_to_binary(Provided),
    ConfBin = iolist_to_binary(Configured),
    case constant_time_equals(ProvBin, ConfBin) of
        true  -> ok;
        false -> {error, <<"invalid token">>}
    end.

constant_time_equals(A, B) when byte_size(A) =:= byte_size(B) ->
    crypto:hash_equals(A, B);
constant_time_equals(_, _) ->
    false.

resolve_webhook_secret() ->
    case os:getenv("TELEGRAM_WEBHOOK_SECRET") of
        false -> undefined;
        Val   -> list_to_binary(Val)
    end.
