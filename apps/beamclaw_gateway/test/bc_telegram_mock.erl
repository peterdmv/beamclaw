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

-module(bc_telegram_mock).
-moduledoc """
Mock Telegram Bot API server for CT integration tests.

A lightweight Cowboy handler that simulates the Telegram Bot API endpoints.
Records all received requests for later assertion. Configurable responses
for different endpoints.

Endpoints handled:
  POST /bot<TOKEN>/getMe         → bot info
  POST /bot<TOKEN>/getUpdates    → polled messages (configurable)
  POST /bot<TOKEN>/sendMessage   → records sent message, returns ok
  POST /bot<TOKEN>/sendChatAction → records typing indicator, returns ok
  POST /bot<TOKEN>/editMessageText → records edit, returns ok
  POST /bot<TOKEN>/setMyCommands → records command registration, returns ok
  POST /bot<TOKEN>/setWebhook    → records webhook setup, returns ok
  POST /bot<TOKEN>/sendPhoto     → records photo upload, returns ok
""".

-export([start/1, stop/0,
         get_requests/0, get_requests/1, clear_requests/0,
         enqueue_update/1, set_response/2]).

%% Cowboy handler
-export([init/2]).

-define(LISTENER, bc_telegram_mock_listener).
-define(ETS_REQUESTS, bc_telegram_mock_requests).
-define(ETS_UPDATES, bc_telegram_mock_updates).
-define(ETS_RESPONSES, bc_telegram_mock_responses).
-define(TABLE_OWNER, bc_telegram_mock_table_owner).

%% ---- Public API ----

-spec start(#{port := pos_integer()}) -> {ok, pos_integer()}.
start(#{port := Port}) ->
    start_table_owner(),
    Dispatch = cowboy_router:compile([
        {'_', [{"/[...]", ?MODULE, []}]}
    ]),
    {ok, _} = cowboy:start_clear(?LISTENER,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}),
    {ok, Port}.

-spec stop() -> ok.
stop() ->
    cowboy:stop_listener(?LISTENER),
    stop_table_owner(),
    ok.

%% Get all recorded requests, optionally filtered by method name.
-spec get_requests() -> [map()].
get_requests() ->
    case ets:info(?ETS_REQUESTS) of
        undefined -> [];
        _         -> [R || {_, R} <- ets:tab2list(?ETS_REQUESTS)]
    end.

-spec get_requests(binary()) -> [map()].
get_requests(Method) ->
    [R || R <- get_requests(), maps:get(method, R) =:= Method].

-spec clear_requests() -> ok.
clear_requests() ->
    case ets:info(?ETS_REQUESTS) of
        undefined -> ok;
        _         -> ets:delete_all_objects(?ETS_REQUESTS)
    end,
    ok.

%% Enqueue a Telegram update to be returned by getUpdates.
-spec enqueue_update(map()) -> ok.
enqueue_update(Update) ->
    ensure_ets(),
    Id = erlang:unique_integer([positive]),
    ets:insert(?ETS_UPDATES, {Id, Update}),
    ok.

%% Set a custom response for a specific method.
-spec set_response(binary(), map()) -> ok.
set_response(Method, Response) ->
    ensure_ets(),
    ets:insert(?ETS_RESPONSES, {Method, Response}),
    ok.

%% ---- Cowboy handler ----

init(Req0, State) ->
    %% Extract the method from the path: /bot<TOKEN>/<METHOD>
    Method = extract_method(cowboy_req:path(Req0)),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Parsed = try jsx:decode(Body, [return_maps])
             catch _:_ -> #{<<"raw">> => Body}
             end,
    %% Record the request
    record_request(Method, Parsed, Req1),
    %% Generate response
    Response = generate_response(Method, Parsed),
    RespBody = jsx:encode(Response),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        RespBody, Req1),
    {ok, Req2, State}.

%% ---- Internal ----

%% Spawn a persistent process to own the ETS tables. CT destroys tables
%% when their owner process (init_per_suite) exits; we need them to survive.
start_table_owner() ->
    case whereis(?TABLE_OWNER) of
        undefined ->
            Pid = spawn(fun table_owner_loop/0),
            register(?TABLE_OWNER, Pid),
            %% Let the new process create the tables
            Pid ! {create_tables, self()},
            receive tables_ready -> ok
            after 5000 -> error(table_owner_timeout)
            end;
        _ ->
            ok
    end.

table_owner_loop() ->
    receive
        {create_tables, From} ->
            ensure_table(?ETS_REQUESTS, [ordered_set, public, named_table]),
            ensure_table(?ETS_UPDATES, [ordered_set, public, named_table]),
            ensure_table(?ETS_RESPONSES, [set, public, named_table]),
            From ! tables_ready,
            table_owner_loop();
        stop ->
            ok
    end.

stop_table_owner() ->
    case whereis(?TABLE_OWNER) of
        undefined -> ok;
        Pid -> Pid ! stop, ok
    end.

%% Extract API method name from path like "/botTOKEN/setMyCommands"
extract_method(Path) ->
    case binary:split(Path, <<"/">>, [global, trim_all]) of
        [_, MethodBin] -> MethodBin;
        _              -> <<"unknown">>
    end.

ensure_ets() ->
    %% Tables are owned by the table_owner process. Only create if
    %% called outside start/0 (e.g. from enqueue_update/set_response).
    case ets:info(?ETS_REQUESTS) of
        undefined -> start_table_owner();
        _         -> ok
    end.

ensure_table(Name, Opts) ->
    case ets:info(Name) of
        undefined -> ets:new(Name, Opts);
        _         -> Name
    end.

record_request(Method, Body, Req) ->
    Id = erlang:unique_integer([positive, monotonic]),
    Record = #{method => Method,
               body   => Body,
               ts     => erlang:system_time(millisecond),
               peer   => cowboy_req:peer(Req)},
    ets:insert(?ETS_REQUESTS, {Id, Record}),
    ok.

generate_response(Method, Parsed) ->
    %% Check for custom response first
    case ets:lookup(?ETS_RESPONSES, Method) of
        [{_, CustomResp}] -> CustomResp;
        []                -> default_response(Method, Parsed)
    end.

default_response(<<"getMe">>, _) ->
    #{<<"ok">> => true,
      <<"result">> => #{
          <<"id">> => 123456789,
          <<"is_bot">> => true,
          <<"first_name">> => <<"BeamClawTestBot">>,
          <<"username">> => <<"beamclaw_test_bot">>}};

default_response(<<"getUpdates">>, _) ->
    Updates = drain_updates(),
    #{<<"ok">> => true,
      <<"result">> => Updates};

default_response(<<"sendMessage">>, Body) ->
    ChatId = maps:get(<<"chat_id">>, Body, 0),
    #{<<"ok">> => true,
      <<"result">> => #{
          <<"message_id">> => erlang:unique_integer([positive]),
          <<"chat">> => #{<<"id">> => ChatId},
          <<"text">> => maps:get(<<"text">>, Body, <<>>),
          <<"date">> => erlang:system_time(second)}};

default_response(<<"sendChatAction">>, _) ->
    #{<<"ok">> => true, <<"result">> => true};

default_response(<<"editMessageText">>, Body) ->
    ChatId = maps:get(<<"chat_id">>, Body, 0),
    #{<<"ok">> => true,
      <<"result">> => #{
          <<"message_id">> => maps:get(<<"message_id">>, Body, 0),
          <<"chat">> => #{<<"id">> => ChatId},
          <<"text">> => maps:get(<<"text">>, Body, <<>>),
          <<"date">> => erlang:system_time(second)}};

default_response(<<"setMyCommands">>, _) ->
    #{<<"ok">> => true, <<"result">> => true};

default_response(<<"setWebhook">>, _) ->
    #{<<"ok">> => true,
      <<"result">> => true,
      <<"description">> => <<"Webhook was set">>};

default_response(<<"sendPhoto">>, Body) ->
    ChatId = maps:get(<<"chat_id">>, Body, 0),
    #{<<"ok">> => true,
      <<"result">> => #{
          <<"message_id">> => erlang:unique_integer([positive]),
          <<"chat">> => #{<<"id">> => ChatId},
          <<"date">> => erlang:system_time(second)}};

default_response(_, _) ->
    #{<<"ok">> => true, <<"result">> => true}.

drain_updates() ->
    case ets:info(?ETS_UPDATES) of
        undefined -> [];
        _ ->
            All = ets:tab2list(?ETS_UPDATES),
            ets:delete_all_objects(?ETS_UPDATES),
            [U || {_, U} <- All]
    end.
