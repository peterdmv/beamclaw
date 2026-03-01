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

-module(beamclaw_memory_app).
-behaviour(application).

-include("bc_memory_mnesia.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = init_mnesia(),
    ok = maybe_transform_table(),
    beamclaw_memory_sup:start_link().

stop(_State) ->
    ok.

%% ---------------------------------------------------------------------------
%% Mnesia initialisation
%% ---------------------------------------------------------------------------

%% Ensure Mnesia is running with a disc schema and the bc_memory_entries table
%% exists.
%%
%% When Mnesia is listed in the app's `applications` list, OTP auto-starts it
%% before start/2 runs. On a fresh install (no schema on disk) this means
%% Mnesia is already running but with use_dir=false — so tables would be
%% created as ram_copies, silently losing persistence.
%%
%% Fix: detect use_dir=false, stop Mnesia, create the disc schema, restart.
init_mnesia() ->
    case mnesia:system_info(is_running) of
        yes ->
            case mnesia:system_info(use_dir) of
                true ->
                    %% Schema exists on disk — disc_copies will be used
                    ensure_tables();
                false ->
                    %% Mnesia auto-started without disk schema.
                    %% Stop, create schema, restart to enable disc_copies.
                    mnesia:stop(),
                    ok = ensure_schema(),
                    ok = mnesia:start(),
                    ensure_tables()
            end;
        _ ->
            ok = ensure_schema(),
            ok = mnesia:start(),
            ensure_tables()
    end.

ensure_schema() ->
    case mnesia:create_schema([node()]) of
        ok                                -> ok;
        {error, {_, {already_exists, _}}} -> ok
    end.

ensure_tables() ->
    StorageType = case mnesia:system_info(use_dir) of
        true  -> disc_copies;
        false -> ram_copies
    end,
    case mnesia:create_table(bc_memory_entries, [
            {attributes, record_info(fields, bc_memory_entry_stored)},
            {StorageType, [node()]},
            {type, set}]) of
        {atomic, ok}                   -> ok;
        {aborted, {already_exists, _}} -> ok
    end.

%% Add the `embedding` column to existing tables created before M22.
%% mnesia:transform_table/3 is idempotent when the attributes already match.
maybe_transform_table() ->
    Expected = record_info(fields, bc_memory_entry_stored),
    Current  = mnesia:table_info(bc_memory_entries, attributes),
    case Current =:= Expected of
        true  -> ok;
        false ->
            Transform = fun({bc_memory_entry_stored, Key, Value, Cat, CreatedAt, UpdatedAt}) ->
                {bc_memory_entry_stored, Key, Value, Cat, CreatedAt, UpdatedAt, undefined}
            end,
            {atomic, ok} = mnesia:transform_table(bc_memory_entries, Transform, Expected),
            ok
    end.
