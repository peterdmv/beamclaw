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

-module(bc_pairing).
-moduledoc """
Telegram (and future channel) pairing — access control for channel users.

Pure functional module. File-based JSON storage under ~/.beamclaw/pairing/.
Two files per channel: <channel>-pending.json and <channel>-allowed.json.

Unknown users receive a pairing code; the bot owner approves via CLI
(`beamclaw pair telegram <CODE>`), and the user is added to a persistent
allowlist.
""".

-export([is_allowed/2, request_pairing/3, approve/2, approve/3, revoke/2,
         list_pending/1, list_allowed/1, get_agent_id/2, pairing_dir/0]).

-define(CODE_ALPHABET, "ABCDEFGHJKLMNPQRSTUVWXYZ23456789").
-define(CODE_LENGTH, 8).
-define(MAX_PENDING, 3).
-define(TTL_MS, 3600000). %% 1 hour

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-doc "Check if a user is in the allowed list for the given channel.".
-spec is_allowed(Channel :: atom(), UserId :: binary()) -> boolean().
is_allowed(Channel, UserId) ->
    Allowed = read_allowed(Channel),
    lists:any(fun(Entry) -> maps:get(<<"id">>, Entry) =:= UserId end, Allowed).

-doc """
Request a pairing code for a user. Returns {ok, Code, created} for new
requests, {ok, Code, existing} if the user already has a pending request,
or {error, capacity} if MAX_PENDING is reached (should not happen since
oldest is evicted).
""".
-spec request_pairing(Channel :: atom(), UserId :: binary(), Meta :: map()) ->
    {ok, Code :: binary(), created | existing}.
request_pairing(Channel, UserId, Meta) ->
    Pending = read_pending(Channel),
    %% Prune expired entries first
    Now = erlang:system_time(millisecond),
    Active = [R || R <- Pending, not is_expired(R, Now)],
    %% Check if user already has a pending request
    case lists:filter(fun(R) -> maps:get(<<"id">>, R) =:= UserId end, Active) of
        [Existing | _] ->
            Code = maps:get(<<"code">>, Existing),
            {ok, Code, existing};
        [] ->
            Code = generate_code(),
            Entry = #{<<"id">> => UserId,
                      <<"code">> => Code,
                      <<"created_at">> => Now,
                      <<"meta">> => Meta},
            %% Evict oldest if at capacity
            Trimmed = case length(Active) >= ?MAX_PENDING of
                true  -> tl(Active);
                false -> Active
            end,
            write_pending(Channel, Trimmed ++ [Entry]),
            {ok, Code, created}
    end.

-doc "Approve a pending request by code. Uses default_agent config for agent_id.".
-spec approve(Channel :: atom(), Code :: binary()) ->
    {ok, UserId :: binary()} | {error, not_found | expired}.
approve(Channel, Code) ->
    approve(Channel, Code, undefined).

-doc "Approve a pending request by code with a specific agent_id.".
-spec approve(Channel :: atom(), Code :: binary(), AgentId :: binary() | undefined) ->
    {ok, UserId :: binary()} | {error, not_found | expired}.
approve(Channel, Code, AgentId) ->
    Pending = read_pending(Channel),
    Now = erlang:system_time(millisecond),
    case lists:filter(fun(R) -> maps:get(<<"code">>, R) =:= Code end, Pending) of
        [] ->
            {error, not_found};
        [Match | _] ->
            case is_expired(Match, Now) of
                true ->
                    %% Remove expired entry
                    Remaining = [R || R <- Pending, maps:get(<<"code">>, R) =/= Code],
                    write_pending(Channel, Remaining),
                    {error, expired};
                false ->
                    UserId = maps:get(<<"id">>, Match),
                    ResolvedAgent = resolve_agent(AgentId),
                    %% Remove from pending
                    Remaining = [R || R <- Pending, maps:get(<<"code">>, R) =/= Code],
                    write_pending(Channel, Remaining),
                    %% Add to allowed (idempotent — update agent_id if already present)
                    Allowed = read_allowed(Channel),
                    NewEntry = #{<<"id">> => UserId, <<"agent_id">> => ResolvedAgent},
                    Filtered = [E || E <- Allowed, maps:get(<<"id">>, E) =/= UserId],
                    write_allowed(Channel, Filtered ++ [NewEntry]),
                    {ok, UserId}
            end
    end.

-doc "Remove a user from the allowed list.".
-spec revoke(Channel :: atom(), UserId :: binary()) -> ok | {error, not_found}.
revoke(Channel, UserId) ->
    Allowed = read_allowed(Channel),
    case lists:any(fun(E) -> maps:get(<<"id">>, E) =:= UserId end, Allowed) of
        false -> {error, not_found};
        true  ->
            Filtered = [E || E <- Allowed, maps:get(<<"id">>, E) =/= UserId],
            write_allowed(Channel, Filtered),
            ok
    end.

-doc "List all pending (non-expired) pairing requests for a channel.".
-spec list_pending(Channel :: atom()) -> [map()].
list_pending(Channel) ->
    Pending = read_pending(Channel),
    Now = erlang:system_time(millisecond),
    [R || R <- Pending, not is_expired(R, Now)].

-doc "List all allowed entries for a channel. Each entry is a map with id and agent_id.".
-spec list_allowed(Channel :: atom()) -> [map()].
list_allowed(Channel) ->
    read_allowed(Channel).

-doc "Look up the agent_id assigned to a user in the allowed list.".
-spec get_agent_id(Channel :: atom(), UserId :: binary()) ->
    {ok, binary()} | {error, not_found}.
get_agent_id(Channel, UserId) ->
    Allowed = read_allowed(Channel),
    case [E || E <- Allowed, maps:get(<<"id">>, E) =:= UserId] of
        [Entry | _] -> {ok, maps:get(<<"agent_id">>, Entry)};
        []          -> {error, not_found}
    end.

-doc "Return the pairing storage directory path.".
-spec pairing_dir() -> string().
pairing_dir() ->
    case os:getenv("BEAMCLAW_HOME") of
        false ->
            Home = os:getenv("HOME"),
            filename:join([Home, ".beamclaw", "pairing"]);
        Dir ->
            filename:join([Dir, "pairing"])
    end.

%%--------------------------------------------------------------------
%% Internal: code generation
%%--------------------------------------------------------------------

generate_code() ->
    Alphabet = ?CODE_ALPHABET,
    Len = length(Alphabet),
    Bytes = crypto:strong_rand_bytes(?CODE_LENGTH),
    list_to_binary([lists:nth((B rem Len) + 1, Alphabet) || <<B>> <= Bytes]).

%%--------------------------------------------------------------------
%% Internal: expiry check
%%--------------------------------------------------------------------

is_expired(Request, Now) ->
    CreatedAt = maps:get(<<"created_at">>, Request, 0),
    (Now - CreatedAt) > ?TTL_MS.

%%--------------------------------------------------------------------
%% Internal: file I/O — pending requests
%%--------------------------------------------------------------------

pending_file(Channel) ->
    filename:join(pairing_dir(), atom_to_list(Channel) ++ "-pending.json").

read_pending(Channel) ->
    File = pending_file(Channel),
    case file:read_file(File) of
        {ok, Bin} ->
            try
                Decoded = jsx:decode(Bin, [return_maps]),
                maps:get(<<"requests">>, Decoded, [])
            catch _:_ -> []
            end;
        {error, enoent} -> []
    end.

write_pending(Channel, Requests) ->
    File = pending_file(Channel),
    ok = filelib:ensure_dir(File),
    Data = jsx:encode(#{<<"version">> => 1, <<"requests">> => Requests}),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------
%% Internal: file I/O — allowed list
%%--------------------------------------------------------------------

allowed_file(Channel) ->
    filename:join(pairing_dir(), atom_to_list(Channel) ++ "-allowed.json").

read_allowed(Channel) ->
    File = allowed_file(Channel),
    case file:read_file(File) of
        {ok, Bin} ->
            try
                Decoded = jsx:decode(Bin, [return_maps]),
                Version = maps:get(<<"version">>, Decoded, 1),
                Entries = maps:get(<<"allowed">>, Decoded, []),
                maybe_migrate_v1(Version, Entries)
            catch _:_ -> []
            end;
        {error, enoent} -> []
    end.

write_allowed(Channel, AllowedList) ->
    File = allowed_file(Channel),
    ok = filelib:ensure_dir(File),
    Data = jsx:encode(#{<<"version">> => 2, <<"allowed">> => AllowedList}),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------
%% Internal: v1 → v2 migration
%%--------------------------------------------------------------------

maybe_migrate_v1(2, Entries) ->
    Entries;
maybe_migrate_v1(_V1, Entries) ->
    DefaultAgent = resolve_agent(undefined),
    [case Entry of
        #{<<"id">> := _} -> Entry;  %% already a map (shouldn't happen in v1)
        Id when is_binary(Id) -> #{<<"id">> => Id, <<"agent_id">> => DefaultAgent}
    end || Entry <- Entries].

resolve_agent(undefined) ->
    case application:get_env(beamclaw_core, default_agent) of
        {ok, Val} -> Val;
        undefined -> <<"default">>
    end;
resolve_agent(AgentId) when is_binary(AgentId) ->
    AgentId.
