%% @doc Session persistence — Mnesia-backed data access layer.
%%
%% Stores session state (history, user/agent IDs, config) in the bc_session_stored
%% Mnesia table. History is serialized via term_to_binary with a version tag
%% (currently version 1) for forward compatibility.
%%
%% Storage type: disc_copies when a disc schema exists (production), ram_copies
%% otherwise (dev/test). Same pattern as bc_memory_mnesia.
%%
%% All reads/writes use dirty operations for speed.
-module(bc_session_store).

-include_lib("beamclaw_core/include/bc_session_store.hrl").

-export([init_table/0, load/1, save/2, delete/1, delete_expired/1]).

%% Mnesia dirty_match_object uses '_' atom as wildcard in record fields.
%% This is a Mnesia runtime convention that Dialyzer cannot model.
-dialyzer({nowarn_function, [delete_expired/1]}).

%% ---------------------------------------------------------------------------
%% Table initialisation (called from beamclaw_core_app:start/2)
%% ---------------------------------------------------------------------------

-spec init_table() -> ok.
init_table() ->
    case mnesia:system_info(is_running) of
        yes ->
            ensure_table();
        _ ->
            case mnesia:create_schema([node()]) of
                ok                                -> ok;
                {error, {_, {already_exists, _}}} -> ok
            end,
            ok = mnesia:start(),
            ensure_table()
    end.

ensure_table() ->
    StorageType = case mnesia:system_info(use_dir) of
        true  -> disc_copies;
        false -> ram_copies
    end,
    case mnesia:create_table(bc_session_stored, [
            {attributes, record_info(fields, bc_session_stored)},
            {StorageType, [node()]},
            {type, set}]) of
        {atomic, ok}                   -> ok;
        {aborted, {already_exists, _}} -> ok
    end.

%% ---------------------------------------------------------------------------
%% CRUD operations
%% ---------------------------------------------------------------------------

-spec load(SessionId :: binary()) -> {ok, #bc_session_stored{}} | {error, not_found}.
load(SessionId) ->
    case mnesia:dirty_read(bc_session_stored, SessionId) of
        [#bc_session_stored{history = HistBin} = Stored] ->
            History = deserialize_history(HistBin),
            {ok, Stored#bc_session_stored{history = History}};
        [] ->
            {error, not_found}
    end.

-spec save(SessionId :: binary(), Fields :: map()) -> ok.
save(SessionId, Fields) ->
    Now = erlang:system_time(second),
    History = maps:get(history, Fields, []),
    SerializedHistory = serialize_history(History),
    Entry = case mnesia:dirty_read(bc_session_stored, SessionId) of
        [Existing] ->
            Existing#bc_session_stored{
                history    = SerializedHistory,
                updated_at = Now,
                config     = maps:get(config, Fields, Existing#bc_session_stored.config)
            };
        [] ->
            #bc_session_stored{
                session_id = SessionId,
                user_id    = maps:get(user_id, Fields, <<"anonymous">>),
                agent_id   = maps:get(agent_id, Fields, <<"default">>),
                autonomy   = maps:get(autonomy, Fields, supervised),
                history    = SerializedHistory,
                created_at = Now,
                updated_at = Now,
                config     = maps:get(config, Fields, #{})
            }
    end,
    ok = mnesia:dirty_write(bc_session_stored, Entry).

-spec delete(SessionId :: binary()) -> ok.
delete(SessionId) ->
    ok = mnesia:dirty_delete(bc_session_stored, SessionId).

-spec delete_expired(TTLSeconds :: non_neg_integer()) -> non_neg_integer().
delete_expired(TTLSeconds) ->
    Cutoff = erlang:system_time(second) - TTLSeconds,
    All = mnesia:dirty_match_object(bc_session_stored,
        #bc_session_stored{
            session_id = '_',
            user_id    = '_',
            agent_id   = '_',
            autonomy   = '_',
            history    = '_',
            created_at = '_',
            updated_at = '_',
            config     = '_'
        }),
    Expired = [S || S <- All, S#bc_session_stored.updated_at < Cutoff],
    lists:foreach(fun(S) ->
        mnesia:dirty_delete(bc_session_stored, S#bc_session_stored.session_id)
    end, Expired),
    length(Expired).

%% ---------------------------------------------------------------------------
%% Serialization (versioned for forward compat)
%% ---------------------------------------------------------------------------

serialize_history(History) ->
    term_to_binary({1, History}).

deserialize_history(Bin) when is_binary(Bin) ->
    case binary_to_term(Bin) of
        {1, History} -> History;
        %% Future: {2, History} -> migrate_v2_to_current(History);
        Other        -> Other  %% fallback: raw term
    end;
deserialize_history(Other) ->
    %% Already deserialized (e.g. in tests passing raw lists)
    Other.
