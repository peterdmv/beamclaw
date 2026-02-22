%% Mnesia table record for bc_session_stored.
%% Shared between bc_session_store (read/write) and beamclaw_core_app (table creation).
%%
%% The table name is `bc_session_stored`; the record tag is `bc_session_stored`.
%% Primary key is session_id (binary).
%% History is serialized via term_to_binary with a version tag for forward compat.

-record(bc_session_stored, {
    session_id  :: binary(),          %% primary key
    user_id     :: binary(),
    agent_id    :: binary(),
    autonomy    :: atom(),            %% autonomy_level()
    history     :: term(),            %% binary() on disk; list after deserialization
    created_at  :: non_neg_integer(),
    updated_at  :: non_neg_integer(),
    config      :: map()
}).
