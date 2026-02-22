%% @doc Shared record and type definitions for BeamClaw.
%%
%% Include with: -include_lib("beamclaw_core/include/bc_types.hrl").

-ifndef(BC_TYPES_HRL).
-define(BC_TYPES_HRL, true).

%% ---- Types ----

-type autonomy_level()  :: read_only | supervised | full.
-type memory_category() :: core | daily | conversation | custom.
-type role()            :: system | user | assistant | tool.

%% ---- Records ----

%% A message in the conversation history.
-record(bc_message, {
    id            :: binary() | undefined,
    role          :: role(),
    content       :: binary() | undefined,
    tool_calls    = [] :: list(),
    tool_call_id  :: binary() | undefined,
    name          :: binary() | undefined,
    ts            = 0 :: non_neg_integer()
}).

%% A tool definition (name, description, JSON schema for parameters).
-record(bc_tool_def, {
    name        :: binary(),
    description :: binary(),
    parameters  :: map(),
    source      :: builtin | mcp | binary()
}).

%% A tool call requested by the LLM.
-record(bc_tool_call, {
    id     :: binary(),
    name   :: binary(),
    args   :: map(),
    source :: builtin | mcp | native | xml | markdown | binary()
}).

%% A tool execution result.
-record(bc_tool_result, {
    tool_call_id :: binary(),
    name         :: binary(),
    content      :: binary(),
    is_error     = false :: boolean()
}).

%% A reference passed to tool execute/3 callbacks.
-record(bc_session_ref, {
    session_id  :: binary(),
    user_id     :: binary(),
    session_pid :: pid(),
    autonomy    :: autonomy_level(),
    agent_id    :: binary()
}).

%% A message arriving from a channel (Telegram, WebSocket, etc.).
%% reply_pid: when set, bc_loop sends {bc_chunk, SessionId, Chunk} and
%% {bc_done, SessionId, Msg} directly to this pid instead of routing via
%% the named channel gen_server. Used by HTTP and WebSocket handlers.
-record(bc_channel_message, {
    session_id :: binary(),
    user_id    :: binary(),
    channel    :: atom(),
    content    :: binary(),
    raw        :: term(),
    ts         :: non_neg_integer(),
    reply_pid  = undefined :: pid() | undefined
}).

%% A stored memory entry.
-record(bc_memory_entry, {
    key        :: term(),
    value      :: term(),
    category   :: memory_category(),
    created_at :: non_neg_integer(),
    updated_at :: non_neg_integer()
}).

%% An observability event.
-record(bc_obs_event, {
    type       :: atom(),
    session_id :: binary() | undefined,
    data       :: map(),
    ts         :: non_neg_integer()
}).

%% An approval request sent to bc_approval.
-record(bc_approval_req, {
    session_id :: binary(),
    tool_call  :: #bc_tool_call{},
    ref        :: reference()
}).

-endif. %% BC_TYPES_HRL
