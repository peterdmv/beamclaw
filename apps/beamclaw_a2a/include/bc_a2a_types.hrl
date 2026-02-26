-ifndef(BC_A2A_TYPES_HRL).
-define(BC_A2A_TYPES_HRL, true).

%% A2A task states per the A2A protocol specification.
-type a2a_task_state() :: submitted | working | input_required
                        | completed | failed | canceled | rejected.

%% A2A message part.
-type a2a_part() :: #{type := text, text := binary()}
                  | #{type := file, file := map()}
                  | #{type := data, data := map()}.

%% A2A message (user or agent).
-record(a2a_message, {
    role     :: user | agent,
    parts    :: [a2a_part()],
    metadata = #{} :: map()
}).

%% A2A task status.
-record(a2a_status, {
    state     :: a2a_task_state(),
    message   :: #a2a_message{} | undefined,
    timestamp :: binary()  %% ISO 8601
}).

%% A2A artifact.
-record(a2a_artifact, {
    name        :: binary() | undefined,
    description :: binary() | undefined,
    parts       :: [a2a_part()],
    index       :: non_neg_integer()
}).

%% A2A task — the fundamental unit of work.
-record(a2a_task, {
    id         :: binary(),
    context_id :: binary() | undefined,
    status     :: #a2a_status{},
    history    = [] :: [#a2a_message{}],
    artifacts  = [] :: [#a2a_artifact{}],
    metadata   = #{} :: map()
}).

-endif.
