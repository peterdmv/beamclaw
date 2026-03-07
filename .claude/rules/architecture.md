# Architecture Reference

Detailed supervision trees, behaviour callbacks, data types, and internal
subsystem documentation. For high-level structure, see the Application
Dependency Graph in CLAUDE.md.

---

## Supervision Trees

### `beamclaw_core` (the brain)

```
beamclaw_core_sup  (one_for_one)
  ├── bc_session_registry     (gen_server, named — ETS: session_id → pid)
  ├── bc_session_cleaner      (gen_server, permanent — periodic expired session cleanup)
  ├── bc_session_maintenance  (gen_server, permanent — idle compaction, nightly flush, pre-expiry)
  ├── bc_user_env             (gen_server, permanent — user environment context injection)
  └── bc_sessions_sup         (simple_one_for_one)
        └── [per session] bc_session_sup  (one_for_one)
              ├── bc_session     (gen_server, permanent — the "lane")
              ├── bc_loop        (gen_statem, transient — agentic loop)
              ├── bc_provider_X  (gen_server, transient — one per session)
              └── bc_approval    (gen_server, transient — started on demand)
```

Critical: `bc_session` is **permanent**, `bc_loop` is **transient**. A crashing loop is restarted by the supervisor; the session history stored in `bc_session` survives. This provides session persistence across loop crashes (borrowed from OpenClaw's session isolation model).

### `beamclaw_mcp`

```
beamclaw_mcp_sup  (one_for_one)
  ├── bc_mcp_registry        (gen_server, named)
  └── bc_mcp_servers_sup     (one_for_one, dynamic)
        └── bc_mcp_server    (gen_server, transient — one per MCP server config)
```

Each `bc_mcp_server` owns an `erlang:open_port` for stdio MCP servers. Supervisor restarts use
exponential backoff via `{MaxR=5, MaxT=30}`. On restart, `bc_mcp_server` rediscovers tools via
`tools/list` RPC.

### `beamclaw_gateway`

```
beamclaw_gateway_sup  (one_for_one)
  ├── bc_rate_limiter              (gen_server, named)
  ├── bc_gateway_http_sup          (one_for_one)
  │     └── bc_gateway_cowboy      (Cowboy listener wrapper)
  └── bc_gateway_channels_sup      (one_for_one)
        ├── bc_channel_telegram_sup → bc_channel_telegram (gen_server)
        └── bc_channel_tui_sup      → bc_channel_tui      (gen_server)
```

### `beamclaw_sandbox`

```
beamclaw_sandbox_sup  (one_for_one)
  ├── bc_sandbox_registry     (gen_server, permanent — ETS: {session_id, scope} → pid)
  ├── bc_sandbox_reaper       (gen_server, permanent — periodic orphan container cleanup)
  └── bc_sandbox_sup          (simple_one_for_one)
        └── [per sandbox] bc_sandbox (gen_server, transient — Docker container lifecycle)
```

Each `bc_sandbox` manages a Docker container with `--network none`, `--cap-drop ALL`,
`--read-only`, and memory/CPU limits. A Unix domain socket bridge enables the container
to call back to BeamClaw tools via JSON-RPC 2.0. The sandbox is opt-in (`{enabled, false}`
by default) and requires Docker.

### `beamclaw_scheduler`

```
beamclaw_scheduler_sup  (one_for_one)
  ├── bc_sched_store       (gen_server, permanent — Mnesia job CRUD)
  ├── bc_sched_runner      (gen_server, permanent — timer management, fire events)
  └── bc_sched_executor    (gen_server, permanent — session creation, dispatch, delivery)
```

Runner manages timers (lightweight, never blocks). Executor does heavy work (session creation, LLM dispatch, HTTP delivery). A crash in execution doesn't lose timer state.

### `beamclaw_a2a`

```
beamclaw_a2a_sup  (one_for_one)
  └── bc_a2a_task_manager     (gen_server, permanent — ETS-backed task store)
```

`bc_a2a_task_manager` owns two ETS tables: `bc_a2a_tasks` (task state) and
`bc_a2a_sessions` (session → task reverse mapping). `bc_channel_a2a` is a
stateless module for response routing (no supervised process).

### `beamclaw_obs`

```
beamclaw_obs_sup  (one_for_one)
  ├── bc_obs_manager     (gen_server — fan-out via pg process groups)
  └── bc_obs_log         (gen_server backend)
```

`bc_obs:emit/2` is a non-blocking cast. Observability **never** creates backpressure on the agentic loop.

### `beamclaw_memory`

```
beamclaw_memory_sup  (one_for_one)
  └── bc_embedding_cache     (gen_server, permanent — ETS embedding cache, 24h TTL)
```

Other search modules (`bc_bm25`, `bc_vector`, `bc_chunker`, `bc_hybrid`, `bc_embedding`) are pure-function or stateless — no supervision needed.

---

## Key Behaviours

Behaviours are the OTP equivalent of Rust traits / TypeScript interfaces.

### `bc_provider` — LLM provider abstraction (`beamclaw_core`)

```erlang
-callback init(Config :: map()) -> {ok, State} | {error, term()}.
-callback complete(Messages, Options, State) ->
    {ok, bc_message(), State} | {error, term(), State}.
-callback stream(Messages, Options, CallerPid :: pid(), State) ->
    {ok, State} | {error, term(), State}.
    %% Sends to CallerPid:
    %%   {stream_chunk, self(), Chunk :: binary()}
    %%   {stream_done,  self(), FullMessage :: bc_message()}
    %%   {stream_error, self(), Reason}
-callback capabilities(State) -> map().
-callback terminate(Reason, State) -> ok.
```

Implementations: `bc_provider_openrouter`, `bc_provider_openai`.

### `bc_tool` — Tool abstraction (`beamclaw_tools`)

```erlang
-callback definition() -> bc_tool_def().
-callback execute(Args :: map(), Session :: bc_session_ref(), Context :: map()) ->
    {ok, binary()} | {error, binary()}.
-callback requires_approval() -> boolean().
-callback min_autonomy() -> autonomy_level().
```

Implementations: `bc_tool_terminal`, `bc_tool_bash`, `bc_tool_curl`, `bc_tool_jq`, `bc_tool_read_file`, `bc_tool_write_file`, `bc_tool_delete_file`, `bc_tool_workspace_memory` (MEMORY.md + daily logs + bootstrap files + delete), `bc_tool_web_search` (Brave Search API), `bc_tool_exec` (sandboxed code execution — in `beamclaw_sandbox`, registered conditionally), `bc_tool_scheduler` (scheduled tasks + heartbeat — in `beamclaw_scheduler`, registered conditionally).

### `bc_channel` — Messaging channel abstraction (`beamclaw_core`)

```erlang
-callback init(Config :: map()) -> {ok, State} | {error, term()}.
-callback listen(State) -> {ok, State} | {error, term()}.
-callback send(SessionId :: binary(), Message :: bc_message(), State) ->
    {ok, State} | {error, term()}.
-callback send_typing(SessionId :: binary(), State) -> ok.
-callback update_draft(SessionId, DraftId, Content :: binary(), State) ->
    {ok, State} | {error, term()}.
-callback finalize_draft(SessionId, DraftId, State) ->
    {ok, State} | {error, term()}.
-callback terminate(Reason, State) -> ok.
```

Implementations: `bc_channel_telegram`, `bc_channel_tui`.

### `bc_memory` — Memory backend abstraction (`beamclaw_memory`)

```erlang
-callback init(Config :: map(), SessionId :: binary()) -> {ok, State} | {error, term()}.
-callback store(Key, Value, Category :: memory_category(), State) ->
    {ok, State} | {error, term()}.
-callback recall(Query, Limit, Category | all, State) ->
    {ok, [bc_memory_entry()], State} | {error, term()}.
-callback get(Key, State) -> {ok, bc_memory_entry()} | {error, not_found | term()}.
-callback forget(Key, State) -> {ok, State} | {error, term()}.
-callback search(Query :: binary(), Limit :: pos_integer(),
                 Options :: map(), State :: term()) ->
    {ok, [{Score :: float(), Entry :: term()}], NewState :: term()} | {error, term()}.
-optional_callbacks([search/4]).
```

Implementations: `bc_memory_ets`, `bc_memory_mnesia`.

Search modules (all in `beamclaw_memory`): `bc_bm25` (BM25 scoring), `bc_vector` (cosine similarity), `bc_chunker` (text chunking), `bc_hybrid` (score merging), `bc_embedding` (API client), `bc_embedding_cache` (ETS cache gen_server).

### `bc_observer` — Observability backend abstraction (`beamclaw_obs`)

```erlang
-callback init(Config :: map()) -> {ok, State} | {error, term()}.
-callback handle_event(Event :: term(), State) -> {ok, State}.
-callback terminate(Reason, State) -> ok.
```

Implementations: `bc_obs_log`.

---

## Key Data Types (`apps/beamclaw_core/include/bc_types.hrl`)

```erlang
-type autonomy_level()   :: read_only | supervised | full.
-type memory_category()  :: core | daily | conversation | custom.
-type role()             :: system | user | assistant | tool.

-record(bc_message, {
    id, role, content, tool_calls = [], tool_call_id, name, ts = 0,
    attachments = [] :: [{binary(), binary()}]  %% [{MimeType, Base64Data}]
}).
-record(bc_tool_def, {
    name, description, parameters, source
}).
-record(bc_tool_call, {
    id, name, args, source
}).
-record(bc_tool_result, {
    tool_call_id, name, content, is_error = false
}).
-record(bc_session_ref, {
    session_id, user_id, session_pid, autonomy, agent_id
}).
-record(bc_channel_message, {
    session_id, user_id, channel, content, raw, ts,
    reply_pid = undefined, attachments = []
}).
-record(bc_memory_entry, {
    key, value, category, created_at, updated_at
}).
-record(bc_obs_event, {
    type, session_id, data, ts
}).
-record(bc_approval_req, {
    session_id, tool_call, ref
}).
-record(bc_skill, {
    name, description, homepage, emoji, content, source, metadata, path
}).
```

Include with: `-include_lib("beamclaw_core/include/bc_types.hrl").`

---

## Agentic Loop: `bc_loop` (`gen_statem`)

States (mirroring ZeroClaw's loop stages):

```
idle → compacting (optional) → streaming → awaiting_approval (optional)
     → executing_tools → streaming (loop back) → finalizing → idle
```

State transition rules:
- Enter `compacting` from `idle` when history tokens exceed `compaction_threshold_pct`% of the model's context window
- In `compacting`: if `memory_flush` is enabled (default), fire a hidden LLM turn to save durable memories before compaction
- Enter `awaiting_approval` from `streaming` when any tool call requires approval and autonomy ≠ `full`
- Loop back to `streaming` after `executing_tools` until no tool calls remain
- On any crash in `streaming`/`executing_tools`, supervisor restarts the loop; `bc_session` retains history

### Session as the "lane" (`bc_session` gen_server)

The session gen_server is the serialization point (inspired by OpenClaw's lane-based command queue):

```erlang
-record(session_state, {
    session_id,
    user_id,
    channel_id,
    autonomy_level   :: autonomy_level(),
    loop_pid         :: pid() | undefined,
    provider_mod     :: module(),
    memory_mod       :: module(),
    history          :: [bc_message()],
    pending_runs     :: queue:queue(),   %% the "lane queue"
    config           :: map()
}).
```

When a run arrives and the loop is busy → enqueue in `pending_runs`.
When loop sends `{turn_complete, SessionId, Result}` → dequeue and dispatch next run.

Progressive streaming: `bc_loop` sends `~80`-character chunks to the channel via `update_draft/finalize_draft` callbacks.

---

## Tool Call Parsing Fallback Chain (`bc_tool_parser`)

Applied in order; stop at first successful parse:

1. **OpenAI native** — `bc_message.tool_calls` non-empty → use directly
2. **XML tags** — match `<tool_call>`, `<toolcall>`, `<invoke>` with `<name>` and `<args>` children
3. **Markdown code blocks** — ` ```json\n{"tool":"foo","args":{...}}\n``` `
4. **Empty** — return `[]`

Security rule: parsers only match structured delimiters. **Never** extract arbitrary JSON from free text.

---

## Context Compaction (`bc_compactor`)

Triggered in `compacting` state:

1. Check: history tokens > `compaction_threshold_pct`% of context window (default: 80%)
2. Split: keep most recent messages whose cumulative tokens fit within `compaction_target_pct`% of the context window (default: 40%)
3. Summarize older messages via LLM: system prompt instructs concise factual summary
4. Summary becomes a `system` role message: `"[Conversation summary]: <text>"`
5. Fallback on LLM failure: deterministic trim to kept messages only
6. Emit `compaction_complete` obs event with `{before, After}` message counts

Token estimation: `byte_size(Content) div 4` (~4 chars/token). Context window
sizes are looked up from the model name via `bc_context:context_window/1`.

---

## Credential Scrubbing (`bc_scrubber`)

Applied to **every** tool result before it enters history or is sent to the LLM:

Patterns to redact:
- `api[_-]?key\s*[:=]\s*\S+`
- `token\s*[:=]\s*\S+`
- `password\s*[:=]\s*\S+`
- `secret\s*[:=]\s*\S+`
- `Bearer\s+\S+`
- `sk-[A-Za-z0-9]+` (OpenAI keys)
- `gsk_[A-Za-z0-9]+` (Groq API keys)
- `ghp_[A-Za-z0-9]+` (GitHub PATs)

Replace matched values with `[REDACTED]`. Use `re:replace/4` with `global` option.

---

## Approval Workflow (`bc_approval`)

`bc_approval` gen_server started transiently on first tool requiring approval.

Logic per tool call:
1. Tool in session-scoped allowlist? → auto-approve
2. `autonomy_level =:= full`? → auto-approve
3. `autonomy_level =:= read_only`? → deny immediately
4. Otherwise: send formatted prompt to channel; await `/yes`, `/no`, `/always` reply
   - `/always` → add to session-scoped allowlist; approve

Audit: emit `approval_requested` and `approval_resolved` obs events.
Allowlist is session-scoped and not persisted across restarts.

---

## Observability Events (`bc_obs:emit/2`)

Non-blocking cast; backends receive events asynchronously via `pg` process groups.

| Event type | Key data fields |
|---|---|
| `agent_start` | `session_id`, `user_id` |
| `llm_request` | `message_count`, `model` |
| `llm_response` | `duration_ms`, `success`, `error` |
| `tool_call_start` | `tool_name`, `args` |
| `tool_call_result` | `tool_name`, `duration_ms`, `success` |
| `turn_complete` | `total_duration_ms` |
| `compaction_complete` | `before`, `after` (message counts) |
| `approval_requested` | `tool_name`, `args` |
| `approval_resolved` | `tool_name`, `decision` |
| `session_start` | `session_id` |
| `session_end` | `session_id` |
| `session_restored` | `session_id`, `message_count` |
| `session_reset` | `session_id` |
| `sandbox_reaped` | `container_name` |
| `sched_job_created` | `job_id`, `schedule_type`, `agent_id` |
| `sched_job_fired` | `job_id`, `session_id`, `heartbeat` |
| `sched_job_completed` | `job_id`, `fire_count` |
| `sched_job_failed` | `job_id`, `error`, `error_count` |
| `sched_job_paused` | `job_id`, `reason` (manual \| auto) |
| `sched_suppressed` | `job_id`, `session_id` |
| `maintenance_compact_complete` | `session_id`, `reason` (idle \| nightly) |
| `maintenance_nightly_start` | `session_count` |
| `maintenance_nightly_complete` | `flushed_count` |
| `maintenance_pre_expiry_flush` | `session_id` |
| `env_refresh` | `section`, `success`, `duration_ms` |
| `a2a_task_created` | `task_id`, `session_id` |
| `a2a_task_updated` | `task_id`, `state` |
| `a2a_request` | `method`, `path`, `rpc_method` |
| `a2a_auth_failed` | `client_ip`, `reason` |

Usage: `bc_obs:emit(tool_call_start, #{tool_name => Name, args => Args, session_id => SId})`.

---

## MCP Protocol Details (`bc_mcp_server`)

- Transport: stdio (default) or HTTP (SSE)
- Protocol: JSON-RPC 2.0
- Handshake: `initialize` → `initialized` notification → `tools/list`
- Tool invocation: `tools/call` with `{name, arguments}` params
- Error handling: exponential backoff on port crash; max 5 restarts in 30 s
- `bc_mcp_registry` maps `tool_name → {server_pid, server_name}` for routing
