# BeamClaw Architecture

BeamClaw is an Erlang/OTP 28 umbrella project composed of nine OTP applications. Each app
has a clearly defined responsibility and a strictly acyclic dependency relationship.

---

## Nine-App Umbrella

```
beamclaw_obs         — fire-and-forget telemetry (zero sibling deps)
     ↑
beamclaw_memory      — memory behaviour + ETS / Mnesia backends
     ↑
beamclaw_tools       — built-in tools + tool registry
     ↑
beamclaw_sandbox     — Docker sandboxed code execution, PII tokenization, tool bridge
     ↑
beamclaw_mcp         — MCP client (stdio / HTTP), tool discovery
     ↑
beamclaw_core        — sessions, agentic loop, LLM providers, approval, compaction
     ↑
beamclaw_scheduler   — scheduled tasks, heartbeat check-ins, timer management
     ↑
beamclaw_gateway     — channels (Telegram, TUI), HTTP gateway, rate limiter

beamclaw_cli         — CLI escript (not a daemon app; bundles all above)
```

The rule: no dependency cycle. `beamclaw_obs` never imports from any sibling.
`beamclaw_cli` is a standalone escript, not part of the runtime supervision tree.

| App | OTP role | Key modules |
|---|---|---|
| `beamclaw_obs` | Observability fan-out | `bc_obs`, `bc_obs_manager`, `bc_obs_log` |
| `beamclaw_memory` | Session memory + search | `bc_memory`, `bc_memory_ets`, `bc_memory_mnesia`, `bc_bm25`, `bc_vector`, `bc_chunker`, `bc_hybrid`, `bc_embedding`, `bc_embedding_cache` |
| `beamclaw_tools` | Tool execution | `bc_tool`, `bc_tool_registry`, built-in tools |
| `beamclaw_sandbox` | Sandboxed execution | `bc_sandbox`, `bc_sandbox_registry`, `bc_tool_exec`, `bc_pii_tokenizer`, `bc_sandbox_policy`, `bc_sandbox_env`, `bc_sandbox_bridge`, `bc_sandbox_docker`, `bc_sandbox_discovery`, `bc_sandbox_skills` |
| `beamclaw_mcp` | MCP protocol | `bc_mcp_server`, `bc_mcp_registry` |
| `beamclaw_core` | Brain | `bc_session`, `bc_loop`, `bc_provider_*`, `bc_approval`, `bc_scrubber`, `bc_skill_parser`, `bc_skill_discovery`, `bc_skill_eligibility`, `bc_skill_installer` |
| `beamclaw_scheduler` | Scheduled tasks | `bc_sched_store`, `bc_sched_runner`, `bc_sched_executor`, `bc_tool_scheduler`, `bc_sched_random`, `bc_sched_interval` |
| `beamclaw_gateway` | Interfaces | `bc_channel_telegram`, `bc_channel_tui`, Cowboy handlers |
| `beamclaw_cli` | CLI escript | `beamclaw_cli` (escript `main/1`); not started as a daemon |

---

## Supervision Trees

### beamclaw_core

```
beamclaw_core_sup  (one_for_one)
  ├── bc_session_registry     (gen_server, named — ETS: session_id → pid)
  ├── bc_session_cleaner      (gen_server, permanent — periodic expired session cleanup)
  └── bc_sessions_sup         (simple_one_for_one)
        └── [per session] bc_session_sup  (one_for_one)
              ├── bc_session     (gen_server, permanent — conversation history)
              ├── bc_loop        (gen_statem, transient — agentic loop)
              ├── bc_provider_X  (gen_server, transient — one per session)
              └── bc_approval    (gen_server, transient — started on demand)
```

### beamclaw_memory

```
beamclaw_memory_sup  (one_for_one)
  └── bc_embedding_cache     (gen_server, permanent — ETS-backed, 24 h TTL)
```

`bc_embedding_cache` caches embedding vectors keyed by `{text_hash, model}` to avoid
redundant API calls. The remaining memory modules (`bc_bm25`, `bc_vector`, `bc_chunker`,
`bc_hybrid`, `bc_embedding`) are pure-function or stateless — they have no supervised
processes.

### beamclaw_sandbox

```
beamclaw_sandbox_sup  (one_for_one)
  ├── bc_sandbox_registry     (gen_server, permanent — ETS: {session_id, scope} → pid)
  ├── bc_sandbox_reaper       (gen_server, permanent — periodic orphan container cleanup)
  └── bc_sandbox_sup          (simple_one_for_one)
        └── [per sandbox] bc_sandbox (gen_server, transient — Docker container lifecycle)
```

Each `bc_sandbox` manages a Docker container with hardened security flags (`--cap-drop ALL`,
`--read-only`, `--security-opt no-new-privileges`, `--network none`, memory/CPU/PID limits).
A Unix domain socket mounted into the container enables a Python bridge to call back to
BeamClaw tools via JSON-RPC 2.0.

The sandbox app is opt-in (`{enabled, false}` by default) and requires Docker on the host.
When enabled, `beamclaw_sandbox_app` registers `bc_tool_exec` in `bc_tool_registry`.

### beamclaw_mcp

```
beamclaw_mcp_sup  (one_for_one)
  ├── bc_mcp_registry        (gen_server, named)
  └── bc_mcp_servers_sup     (one_for_one, dynamic)
        └── bc_mcp_server    (gen_server, transient — one per configured server)
```

### beamclaw_scheduler

```
beamclaw_scheduler_sup  (one_for_one)
  ├── bc_sched_store       (gen_server, permanent — Mnesia job CRUD)
  ├── bc_sched_runner      (gen_server, permanent — timer management, fire events)
  └── bc_sched_executor    (gen_server, permanent — session dispatch, delivery)
```

Runner manages timers (lightweight, never blocks). Executor does heavy work
(session creation, LLM dispatch, HTTP delivery). A crash in execution doesn't
lose timer state — same failure-domain split as `bc_session` / `bc_loop`.

### beamclaw_gateway

```
beamclaw_gateway_sup  (one_for_one)
  ├── bc_rate_limiter              (gen_server, named)
  ├── bc_gateway_http_sup          (one_for_one)
  │     └── bc_gateway_cowboy      (Cowboy listener wrapper)
  └── bc_gateway_channels_sup      (one_for_one)
        ├── bc_channel_telegram_sup → bc_channel_telegram (gen_server)
        └── bc_channel_tui_sup      → bc_channel_tui      (gen_server)
```

### beamclaw_obs

```
beamclaw_obs_sup  (one_for_one)
  ├── bc_obs_manager     (gen_server — fan-out via pg process groups)
  └── bc_obs_log         (gen_server — OTP logger backend)
```

`beamclaw_cli` has no supervision tree — it is an escript that starts and stops
as a single OS process. Daemon lifecycle (start/stop/restart) is managed via
Erlang distribution IPC to a separately running `beamclaw_gateway` node.

---

## Session Persistence Design

The key architectural insight is splitting session state across two processes with
different failure domains:

**`bc_session` (permanent gen_server)** owns:
- Conversation history (`[bc_message()]`)
- Pending run queue (in-order delivery across loop restarts)
- Session metadata (user ID, autonomy level, channel)

**`bc_loop` (transient gen_statem)** owns:
- Current loop state (streaming cursor, tool call context)
- Provider state (HTTP connection, streaming buffer)

When `bc_loop` crashes (e.g., due to a malformed LLM response), the `one_for_one`
supervisor restarts only the loop. `bc_session` — and all history — survives. The loop
re-fetches history from `bc_session` on restart and resumes the next pending run.

This pattern is borrowed from OpenClaw's lane-based command queue: the "lane" (`bc_session`)
serialises runs and survives crashes; the "worker" (`bc_loop`) is ephemeral.

### Mnesia-backed session persistence

`bc_session_store` persists session state (history, user/agent IDs, config) to a Mnesia table
(`bc_session_stored`). History is serialized via `term_to_binary` with a version tag for forward
compatibility. On session `init/1`, `bc_session` checks for a persisted record and restores
history if found — this means conversations survive VM restarts.

`bc_session_cleaner` is a permanent gen_server that periodically (every 5 minutes by default)
deletes sessions whose `updated_at` timestamp exceeds the `session_ttl_seconds` config.

Persistence can be disabled with `{session_persistence, false}` in `sys.config`.

### Cross-channel session sharing

Session IDs are derived deterministically from `{user_id, agent_id}` via SHA-256:

```erlang
bc_session_registry:derive_session_id(UserId, AgentId) ->
    <<"session-", Hex/binary>>.
```

Same user + same agent = same session ID regardless of channel (TUI, Telegram, HTTP, WebSocket).
This means conversations are shared: a user can start in TUI and continue on Telegram.

Each channel derives a structured user ID with a prefix to prevent cross-domain collisions:
- TUI: `<<"local:username">>` (from `BEAMCLAW_USER` or `USER` env var)
- Telegram: `<<"tg:12345">>` (from Telegram user ID)
- HTTP: `<<"api:user_id">>` (from `X-User-Id` header or request body)
- WebSocket: `<<"ws:user_id">>` (from message payload)

Response routing is per-run, not per-session: `bc_loop` uses `reply_channel` from each
incoming message to determine which channel module to route the response through. This
ensures that if a user sends via Telegram, the response goes to Telegram — even though the
session was originally created from TUI.

Configurable via `{session_sharing, shared | per_channel}`. When `per_channel`, the channel
atom is included in the session ID hash, producing separate sessions per channel.

---

## Agentic Loop State Machine

`bc_loop` is a `gen_statem` with the following states:

```
idle
 │
 ├─(history > compaction_threshold)─→ compacting
 │                                       │
 │                              (memory_flush enabled)─→ memory flush (hidden LLM turn)
 │                                       │                    │
 │                                       └────────────────────┘
 │                                       │
 │                                   do_compact ─→ streaming
 │
 └─(new run arrives)─→ streaming
                          │
                          ├─(tool calls + approval needed)─→ awaiting_approval
                          │                                       │
                          │                              (approved)─→ executing_tools
                          │                              (denied) ─→ finalizing
                          │
                          ├─(tool calls, no approval needed)─→ executing_tools
                          │         │
                          │         └─(more tool calls)─→ streaming (loop)
                          │
                          └─(no tool calls)─→ finalizing ─→ idle
```

Each transition emits an observability event via `bc_obs:emit/2` (non-blocking cast).

---

## Tool Call Parsing

`bc_tool_parser` applies a four-step fallback chain to extract tool calls from LLM output:

1. **OpenAI native** — `bc_message.tool_calls` field non-empty → use directly
2. **XML tags** — match `<tool_call>`, `<toolcall>`, `<invoke>` with `<name>` / `<args>` children
3. **Markdown code blocks** — ` ```json\n{"tool":"foo","args":{...}}\n``` `
4. **Empty** — return `[]`

Security rule: parsers only match structured delimiters. Arbitrary JSON extraction from
free text is explicitly forbidden to prevent prompt injection.

---

## Credential Scrubbing

`bc_scrubber:scrub/1` is applied to every tool result before it enters history or is
forwarded to an LLM. It uses `re:replace/4` with the `global` option against patterns for:

- OpenAI keys (`sk-[A-Za-z0-9]+`)
- GitHub PATs (`ghp_[A-Za-z0-9]+`)
- Bearer tokens (`Bearer\s+\S+`)
- Generic `api_key=`, `token=`, `password=`, `secret=` assignments

Matched values are replaced with `[REDACTED]`. The LLM never sees live credentials.

---

## Approval Workflow

`bc_approval` is a transient gen_server started on demand when a tool requiring approval
is encountered. Logic per tool call:

1. Tool in session-scoped allowlist? → auto-approve
2. `autonomy_level =:= full`? → auto-approve
3. `autonomy_level =:= read_only`? → deny immediately
4. Otherwise: send a formatted prompt to the channel; wait for `/yes`, `/no`, or `/always`
   - `/always` → add to session-scoped allowlist; approve

Audit events (`approval_requested`, `approval_resolved`) are emitted via `bc_obs`.

---

## Key Behaviours

### `bc_provider` — LLM provider abstraction

```erlang
-callback init(Config :: map()) -> {ok, State} | {error, term()}.
-callback complete(Messages, Options, State) ->
    {ok, bc_message(), State} | {error, term(), State}.
-callback stream(Messages, Options, CallerPid :: pid(), State) ->
    {ok, State} | {error, term(), State}.
-callback capabilities(State) -> map().
-callback terminate(Reason, State) -> ok.
```

Implementations: `bc_provider_openrouter`, `bc_provider_openai`.

### `bc_tool` — Tool abstraction

```erlang
-callback definition() -> bc_tool_def().
-callback execute(Args :: map(), Session :: bc_session_ref(), Context :: map()) ->
    {ok, binary()} | {error, binary()}.
-callback requires_approval() -> boolean().
-callback min_autonomy() -> autonomy_level().
```

Built-in implementations: `bc_tool_bash`, `bc_tool_terminal`, `bc_tool_curl`,
`bc_tool_jq`, `bc_tool_read_file`, `bc_tool_write_file`, `bc_tool_workspace_memory`,
`bc_tool_exec` (sandboxed code execution — in `beamclaw_sandbox`, registered conditionally).

### `bc_channel` — Messaging channel abstraction

```erlang
-callback init(Config :: map()) -> {ok, State} | {error, term()}.
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

### `bc_memory` — Memory backend abstraction

```erlang
-callback init(Config :: map(), SessionId :: binary()) -> {ok, State} | {error, term()}.
-callback store(Key, Value, Category :: memory_category(), State) ->
    {ok, State} | {error, term()}.
-callback recall(Query, Limit, Category | all, State) ->
    {ok, [bc_memory_entry()], State} | {error, term()}.
-callback get(Key, State) -> {ok, bc_memory_entry()} | {error, not_found | term()}.
-callback forget(Key, State) -> {ok, State} | {error, term()}.

%% Optional callback — backends may implement for ranked search:
-callback search(Query, Limit, Mode, State) ->
    {ok, [{Score :: float(), bc_memory_entry()}], State} | {error, term()}.
%% Mode :: keyword | semantic | hybrid
```

Implementations: `bc_memory_ets` (default), `bc_memory_mnesia` (persistent). Both
implement the optional `search/4` callback with BM25-scored keyword search. The Mnesia
backend additionally stores an `embedding` field per entry for vector-based semantic search.

#### Search modules (pure-function, no supervised processes)

| Module | Role |
|---|---|
| `bc_bm25` | BM25 keyword search: tokenization, TF-IDF scoring, document ranking |
| `bc_vector` | Cosine similarity, L2 normalization, dot product on float lists |
| `bc_chunker` | Split text into overlapping word-boundary chunks for embedding |
| `bc_hybrid` | Merge BM25 + vector scores with configurable weights |
| `bc_embedding` | HTTP client for OpenAI-compatible `/v1/embeddings` API |
| `bc_embedding_cache` | gen_server, ETS-backed embedding cache with 24 h TTL |

---

## Observability

`bc_obs:emit/2` is a non-blocking cast — it never creates backpressure on the agentic
loop. `bc_obs_manager` fans events out to registered backends via OTP `pg` process groups.

Key events:

| Event | Data |
|---|---|
| `agent_start` | `session_id`, `user_id` |
| `llm_request` | `message_count`, `model` |
| `llm_response` | `duration_ms`, `success` |
| `tool_call_start` | `tool_name`, `args` (scrubbed) |
| `tool_call_result` | `tool_name`, `duration_ms`, `success` |
| `turn_complete` | `total_duration_ms` |
| `compaction_complete` | `before`, `after` (message counts) |
| `approval_requested` | `tool_name` |
| `approval_resolved` | `tool_name`, `decision` |
| `session_restored` | `session_id`, `message_count` |

Adding a new observability backend: implement `bc_observer`, join the `bc_obs_backends` pg
group in `init/1`, and add the process as a child of `beamclaw_obs_sup`. No changes to
`bc_obs_manager` or any caller are required.

---

## Agent Workspaces

Agent identity is a *data path* problem, not a *process architecture* problem. The existing
session/loop/supervisor structure is unchanged — an `agent_id` binary is threaded through
the session Config and used to load bootstrap files before each LLM call.

### Workspace directory structure

```
~/.beamclaw/agents/
  default/
    SOUL.md        — personality / instructions → primary system prompt
    IDENTITY.md    — metadata: Name, Type, Emoji
    USER.md        — owner profile
    TOOLS.md       — tool guidance
    MEMORY.md      — long-term curated memory (agent reads + updates)
    AGENTS.md      — workspace guidelines
    BOOTSTRAP.md   — first-run discovery ritual; self-deleting
    memory/        — daily log files (YYYY-MM-DD.md)
  my-custom-agent/
    ...
```

Override the base directory with the `BEAMCLAW_HOME` environment variable.

### System prompt assembly (`bc_system_prompt`)

On each LLM call, `bc_system_prompt:assemble(AgentId)` reads all seven bootstrap files and
converts them into system-role messages prepended to the conversation history. Order:
IDENTITY → SOUL → USER → TOOLS → AGENTS → BOOTSTRAP → MEMORY (MEMORY last = closest to conversation).
Empty/missing files are skipped. If the agent doesn't exist, a single fallback message is used.

### Workspace memory tool (`bc_tool_workspace_memory`)

A built-in tool that allows the agent to manage its MEMORY.md, daily logs, and bootstrap
files (IDENTITY.md, USER.md, SOUL.md, TOOLS.md, AGENTS.md). The path is constructed
internally from `bc_session_ref.agent_id`, preventing path traversal. Requires no approval
and runs at `read_only` autonomy.

Actions: `read`, `append`, `replace` (MEMORY.md); `read_daily`, `append_daily`, `list_daily`
(daily logs); `read_bootstrap`, `update_bootstrap` (bootstrap files — requires `file` parameter).

### Daily logs

Agents can write daily logs to `memory/YYYY-MM-DD.md` files via the `workspace_memory`
tool (actions: `read_daily`, `append_daily`, `list_daily`). The system prompt automatically
includes today's and yesterday's daily logs, providing recent context without polluting
the long-term MEMORY.md.

### Skill system

Skills are markdown SKILL.md files that provide specialized knowledge and capabilities.
Discovery sources (lowest to highest precedence):

1. Bundled: `<app_priv>/skills/*/SKILL.md`
2. Global: `~/.beamclaw/skills/*/SKILL.md`
3. Per-agent: `~/.beamclaw/agents/<name>/skills/*/SKILL.md`

Each skill has a frontmatter block with `name:`, `description:`, and optional `metadata:`
(JSON with requirements, install specs, emoji). `bc_skill_eligibility` checks that required
binaries, environment variables, and OS constraints are met before including a skill in the
system prompt.

Key modules: `bc_skill_parser`, `bc_skill_discovery`, `bc_skill_eligibility`, `bc_skill_installer`.

### Tool definitions in LLM requests

`bc_loop` fetches all registered tool definitions from `bc_tool_registry:list()` and passes
them in the `Options` map to the provider. `bc_provider_openrouter` includes them in the
request body as OpenAI-format function-calling tool definitions.

---

## HTTP Gateway Routes

| Method | Path | Handler | Description |
|---|---|---|---|
| `GET` | `/health` | `bc_http_health_h` | Liveness check |
| `GET` | `/metrics` | `bc_http_metrics_h` | Prometheus stub |
| `POST` | `/v1/chat/completions` | `bc_http_completions_h` | OpenAI-compatible API (SSE + sync) |
| `GET` | `/ws` | `bc_ws_h` | WebSocket session |
| `POST` | `/webhook/telegram` | `bc_webhook_telegram_h` | Telegram webhook receiver |

Rate limiting: sliding-window per client IP, ETS-backed, pruned every 60 s. Checked in
every handler before dispatch.

---

## Sandbox Execution

The `beamclaw_sandbox` app implements the "Code Execution with MCP" pattern: instead of
the LLM making N separate tool calls (each a round-trip through the context window), the
agent writes a **script** that runs in a Docker sandbox, discovers tools via a filesystem,
calls them locally via a bridge, and returns only the filtered/aggregated result.

### Container architecture

```
┌─────────────────────────────────────────────┐
│  BeamClaw (Erlang VM)                       │
│                                             │
│  bc_sandbox ─── Unix socket ───┐            │
│  bc_sandbox_bridge             │            │
│  bc_pii_tokenizer              │            │
│  bc_sandbox_policy             │            │
│  bc_sandbox_discovery          │            │
│                                │            │
└────────────────────────────────┼────────────┘
                                 │
┌────────────────────────────────┼────────────┐
│  Docker container              │            │
│  (--network none, --read-only) │            │
│                                │            │
│  /tools/                       │            │
│    index.txt                   │            │
│    <tool>/description.txt      │            │
│    <tool>/schema.json          │            │
│                                │            │
│  beamclaw_bridge.py ─── Unix socket         │
│    search_tools()                           │
│    get_tool(name)                           │
│    call_tool(name, **args)                  │
│                                             │
│  agent_script.py  ← written via docker cp   │
└─────────────────────────────────────────────┘
```

### Security layers

1. **Docker isolation**: `--cap-drop ALL`, `--read-only`, `--security-opt no-new-privileges`,
   `--tmpfs /tmp`, `--memory 512m`, `--cpus 1.0`, `--pids-limit 256`, `--network none`
2. **PII tokenization** (`bc_pii_tokenizer`): bidirectional masking at all boundary crossings
3. **Tool policy** (`bc_sandbox_policy`): allow/deny rules with wildcard support
4. **Environment hardening** (`bc_sandbox_env`): allowlist/blocklist filtering; API keys
   never exposed to containers
5. **Credential scrubbing** (`bc_scrubber`): standard scrubbing on final output

### Tool bridge

`bc_loop` injects a `tool_bridge_fn` closure into the Context map passed to
`bc_tool_exec:execute/3`. This callback captures access to both `bc_tool_registry` and
`bc_mcp_registry`, avoiding a dependency cycle between `beamclaw_sandbox` and `beamclaw_core`.
When the bridge receives a `call_tool` request from the container, it dispatches through
the callback to execute the tool server-side and returns the result via the Unix socket.

### Skills persistence

Successful sandbox scripts can be saved as SKILL.md files (`bc_sandbox_skills`) with
`type: sandbox_script` metadata. These integrate with the existing skill system (M16–M17)
and can be reloaded by name in future `exec` tool calls.
