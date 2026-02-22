# BeamClaw Architecture

BeamClaw is an Erlang/OTP 28 umbrella project composed of seven OTP applications. Each app
has a clearly defined responsibility and a strictly acyclic dependency relationship.

---

## Seven-App Umbrella

```
beamclaw_obs         — fire-and-forget telemetry (zero sibling deps)
     ↑
beamclaw_memory      — memory behaviour + ETS / Mnesia backends
     ↑
beamclaw_tools       — built-in tools + tool registry
     ↑
beamclaw_mcp         — MCP client (stdio / HTTP), tool discovery
     ↑
beamclaw_core        — sessions, agentic loop, LLM providers, approval, compaction
     ↑
beamclaw_gateway     — channels (Telegram, TUI), HTTP gateway, rate limiter

beamclaw_cli         — CLI escript (not a daemon app; bundles all six above)
```

The rule: no dependency cycle. `beamclaw_obs` never imports from any sibling.
`beamclaw_cli` is a standalone escript, not part of the runtime supervision tree.

| App | OTP role | Key modules |
|---|---|---|
| `beamclaw_obs` | Observability fan-out | `bc_obs`, `bc_obs_manager`, `bc_obs_log` |
| `beamclaw_memory` | Session memory | `bc_memory`, `bc_memory_ets`, `bc_memory_mnesia` |
| `beamclaw_tools` | Tool execution | `bc_tool`, `bc_tool_registry`, built-in tools |
| `beamclaw_mcp` | MCP protocol | `bc_mcp_server`, `bc_mcp_registry` |
| `beamclaw_core` | Brain | `bc_session`, `bc_loop`, `bc_provider_*`, `bc_approval`, `bc_scrubber` |
| `beamclaw_gateway` | Interfaces | `bc_channel_telegram`, `bc_channel_tui`, Cowboy handlers |
| `beamclaw_cli` | CLI escript | `beamclaw_cli` (escript `main/1`); not started as a daemon |

---

## Supervision Trees

### beamclaw_core

```
beamclaw_core_sup  (one_for_one)
  ├── bc_session_registry     (gen_server, named — ETS: session_id → pid)
  └── bc_sessions_sup         (simple_one_for_one)
        └── [per session] bc_session_sup  (one_for_one)
              ├── bc_session     (gen_server, permanent — conversation history)
              ├── bc_loop        (gen_statem, transient — agentic loop)
              ├── bc_provider_X  (gen_server, transient — one per session)
              └── bc_approval    (gen_server, transient — started on demand)
```

### beamclaw_mcp

```
beamclaw_mcp_sup  (one_for_one)
  ├── bc_mcp_registry        (gen_server, named)
  └── bc_mcp_servers_sup     (one_for_one, dynamic)
        └── bc_mcp_server    (gen_server, transient — one per configured server)
```

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

---

## Agentic Loop State Machine

`bc_loop` is a `gen_statem` with the following states:

```
idle
 │
 ├─(history > compaction_threshold)─→ compacting ─→ idle (then re-enter)
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
`bc_tool_jq`, `bc_tool_read_file`, `bc_tool_write_file`, `bc_tool_workspace_memory`.

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
```

Implementations: `bc_memory_ets` (default), `bc_memory_mnesia` (persistent).

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
  my-custom-agent/
    ...
```

Override the base directory with the `BEAMCLAW_HOME` environment variable.

### System prompt assembly (`bc_system_prompt`)

On each LLM call, `bc_system_prompt:assemble(AgentId)` reads all six bootstrap files and
converts them into system-role messages prepended to the conversation history. Order:
IDENTITY → SOUL → USER → TOOLS → AGENTS → MEMORY (MEMORY last = closest to conversation).
Empty/missing files are skipped. If the agent doesn't exist, a single fallback message is used.

### Workspace memory tool (`bc_tool_workspace_memory`)

A built-in tool that allows the agent to read, append to, or replace its own `MEMORY.md`.
The path is constructed internally from `bc_session_ref.agent_id`, preventing path traversal.
Requires no approval and runs at `read_only` autonomy.

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
