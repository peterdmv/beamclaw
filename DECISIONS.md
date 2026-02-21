# Architectural Decision Records

ADRs capture the context, reasoning, and consequences of significant design
choices made in BeamClaw. Each record is immutable once accepted; superseded
records are marked accordingly and link to their replacement.

**Status vocabulary**: Proposed → Accepted | Rejected | Deprecated | Superseded

---

## ADR-001 — Erlang/OTP 28 as the implementation language

**Date**: 2026-02-20
**Status**: Accepted

### Context

BeamClaw is a fault-tolerant AI agent gateway. The system must handle many
concurrent sessions, survive partial failures without losing user history, and
provide deep runtime observability. Two prior implementations exist: ZeroClaw
(Rust) and OpenClaw (TypeScript).

### Decision

Implement BeamClaw in Erlang/OTP 28, using idiomatic OTP patterns throughout.

### Rationale

- **Fault isolation**: OTP supervisors provide process-level crash isolation
  out of the box. A crashing agentic loop does not corrupt other sessions.
- **Concurrency model**: The actor model maps directly onto the problem domain
  (one process per session, one process per MCP server, etc.) with no shared
  mutable state.
- **Hot code reload**: Enables future zero-downtime upgrades of running agents.
- **Battle-tested primitives**: `gen_statem`, `gen_server`, `pg`, ETS, and
  `erlang:open_port` are mature and well-understood.
- **`rebar3`**: First-class toolchain for umbrella projects.

### Consequences

- Team requires Erlang expertise; onboarding cost is non-trivial.
- Fewer AI/ML libraries than Python; LLM interaction is via HTTP APIs only.
- Binary and string handling is more verbose than in Rust or TypeScript.

---

## ADR-002 — Rebar3 umbrella project with six OTP applications

**Date**: 2026-02-20
**Status**: Accepted

### Context

The system has distinct concerns: telemetry, memory, tools, MCP protocol,
agentic core logic, and the external-facing gateway. These could be one
monolithic app or multiple apps.

### Decision

Structure the project as a `rebar3` umbrella with six OTP applications, each
with a strictly acyclic dependency relationship:

```
beamclaw_obs → beamclaw_memory → beamclaw_tools → beamclaw_mcp
→ beamclaw_core → beamclaw_gateway
```

`beamclaw_obs` has zero sibling dependencies.

### Rationale

- Clear ownership and compilation order enforced by the build tool.
- Each app can be tested, released, or swapped independently.
- Cycle prevention is enforced structurally, not just by convention.
- Matches the layering in ZeroClaw's crate graph.

### Consequences

- More boilerplate (`app.src`, `_app.erl`, `_sup.erl`) per component.
- Cross-app calls go through the module API, not internal functions; this is
  the correct OTP boundary and is not a drawback.

---

## ADR-003 — Session persistence via permanent `bc_session` + transient `bc_loop`

**Date**: 2026-02-20
**Status**: Accepted

### Context

A crashing agentic loop (e.g., due to a bad LLM response or a tool exception)
should not lose the session history. The session must be recoverable.

### Decision

Split session state into two processes under `bc_session_sup` (one_for_one):

- `bc_session` (`gen_server`, **permanent**): owns history, pending run queue,
  and session metadata. Never crashes under normal conditions.
- `bc_loop` (`gen_statem`, **transient**): owns transient loop state
  (streaming cursor, tool call context). Supervisor restarts it on crash.

The supervisor is `one_for_one`, so a crashing loop does not restart the
session process.

### Rationale

Borrowed from OpenClaw's session isolation model. The "lane" (session) and
the "worker" (loop) have different failure domains. Permanent session process
means zero history loss on loop crash; transient loop means automatic recovery
without manual intervention.

### Consequences

- `bc_loop` must re-fetch necessary state from `bc_session` on (re)start.
- The `pending_runs` queue in `bc_session` ensures in-order delivery even
  across loop restarts.
- `bc_approval` must be a transient child too; it references the current loop.

---

## ADR-004 — Fire-and-forget observability via `bc_obs:emit/2`

**Date**: 2026-02-20
**Status**: Accepted

### Context

Observability (metrics, logging) must never introduce backpressure on the
agentic loop. A slow logging backend must not slow down LLM streaming.

### Decision

`bc_obs:emit/2` is implemented as a non-blocking `gen_server:cast` to
`bc_obs_manager`, which fans out to registered backends via `pg` process groups.
Backends process events asynchronously and independently.

### Rationale

- Zero latency impact on the caller.
- Backend failures are isolated; a dead Prometheus backend does not affect
  logging or the agentic loop.
- `pg` provides a lightweight pub/sub without an external broker.

### Consequences

- Events may be dropped if a backend mailbox overflows (acceptable trade-off).
- No back-pressure means backends must be designed for throughput, not
  reliability (again, acceptable for observability).
- The event ordering guarantee is per-backend, not global.

---

## ADR-005 — Tool call parsing fallback chain

**Date**: 2026-02-20
**Status**: Accepted

### Context

LLMs emit tool calls in different formats depending on provider, model, and
prompt. Strict reliance on OpenAI's native format excludes models that produce
XML or Markdown-fenced JSON.

### Decision

`bc_tool_parser` applies a four-step fallback chain, stopping at first success:

1. **OpenAI native** — `bc_message.tool_calls` field non-empty.
2. **XML tags** — match `<tool_call>`, `<toolcall>`, `<invoke>` with `<name>`
   and `<args>` children.
3. **Markdown code blocks** — ` ```json\n{"tool":"foo","args":{...}}\n``` `
4. **Empty** — return `[]` (no tool calls detected).

**Security rule**: parsers match only structured delimiters. Arbitrary JSON
extraction from free text is explicitly forbidden.

### Rationale

Supports the widest range of models while remaining deterministic and safe.
The security rule prevents prompt injection via tool-like text in assistant
output.

### Consequences

- Parser complexity is bounded; each step is independently testable.
- Adding a new format requires inserting a step before the empty fallback.
- False negatives (missed tool calls) are preferred over false positives
  (hallucinated tool calls).

---

## ADR-006 — Credential scrubbing before history entry

**Date**: 2026-02-20
**Status**: Accepted

### Context

Tool results (e.g., from `bc_tool_bash` or `bc_tool_curl`) may contain
secrets: API keys, tokens, passwords. These must not enter the LLM context
window or session history.

### Decision

`bc_scrubber` is applied to every tool result before it is stored in
`bc_session` history or forwarded to a provider. It uses `re:replace/4` with
the `global` option against a set of well-known patterns (OpenAI keys, GitHub
PATs, `Bearer` tokens, `api_key=`, `password=`, etc.), replacing matches with
`[REDACTED]`.

### Rationale

Defense-in-depth: even if a tool returns a secret unintentionally, it is
redacted at the history boundary. The LLM never sees live credentials.

### Consequences

- Regex patterns must be maintained as new secret formats emerge.
- Overly broad patterns could redact legitimate content; patterns should be
  anchored to known prefixes (`sk-`, `ghp_`, etc.) where possible.
- Scrubbing is applied unconditionally; there is no per-session opt-out.

---

## ADR-007 — Sliding-window rate limiting per client IP

**Date**: 2026-02-20
**Status**: Accepted

### Context

The HTTP gateway is publicly addressable. Without rate limiting, a single
client could saturate the session pool or the LLM API budget.

### Decision

`bc_rate_limiter` implements a sliding-window counter per client IP using ETS.
It is checked in every Cowboy handler before dispatch. The ETS table is pruned
every 60 seconds to prevent unbounded growth. Limits are configurable.

### Rationale

- ETS provides O(1) read/write with no cross-process messaging overhead.
- Sliding window is fairer than fixed-window (no boundary-burst effect).
- Single named gen_server owns the pruning ticker; no distributed coordination
  needed for a single-node deployment.

### Consequences

- State is in-memory and non-distributed; a multi-node deployment would require
  a shared backend (Redis, Mnesia). This is deferred.
- IP-based limiting is easily bypassed by rotating IPs; it is a basic DoS
  deterrent, not a strict auth control.

---

## ADR-008 — MCP protocol via stdio port, JSON-RPC 2.0

**Date**: 2026-02-20
**Status**: Accepted

### Context

MCP servers are external processes. They may expose a stdio interface or an
HTTP/SSE interface. The system needs a reliable, restartable connection to each.

### Decision

`bc_mcp_server` owns an `erlang:open_port` for stdio MCP servers. The
supervisor uses `{MaxR=5, MaxT=30}` restart intensity with transient children.
On restart, `bc_mcp_server` performs the full JSON-RPC 2.0 handshake
(`initialize` → `initialized` → `tools/list`) to rediscover tools.

### Rationale

`erlang:open_port` is the idiomatic OTP primitive for managing external OS
processes. Port crash propagates to the owning process, triggering supervisor
restart. Stateless reconnect (rediscover tools on restart) avoids stale tool
registrations.

### Consequences

- stdio servers must be idempotent on reconnect.
- HTTP/SSE transport is supported as an alternative path but shares the same
  `bc_mcp_registry` routing.
- `bc_mcp_registry` is updated on each server restart; stale entries for dead
  servers are cleaned up via monitor.

---

## ADR-009 — Drop Prometheus; use OTP logger for local observability

**Date**: 2026-02-20
**Status**: Accepted

### Context

The original spec included a `prometheus.erl` backend for metrics scraping in
production. For the current local-development phase, adding and maintaining a
Prometheus dependency (and its scrape endpoint) provides no benefit and
introduces unnecessary complexity.

### Decision

Remove `prometheus` from `rebar.config` deps and delete `bc_obs_prometheus.erl`.
The `beamclaw_obs_sup` now supervises only three children: the `pg` scope,
`bc_obs_manager`, and `bc_obs_log`. `bc_obs_log` emits one structured log line
per event via OTP's `logger`. Log level filtering is controlled by the operator
via the `kernel` app's logger configuration in `sys.config`.

The `bc_obs` fan-out design is unchanged: adding a metrics backend later only
requires writing a new gen_server that joins the `bc_obs_backends` pg group and
adding it as a supervisor child — no changes to `bc_obs_manager` or callers.

### Rationale

- Removes a compile-time and runtime dependency with no current use.
- OTP logger provides structured output with zero additional deps.
- The existing pg fan-out architecture makes it trivial to add a metrics
  backend when production deployment warrants it.

### Consequences

- No Prometheus metrics in this phase; `/metrics` HTTP handler (M6) will be
  a stub until a backend is added.
- A future ADR should document the chosen metrics approach when production
  needs arise (prometheus.erl, telemetry, or otherwise).

---

## ADR-010 — Replace bc_memory_sqlite stub with bc_memory_mnesia

**Date**: 2026-02-21
**Status**: Accepted

### Context

`bc_memory_sqlite` was a stub (every callback returned `not_implemented`).
The planned SQLite backend would require a NIF or port driver (e.g. esqlite),
adding an external dependency and C-compilation risk. For the BeamClaw memory
use case, the schema is stable (five fixed fields, no migrations expected), the
data volume is small (single-user workloads), and Mnesia is already available
as part of OTP.

### Decision

Delete `bc_memory_sqlite.erl` and implement `bc_memory_mnesia.erl` as the
persistent memory backend. One global Mnesia table `bc_memory_entries` uses a
composite `{SessionId, UserKey}` primary key to provide per-session isolation
without dynamic table creation or per-session atom leakage.

Storage type is `disc_copies` when a disc schema exists (production), falling
back to `ram_copies` when no schema is on disk (dev/test shell). The
`beamclaw_memory_app` handles schema and table creation on application start,
guarding against `already_exists` on subsequent restarts.

The ETS backend remains the default (`{backend, ets}` in `sys.config`) for
ephemeral conversation memory. The Mnesia backend is opt-in for sessions
requiring persistence across restarts.

### Rationale

- Zero extra dependencies: Mnesia ships with OTP.
- Transactional semantics protect against partial-write corruption if `bc_loop`
  crashes mid-store (dirty writes are used today; upgrading to transactions
  requires no API changes).
- Single composite-key table is simpler than one table per session; avoids
  dynamic schema changes and atom-per-session naming.
- `bc_memory_sqlite` was a stub — no migration of existing data is needed.

### Consequences

- Mnesia disc_copies requires a disc schema to be created before the first
  `mnesia:start/0`. In production this is a one-time `mnesia:create_schema/1`
  call (release hook or setup script). In dev/test the backend silently uses
  `ram_copies`, which is acceptable.
- The 2 GB Mnesia disc_copies limit per table is not a constraint for
  single-user agent memory workloads.
- If a distributed (multi-node) deployment is later required, Mnesia's built-in
  replication can be configured without changing the `bc_memory` behaviour API.

---

## ADR-011 — Add read_file / write_file built-in tools; fix curl inets dep

**Date**: 2026-02-21
**Status**: Accepted

### Context

M3 (Tool Registry) is complete with four built-in tools: `terminal`, `bash`,
`curl`, and `jq`. However, reading and writing files — a core capability for
any digital assistant agent — requires going through `bash`, which has
`requires_approval = true` and `min_autonomy = supervised`. This means the
agent cannot read a local file (e.g., documentation or a config) without
triggering an approval prompt, even in `read_only` autonomy contexts.

Additionally, `bc_tool_curl` uses `httpc:request/4` from OTP's `inets`
application, but `inets` was missing from the `beamclaw_tools` application
dependency list, causing runtime crashes when curl is first invoked.

### Decision

1. Add `bc_tool_read_file`: reads a file via `file:read_file/1`. Sets
   `requires_approval = false` and `min_autonomy = read_only`. Zero new deps.

2. Add `bc_tool_write_file`: writes content to a file via `file:write_file/2`.
   Sets `requires_approval = true` and `min_autonomy = supervised` (destructive
   operation — overwrites without confirmation). Zero new deps.

3. Add `inets` to the `{applications, [...]}` list in `beamclaw_tools.app.src`
   so OTP ensures `inets` is started before `beamclaw_tools`.

4. Register both new tools in `bc_tool_registry`'s `BuiltIns` list.

Web search and memory-as-tool are deferred: web search belongs as an MCP
server (external API dependency); memory tools would require `beamclaw_tools`
to depend on `beamclaw_memory`, creating a cycle with `beamclaw_core`.

### Rationale

- Dedicated file tools let the agent read configs and source at `read_only`
  autonomy without an approval prompt — the highest-value gap vs. Claude Code
  and GPT tool sets.
- OTP `file` module requires no additional dependencies.
- The `inets` fix is a correctness bug: `httpc` is part of `inets` and must be
  started explicitly.

### Consequences

- The agent can now read files without human approval at `read_only` autonomy.
- File writes still require approval (consistent with `bash` and `terminal`).
- `beamclaw_tools` now lists six built-in tools.
- `inets` is started as part of the `beamclaw_tools` application boot sequence.
