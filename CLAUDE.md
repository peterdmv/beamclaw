# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## State Management
- **Task List**: Always read and update @STATUS.md before and after every task.
  - When a milestone is fully complete (all ✅), collapse it into the
    "Completed Milestones" summary table and move its detail to
    `STATUS_ARCHIVE.md`. Keep only the last 2–3 completed milestones
    with full detail in STATUS.md.
  - STATUS.md should stay under ~150 lines. If it exceeds this, archive
    older completed milestones.
- **Decision Log**: Consult @DECISIONS.md before suggesting architectural changes.
- **Documentation Sync**: When adding or removing a module, app, CLI command,
  config option, HTTP route, or behaviour — or when making an architectural
  change — update the relevant inline section(s) of this file **and** the
  corresponding file(s) under `docs/`. See the table below.
- **Progress Tracking**: If context window exceeds 80%, summarize the current sub-task into STATUS.md and prompt for a `/compact`.
- **Human-only files**: Do not read or reference `HUMAN_NOTES.md` — this file is for human engineers only.

| Change type | Update in CLAUDE.md | Update in docs/ |
|---|---|---|
| New/removed OTP app | Application Dependency Graph, File Layout | `docs/architecture.md` |
| New/removed module | File Layout | — |
| Supervision tree change | Supervision Trees | `docs/architecture.md` |
| New/removed behaviour or callback | Key Behaviours | `docs/architecture.md` |
| New/removed CLI command | Common Commands | `docs/running.md` |
| New/removed config key | Configuration | `docs/configuration.md`, `config/sys.docker.config` |
| New/removed HTTP route | HTTP Gateway Routes | `docs/running.md` |
| New rebar3 dependency | Rebar3 Dependencies | `docs/building.md` |
| Security pattern added | Credential Scrubbing | — |

## Project Overview

BeamClaw is a fault-tolerant, security-conscious AI agent gateway and MCP (Model Context
Protocol) host implemented in Erlang/OTP 28, built as a rebar3 umbrella project. It is
architecturally inspired by ZeroClaw (Rust) and OpenClaw (TypeScript), porting their best
ideas into idiomatic OTP.

**Project goals**: fault tolerance · observability · security · extensibility

## Security Rules

Security is a first-class project goal. The rules below apply to all code,
configuration, and documentation changes.

### Secret handling — configuration
- **Never hardcode secrets.** API keys, tokens, and passwords MUST be expressed
  as `{env, "VAR_NAME"}` tuples in `config/sys.config`, resolved at runtime by
  `bc_config:get/2`. Hardcoded credentials in source files or config are a
  blocking review issue.
- **`.env` files and `*.secret` files must never be committed.** The `.gitignore`
  must exclude `*.env`, `.env`, `*.secret`, `priv/secrets/`, and similar paths.
  Warn before staging any file whose name matches these patterns.

### Secret handling — runtime
- **`bc_scrubber` is mandatory on all tool results.** Every value returned by a
  tool (`bc_tool_*:execute/3`) and every LLM API response MUST pass through
  `bc_scrubber:scrub/1` before being stored in session history or logged.
  `bc_loop` applies this at the boundary — never bypass it.
- **Keep `bc_scrubber` patterns current.** When adding support for a new API or
  service whose keys have a recognizable prefix (e.g. `sk-proj-`, `xoxb-` for
  Slack), add the pattern to `bc_scrubber` in the same PR that introduces the
  integration.
- **Never pass raw secrets as tool arguments.** If a tool needs an API key (e.g.
  `bc_tool_curl` calling an authenticated endpoint), the key should be injected
  from the environment inside the tool implementation — not passed as a literal
  string in the LLM tool call arguments. Tool arguments are logged and stored in
  history.

### Secret handling — logging
- **Observability events must not contain raw secret values.** `bc_obs:emit/2`
  calls that include `args` or `result` fields must use the scrubbed values.
  The `tool_call_start` event logs `args` — ensure args are scrubbed before
  emission or redact the args field in the obs event.
- **Avoid logging full HTTP request/response bodies** from provider calls
  (`bc_provider_openrouter`, `bc_provider_openai`). Log only metadata: status
  code, duration, model name, token count.

### Code review checklist
Before merging any PR, verify:
- [ ] No hardcoded secrets in `*.erl`, `*.config`, `*.hrl`, or docs
- [ ] `bc_scrubber:scrub/1` called on all new tool result paths
- [ ] New API integrations have corresponding `bc_scrubber` patterns if keys
      have a recognizable format
- [ ] `{env, "VAR"}` used for any new config secret values
- [ ] No new files that might contain real secrets (check with `git diff --name-only`)
- [ ] CLAUDE.md inline sections updated for any structural changes
      (File Layout, App Dependency Graph, Supervision Trees, Key Behaviours,
      HTTP Gateway Routes, Rebar3 Dependencies, Common Commands)
- [ ] `docs/` files updated for user-visible changes
      (new commands → `running.md`; new config → `configuration.md`;
      architecture change → `architecture.md`; build change → `building.md`)

## Testing Policy

Three tiers of automated tests:

| Tier | Framework | Speed | External deps | Command |
|------|-----------|-------|---------------|---------|
| 1 — Unit | EUnit | < 5s | None | `rebar3 eunit` |
| 2 — Integration | CT | < 30s | OTP apps only | `rebar3 ct --dir=apps/<app>/test --suite=<suite>` |
| 3 — Docker E2E | CT | 1–3 min | Docker + sandbox image | `rebar3 ct --dir=apps/beamclaw_sandbox/test --suite=bc_sandbox_docker_SUITE` |

CT suites:

| Suite | App | Tier | Tests |
|---|---|---|---|
| `bc_agentic_loop_SUITE` | `beamclaw_core` | 2 | Session + loop + provider round-trip |
| `bc_http_integration_SUITE` | `beamclaw_gateway` | 2 | Cowboy HTTP handlers end-to-end |
| `bc_sandbox_docker_SUITE` | `beamclaw_sandbox` | 3 | Docker container lifecycle, script execution, bridge |
| `bc_scheduler_SUITE` | `beamclaw_scheduler` | 2 | Scheduler timer fire, delivery, heartbeat, tool actions |
| `bc_context_integration_SUITE` | `beamclaw_gateway` | 2 | /context command dispatch (TUI + Telegram) |
| `bc_a2a_http_integration_SUITE` | `beamclaw_a2a` | 2 | A2A Agent Card + JSON-RPC endpoints |

**When to run**:
- Before every commit: `rebar3 eunit`
- After gateway/core changes: `rebar3 eunit` + CT integration suites
- After Docker/sandbox changes or before release: all tests including Docker E2E

| Change type | Required tests |
|---|---|
| Pure-function module | EUnit |
| Multi-module OTP interaction (session + loop + tools) | CT integration suite |
| HTTP/WebSocket handler | CT integration suite |
| Scheduler/heartbeat/cron changes | CT `bc_scheduler_SUITE` |
| Docker/sandbox/external process | CT Docker E2E suite |

## Technology Stack

- **Language/Runtime**: Erlang/OTP 28
- **Build Tool**: rebar3
- **Project Structure**: Umbrella project — ten OTP apps under `apps/`
- **HTTP server**: Cowboy 2.x
- **HTTP client**: Hackney
- **JSON**: jsx
- **Metrics**: prometheus.erl

## Common Commands

```bash
rebar3 compile          # compile all apps
rebar3 eunit            # run unit tests (< 5s)
rebar3 eunit --module=<mod>
rebar3 ct --dir=apps/<app>/test --suite=<suite>  # run CT integration/E2E suite
rebar3 shell            # start REPL with all apps
rebar3 clean
rebar3 release
rebar3 escriptize       # build beamclaw CLI → _build/default/bin/beamclaw
rebar3 dialyzer
rebar3 lint

# Agent management (via CLI escript)
beamclaw agent create NAME   # create workspace
beamclaw agent list          # list agents
beamclaw agent show NAME     # show bootstrap files
beamclaw agent delete NAME   # delete workspace
beamclaw agent rehatch NAME  # factory reset: restore defaults
beamclaw tui --agent NAME    # TUI with specific agent

# Skill management (via CLI escript)
beamclaw skills list         # list discovered skills
beamclaw skills status       # detailed requirements check
beamclaw skills show NAME    # show skill content
beamclaw skills install NAME # install skill dependencies

# Sandbox management (via CLI escript)
beamclaw sandbox status      # Docker availability, image status, config
beamclaw sandbox list        # active sandbox containers
beamclaw sandbox kill ID     # force-kill a sandbox container
beamclaw sandbox build       # build sandbox Docker image

# Scheduler management (via CLI escript)
beamclaw scheduler list              # list active/paused scheduled jobs
beamclaw scheduler cancel JOB_ID     # cancel a scheduled job
beamclaw scheduler pause JOB_ID      # pause a scheduled job
beamclaw scheduler resume JOB_ID     # resume a paused job

# Pairing / access control (via CLI escript)
beamclaw pair                       # list pending + approved
beamclaw pair list                  # same as above
beamclaw pair telegram <CODE>       # approve a pending request (default agent)
beamclaw pair telegram <CODE> --agent mom  # approve with specific agent
beamclaw pair revoke telegram <ID>  # revoke user from allowlist
```

---

## Application Dependency Graph

Ten OTP apps under `apps/`. Dependency direction (arrow = "depends on"):

```
beamclaw_gateway → beamclaw_core → beamclaw_sandbox    → beamclaw_tools → beamclaw_obs
                                 → beamclaw_scheduler  → beamclaw_tools
                                                        → beamclaw_obs
                                 → beamclaw_mcp        → beamclaw_tools
                                 → beamclaw_memory     → beamclaw_obs
                                 → beamclaw_tools
                                 → beamclaw_obs
                 → beamclaw_a2a  → beamclaw_core
                                 → beamclaw_obs
                 → beamclaw_obs
```

| App | Role | Sibling deps |
|---|---|---|
| `beamclaw_obs` | Fire-and-forget telemetry | none |
| `beamclaw_memory` | Memory behaviour + backends | obs |
| `beamclaw_tools` | Built-in tools + tool registry | obs |
| `beamclaw_sandbox` | Docker sandboxed code execution, PII tokenization, tool policy | obs, tools |
| `beamclaw_scheduler` | Scheduled tasks, heartbeat, cron-like jobs | obs, tools, core |
| `beamclaw_mcp` | MCP server connections (stdio/HTTP), tool discovery | obs, tools |
| `beamclaw_core` | Sessions, agentic loop, LLM providers, approval, compaction | obs, memory, tools, mcp, sandbox |
| `beamclaw_a2a` | A2A (Agent2Agent) protocol: task manager, JSON-RPC server, agent card | core, obs |
| `beamclaw_gateway` | Channels (Telegram, TUI), HTTP gateway, rate limiter | core, a2a, obs |
| `beamclaw_cli` | CLI escript (`beamclaw` binary); not a daemon OTP app | all (bundled via `rebar3 escriptize`) |

**Rule**: never introduce a dependency cycle. `beamclaw_obs` must have zero sibling deps.

---

## Supervision Trees

### `beamclaw_core` (the brain)

```
beamclaw_core_sup  (one_for_one)
  ├── bc_session_registry     (gen_server, named — ETS: session_id → pid)
  ├── bc_session_cleaner      (gen_server, permanent — periodic expired session cleanup)
  ├── bc_session_maintenance  (gen_server, permanent — idle compaction, nightly flush, pre-expiry)
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
| `a2a_task_created` | `task_id`, `session_id` |
| `a2a_task_updated` | `task_id`, `state` |
| `a2a_request` | `method`, `path`, `rpc_method` |

Usage: `bc_obs:emit(tool_call_start, #{tool_name => Name, args => Args, session_id => SId})`.

---

## Configuration (`config/sys.config` + `config/vm.args`)

`bc_config:get(App, Key)` resolves `{env, "VAR"}` tuples at runtime via `os:getenv/1`.

`bc_config:canonical_user_id/0` returns the `BEAMCLAW_USER` env var as a binary,
or `undefined` if not set. When set, all channels use this value as-is (no prefix)
as the user_id, enabling cross-channel session sharing for single-user deployments.

```erlang
{kernel, [
    {logger_level, info},
    {logger, [
        {handler, default, logger_std_h, #{level => info, ...}},
        {handler, file, logger_std_h, #{
            level => debug,
            config => #{file => "/tmp/beamclaw_daemon.log",
                        max_no_bytes => 5242880, max_no_files => 3}, ...}}
    ]}
]},
{beamclaw_core, [
    {default_provider, openrouter},
    {providers, [
        {openrouter, #{api_key => {env, "OPENROUTER_API_KEY"},
                       base_url => "https://openrouter.ai/api/v1",
                       model    => "anthropic/claude-sonnet-4-5"}},
        {openai, #{api_key  => {env, "OPENAI_API_KEY"},
                   base_url => "https://api.openai.com/v1",
                   model    => "gpt-4o"}}
    ]},
    {agentic_loop, #{max_tool_iterations      => 10,
                     compaction_threshold_pct => 80,
                     compaction_target_pct    => 40,
                     compaction_provider      => openrouter,
                     compaction_model         => "moonshotai/kimi-k2.5",
                     stream_chunk_size        => 80,
                     memory_flush             => true,
                     auto_context             => false,
                     auto_context_limit       => 3}},
    {autonomy_level, supervised},
    {session_ttl_seconds, 3600},
    {default_agent, <<"default">>},
    {session_persistence, true},
    {session_sharing, shared},
    {session_cleanup_interval_ms, 300000},
    {maintenance, #{
        enabled                       => false,   %% opt-in proactive maintenance
        scan_interval_ms              => 300000,   %% 5 min scan interval
        idle_compaction_minutes       => 15,       %% min idle before compaction
        idle_compaction_threshold_pct => 20,       %% trigger: >20% of window
        idle_compaction_target_pct    => 10,       %% compact to 10% of window
        quiet_hours                   => {2, 4},   %% UTC hour range for nightly
        nightly_min_messages          => 10,       %% min history for nightly
        pre_expiry_minutes            => 10        %% flush window before TTL
    }},
    {skills, #{}}
]},
{beamclaw_mcp, [
    {servers, []}
]},
{beamclaw_gateway, [
    {http, #{port => 18800}},
    {channels, [
        {telegram, #{token => {env, "TELEGRAM_BOT_TOKEN"}, mode => long_poll,
                     %% Webhook mode settings (only used when mode => webhook):
                     %% TELEGRAM_WEBHOOK_URL and TELEGRAM_WEBHOOK_SECRET are
                     %% read directly from env vars at runtime (not via config)
                     %% to avoid crashes when unset in long_poll mode.
                     dm_policy => pairing, allow_from => [],
                     photo => #{enabled => true,
                                max_size_bytes => 5242880},   %% 5 MB
                     voice => #{enabled => true,
                                max_duration_seconds => 120,
                                stt_base_url => "https://api.groq.com/openai/v1",
                                stt_api_key => {env, "GROQ_API_KEY"},
                                stt_model => "whisper-large-v3-turbo"}}},
        {tui,      #{enabled => true}}
    ]}
]},
{beamclaw_sandbox, [
    {enabled, false},                          %% opt-in; requires Docker
                                               %% In Docker: mount host socket
                                               %% -v /var/run/docker.sock:/var/run/docker.sock
    {docker_image, "beamclaw-sandbox:latest"},
    {scope, session},                          %% session | agent | shared
    {timeout_seconds, 60},
    {reaper_interval_ms, 60000},               %% orphan container sweep interval
    {memory_limit, "512m"},
    {cpu_limit, "1.0"},
    {network, none},                           %% none | bridge | host
    {workspace_mount, ro},                     %% none | ro | rw
    {max_output_bytes, 1048576},               %% 1 MB
    {bridge_socket_dir, "/tmp/beamclaw-bridges"},
    {pii, #{enabled => true, patterns => [],
            tokenize_scripts => true, tokenize_results => true}},
    {policy, #{default_action => allow,
               rules => [{allow, <<"read_file">>}, {allow, <<"curl">>},
                          {deny, <<"bash">>}, {deny, <<"terminal">>}]}},
    {env_allowlist, [<<"PATH">>, <<"HOME">>, <<"LANG">>, <<"TERM">>]},
    {env_blocklist, [<<"OPENROUTER_API_KEY">>, <<"OPENAI_API_KEY">>,
                     <<"TELEGRAM_BOT_TOKEN">>, <<"AWS_SECRET_ACCESS_KEY">>,
                     <<"GROQ_API_KEY">>, <<"TELEGRAM_WEBHOOK_SECRET">>]}
]},
{beamclaw_tools, [
    {web_search, #{api_key => {env, "BRAVE_API_KEY"},
                   max_results => 10}}
]},
{beamclaw_scheduler, [
    {enabled, false},                      %% opt-in; creates bc_tool_scheduler when true
    {max_jobs_per_agent, 50},
    {default_autonomy, supervised},
    {max_errors, 3},                       %% consecutive failures → auto-pause
    {heartbeat, #{
        default_interval_ms => 1800000,    %% 30 min default heartbeat interval
        suppress_ok => true,               %% suppress HEARTBEAT_OK output
        active_hours => {8, 22}            %% UTC hour range (skip outside)
    }}
]},
{beamclaw_obs, []},
{beamclaw_memory, [
    {backend, ets},
    {embedding, #{enabled => true, model => "text-embedding-3-small",
                  base_url => "https://api.openai.com/v1",
                  api_key => {env, "BEAMCLAW_EMBEDDING_API_KEY"},
                  dimensions => 1536}},
    {search, #{vector_weight => 0.7, bm25_weight => 0.3,
               min_score => 0.35, default_limit => 6,
               chunk_size => 400, chunk_overlap => 80,
               workspace_files => [<<"MEMORY.md">>, ...],
               daily_log_lookback => 7}}
]}
```

Key `vm.args` flags:

| Flag | Value | Description |
|---|---|---|
| `-sname` | `beamclaw` | Local node name |
| `-mnesia dir` | `'"/home/beamclaw/.beamclaw/mnesia"'` | Mnesia data directory on the persistent volume. Required for Docker session persistence across container rebuilds. |

---

## HTTP Gateway Routes (Cowboy)

```
GET  /health                 → bc_http_health_h
GET  /metrics                → bc_http_metrics_h        (Prometheus scrape)
POST /v1/chat/completions    → bc_http_completions_h    (OpenAI-compatible, SSE streaming)
GET  /ws                     → bc_ws_h                  (WebSocket)
POST /webhook/telegram       → bc_webhook_telegram_h
GET  /.well-known/agent.json → bc_a2a_http_h            (A2A Agent Card discovery)
POST /a2a                    → bc_a2a_http_h            (A2A JSON-RPC 2.0)
```

Rate limiting (`bc_rate_limiter`): sliding-window per client IP, ETS-backed, pruned every 60 s. Checked in every handler before dispatch.

Channel message idempotency: `bc_channel_telegram` and `bc_ws_h` de-duplicate by message ID before dispatching to session.

---

## Rebar3 Dependencies

```erlang
{deps, [
    {cowboy,  "2.12.0"},   %% HTTP server + WebSocket
    {hackney, "1.20.1"},   %% HTTP client (LLM APIs, MCP HTTP)
    {jsx,     "3.1.0"}     %% JSON encode/decode
]}.
```

---

## MCP Protocol Details (`bc_mcp_server`)

- Transport: stdio (default) or HTTP (SSE)
- Protocol: JSON-RPC 2.0
- Handshake: `initialize` → `initialized` notification → `tools/list`
- Tool invocation: `tools/call` with `{name, arguments}` params
- Error handling: exponential backoff on port crash; max 5 restarts in 30 s
- `bc_mcp_registry` maps `tool_name → {server_pid, server_name}` for routing

---

## Naming Conventions

- Module prefix: `bc_` for all internal modules
- Behaviour modules export only `-callback` specs and helper functions
- Records in `bc_types.hrl`; types defined alongside records
- Config keys: atoms (not binaries)
- Session IDs: binaries (UUID v4)
- All public gen_server APIs go through named functions, never call `gen_server:call` directly from outside the module

---

## File Layout

```
beamclaw/
  rebar.config
  config/
    sys.config
    vm.args
  apps/
    beamclaw_obs/src/
      beamclaw_obs.app.src
      beamclaw_obs_app.erl
      beamclaw_obs_sup.erl
      bc_obs.erl              %% behaviour + emit/2 API
      bc_obs_manager.erl      %% fan-out gen_server
      bc_obs_log.erl          %% log backend
    beamclaw_memory/
      include/
        bc_memory_mnesia.hrl  %% Mnesia record (with embedding field)
      src/
        beamclaw_memory.app.src
        beamclaw_memory_app.erl
        beamclaw_memory_sup.erl
        bc_memory.erl           %% behaviour (incl. optional search/4)
        bc_memory_ets.erl
        bc_memory_mnesia.erl
        bc_bm25.erl             %% BM25 keyword search (pure function)
        bc_vector.erl           %% cosine similarity, dot product (pure function)
        bc_chunker.erl          %% text chunking for embeddings (pure function)
        bc_hybrid.erl           %% BM25 + vector score merging (pure function)
        bc_embedding.erl        %% OpenAI-compatible embedding API client
        bc_embedding_cache.erl  %% gen_server, ETS embedding cache (24h TTL)
    beamclaw_tools/src/
      beamclaw_tools.app.src
      beamclaw_tools_app.erl
      beamclaw_tools_sup.erl
      bc_tool.erl             %% behaviour
      bc_tool_registry.erl    %% gen_server, named ETS table
      bc_tool_terminal.erl
      bc_tool_bash.erl
      bc_tool_curl.erl
      bc_tool_jq.erl
      bc_tool_read_file.erl
      bc_tool_write_file.erl
      bc_tool_delete_file.erl
      bc_tool_web_search.erl        %% Brave Search API web search
      bc_tool_workspace_memory.erl  %% agent MEMORY.md + daily logs + bootstrap files + search
      bc_workspace_path.erl         %% pure path resolution + memory dir (avoids dep cycle)
    beamclaw_sandbox/
      src/
        beamclaw_sandbox.app.src
        beamclaw_sandbox_app.erl
        beamclaw_sandbox_sup.erl
        bc_sandbox_registry.erl    %% ETS: {session_id, scope} → pid; immediate cleanup on DOWN
        bc_sandbox_reaper.erl      %% gen_server: periodic orphan container cleanup
        bc_sandbox_sup.erl         %% simple_one_for_one for bc_sandbox
        bc_sandbox.erl             %% per-sandbox Docker lifecycle gen_server
        bc_sandbox_docker.erl      %% pure: Docker command arg building
        bc_sandbox_bridge.erl      %% JSON-RPC 2.0 bridge encode/decode/dispatch
        bc_sandbox_discovery.erl   %% generate /tools/ filesystem for container
        bc_tool_exec.erl           %% bc_tool behaviour: sandboxed code execution
        bc_pii_tokenizer.erl       %% gen_server: bidirectional PII masking
        bc_sandbox_policy.erl      %% pure: tool access allow/deny rules
        bc_sandbox_env.erl         %% pure: env var allowlist/blocklist filtering
        bc_sandbox_skills.erl      %% pure: save/load sandbox scripts as SKILL.md
      priv/
        docker/
          Dockerfile.sandbox       %% python:3.12-alpine sandbox image
          bridge/
            __init__.py
            beamclaw_bridge.py     %% Python bridge: search_tools, call_tool
    beamclaw_scheduler/
      include/
        bc_sched_job.hrl          %% Mnesia record for scheduled jobs
      src/
        beamclaw_scheduler.app.src
        beamclaw_scheduler_app.erl
        beamclaw_scheduler_sup.erl
        bc_sched_store.erl        %% Mnesia persistence (init_table, CRUD)
        bc_sched_runner.erl       %% Timer management gen_server
        bc_sched_executor.erl     %% Session dispatch + delivery gen_server
        bc_sched_random.erl       %% Random slot algorithm (pure)
        bc_sched_interval.erl     %% Interval string parsing (pure)
        bc_tool_scheduler.erl     %% bc_tool behaviour: scheduled tasks + heartbeat
    beamclaw_mcp/src/
      beamclaw_mcp.app.src
      beamclaw_mcp_app.erl
      beamclaw_mcp_sup.erl
      bc_mcp_registry.erl
      bc_mcp_servers_sup.erl
      bc_mcp_server.erl
    beamclaw_core/
      include/
        bc_types.hrl
        bc_session_store.hrl  %% Mnesia record for session persistence
      src/
        beamclaw_core.app.src
        beamclaw_core_app.erl
        beamclaw_core_sup.erl
        bc_provider.erl       %% behaviour
        bc_provider_openrouter.erl
        bc_provider_openai.erl
        bc_channel.erl        %% behaviour
        bc_session_registry.erl   %% session_id → pid + derive_session_id/2,3
        bc_session_store.erl      %% Mnesia-backed session persistence
        bc_session_cleaner.erl    %% periodic expired session cleanup
        bc_sessions_sup.erl
        bc_session_sup.erl
        bc_session.erl
        bc_loop.erl           %% gen_statem agentic loop
        bc_approval.erl
        bc_compactor.erl
        bc_memory_flush.erl       %% extracted pre-compaction memory flush
        bc_session_maintenance.erl  %% periodic idle/nightly/pre-expiry maintenance
        bc_scrubber.erl
        bc_thinking.erl       %% strip LLM thinking/reasoning tags
        bc_tool_parser.erl
        bc_config.erl
        bc_context.erl              %% context window usage display (pure function)
        bc_workspace_templates.erl  %% eight default bootstrap file templates
        bc_workspace.erl            %% agent workspace filesystem ops
        bc_system_prompt.erl        %% assemble bootstrap files into system messages
        bc_skill_parser.erl         %% parse SKILLS.md front-matter
        bc_skill_discovery.erl      %% discover skills from bundled + workspace
        bc_skill_eligibility.erl    %% check skill requirements (tools, MCP, env)
        bc_skill_installer.erl      %% install skill dependencies
        bc_pairing.erl              %% channel access control via pairing codes
      priv/
        skills/                     %% bundled skills
          example-skill/
          finnhub/
          nano-banana-pro/
            scripts/
              generate_image.py     %% Gemini image generation script
    beamclaw_a2a/
      include/
        bc_a2a_types.hrl          %% A2A records: a2a_task, a2a_message, a2a_status, a2a_artifact
      src/
        beamclaw_a2a.app.src
        beamclaw_a2a_app.erl
        beamclaw_a2a_sup.erl
        bc_a2a_task.erl           %% task state machine + serialization
        bc_a2a_task_manager.erl   %% gen_server: ETS-backed task store + session dispatch
        bc_a2a_server.erl         %% JSON-RPC 2.0 method dispatch (stateless)
        bc_a2a_http_h.erl         %% Cowboy handler: agent card + /a2a endpoint
        bc_a2a_agent_card.erl     %% agent card builder + serialization
        bc_channel_a2a.erl        %% stateless response routing for A2A sessions
    beamclaw_gateway/src/
      beamclaw_gateway.app.src
      beamclaw_gateway_app.erl
      beamclaw_gateway_sup.erl
      bc_rate_limiter.erl
      bc_gateway_http_sup.erl
      bc_gateway_cowboy.erl
      bc_gateway_channels_sup.erl
      bc_channel_telegram.erl
      bc_telegram_format.erl    %% pure-function markdown→Telegram HTML converter
      bc_telegram_photo.erl     %% photo extraction, download, base64 encoding
      bc_telegram_audio.erl    %% voice/audio extraction and download
      bc_stt.erl               %% speech-to-text client (Groq Whisper / OpenAI-compatible)
      bc_channel_tui.erl
      bc_http_health_h.erl
      bc_http_metrics_h.erl
      bc_http_completions_h.erl
      bc_ws_h.erl
      bc_webhook_telegram_h.erl
    beamclaw_cli/src/
      beamclaw_cli.app.src
      beamclaw_cli.erl        %% escript main; 30 commands (tui/start/stop/restart/remote_console/agent create/list/show/delete/rehatch/skills list/status/show/install/scheduler list/cancel/pause/resume/pair/pair list/pair approve/pair revoke/sandbox status/list/kill/build/doctor/status/version/help)
```
