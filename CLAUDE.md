# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## State Management
- **Task List**: Always read and update @STATUS.md before and after every task.
- **Decision Log**: Consult @DECISIONS.md before suggesting architectural changes.
- **Documentation Sync**: When adding or removing a module, app, CLI command,
  config option, HTTP route, or behaviour — or when making an architectural
  change — update the relevant inline section(s) of this file **and** the
  corresponding file(s) under `docs/`. See the table below.
- **Progress Tracking**: If context window exceeds 80%, summarize the current sub-task into STATUS.md and prompt for a `/compact`.

| Change type | Update in CLAUDE.md | Update in docs/ |
|---|---|---|
| New/removed OTP app | Application Dependency Graph, File Layout | `docs/architecture.md` |
| New/removed module | File Layout | — |
| Supervision tree change | Supervision Trees | `docs/architecture.md` |
| New/removed behaviour or callback | Key Behaviours | `docs/architecture.md` |
| New/removed CLI command | Common Commands | `docs/running.md` |
| New/removed config key | Configuration | `docs/configuration.md` |
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

## Technology Stack

- **Language/Runtime**: Erlang/OTP 28
- **Build Tool**: rebar3
- **Project Structure**: Umbrella project — seven OTP apps under `apps/`
- **HTTP server**: Cowboy 2.x
- **HTTP client**: Hackney
- **JSON**: jsx
- **Metrics**: prometheus.erl

## Common Commands

```bash
rebar3 compile          # compile all apps
rebar3 eunit            # run tests
rebar3 eunit --module=<mod>
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
beamclaw tui --agent NAME    # TUI with specific agent
```

---

## Application Dependency Graph

Seven OTP apps under `apps/`. Dependency direction (arrow = "depends on"):

```
beamclaw_gateway → beamclaw_core → beamclaw_mcp   → beamclaw_tools → beamclaw_obs
                                 → beamclaw_memory → beamclaw_obs
                                 → beamclaw_tools
                                 → beamclaw_obs
                 → beamclaw_obs
```

| App | Role | Sibling deps |
|---|---|---|
| `beamclaw_obs` | Fire-and-forget telemetry | none |
| `beamclaw_memory` | Memory behaviour + backends | obs |
| `beamclaw_tools` | Built-in tools + tool registry | obs |
| `beamclaw_mcp` | MCP server connections (stdio/HTTP), tool discovery | obs, tools |
| `beamclaw_core` | Sessions, agentic loop, LLM providers, approval, compaction | obs, memory, tools, mcp |
| `beamclaw_gateway` | Channels (Telegram, TUI), HTTP gateway, rate limiter | core, obs |
| `beamclaw_cli` | CLI escript (`beamclaw` binary); not a daemon OTP app | all (bundled via `rebar3 escriptize`) |

**Rule**: never introduce a dependency cycle. `beamclaw_obs` must have zero sibling deps.

---

## Supervision Trees

### `beamclaw_core` (the brain)

```
beamclaw_core_sup  (one_for_one)
  ├── bc_session_registry     (gen_server, named — ETS: session_id → pid)
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

### `beamclaw_obs`

```
beamclaw_obs_sup  (one_for_one)
  ├── bc_obs_manager     (gen_server — fan-out via pg process groups)
  └── bc_obs_log         (gen_server backend)
```

`bc_obs:emit/2` is a non-blocking cast. Observability **never** creates backpressure on the agentic loop.

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

Implementations: `bc_tool_terminal`, `bc_tool_bash`, `bc_tool_curl`, `bc_tool_jq`, `bc_tool_read_file`, `bc_tool_write_file`, `bc_tool_workspace_memory`.

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
```

Implementations: `bc_memory_ets`, `bc_memory_mnesia`.

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
    id, role, content, tool_calls = [], tool_call_id, name, ts = 0
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
    session_id, user_id, channel, content, raw, ts
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
- Enter `compacting` from `idle` when `length(History) > compaction_threshold`
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

1. Check: `length(History) > compaction_threshold` (default: 50)
2. Split: keep last `compaction_target` (default: 20) messages verbatim
3. Summarize older messages via LLM: system prompt instructs concise factual summary
4. Summary becomes a `system` role message: `"[Conversation summary]: <text>"`
5. Fallback on LLM failure: deterministic trim to last `compaction_target` messages
6. Emit `compaction_complete` obs event with `{before, After}` message counts

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

Usage: `bc_obs:emit(tool_call_start, #{tool_name => Name, args => Args, session_id => SId})`.

---

## Configuration (`config/sys.config`)

`bc_config:get(App, Key)` resolves `{env, "VAR"}` tuples at runtime via `os:getenv/1`.

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
    {agentic_loop, #{max_tool_iterations => 10,
                     compaction_threshold => 50,
                     compaction_target    => 20,
                     stream_chunk_size    => 80}},
    {autonomy_level, supervised},
    {session_ttl_seconds, 3600},
    {default_agent, <<"default">>}
]},
{beamclaw_mcp, [
    {servers, []}
]},
{beamclaw_gateway, [
    {http, #{port => 8080}},
    {channels, [
        {telegram, #{token => {env, "TELEGRAM_BOT_TOKEN"}, mode => long_poll}},
        {tui,      #{enabled => true}}
    ]}
]},
{beamclaw_obs, []},
{beamclaw_memory, [
    {backend, ets}
]}
```

---

## HTTP Gateway Routes (Cowboy)

```
GET  /health                 → bc_http_health_h
GET  /metrics                → bc_http_metrics_h        (Prometheus scrape)
POST /v1/chat/completions    → bc_http_completions_h    (OpenAI-compatible, SSE streaming)
GET  /ws                     → bc_ws_h                  (WebSocket)
POST /webhook/telegram       → bc_webhook_telegram_h
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
    beamclaw_memory/src/
      beamclaw_memory.app.src
      beamclaw_memory_app.erl
      beamclaw_memory_sup.erl
      bc_memory.erl           %% behaviour
      bc_memory_ets.erl
      bc_memory_mnesia.erl
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
      bc_tool_workspace_memory.erl  %% agent MEMORY.md read/append/replace
      bc_workspace_path.erl         %% pure path resolution (avoids dep cycle)
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
      src/
        beamclaw_core.app.src
        beamclaw_core_app.erl
        beamclaw_core_sup.erl
        bc_provider.erl       %% behaviour
        bc_provider_openrouter.erl
        bc_provider_openai.erl
        bc_channel.erl        %% behaviour
        bc_session_registry.erl
        bc_sessions_sup.erl
        bc_session_sup.erl
        bc_session.erl
        bc_loop.erl           %% gen_statem agentic loop
        bc_approval.erl
        bc_compactor.erl
        bc_scrubber.erl
        bc_tool_parser.erl
        bc_config.erl
        bc_workspace_templates.erl  %% default bootstrap file content
        bc_workspace.erl            %% agent workspace filesystem ops
        bc_system_prompt.erl        %% assemble bootstrap files into system messages
    beamclaw_gateway/src/
      beamclaw_gateway.app.src
      beamclaw_gateway_app.erl
      beamclaw_gateway_sup.erl
      bc_rate_limiter.erl
      bc_gateway_http_sup.erl
      bc_gateway_cowboy.erl
      bc_gateway_channels_sup.erl
      bc_channel_telegram.erl
      bc_channel_tui.erl
      bc_http_health_h.erl
      bc_http_metrics_h.erl
      bc_http_completions_h.erl
      bc_ws_h.erl
      bc_webhook_telegram_h.erl
    beamclaw_cli/src/
      beamclaw_cli.app.src
      beamclaw_cli.erl        %% escript main; 14 commands (tui/start/stop/restart/remote_console/agent create/list/show/delete/doctor/status/version/help)
```
