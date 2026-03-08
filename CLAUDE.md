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
  change — update the relevant location(s) below **and** the corresponding
  file(s) under `docs/`. See the table below.
- **Progress Tracking**: If context window exceeds 80%, summarize the current sub-task into STATUS.md and prompt for a `/compact`.
- **Human-only files**: Do not read or reference `HUMAN_NOTES.md` — this file is for human engineers only.

| Change type | Update in CLAUDE.md | Update in `.claude/rules/` | Update in `docs/` |
|---|---|---|---|
| New/removed OTP app | App Dependency Graph | `file-layout.md` | `docs/architecture.md` |
| New/removed module | — | `file-layout.md` | — |
| Supervision tree change | — | `architecture.md` | `docs/architecture.md` |
| New/removed behaviour or callback | — | `architecture.md` | `docs/architecture.md` |
| New/removed CLI command | Common Commands | — | `docs/running.md` |
| New/removed config key | — | `configuration.md` | `docs/configuration.md`, `config/sys.docker.config` |
| New/removed HTTP route | HTTP Gateway Routes | — | `docs/running.md` |
| New rebar3 dependency | Rebar3 Dependencies | — | `docs/building.md` |
| Security pattern added | — | `architecture.md` | — |

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
  integration. See `.claude/rules/architecture.md` for the full regex list.
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
- [ ] `.claude/rules/` files updated for structural changes
      (file-layout.md, architecture.md, configuration.md)
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
| `bc_loop_edge_SUITE` | `beamclaw_core` | 2 | Stream timeout/error recovery, tool crash, max iterations, supervisor restart |
| `bc_http_integration_SUITE` | `beamclaw_gateway` | 2 | Cowboy HTTP handlers end-to-end |
| `bc_telegram_integration_SUITE` | `beamclaw_gateway` | 2 | Telegram mock API: bot commands, sendMessage format, typing, Unicode, webhook |
| `bc_cli_smoke_SUITE` | `beamclaw_gateway` | 2 | CLI escript: version, help, agent list, skills list, sandbox status |
| `bc_sandbox_docker_SUITE` | `beamclaw_sandbox` | 3 | Docker container lifecycle, script execution, bridge |
| `bc_docker_integration_SUITE` | `beamclaw_gateway` | 3 | Docker deployment: image build, health, CLI, UTF-8, workspace |
| `bc_scheduler_SUITE` | `beamclaw_scheduler` | 2 | Scheduler timer fire, delivery, heartbeat, tool actions |
| `bc_context_integration_SUITE` | `beamclaw_gateway` | 2 | /context command dispatch (TUI + Telegram) |
| `bc_a2a_http_integration_SUITE` | `beamclaw_a2a` | 2 | A2A Agent Card, JSON-RPC, Bearer auth |
| `bc_webhook_integration_SUITE` | `beamclaw_gateway` | 2 | Generic webhook: auth (header/body/query), open mode, rate limit, custom agent |

**When to run**:
- Before every commit: `rebar3 eunit`
- After gateway/core changes: `rebar3 eunit` + CT integration suites
- After Docker/sandbox changes or before release: all tests including Docker E2E

| Change type | Required tests |
|---|---|
| Pure-function module | EUnit |
| Multi-module OTP interaction (session + loop + tools) | CT `bc_agentic_loop_SUITE` + `bc_loop_edge_SUITE` |
| HTTP/WebSocket handler | CT `bc_http_integration_SUITE` |
| Telegram channel changes | CT `bc_telegram_integration_SUITE` |
| CLI escript changes | CT `bc_cli_smoke_SUITE` |
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

## HTTP Gateway Routes (Cowboy)

```
GET  /health                 → bc_http_health_h
GET  /metrics                → bc_http_metrics_h        (Prometheus scrape)
POST /v1/chat/completions    → bc_http_completions_h    (OpenAI-compatible, SSE streaming)
GET  /ws                     → bc_ws_h                  (WebSocket)
POST /webhook/telegram       → bc_webhook_telegram_h
POST /webhook/:source        → bc_webhook_h             (generic webhook ingestion)
GET  /.well-known/agent.json → bc_a2a_http_h            (A2A Agent Card discovery)
POST /a2a                    → bc_a2a_http_h            (A2A JSON-RPC 2.0, Bearer token auth)
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

## Naming Conventions

- Module prefix: `bc_` for all internal modules
- Behaviour modules export only `-callback` specs and helper functions
- Records in `bc_types.hrl`; types defined alongside records
- Config keys: atoms (not binaries)
- Session IDs: binaries (UUID v4)
- All public gen_server APIs go through named functions, never call `gen_server:call` directly from outside the module

---

## Detailed Reference

| File | Contents |
|---|---|
| `.claude/rules/architecture.md` | Supervision trees, behaviour callbacks, data types, agentic loop, tool parsing, compaction, scrubber patterns, approval workflow, observability events, MCP protocol |
| `.claude/rules/configuration.md` | Full `sys.config` + `vm.args` configuration reference |
| `.claude/rules/file-layout.md` | Complete directory tree for all ten OTP apps |
