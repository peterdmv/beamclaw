# BeamClaw — Project Status

## Current Phase: Implementation

Scaffolding is complete. All six OTP apps compile clean with zero warnings.
Active work is building out the core modules from stub to full implementation.

---

## Legend

| Symbol | Meaning |
|--------|---------|
| ✅ | Complete |
| 🚧 | In progress |
| ⬜ | Pending |
| ❌ | Blocked |

---

## Milestones

### M0 — Project Scaffolding ✅
All six OTP apps created, supervision trees defined, behaviours declared,
`rebar.config` with all deps. Compiles clean on OTP 28.

### M1 — Observability Layer ✅
`beamclaw_obs` is complete. Prometheus dropped (see ADR-009); OTP logger used instead.

| Module | Status | Notes |
|--------|--------|-------|
| `bc_obs` | ✅ | behaviour + `emit/2` API |
| `bc_obs_manager` | ✅ | fan-out via `pg` process groups |
| `bc_obs_log` | ✅ | OTP logger backend |

### M2 — Memory Layer ✅

| Module | Status | Notes |
|--------|--------|-------|
| `bc_memory` | ✅ | behaviour |
| `bc_memory_ets` | ✅ | in-process ETS backend (default) |
| `bc_memory_mnesia` | ✅ | Mnesia backend (disc_copies / ram_copies fallback); replaces SQLite stub (ADR-010) |

### M3 — Tool Registry ✅

| Module | Status | Notes |
|--------|--------|-------|
| `bc_tool` | ✅ | behaviour |
| `bc_tool_registry` | ✅ | named ETS gen_server |
| `bc_tool_terminal` | ✅ | built-in tool |
| `bc_tool_bash` | ✅ | built-in tool |
| `bc_tool_curl` | ✅ | built-in tool (inets dep added, ADR-011) |
| `bc_tool_jq` | ✅ | built-in tool |
| `bc_tool_read_file` | ✅ | new: read-only file read, no approval |
| `bc_tool_write_file` | ✅ | new: file write, requires approval |

### M4 — MCP Client ✅

| Module | Status | Notes |
|--------|--------|-------|
| `bc_mcp_server` | ✅ | stdio transport, JSON-RPC 2.0, state threading fixed |
| `bc_mcp_registry` | ✅ | tool-name → server routing, PID monitors for auto-cleanup |
| `bc_mcp_servers_sup` | ✅ | simple_one_for_one, servers started from app callback |

### M5 — Core Agentic Loop ✅

| Module | Status | Notes |
|--------|--------|-------|
| `bc_config` | ✅ | `{env, "VAR"}` resolution |
| `bc_scrubber` | ✅ | credential redaction |
| `bc_tool_parser` | ✅ | OpenAI → XML → Markdown → empty fallback chain |
| `bc_compactor` | ✅ | context compaction via LLM; writes back via `bc_session:set_history/2` |
| `bc_approval` | ✅ | approval gen_server; channel wiring deferred to M6 |
| `bc_session_registry` | ✅ | named ETS: session_id → pid, with monitors |
| `bc_session` | ✅ | permanent gen_server; loop_busy flag; queue drains on set_loop_pid |
| `bc_loop` | ✅ | gen_statem; looks up session from registry; provider state threaded |
| `bc_provider` | ✅ | behaviour |
| `bc_provider_openrouter` | ✅ | OpenRouter provider (functional API) |
| `bc_provider_openai` | ✅ | OpenAI provider (delegates to openrouter) |
| `bc_channel` | ✅ | behaviour |

### M6 — Gateway ✅

| Module | Status | Notes |
|--------|--------|-------|
| `bc_rate_limiter` | ✅ | sliding-window ETS, 60 s prune |
| `bc_gateway_cowboy` | ✅ | Cowboy listener wrapper |
| `bc_http_health_h` | ✅ | `GET /health` |
| `bc_http_metrics_h` | ✅ | `GET /metrics` (stub; Prometheus deferred, ADR-009) |
| `bc_http_completions_h` | ✅ | SSE streaming + sync; `reply_pid` routing |
| `bc_ws_h` | ✅ | session dispatch; `reply_pid` routing |
| `bc_webhook_telegram_h` | ✅ | `POST /webhook/telegram` |
| `bc_channel_telegram` | ✅ | long-poll / webhook; `send_response/2`; race fix |
| `bc_channel_tui` | ✅ | stdin/stdout; `send_response/2`; race fix |

### M7 — Testing & Hardening ✅

| Task | Status | Notes |
|------|--------|-------|
| EUnit tests for `bc_scrubber` | ✅ | 21 tests; all patterns + scrub_message/scrub_result |
| EUnit tests for `bc_tool_parser` | ✅ | 13 tests; all 4 parse paths + security no-free-text rule |
| EUnit tests for `bc_compactor` | ✅ | 3 tests; no-op paths (LLM path needs integration test) |
| EUnit tests for `bc_approval` | ✅ | 5 tests; full/read_only/supervised/allowlist/unknown |
| EUnit tests for `bc_rate_limiter` | ✅ | 4 tests; allow/within-limit/exceed/client-isolation |
| Dialyzer clean | ✅ | 25 → 0 warnings; 3 targeted -dialyzer suppressions for runtime patterns |
| `rebar3 lint` clean | ✅ | elvis.config; 6 rules disabled for intentional patterns; code fixes |
| End-to-end smoke test (TUI channel) | ✅ | 1 test; bc_provider_smoke_mock + bc_smoke_tests; also fixed bc_loop callback_mode bug |

### M8 — Documentation + Docker Release ✅

| Task | Status | Notes |
|------|--------|-------|
| `rebar3 release` config (`relx` + `docker` profile) | ✅ | Added to `rebar.config`; `{include_erts, true}` bundles ERTS |
| `vm.args` production tuning | ✅ | `-sname`, `+sbwt none`, `+MBas aobf`, comments on every flag |
| `sys.docker.config` | ✅ | TUI disabled; identical otherwise to `sys.config` |
| `Dockerfile` (multi-stage) | ✅ | `erlang:28-alpine` builder → `alpine:3.21` runtime; non-root user |
| `.dockerignore` | ✅ | Excludes `_build/`, beams, secrets, `.git/` |
| `.gitignore` security fix | ✅ | Added `.env`, `*.env`, `*.secret`, `priv/secrets/` |
| `README.md` rewrite | ✅ | Pitch, Docker quick-start, source quick-start, docs links |
| `docs/building.md` | ✅ | Prerequisites, compile, test, dialyzer, release, Docker, CLI escript |
| `docs/running.md` | ✅ | beamclaw CLI (all 9 commands), rebar3 shell, OTP release, Docker, channels, MCP |
| `docs/configuration.md` | ✅ | All env vars, sys.config keys, MCP server setup |
| `docs/architecture.md` | ✅ | Seven-app graph, supervision trees, loop state machine, behaviours |

### Post-M8 — Contributor Docs

| Task | Status | Notes |
|------|--------|-------|
| `CONTRIBUTING.md` | ✅ | Welcome, dev workflow, coding standards, security rules, AI-assisted contribution guidelines |

### M9 — `beamclaw` CLI (escript) ✅

| Task | Status | Notes |
|------|--------|-------|
| `apps/beamclaw_cli/src/beamclaw_cli.app.src` | ✅ | Minimal app descriptor; no callback/supervisor |
| `apps/beamclaw_cli/src/beamclaw.erl` | ✅ | escript main; all 9 commands implemented |
| `rebar3 escriptize` config in `rebar.config` | ✅ | `escript_main_app`, `escript_name`, `escript_incl_apps` |
| ADR-012 (fat escript) | ✅ | Documents CLI approach and rationale |
| ADR-013 (daemon via Erlang distribution) | ✅ | Documents IPC pattern (nodetool) |
| `beamclaw tui` | ✅ | Embedded config; exclusive stdin; monitor TUI pid; blocks until EOF |
| `beamclaw start` | ✅ | Spawns detached erl daemon; polls net_adm:ping for confirmation |
| `beamclaw stop` | ✅ | RPC `init:stop/0`; polls until node gone |
| `beamclaw restart` | ✅ | stop + start |
| `beamclaw remote_console` | ✅ | Prints `erl -remsh beamclaw@localhost` command |
| `beamclaw doctor` | ✅ | 5 local checks + optional OpenRouter network check |
| `beamclaw status` | ✅ | HTTP GET /health via httpc |
| `beamclaw version` | ✅ | Prints version string |
| `beamclaw help` | ✅ | Usage summary |

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-02-21 (Post-M9: documentation synced with all milestones; CLAUDE.md, docs/architecture.md, docs/building.md, docs/running.md updated)
