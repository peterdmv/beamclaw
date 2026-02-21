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

### M4 — MCP Client ⬜

| Module | Status | Notes |
|--------|--------|-------|
| `bc_mcp_server` | ⬜ | stdio/HTTP transport, JSON-RPC 2.0 |
| `bc_mcp_registry` | ⬜ | tool-name → server routing |
| `bc_mcp_servers_sup` | ⬜ | dynamic one_for_one supervisor |

### M5 — Core Agentic Loop ⬜

| Module | Status | Notes |
|--------|--------|-------|
| `bc_config` | ⬜ | `{env, "VAR"}` resolution |
| `bc_scrubber` | ⬜ | credential redaction |
| `bc_tool_parser` | ⬜ | OpenAI → XML → Markdown → empty fallback chain |
| `bc_compactor` | ⬜ | context compaction via LLM |
| `bc_approval` | ⬜ | human-in-the-loop approval gen_server |
| `bc_session_registry` | ⬜ | named ETS: session_id → pid |
| `bc_session` | ⬜ | permanent gen_server, session lane |
| `bc_loop` | ⬜ | gen_statem agentic loop |
| `bc_provider` | ⬜ | behaviour |
| `bc_provider_openrouter` | ⬜ | OpenRouter provider |
| `bc_provider_openai` | ⬜ | OpenAI provider |
| `bc_channel` | ⬜ | behaviour |

### M6 — Gateway ⬜

| Module | Status | Notes |
|--------|--------|-------|
| `bc_rate_limiter` | ⬜ | sliding-window ETS, 60 s prune |
| `bc_gateway_cowboy` | ⬜ | Cowboy listener wrapper |
| `bc_http_health_h` | ⬜ | `GET /health` |
| `bc_http_metrics_h` | ⬜ | `GET /metrics` |
| `bc_http_completions_h` | ⬜ | `POST /v1/chat/completions` (SSE) |
| `bc_ws_h` | ⬜ | `GET /ws` WebSocket handler |
| `bc_webhook_telegram_h` | ⬜ | `POST /webhook/telegram` |
| `bc_channel_telegram` | ⬜ | Telegram long-poll / webhook channel |
| `bc_channel_tui` | ⬜ | Terminal UI channel |

### M7 — Testing & Hardening ⬜

| Task | Status |
|------|--------|
| EUnit tests for `bc_scrubber` | ⬜ |
| EUnit tests for `bc_tool_parser` | ⬜ |
| EUnit tests for `bc_compactor` | ⬜ |
| EUnit tests for `bc_approval` | ⬜ |
| EUnit tests for `bc_rate_limiter` | ⬜ |
| Dialyzer clean | ⬜ |
| `rebar3 lint` clean | ⬜ |
| End-to-end smoke test (TUI channel) | ⬜ |

### M8 — Release ⬜

| Task | Status |
|------|--------|
| `rebar3 release` config | ⬜ |
| `vm.args` tuning | ⬜ |
| `sys.config` production template | ⬜ |
| Docker image | ⬜ |

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-02-21 (M3 refresh: mark existing items ✅, add read_file/write_file tools)
