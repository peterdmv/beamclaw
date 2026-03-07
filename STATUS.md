# BeamClaw — Project Status

## Current Phase: Implementation

Scaffolding is complete. All ten OTP apps compile clean with zero warnings.
Core systems (M0–M10), workspaces (M11–M17), session persistence and sharing
(M18–M19), Telegram pairing (M20), memory search (M21–M23), photo/vision (M24),
Docker sandbox (M25–M30), scheduler/heartbeat (M31–M37), Brave Search, bundled
skills, on-demand skill loading, Telegram markdown-to-HTML formatting,
BM25-based skill auto-injection, `/context` command, outgoing photo delivery,
per-user agent mapping, voice message transcription, token-based compaction,
webhook secret token validation, smart session memory maintenance,
Telegram bot command registration, `/new` session reset,
v0.1.0 release preparation, user environment context injection,
per-agent weather location, timezone abbreviations + UTC offset display,
UTF-8 Hungarian USER.md field regex fix, A2A protocol,
A2A Bearer token authentication,
and development process retrospective test suites
(Post-M37) are all complete.
814 EUnit tests + 67 CT tests pass (881 total).
21 new CT tests added for edge-case, Telegram, and CLI coverage.

---

## Legend

| Symbol | Meaning |
|--------|---------|
| ✅ | Complete |
| 🚧 | In progress |
| ⬜ | Pending |
| ❌ | Blocked |

---

## Completed Milestones (see STATUS_ARCHIVE.md for details)

| Milestone | Description |
|-----------|-------------|
| M0 | Project Scaffolding |
| M1 | Observability Layer |
| M2 | Memory Layer |
| M3 | Tool Registry |
| M4 | MCP Client |
| M5 | Core Agentic Loop |
| M6 | Gateway |
| M7 | Testing & Hardening |
| M8 | Documentation + Docker Release |
| Post-M8 | Contributor Docs |
| M9 | `beamclaw` CLI (escript) |
| M10 | Remote TUI |
| Post-M10 | Daemon File Logging |
| M11 | Workspace Foundation |
| M12 | CLI Agent Management + Channel Integration |
| M13 | Workspace Memory Tool + Tool Defs in LLM |
| M14 | Rich Agent Templates + BOOTSTRAP.md |
| M15 | Daily Log System |
| M16 | Skill System Core |
| M17 | Skill CLI & Installation |
| Post-M17 | Agent Rehatch |
| M18 | Session Persistence (Mnesia) |
| M19 | Cross-Channel Session Sharing |
| Post-M19 | Session Sharing Fix, EEP-59 Migration |
| M20 | Telegram Pairing (Access Control) |
| Post-M20 | Typing Indicators, Daemon Shutdown Fix, Port Change, Docker Compose, Bootstrap Routing, Thinking Tags |
| M21 | BM25 Keyword Search |
| M22 | Vector Semantic Search + Hybrid Merge |
| M23 | Loop Integration + Search Polish |
| M24 | Telegram Photo/Vision Support |
| M25–M30 | Docker Sandbox (Lifecycle, Bridge, Tool Exec, PII, Policy, Skills, CLI) |
| Post-M30 | Docker Sibling Containers, CT Suites, delete_bootstrap/delete_file, Reaper, Typing Fix |
| M31–M37 | Scheduler & Heartbeat (Data Model, Store, Runner, Executor, Tool, Templates, CLI) |
| Post-M37 | Scheduler CT Suite, Brave Search Tool, Bundled Skills (finnhub, nano-banana-pro) |
| Post-M37 | On-Demand Skill Loading (Token Optimization) |
| Post-M37 | Scrubber env var fix, empty Telegram messages, obs args scrubbing |
| Post-M37 | Telegram Markdown-to-HTML Formatting |
| Post-M37 | BM25 Skill Auto-Injection |
| Post-M37 | `/context` Command (TUI + Telegram) |
| Post-M37 | Outgoing Photo Delivery (Telegram + TUI) |
| Post-M37 | Per-User Agent Mapping (Telegram Pairing) |
| Post-M37 | Voice Message Transcription (Telegram → Groq Whisper) |
| Post-M37 | Token-Based Automatic Compaction Trigger |
| Post-M37 | Per-Session Provider Model for Compaction |
| Post-M37 | Telegram Webhook Secret Token Validation |
| Post-M37 | Fix Docker Cyclic Restarts (Webhook Env Vars) |
| Post-M37 | Fix /context Compaction Buffer Display |
| Post-M37 | Fix /context Grid Clipping Compaction Buffer Cells |
| Post-M37 | Smart Session Memory Maintenance |
| Post-M37 | Fix Mnesia Session Persistence Across Docker Rebuilds |
| Post-M37 | Fix Mnesia Tables Always Created as ram_copies |
| Post-M37 | Fix /context Header Token Count Including Compaction Buffer |
| Post-M37 | Telegram Bot Commands Registration + `/new` Session Reset |
| Post-M37 | v0.1.0 Release Preparation |
| Post-M37 | Incoming Image Attachment Disk Save + bash Tool Arg Fix |
| Post-M37 | Skill Prompt Fix + Strip Old Image Attachments |
| Post-M37 | User Environment Context Injection |
| Post-M37 | Fix User Env: Async Refresh + Open-Meteo |
| Post-M37 | Per-Agent Weather Location |
| Post-M37 | Timezone Abbreviations + UTC Offset Display |
| Post-M37 | Fix UTF-8 Hungarian USER.md Field Regex Matching |
| Post-M37 | A2A (Agent2Agent) Protocol |
| Post-M37 | A2A Bearer Token Authentication |
| Post-M37 | Development Process Retrospective — Test Suites |

---

## Recent Milestones

### Post-M37 — Development Process Retrospective — Test Suites ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_unicode_tests.erl` — 34 UTF-8 EUnit tests | ✅ | Hungarian, emoji, CJK, Arabic, Cyrillic through scrubber, formatter, parser |
| `bc_loop_edge_SUITE.erl` — 6 state machine edge-case CT tests | ✅ | Stream error/timeout recovery, tool crash, max iterations, queue drain, supervisor restart |
| `bc_loop.erl` — fix stream timeout not firing due to typing ticks | ✅ | `receive_stream` now uses elapsed-time-based deadline instead of resetting `after` timer |
| `bc_telegram_mock.erl` — Cowboy-based mock Telegram Bot API | ✅ | Records requests in ETS; supports configurable responses |
| `bc_telegram_integration_SUITE.erl` — 8 Telegram integration CT tests | ✅ | Bot commands, sendMessage format, HTML parse_mode, Unicode, typing, empty skip, editMessage, webhook |
| `bc_channel_telegram.erl` — configurable `api_base_url` for testing | ✅ | Process dictionary stores mock base URL; `make_api_url/2` reads from it |
| `bc_cli_smoke_SUITE.erl` — 7 CLI escript smoke CT tests | ✅ | version, help, unknown command, agent list, skills list, sandbox status |
| `bc_docker_integration_SUITE.erl` — 10 Docker deployment CT tests | ✅ | Auto-skips without Docker; builds image, starts container, health check, UTF-8, workspace |
| Mock providers for edge tests (error, timeout, infinite tool, toolcall) | ✅ | 5 mock modules for systematic state machine edge-case testing |
| CLAUDE.md — CT suites table updated | ✅ | 4 new suites documented |
| All tests pass | ✅ | 814 EUnit + 67 CT = 881 total |

---

### Post-M37 — Fix UTF-8 Hungarian USER.md Field Regex Matching ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_user_env.erl` — add `/utf8` + `unicode` to `parse_timezone_from_user_md/1` | ✅ | Regex now matches real UTF-8 file content |
| `bc_user_env.erl` — add `/utf8` + `unicode` to `parse_location_from_user_md/1` | ✅ | Same fix for Helyszín field |
| `bc_user_env_tests.erl` — `/utf8` suffix on Hungarian test data | ✅ | Tests now exercise real UTF-8 matching |
| `bc_user_env_tests.erl` — new `parse_tz_hungarian_utf8_file_test` | ✅ | Explicit UTF-8 bytes simulating `file:read_file/1` |
| All tests pass | ✅ | 746 EUnit tests, 0 warnings |

---

### Post-M37 — A2A Bearer Token Authentication ✅

| Task | Status | Notes |
|------|--------|-------|
| Rebase `feature/a2a-protocol` onto main | ✅ | 9 commits behind resolved, duplicate `version/0` fixed |
| `bc_a2a_http_h.erl` — Bearer token auth on POST /a2a | ✅ | `authenticate/1`, constant-time comparison via `crypto:hash_equals/2` |
| `bc_a2a_http_h.erl` — 401 JSON-RPC error + `WWW-Authenticate: Bearer` | ✅ | RFC 6750 compliant |
| `bc_a2a_agent_card.erl` — dynamic auth scheme in Agent Card | ✅ | `resolve_auth_scheme/0` reads `A2A_BEARER_TOKEN` env var |
| Config — `A2A_BEARER_TOKEN` in sandbox `env_blocklist` | ✅ | sys.config + sys.docker.config |
| Config — `beamclaw_a2a` app section in sys.config | ✅ | Agent card name + url |
| `bc_a2a_auth_tests.erl` — 6 EUnit tests | ✅ | verify_bearer, resolve_token |
| `bc_a2a_server_tests.erl` — 2 Agent Card auth tests | ✅ | Auth present/absent based on env var |
| `bc_a2a_http_integration_SUITE.erl` — 5 CT auth tests | ✅ | Missing header, invalid token, valid token, unconfigured, card auth |
| CLAUDE.md, docs/configuration.md updates | ✅ | `a2a_auth_failed` obs event, `A2A_BEARER_TOKEN` env var |
| All tests pass | ✅ | 780 EUnit + 46 CT = 826 total |

---

## Active Work

_No milestones currently in progress._

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-03-07
