# BeamClaw — Project Status

## Current Phase: Implementation

Scaffolding is complete. All nine OTP apps compile clean with zero warnings.
Core systems (M0–M10), workspaces (M11–M17), session persistence and sharing
(M18–M19), Telegram pairing (M20), memory search (M21–M23), photo/vision (M24),
Docker sandbox (M25–M30), scheduler/heartbeat (M31–M37), Brave Search, bundled
skills, on-demand skill loading, Telegram markdown-to-HTML formatting,
BM25-based skill auto-injection, `/context` command, outgoing photo delivery,
per-user agent mapping, voice message transcription, token-based compaction,
webhook secret token validation, smart session memory maintenance,
Telegram bot command registration, `/new` session reset, and
v0.1.0 release preparation (Post-M37) are all complete.
684 EUnit tests + 37 CT tests pass (721 total).

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

---

## Recent Milestones

### Post-M37 — Telegram Bot Commands Registration + `/new` Session Reset ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_session.erl` — `clear_history/1` | ✅ | Atomic history clear + Mnesia delete |
| `bc_channel_telegram.erl` — `setMyCommands` | ✅ | Registers `/context`, `/new` in bot menu on init |
| `bc_channel_telegram.erl` — `/new` dispatch + handler | ✅ | Busy guard, memory flush, typing indicator |
| `bc_channel_tui.erl` — `/new` dispatch + handler | ✅ | Same logic, `io:format` output |
| `beamclaw_cli.erl` — `/new` in remote TUI | ✅ | All operations via `rpc:call/4` |
| Documentation | ✅ | CLAUDE.md (obs event), docs/running.md, STATUS.md |
| All tests pass | ✅ | 683 EUnit tests pass, 0 warnings |

### Post-M37 — v0.1.0 Release Preparation ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_context.erl` — `version/0` + display in all formats | ✅ | Exported; shows "BeamClaw 0.1.0" in TUI, Telegram, SVG |
| `CHANGELOG.md` — release notes | ✅ | New file, feature inventory for v0.1.0 |
| `README.md` — fix stale content | ✅ | Repo URL, Docker image ref, app count (6→9), dep graph |
| `docs/building.md` — fix stale counts | ✅ | App count (8→9), test count (407→683) |
| Tests | ✅ | 1 new test (version_returns_binary_test), updated existing |
| Git tag + GitHub Release | ✅ | Annotated `v0.1.0` tag, release from CHANGELOG.md |

---

## Active Work

_No milestones currently in progress._

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-03-05
