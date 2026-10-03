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
development process retrospective test suites,
generic webhook ingestion endpoint,
webhook body-based auth for TradingView,
a fix for empty native tool_call ids corrupting session history,
case-insensitive Telegram command matching,
and a fix for dropped tool_call_id/tool_calls fields in provider requests
(Post-M37) are all complete.
841 EUnit tests + 74 CT tests pass (915 total).

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
| Post-M37 | Generic Webhook Ingestion Endpoint |
| Post-M37 | Webhook Body-Based Auth (TradingView Support) |
| Post-M37 | Fix Empty Native Tool-Call ID Corrupting Session History |
| Post-M37 | Case-Insensitive Telegram Command Matching |
| Post-M37 | Fix Dropped tool_call_id/tool_calls Fields in Provider Requests |

---

## Recent Milestones

### Post-M37 — Fix Dropped tool_call_id/tool_calls Fields in Provider Requests ✅

| Task | Status | Notes |
|------|--------|-------|
| Actual root cause of the recurring "tool messages must include a non-empty string tool_call_id" 400 | ✅ | `bc_provider_openrouter:message_to_map/1` dropped `tool_call_id`/`name` for `role=tool` messages and `tool_calls` for `role=assistant` messages on *every* multi-turn tool-call round-trip — not a corrupted/empty id as first suspected (two prior fixes in this area addressed a symptom, not this) |
| `bc_provider_openrouter.erl` — `message_to_map/1` now forwards `tool_call_id`/`name` for tool messages and the native `tool_calls` array for assistant messages | ✅ | `tool_calls` on a `role=tool` message is a repurposed internal `is_error` marker, not wire data — explicitly not forwarded for that role |
| `bc_provider_vision_tests.erl` — 3 new regression tests | ✅ | tool message includes tool_call_id/name; assistant tool_calls forwarded verbatim; plain assistant message omits the field |
| Verified by replaying the exact previously-failing history through the live OpenRouter API with the fix applied | ✅ | `replay_ok` — confirms the fix, not just a session reset, resolves the failure |
| All tests pass | ✅ | 841 EUnit + 74 CT = 915 total |
| Docker image rebuilt and redeployed | ✅ | `docker compose build && docker compose up -d`; container healthy post-restart |

---

## Active Work

_No milestones currently in progress._

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-10-03
