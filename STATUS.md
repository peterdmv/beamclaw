# BeamClaw — Project Status

## Current Phase: Implementation

Scaffolding is complete. All nine OTP apps compile clean with zero warnings.
Core systems (M0–M10), workspaces (M11–M17), session persistence and sharing
(M18–M19), Telegram pairing (M20), memory search (M21–M23), photo/vision (M24),
Docker sandbox (M25–M30), scheduler/heartbeat (M31–M37), Brave Search, bundled
skills, and on-demand skill loading (Post-M37) are all complete.
506 EUnit tests + 31 CT tests pass (537 total).

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

---

## Recent Milestones

### Post-M37 — Scrubber + Telegram + Obs Fixes ✅

| Task | Status | Notes |
|------|--------|-------|
| Scrubber skips `$VAR` env references | ✅ | `(?!\$)` lookahead on 4 generic key=value patterns |
| Skip empty Telegram messages | ✅ | Guard `send/3` for `<<>>` and `undefined` content |
| Scrub tool call args before obs logging | ✅ | `scrub_map/1` on `tool_call_start` event args |
| EUnit tests | ✅ | 8 new: 4 env var passthrough, 1 real-value-still-scrubbed, 3 scrub_map |

### Post-M37 — Empty Final LLM Response Fix ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_thinking:strip` preserves content on full-strip | ✅ | Fallback to `do_strip_tags_only` when `do_strip` returns `<<>>` |
| `bc_thinking:strip(null)` clause | ✅ | Prevents `function_clause` crash when provider returns JSON null |
| `bc_provider_openrouter` null content normalization | ✅ | `null` atom → `<<>>` before `bc_thinking:strip` |
| `bc_channel_telegram` debug logging on empty skip | ✅ | `logger:debug` in empty-content guards for observability |
| `bc_loop` fallback for empty final response | ✅ | Warning + fallback message when final LLM content is empty |
| EUnit tests | ✅ | 3 new: null, all-thinking-preserves, all-think-preserves; 1 updated: unclosed |

---

## Active Work

_No milestones currently in progress._

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-02-28
