# BeamClaw — Project Status

## Current Phase: Implementation

Scaffolding is complete. All nine OTP apps compile clean with zero warnings.
Core systems (M0–M10), workspaces (M11–M17), session persistence and sharing
(M18–M19), Telegram pairing (M20), memory search (M21–M23), photo/vision (M24),
Docker sandbox (M25–M30), scheduler/heartbeat (M31–M37), and Brave Search
(Post-M37) are all complete. Bundled skills (finnhub, nano-banana-pro) added
with `{baseDir}` template resolution. 482 EUnit tests + 31 CT tests pass (513 total).

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

---

## Recent Milestones

### Post-M37 — Scheduler CT Integration Suite ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_scheduler_SUITE` | ✅ | 11 tests: tool actions, timer fire, heartbeat, error handling, concurrency |
| `bc_provider_heartbeat_ok_mock` | ✅ | Mock returning "HEARTBEAT_OK" |
| `bc_provider_heartbeat_alert_mock` | ✅ | Mock returning alert text |
| CLAUDE.md update | ✅ | CT suites table, testing policy |
| docs/building.md update | ✅ | CT suite table, testing policy |

### Post-M37 — Brave Search Built-in Tool ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_tool_web_search` | ✅ | Built-in tool; Brave Search API; read_only autonomy, no approval |
| `bc_tool_registry` update | ✅ | 9 built-in tools (added web_search) |
| `bc_workspace_templates` update | ✅ | TOOLS.md template lists web_search |
| `sys.config` + `sys.docker.config` | ✅ | `beamclaw_tools` config section with `web_search` key |
| `bc_provider_websearch_mock` | ✅ | Mock provider for CT |
| `bc_agentic_loop_SUITE` update | ✅ | `web_search_tool_call` test case (no-API-key error path) |
| EUnit tests | ✅ | 10 tests: definition, approval, autonomy, no-key, missing-query, format_results |
| CLAUDE.md update | ✅ | File Layout, tool implementations list, Configuration |
| docs/configuration.md update | ✅ | BRAVE_API_KEY env var, beamclaw_tools config section |

### Post-M37 — Bundled Skills (finnhub, nano-banana-pro) ✅

| Task | Status | Notes |
|------|--------|-------|
| `finnhub/SKILL.md` | ✅ | Bundled skill; requires FINNHUB_TOKEN, curl, jq |
| `nano-banana-pro/SKILL.md` | ✅ | Bundled skill; requires GEMINI_API_KEY, uv; includes scripts/ |
| `generate_image.py` | ✅ | Gemini image generation script (copied from OpenClaw) |
| `bc_system_prompt` `{baseDir}` | ✅ | `resolve_base_dir/2` replaces `{baseDir}` with skill directory path |
| `.env.example` | ✅ | FINNHUB_TOKEN, GEMINI_API_KEY, GEMINI_MODEL |
| CLAUDE.md File Layout | ✅ | Updated priv/skills/ tree |
| EUnit tests | ✅ | 6 new: bundled skill parsing (2) + resolve_base_dir (4) |

---

## Active Work

_No milestones currently in progress._

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-02-27
