# BeamClaw — Project Status

## Current Phase: Implementation

Scaffolding is complete. All nine OTP apps compile clean with zero warnings.
Core systems (M0–M10), workspaces (M11–M17), session persistence and sharing
(M18–M19), Telegram pairing (M20), memory search (M21–M23), photo/vision (M24),
Docker sandbox (M25–M30), scheduler/heartbeat (M31–M37), Brave Search, bundled
skills, on-demand skill loading, Telegram markdown-to-HTML formatting,
BM25-based skill auto-injection, `/context` command, and outgoing photo
delivery, and per-user agent mapping (Post-M37) are all complete.
618 EUnit tests + 37 CT tests pass (655 total).

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

---

## Recent Milestones

### Post-M37 — Per-User Agent Mapping ✅

| Task | Status | Notes |
|------|--------|-------|
| v2 storage format in `bc_pairing.erl` | ✅ | `{"version": 2, "allowed": [{"id": "...", "agent_id": "..."}]}` with v1 auto-migration |
| `approve/3` with agent_id | ✅ | New arity stores specified agent; `approve/2` defaults to `default_agent` config |
| `get_agent_id/2` lookup | ✅ | Returns `{ok, AgentId}` or `{error, not_found}` |
| `resolve_agent_id/1` in `bc_channel_telegram.erl` | ✅ | Both `do_dispatch` clauses use pairing lookup → config fallback |
| `--agent NAME` flag in CLI `pair` command | ✅ | `beamclaw pair telegram CODE --agent mom` |
| `pair list` shows agent column | ✅ | `telegram  12345  agent=mom` format |
| EUnit tests | ✅ | 8 new: default/custom agent, get_agent_id, v1 migration, list/revoke/is_allowed v2 |
| Update CLAUDE.md + STATUS.md | ✅ | CLI commands, milestone |

### Post-M37 — Outgoing Photo Delivery ✅

| Task | Status | Notes |
|------|--------|-------|
| `MEDIA:` token extraction in `bc_loop.erl` | ✅ | `extract_media/1`: parse `MEDIA: /path` from tool results, read file, base64-encode, attach to message |
| `pending_media` field in loop state | ✅ | Accumulated across tool iterations, attached to final assistant message |
| `sendPhoto` in `bc_channel_telegram.erl` | ✅ | Multipart form-data upload via `/sendPhoto` API, caption truncation (1024 char limit), text fallback on failure |
| `is_image_mime/1` + `truncate_caption/1` | ✅ | Pure helpers for attachment routing and Telegram caption limits |
| TUI attachment display in `bc_channel_tui.erl` | ✅ | `[Attachment: image/png]` indicator for terminal users |
| EUnit tests | ✅ | 32 new: 18 media extraction (tokens, mime types, file I/O) + 14 Telegram photo (mime check, caption, multipart) |
| Update CLAUDE.md + STATUS.md | ✅ | Obs events, file layout, milestone |

### Post-M37 — `/context` Command ✅

| Task | Status | Notes |
|------|--------|-------|
| Create `bc_context.erl` in `beamclaw_core` | ✅ | Pure-function module: gather/1, format_text/1,2, render_svg/1, render_png/1 |
| Token estimation + context window lookup | ✅ | `byte_size/4` approximation, hardcoded model→window map |
| 10x10 Unicode grid with category colors | ✅ | ANSI colors for TUI, plain chars for Telegram fallback |
| SVG rendering (dark theme) | ✅ | Grid + legend + bootstrap listing; PNG via `rsvg-convert` |
| Intercept `/context` in `bc_channel_tui.erl` | ✅ | ANSI-colored output with model name + category breakdown |
| Intercept `/context` in `bc_channel_telegram.erl` | ✅ | Emoji grid via `format_telegram/1`, plain-text fallback |
| EUnit tests | ✅ | 17 total: tokens, context windows, format_size, gather, text/ANSI/SVG/PNG, telegram (emoji/legend/bootstrap/grid) |
| Update CLAUDE.md + STATUS.md | ✅ | File Layout, milestone |

### Post-M37 — BM25 Skill Auto-Injection ✅

| Task | Status | Notes |
|------|--------|-------|
| Add `assemble/3` overload to `bc_system_prompt.erl` | ✅ | Accepts user message, passes to `load_skills/3` |
| BM25-based skill promotion in `load_skills/3` | ✅ | `maybe_promote_skill/2`: rank on-demand skills by name+desc, promote top if score ≥ 0.5 |
| Pass user message from `bc_loop.erl` | ✅ | `last_user_content(History)` → `assemble/3` in streaming `do_stream` |
| EUnit tests | ✅ | 5 new test generators (11 assertions): promotes, no-match, best-of-multiple, threshold, always-unaffected |
| Update STATUS.md | ✅ | Milestone |

---

## Active Work

_No milestones currently in progress._

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-02-28
