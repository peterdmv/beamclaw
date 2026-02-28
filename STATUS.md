# BeamClaw — Project Status

## Current Phase: Implementation

Scaffolding is complete. All nine OTP apps compile clean with zero warnings.
Core systems (M0–M10), workspaces (M11–M17), session persistence and sharing
(M18–M19), Telegram pairing (M20), memory search (M21–M23), photo/vision (M24),
Docker sandbox (M25–M30), scheduler/heartbeat (M31–M37), Brave Search, bundled
skills, on-demand skill loading, Telegram markdown-to-HTML formatting,
BM25-based skill auto-injection, and `/context` command (Post-M37) are all complete.
573 EUnit tests + 37 CT tests pass (610 total).

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

---

## Recent Milestones

### Post-M37 — `/context` Command ✅

| Task | Status | Notes |
|------|--------|-------|
| Create `bc_context.erl` in `beamclaw_core` | ✅ | Pure-function module: gather/1, format_text/1,2, render_svg/1, render_png/1 |
| Token estimation + context window lookup | ✅ | `byte_size/4` approximation, hardcoded model→window map |
| 10x10 Unicode grid with category colors | ✅ | ANSI colors for TUI, plain chars for Telegram fallback |
| SVG rendering (dark theme) | ✅ | Grid + legend + bootstrap listing; PNG via `rsvg-convert` |
| Intercept `/context` in `bc_channel_tui.erl` | ✅ | ANSI-colored output with model name + category breakdown |
| Intercept `/context` in `bc_channel_telegram.erl` | ✅ | Monospace `<pre>` HTML text via `send_message_html/4` |
| EUnit tests | ✅ | 12 new: tokens, context windows, format_size, gather, text/ANSI/SVG/PNG |
| Update CLAUDE.md + STATUS.md | ✅ | File Layout, milestone |

### Post-M37 — BM25 Skill Auto-Injection ✅

| Task | Status | Notes |
|------|--------|-------|
| Add `assemble/3` overload to `bc_system_prompt.erl` | ✅ | Accepts user message, passes to `load_skills/3` |
| BM25-based skill promotion in `load_skills/3` | ✅ | `maybe_promote_skill/2`: rank on-demand skills by name+desc, promote top if score ≥ 0.5 |
| Pass user message from `bc_loop.erl` | ✅ | `last_user_content(History)` → `assemble/3` in streaming `do_stream` |
| EUnit tests | ✅ | 5 new test generators (11 assertions): promotes, no-match, best-of-multiple, threshold, always-unaffected |
| Update STATUS.md | ✅ | Milestone |

### Post-M37 — Telegram Markdown-to-HTML Formatting ✅

| Task | Status | Notes |
|------|--------|-------|
| Create `bc_telegram_format.erl` | ✅ | Pure-function markdown→HTML: format/1, chunk/2, escape_html/1 |
| Integrate formatter in `bc_channel_telegram.erl` | ✅ | `parse_mode: HTML`, plain-text fallback on 400, `make_api_url/2` helper |
| EUnit tests | ✅ | 33 new: escaping, code blocks, inline, block-level, edge cases, chunking |
| Update CLAUDE.md + STATUS.md | ✅ | File Layout, milestone |

### Post-M37 — Scrubber + Telegram + Obs Fixes ✅

| Task | Status | Notes |
|------|--------|-------|
| Scrubber skips `$VAR` env references | ✅ | `(?!\$)` lookahead on 4 generic key=value patterns |
| Skip empty Telegram messages | ✅ | Guard `send/3` for `<<>>` and `undefined` content |
| Scrub tool call args before obs logging | ✅ | `scrub_map/1` on `tool_call_start` event args |
| EUnit tests | ✅ | 8 new: 4 env var passthrough, 1 real-value-still-scrubbed, 3 scrub_map |

---

## Active Work

_No milestones currently in progress._

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-02-28
