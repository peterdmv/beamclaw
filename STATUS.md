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
Telegram bot command registration, `/new` session reset,
v0.1.0 release preparation, user environment context injection,
per-agent weather location, timezone abbreviations + UTC offset display,
and UTF-8 Hungarian USER.md field regex fix
(Post-M37) are all complete.
746 EUnit tests + 37 CT tests pass (783 total).

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

---

## Recent Milestones

### Post-M37 — Fix User Env: Async Refresh + Open-Meteo ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_user_env.erl` — switch weather API from wttr.in to Open-Meteo | ✅ | No API key needed, works from Docker |
| `bc_user_env.erl` — async periodic refresh via `handle_info` | ✅ | `handle_call` never blocks on HTTP |
| `bc_user_env.erl` — `wmo_description/1` WMO weather code mapper | ✅ | 28 WMO codes → human-readable strings |
| `bc_user_env.erl` — enable by default | ✅ | `enabled => true`, Stockholm lat/lon |
| Config — new shape: `latitude`/`longitude`/`location_name`/`refresh_interval_ms` | ✅ | sys.config + sys.docker.config |
| `bc_user_env_tests.erl` — Open-Meteo JSON + WMO tests | ✅ | 8 new tests (wmo, open-meteo format) |
| CLAUDE.md, docs/configuration.md updates | ✅ | New config keys documented |
| All tests pass | ✅ | 733 EUnit tests pass, 0 warnings |

### Post-M37 — Per-Agent Weather Location ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_workspace_templates.erl` — add `**Location:**` to USER.md template | ✅ | City, lat, lon format |
| `bc_user_env.erl` — `parse_location_from_user_md/1` | ✅ | English + Hungarian (`**Helyszín:**`) |
| `bc_user_env.erl` — `resolve_location/2` | ✅ | USER.md → global config fallback |
| `bc_user_env.erl` — per-location `weather_cache` map | ✅ | `#{LocKey => WeatherText}` |
| `bc_user_env.erl` — on-demand `fetch_location` for cache miss | ✅ | Async spawn, weather appears next LLM call |
| `bc_user_env.erl` — refresh fetches all cached locations | ✅ | Periodic timer refreshes all known locations |
| `bc_user_env.erl` — Hungarian timezone parsing (`**Időzóna:**`) | ✅ | Bonus: mom's USER.md works for timezone too |
| `bc_user_env_tests.erl` — 6 new location parsing tests | ✅ | Present, missing, Hungarian, bad format, placeholder, integer coords |
| All tests pass | ✅ | 739 EUnit tests pass, 0 warnings |

### Post-M37 — Timezone Abbreviations + UTC Offset Display ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_workspace_templates.erl` — timezone format hint in USER.md template | ✅ | Guides IANA name or abbreviation |
| `bc_user_env.erl` — `strip_parenthetical/1` in `parse_timezone_from_user_md` | ✅ | `CET (Central European Time)` → `CET` |
| `bc_user_env.erl` — 30 common timezone abbreviations in `tz_offset/1` | ✅ | CET, EST, PST, JST, etc. |
| `bc_user_env.erl` — UTC offset in time section display | ✅ | `(CET, UTC+1)` — no duplicate for UTC/GMT |
| `bc_user_env_tests.erl` — 6 new tests | ✅ | Abbreviations, parenthetical, UTC offset display |
| Container: default agent USER.md | ✅ | `CET (Central European Time)` → `Europe/Stockholm` |
| Container: mom agent USER.md | ✅ | `CET` → `Europe/Budapest` |
| All tests pass | ✅ | 745 EUnit tests pass, 0 warnings |

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

## Active Work

_No milestones currently in progress._

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-03-11
