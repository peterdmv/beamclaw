# BeamClaw — Project Status

## Current Phase: Implementation

Scaffolding is complete. All seven OTP apps compile clean with zero warnings.
Multi-agent workspaces (M11–M13), rich templates (M14), daily logs (M15),
skill system (M16–M17), session persistence (M18), cross-channel session
sharing (M19), Telegram pairing access control (M20), and typing indicators
(Post-M20) are complete. Cross-channel session fix applied. 192 EUnit tests pass.

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

### M10 — Remote TUI (`beamclaw start` + `beamclaw tui`) ✅

| Task | Status | Notes |
|------|--------|-------|
| `{bc_turn_complete}` signal in `bc_loop` finalizing | ✅ | 4 lines; benefits all `reply_pid` consumers |
| `ensure_ctl_node_soft/0` + `try_connect_daemon/0` | ✅ | Soft daemon detection; no halt on epmd absence |
| Refactor `cmd_tui/0` → auto-detect daemon | ✅ | connected → remote; not_running → local |
| `cmd_remote_tui/0` + `remote_tui_loop/1` | ✅ | Blocking stdin loop on escript node |
| `dispatch_remote/2` | ✅ | RPC session create + dispatch with `reply_pid = self()` |
| `receive_remote_response/1` | ✅ | Chunk streaming + `bc_done` + `bc_turn_complete` |
| `generate_remote_session_id/0` | ✅ | UUID v4 with `remote-tui-` prefix |
| `spawn_daemon/0` TUI disable | ✅ | `lists:keyreplace` to set `tui enabled=false` |
| `nodedown` + `badrpc` handling | ✅ | Graceful disconnect on daemon death or RPC error |
| `cmd_help/0` update | ✅ | Documents auto-connect behaviour |
| `docs/running.md` update | ✅ | Remote TUI workflow documented |

### Post-M10 — Daemon File Logging ✅

| Task | Status | Notes |
|------|--------|-------|
| `kernel` logger config in `sys.config` | ✅ | Console handler (info) + file handler (debug, `/tmp/beamclaw_daemon.log`, 5 MB × 3 rotation) |
| `bc_channel_telegram` debug log | ✅ | Traces message dispatch with chat_id and text |
| `bc_loop` debug logs | ✅ | Traces `run received` and `route_response` with session/channel/reply_pid |
| CLI start message update | ✅ | Prints log file path after "Gateway started." |
| `docs/configuration.md` update | ✅ | Kernel logger section with runtime level change example |
| `docs/running.md` update | ✅ | "Viewing daemon logs" subsection |
| `CLAUDE.md` Configuration update | ✅ | Added `kernel` logger entry to config block |

### M11 — Workspace Foundation (Filesystem + System Prompt) ✅

| Module | Status | Notes |
|--------|--------|-------|
| `bc_workspace_templates` | ✅ | Pure data module; 6 default bootstrap file templates |
| `bc_workspace` | ✅ | Agent workspace filesystem ops (create/delete/list/read/write) |
| `bc_system_prompt` | ✅ | Assembles bootstrap files into system messages for LLM |
| `bc_types.hrl` update | ✅ | Added `agent_id` to `#bc_session_ref{}` |
| `bc_session` update | ✅ | `agent_id` in state; `get_agent_id/1` API |
| `bc_loop` update | ✅ | `agent_id` in loop_data; system prompt injection before LLM call |
| `beamclaw_core_app` update | ✅ | `ensure_default_agent/0` on app start |
| `sys.config` update | ✅ | `{default_agent, <<"default">>}` |
| EUnit tests | ✅ | 37 tests (bc_workspace_tests + bc_system_prompt_tests) |

### M12 — CLI Agent Management + Channel Integration ✅

| Task | Status | Notes |
|------|--------|-------|
| `beamclaw agent create NAME` | ✅ | Validate ID → create workspace → print path |
| `beamclaw agent list` | ✅ | List agents with display name from IDENTITY.md |
| `beamclaw agent show NAME` | ✅ | Print all bootstrap file contents |
| `beamclaw agent delete NAME` | ✅ | Refuse "default"; recursive delete |
| `beamclaw tui --agent NAME` | ✅ | Thread agent_id through local + remote TUI |
| `BEAMCLAW_AGENT` env var | ✅ | Default agent name when `--agent` not specified |
| `bc_channel_tui` update | ✅ | `agent_id` in session Config |
| `bc_channel_telegram` update | ✅ | `agent_id` in session Config |
| `bc_http_completions_h` update | ✅ | Accept `agent_id` from request body |
| `bc_ws_h` update | ✅ | Accept `agent_id` in WebSocket messages |
| `cmd_doctor` update | ✅ | Workspace directory + default agent check |
| `cmd_help` update | ✅ | Agent commands and `--agent` flag documented |
| `docs/running.md` update | ✅ | Agent Management section |

### M13 — Workspace Memory Tool + Tool Defs in LLM ✅

| Module | Status | Notes |
|--------|--------|-------|
| `bc_workspace_path` | ✅ | Pure path resolution in `beamclaw_tools` (avoids dep cycle) |
| `bc_tool_workspace_memory` | ✅ | read/append/replace MEMORY.md; no approval; read_only autonomy |
| `bc_tool_registry` update | ✅ | 7 built-in tools (added workspace_memory) |
| `bc_loop` update | ✅ | Fetch tool defs from registry; pass in Options |
| `bc_provider_openrouter` update | ✅ | Include tool defs in request body (OpenAI function-calling format) |
| EUnit tests | ✅ | 14 tests (bc_tool_workspace_memory_tests + bc_workspace_path_tests) |

### M14 — Rich Agent Templates + BOOTSTRAP.md ✅

| Task | Status | Notes |
|------|--------|-------|
| Rich template content (all 7 files) | ✅ | OpenClaw-derived: SOUL, IDENTITY, USER, TOOLS, MEMORY, AGENTS, BOOTSTRAP |
| BOOTSTRAP.md as 7th template | ✅ | First-run discovery ritual; self-deleting |
| Workspace: 7 files + memory/ dir | ✅ | `create_agent` creates memory/ subdirectory |
| System prompt: BOOTSTRAP.md ordering | ✅ | IDENTITY → SOUL → USER → TOOLS → AGENTS → BOOTSTRAP → MEMORY |
| EUnit tests updated | ✅ | 7-file assertions, memory dir check |

### M15 — Daily Log System ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_workspace_path` extensions | ✅ | `memory_dir/1`, `daily_log_file/2` |
| `bc_tool_workspace_memory` daily actions | ✅ | `read_daily`, `append_daily`, `list_daily` |
| `bc_workspace` daily log functions | ✅ | `read_daily_log/2`, `list_daily_logs/1` |
| `bc_system_prompt` daily log loading | ✅ | Today + yesterday auto-included |
| EUnit tests | ✅ | Daily log tool tests, workspace tests, system prompt tests |

### M16 — Skill System Core ✅

| Module | Status | Notes |
|--------|--------|-------|
| `bc_skill` record in `bc_types.hrl` | ✅ | name, description, homepage, emoji, content, source, metadata, path |
| `bc_skill_parser` | ✅ | SKILL.md frontmatter parser (key:value + JSON metadata) |
| `bc_skill_discovery` | ✅ | Bundled + global + per-agent discovery; name-based merge |
| `bc_skill_eligibility` | ✅ | bins/env/os requirement checks; `always` bypass flag |
| System prompt skill injection | ✅ | Skills appended after daily logs |
| `sys.config` skills entry | ✅ | `{skills, #{}}` |
| EUnit tests | ✅ | Parser, discovery, eligibility tests |

### M17 — Skill CLI & Installation ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_skill_installer` | ✅ | apt/brew/npm/pip/download install specs |
| `beamclaw skills list` | ✅ | Discovered skills with eligible status |
| `beamclaw skills status` | ✅ | Detailed requirements check |
| `beamclaw skills show NAME` | ✅ | Show SKILL.md content |
| `beamclaw skills install NAME` | ✅ | Run compatible install spec |
| Bundled example skill | ✅ | `priv/skills/example-skill/SKILL.md` |
| `cmd_doctor` skills check | ✅ | Skills directory + count |
| `cmd_help` updated | ✅ | Skills commands documented |
| EUnit tests | ✅ | Installer tests |

### Post-M17 — Agent Rehatch ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_workspace:rehatch_agent/1` | ✅ | Restore 7 bootstrap files to defaults, wipe daily logs, preserve skills/ |
| `beamclaw agent rehatch NAME` | ✅ | CLI command with error handling |
| EUnit tests | ✅ | 3 tests: rehatch, not_found, preserves_skills |
| `docs/running.md` update | ✅ | Agent rehatch documented |

### M18 — Session Persistence (Mnesia-backed history) ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_session_store.hrl` | ✅ | Mnesia record: session_id, user_id, agent_id, autonomy, history, timestamps, config |
| `bc_session_store.erl` | ✅ | init_table, load, save, delete, delete_expired; versioned serialization |
| `bc_session_cleaner.erl` | ✅ | gen_server; periodic cleanup every 5 min; uses session_ttl_seconds |
| `beamclaw_core_app.erl` update | ✅ | `bc_session_store:init_table()` on app start |
| `beamclaw_core_sup.erl` update | ✅ | `bc_session_cleaner` as permanent child |
| `bc_session.erl` persistence hooks | ✅ | Load history on init; persist on append/set_history; configurable |
| `beamclaw_core.app.src` update | ✅ | Added `mnesia` to applications |
| `sys.config` update | ✅ | `session_persistence`, `session_sharing`, `session_cleanup_interval_ms` |
| EUnit tests | ✅ | 6 tests (bc_session_store_tests) |

### M19 — Cross-Channel Session Sharing ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_session_registry` derive_session_id | ✅ | SHA-256 based; shared/per_channel modes |
| `bc_types.hrl` update | ✅ | Added `agent_id` to `#bc_channel_message{}` |
| `bc_loop.erl` per-run routing | ✅ | `reply_channel` replaces `channel_mod`; `channel_mod_for/1` |
| `bc_session.erl` deprecate channel_mod | ✅ | `get_channel_mod/1` deprecated (returns stored value for compat) |
| `bc_channel_tui.erl` update | ✅ | `tui_user_id/0`; derive session_id; pass user_id/agent_id |
| `bc_channel_telegram.erl` update | ✅ | `tg:` prefix; derive session_id; ETS chat_id mapping |
| `bc_http_completions_h.erl` update | ✅ | `X-User-Id` header; `api:` prefix; derive or explicit session_id |
| `bc_ws_h.erl` update | ✅ | `ws:` prefix; derive session_id per message |
| `beamclaw_cli.erl` update | ✅ | `cli_user_id/0`; remote derive_session_id via RPC; `BEAMCLAW_USER` |
| EUnit tests | ✅ | 7 tests (bc_session_registry_tests) |

### Post-M19 — Cross-Channel Session Sharing Fix ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_config:canonical_user_id/0` | ✅ | Centralised `BEAMCLAW_USER` check; returns binary or `undefined` |
| `bc_channel_tui.erl` update | ✅ | Use canonical_user_id; skip `local:` prefix when set |
| `bc_channel_telegram.erl` update | ✅ | Use canonical_user_id; skip `tg:` prefix when set |
| `bc_http_completions_h.erl` update | ✅ | Use canonical_user_id; skip `api:` prefix when set |
| `bc_ws_h.erl` update | ✅ | Use canonical_user_id; skip `ws:` prefix when set |
| `beamclaw_cli.erl` update | ✅ | Use canonical_user_id; skip `local:` prefix when set |
| EUnit test | ✅ | 8 tests (bc_session_registry_tests); canonical cross-channel test added |
| Docs update | ✅ | running.md, configuration.md, CLAUDE.md |

### Post-M19 — Migrate `%% @doc` to EEP-59 `-doc`/`-moduledoc` ✅

| Task | Status | Notes |
|------|--------|-------|
| Erlang escript migration tool | ✅ | One-time tool; handles single/multi-line, module/function-level |
| 70 `.erl` files migrated | ✅ | All 7 apps + test files; single-line → `-doc "...".`; multi-line/quoted → triple-quoted strings |
| `bc_types.hrl` manual fix | ✅ | Header file `%% @doc` → plain `%%` comment (attributes invalid in `.hrl`) |
| Zero remaining `%% @doc` | ✅ | Verified via grep |
| Compilation clean | ✅ | 0 warnings |
| All 180 tests pass | ✅ | No regressions |

### M20 — Telegram Pairing (Access Control) ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_pairing.erl` | ✅ | Pure functional module; file-based JSON storage; code gen, TTL, capacity |
| `bc_pairing_tests.erl` | ✅ | 12 tests: allowed, request, idempotent, approve, revoke, expiry, capacity |
| `bc_channel_telegram.erl` update | ✅ | `dm_policy` check (pairing/allowlist/open); pairing reply message |
| `beamclaw_cli.erl` update | ✅ | `pair` / `pair list` / `pair <ch> <code>` / `pair revoke <ch> <id>` commands |
| `sys.config` update | ✅ | `dm_policy => pairing`, `allow_from => []` in telegram config |
| `CLAUDE.md` update | ✅ | File Layout, Common Commands, Configuration sections |
| `docs/running.md` update | ✅ | Telegram Pairing section with flow, CLI, modes, storage |
| `docs/configuration.md` update | ✅ | `dm_policy` and `allow_from` config keys documented |

### Post-M20 — Typing Indicators ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_channel_telegram` `notify_typing/1` | ✅ | Public API + handle_cast; calls existing `send_typing/2` |
| `bc_channel_tui` `notify_typing/1` | ✅ | Public API + handle_cast; calls existing `send_typing/2` |
| `bc_loop` typing on `streaming` enter | ✅ | `emit_typing/1` fires before LLM call |
| `bc_loop` typing on `executing_tools` enter | ✅ | `emit_typing/1` fires before tool execution |
| `bc_loop` periodic typing tick | ✅ | 4 s timer in `receive_stream/3`; re-sends typing during long streams |

### Post-M20 — Daemon Shutdown Fix ✅

| Task | Status | Notes |
|------|--------|-------|
| `beamclaw_cli.erl` stop timeout | ✅ | 10s → 20s (40 × 500ms); covers Telegram long-poll drain |
| `beamclaw_gateway_app.erl` `prep_stop/1` | ✅ | `cowboy:stop_listener/1` before supervision tree teardown; eliminates Ranch `eaddrinuse` noise |

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-02-22 (Daemon shutdown fix — Post-M20)
