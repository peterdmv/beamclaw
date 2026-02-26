# BeamClaw — Project Status

## Current Phase: Implementation

Scaffolding is complete. All eight OTP apps compile clean with zero warnings.
Multi-agent workspaces (M11–M13), rich templates (M14), daily logs (M15),
skill system (M16–M17), session persistence (M18), cross-channel session
sharing (M19), Telegram pairing access control (M20), and typing indicators
(Post-M20) are complete. Cross-channel session fix applied. Workspace memory
bootstrap file routing and pre-compaction memory flush (Post-M20) added.
Memory search with BM25 + vector hybrid (M21–M23) is complete.
Telegram photo/vision support (M24) is complete.
Docker sandbox code execution with MCP tool bridge (M25–M30) is complete.
Docker sibling container support for sandbox in Docker deployments (Post-M30) is complete.
409 EUnit tests pass.

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
| `Dockerfile` (multi-stage) | ✅ | `erlang:28-alpine` builder → `alpine:3.23` runtime; non-root user |
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
| `bc_gateway_cowboy.erl` gen_server wrap | ✅ | Proper gen_server; `terminate/2` calls `cowboy:stop_listener/1`; supervisor owns our pid, not Ranch's |
| `beamclaw_gateway_app.erl` `prep_stop/1` removed | ✅ | No longer needed; gen_server terminate handles cleanup |

### Post-M20 — Default Port Change (8080 → 18800) + BEAMCLAW_PORT ✅

| Task | Status | Notes |
|------|--------|-------|
| `sys.config` port update | ✅ | `#{port => 8080}` → `#{port => 18800}` |
| `sys.docker.config` port update | ✅ | Same change |
| `bc_gateway_cowboy.erl` env var | ✅ | Resolves `BEAMCLAW_PORT` at startup; falls back to config |
| `beamclaw_cli.erl` macro + TUI config | ✅ | `?GATEWAY_PORT` → 18800; `apply_tui_config` resolves `BEAMCLAW_PORT` |
| `Dockerfile` update | ✅ | `EXPOSE 18800`; healthcheck URL updated |
| Docs update | ✅ | README, running.md, configuration.md, CLAUDE.md |

### Post-M20 — Sync sys.docker.config ✅

| Task | Status | Notes |
|------|--------|-------|
| `sys.docker.config` full sync | ✅ | Added kernel logger (console-only), default_agent, session persistence/sharing/cleanup, skills, dm_policy, allow_from |
| `docs/configuration.md` update | ✅ | Docker-Specific Config section lists both differences (TUI + logger) |

### Post-M20 — Docker CLI Control (`beamclaw-ctl`) ✅

| Task | Status | Notes |
|------|--------|-------|
| `Dockerfile` escriptize step | ✅ | `rebar3 escriptize` in build stage |
| `Dockerfile` escript copy | ✅ | Copied as `/opt/beamclaw/beamclaw-ctl` |
| `Dockerfile` wrapper script | ✅ | `/usr/local/bin/beamclaw-ctl` runs escript via release ERTS |
| `docs/running.md` update | ✅ | Docker CLI control section with usage examples |

---

### Post-M20 — Workspace Memory: Bootstrap File Routing + Memory Flush ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_tool_workspace_memory` bootstrap actions | ✅ | `read_bootstrap`/`update_bootstrap` with `file` param; allowlist: IDENTITY/USER/SOUL/TOOLS/AGENTS |
| AGENTS.md template: file routing table | ✅ | Explicit routing rules for identity/user/soul/tools/memory/daily |
| AGENTS.md template: memory review cycle | ✅ | Daily logs as inbox, MEMORY.md as archive; promote/prune guidance |
| MEMORY.md template: "What Does NOT Belong Here" | ✅ | Redirects identity/user/personality info to correct files |
| `bc_loop` pre-compaction memory flush | ✅ | Hidden LLM turn before compaction; executes workspace_memory tool calls |
| `memory_flush` config key | ✅ | `agentic_loop` map; default `true`; both sys.config files |
| EUnit tests | ✅ | 8 new tests (26 total for bc_tool_workspace_memory_tests); 200 total |
| Docs update | ✅ | CLAUDE.md, architecture.md, configuration.md |

### Post-M20 — Strip LLM Thinking/Reasoning Tags ✅

| Task | Status | Notes |
|------|--------|-------|
| `bc_thinking.erl` | ✅ | Pure-function module; strips `<think>`, `<thinking>`, `<thought>`, `<antThinking>`, `<final>` tags |
| Code block preservation | ✅ | Skips tags inside fenced (` ``` `/`~~~`) and inline (`` ` ``) code blocks |
| Strict mode (unclosed tags) | ✅ | Truncates content from unclosed opening tag to prevent partial leak |
| `bc_provider_openrouter` integration | ✅ | `bc_thinking:strip/1` applied in `parse_response/1` before history entry |
| EUnit tests | ✅ | 17 tests: passthrough, all tag types, code preservation, strict mode, edge cases |
| CLAUDE.md update | ✅ | `bc_thinking.erl` added to File Layout |

### Post-M20 — Docker Compose ✅

| Task | Status | Notes |
|------|--------|-------|
| `docker-compose.yml` | ✅ | Single-service compose file; env_file, named volume, healthcheck |
| `.env.example` | ✅ | Template with required/optional env vars (committed) |
| `.env` placeholder | ✅ | Working env file with placeholders (gitignored) |
| `README.md` update | ✅ | Docker quick-start uses compose as primary method |
| `docs/running.md` update | ✅ | Docker Compose subsection under Mode 3 |

### M21 — BM25 Keyword Search ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_bm25` | ✅ | Pure-function BM25: tokenize, TF-IDF, corpus_stats, score, rank |
| `bc_memory` behaviour update | ✅ | Added optional `search/4` callback |
| `bc_memory_ets` BM25 recall | ✅ | Replaced `query_matches` stub with BM25 scoring |
| `bc_memory_mnesia` BM25 recall | ✅ | Same BM25 scoring for Mnesia backend |
| `bc_tool_workspace_memory` search action | ✅ | BM25 search across MEMORY.md + daily logs |
| EUnit tests | ✅ | 14 (bc_bm25_tests) + 6 (bc_memory_ets_search_tests) + 4 (workspace search) |

### M22 — Vector Semantic Search + Hybrid Merge ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_vector` | ✅ | Cosine similarity, L2 normalize, dot product |
| `bc_chunker` | ✅ | Overlapping word-boundary text chunking |
| `bc_hybrid` | ✅ | Min-max normalize, weighted BM25+vector merge, min_score filter |
| `bc_embedding` | ✅ | OpenAI-compatible embedding API client via httpc |
| `bc_embedding_cache` | ✅ | gen_server, ETS cache with 24h TTL, agent invalidation |
| `bc_memory_mnesia.hrl` update | ✅ | Added `embedding :: [float()] | undefined` field |
| `beamclaw_memory_app` migration | ✅ | `maybe_transform_table/0` adds embedding column |
| `beamclaw_memory_sup` update | ✅ | `bc_embedding_cache` as permanent child |
| `bc_tool_workspace_memory` hybrid | ✅ | `mode` param: keyword/semantic/hybrid (default) |
| Config updates | ✅ | `embedding` + `search` maps in sys.config |
| EUnit tests | ✅ | 11 (vector) + 6 (chunker) + 7 (hybrid) + 6 (embedding_cache) + 6 (embedding) |

### M23 — Loop Integration + Search Polish ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_loop` auto-context | ✅ | Optional BM25 search of MEMORY.md before LLM call; `auto_context => false` default |
| `bc_tool_workspace_memory` search_all | ✅ | Cross-source search: all bootstrap files + daily logs |
| `bc_tool_workspace_memory` attribution | ✅ | Source + relevance % in results; truncated snippets |
| `bc_compactor` async embed | ✅ | Fire-and-forget embed of compaction summary |
| Config updates | ✅ | `auto_context`, `auto_context_limit`, `workspace_files`, `daily_log_lookback` |
| ADR-014 | ✅ | Pure Erlang BM25 + embedding hybrid search |
| Docs update | ✅ | CLAUDE.md, architecture.md, configuration.md, running.md |
| EUnit tests | ✅ | 4 (search_all + keyword mode) |

### M24 — Telegram Photo/Vision Support ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_types.hrl` update | ✅ | `attachments` field added to `#bc_message{}` and `#bc_channel_message{}` |
| `bc_telegram_photo` | ✅ | Pure-function module: extract, download, validate, base64 encode |
| `bc_channel_telegram` update | ✅ | Photo detection, download via `bc_telegram_photo`, threading attachments |
| `bc_loop` update | ✅ | Thread attachments from `bc_channel_message` to `bc_message` |
| `bc_provider_openrouter` update | ✅ | Multimodal `message_to_map/1`: content array with text + image_url parts |
| `bc_session_store` update | ✅ | v2 serialization; v1→v2 migration appends `attachments = []` |
| Config updates | ✅ | `photo` map in telegram channel config (enabled, max_size_bytes) |
| EUnit tests | ✅ | 25 new: 16 (photo), 7 (vision provider), 2 (session store migration) |
| Docs update | ✅ | CLAUDE.md, STATUS.md, configuration.md |

### M25 — Sandbox Foundation (Docker Lifecycle + Bridge) ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `beamclaw_sandbox` app scaffold | ✅ | New OTP app: app.src, app, sup; deps: kernel, stdlib, jsx, obs, tools |
| `bc_sandbox_registry` | ✅ | gen_server; ETS `{session_id, scope}` → pid; PID monitors |
| `bc_sandbox_sup` | ✅ | simple_one_for_one for bc_sandbox gen_servers |
| `bc_sandbox` | ✅ | Per-sandbox Docker lifecycle gen_server; Unix socket bridge; exec_script/4 |
| `bc_sandbox_docker` | ✅ | Pure: Docker run/exec/kill/rm arg building; security flags |
| `bc_sandbox_bridge` | ✅ | JSON-RPC 2.0 encode/decode; dispatch search_tools/get_tool_schema/call_tool |
| `Dockerfile.sandbox` | ✅ | python:3.12-alpine; bridge module; non-root user |
| `beamclaw_bridge.py` | ✅ | Python bridge: search_tools, get_tool, call_tool over Unix socket |
| `rebar.config` update | ✅ | Added beamclaw_sandbox to escript_incl_apps, shell.apps, relx.release |
| `sys.config` + `sys.docker.config` | ✅ | Full sandbox config section (enabled=false default) |
| EUnit tests | ✅ | 42 tests: docker (19) + bridge (17) + registry (6) |

### M26 — `bc_tool_exec` + Progressive Tool Discovery ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_tool_exec` | ✅ | bc_tool behaviour in beamclaw_sandbox; params: script, language, timeout, skill, save_as |
| `bc_sandbox_discovery` | ✅ | Generate /tools/ filesystem: index.txt, descriptions, schemas |
| `bc_tool_registry` update | ✅ | Added `list_names/0`, `get_definition/1` APIs |
| `bc_loop` update | ✅ | Inject `tool_bridge_fn` into Context map via `make_tool_bridge_fn/1` |
| `beamclaw_sandbox_app` update | ✅ | Register bc_tool_exec on start when sandbox enabled |
| EUnit tests | ✅ | 13 tests: tool_exec (7) + discovery (6) |

### M27 — PII Tokenization ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_pii_tokenizer` | ✅ | gen_server; ETS forward/reverse mapping; token format `<<PII:tok_NNNN>>` |
| Patterns | ✅ | OpenAI keys, GitHub PATs, AWS keys, Bearer tokens, emails, credit cards, SSN, phone numbers |
| Custom patterns | ✅ | Configurable via `#{patterns => [...]}` on start_link |
| EUnit tests | ✅ | 15 tests: all patterns, roundtrip, idempotency, clear, custom, edge cases |

### M28 — Tool Policy + Security Hardening ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_sandbox_policy` | ✅ | Pure: `check/2` with exact match + wildcard glob (`<<"mcp:*">>`); default rules |
| `bc_sandbox_env` | ✅ | Pure: allowlist/blocklist env var filtering; `is_dangerous/1` |
| EUnit tests | ✅ | 19 tests: policy (10) + env (9) |

### M29 — Skills Persistence + Code Pattern Caching ✅

| Module/Task | Status | Notes |
|-------------|--------|-------|
| `bc_sandbox_skills` | ✅ | Pure: generate/extract SKILL.md; save/load scripts; hash-based versioning |
| EUnit tests | ✅ | 9 tests: format, hash, language, extract, roundtrip, save/load |

### M30 — CLI, Docs, Integration ✅

| Task | Status | Notes |
|------|--------|-------|
| `beamclaw sandbox status` | ✅ | Docker availability, image status, config |
| `beamclaw sandbox list` | ✅ | Active sandbox containers |
| `beamclaw sandbox kill ID` | ✅ | Force-kill a sandbox container |
| `beamclaw sandbox build` | ✅ | Build sandbox Docker image from priv/docker/ |
| `cmd_doctor` Docker check | ✅ | Docker + sandbox image availability |
| CLAUDE.md update | ✅ | File Layout, Dep Graph, Supervision Trees, Config, Commands |
| STATUS.md update | ✅ | M25–M30 milestones |
| DECISIONS.md update | ✅ | ADR-015 (Docker sandbox), ADR-016 (PII tokenization) |
| docs/architecture.md update | ✅ | Eight-app graph, sandbox supervision tree, sandbox execution section |
| docs/configuration.md update | ✅ | Sandbox config keys |
| docs/running.md update | ✅ | Sandbox CLI commands, setup and usage |
| docs/building.md update | ✅ | Sandbox image build instructions |

### Post-M30 — Docker Sibling Container Support ✅

| Task | Status | Notes |
|------|--------|-------|
| `Dockerfile` update | ✅ | Install `docker-cli` in runtime stage; pre-create bridge socket dir |
| `docker-compose.yml` update | ✅ | Mount Docker socket + bridge dir |
| `docs/running.md` update | ✅ | Sandbox in Docker deployments subsection |
| `docs/configuration.md` update | ✅ | Docker socket note in beamclaw_sandbox section |
| `docs/building.md` update | ✅ | Docker deployment note for sandbox image build |
| `CLAUDE.md` update | ✅ | Docker socket comment in Configuration section |

### Post-M30 — Sandbox in Docker E2E Verification + Fixes ✅

| Task | Status | Notes |
|------|--------|-------|
| Entrypoint Docker socket group fix | ✅ | Auto-detect socket GID, create group, add beamclaw user; replaces `group_add`/`DOCKER_GID` |
| `docker cp` → `docker exec` base64 | ✅ | `docker cp` fails on `--read-only` containers; use `docker exec` + base64 for script injection |
| Bash → sh in sandbox | ✅ | Alpine sandbox image has `sh` not `bash`; `bc_sandbox_docker:exec_args/3` updated |
| `docker-compose.yml` cleanup | ✅ | Removed `group_add` (entrypoint handles it); removed `DOCKER_GID` from `.env.example` |
| `docs/running.md` update | ✅ | Simplified sandbox-in-Docker setup (no manual GID config) |
| 409 EUnit tests pass | ✅ | All tests green including updated `exec_args_bash_test` |

---

## Known Issues / Blockers

_None at this time._

---

## Last Updated

2026-02-26 (Sandbox in Docker E2E Verification + Fixes — Post-M30)
