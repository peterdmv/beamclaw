# Configuration Reference

BeamClaw is configured via two files:

- `config/sys.config` — OTP application environment (Erlang terms)
- `config/vm.args` — BEAM VM flags

Secrets are **never** hardcoded. All sensitive values use the `{env, "VAR_NAME"}` tuple,
resolved at runtime by `bc_config:get/2` via `os:getenv/1`.

---

## Environment Variables

| Variable | Required | Description |
|---|---|---|
| `OPENROUTER_API_KEY` | Yes (if using OpenRouter) | OpenRouter API key (`sk-or-...`) |
| `OPENAI_API_KEY` | Yes (if using OpenAI) | OpenAI API key (`sk-...`) |
| `TELEGRAM_BOT_TOKEN` | Only if using Telegram | Telegram bot token from @BotFather |
| `BEAMCLAW_PORT` | No | Override gateway HTTP port (default: `18800`) |
| `BEAMCLAW_COOKIE` | No | Erlang cluster cookie (default: `beamclaw_dev`) |
| `BEAMCLAW_HOME` | No | Override workspace base directory (default: `~/.beamclaw`) |
| `BEAMCLAW_AGENT` | No | Default agent name for TUI sessions (default: `default`) |
| `BEAMCLAW_USER` | No | Canonical user identity for cross-channel session sharing. When set, all channels (TUI, Telegram, HTTP, WebSocket) use this value as-is (no prefix), enabling a single shared session across channels. When unset, each channel prefixes user IDs independently. |
| `BRAVE_API_KEY` | No | Brave Search API key for the `web_search` built-in tool. Get one at [brave.com/search/api](https://brave.com/search/api/). When unset, the tool returns a configuration error. |
| `BEAMCLAW_EMBEDDING_API_KEY` | No | API key for the embedding service (OpenAI-compatible). When unset, semantic/hybrid search degrades to BM25 keyword-only. |
| `BEAMCLAW_EMBEDDING_URL` | No | Base URL for the embedding API (default: `https://api.openai.com/v1`). Supports OpenAI, Azure OpenAI, Ollama, or any OpenAI-compatible endpoint. |
| `BEAMCLAW_EMBEDDING_MODEL` | No | Embedding model name (default: `text-embedding-3-small`). |

At least one of `OPENROUTER_API_KEY` or `OPENAI_API_KEY` must be set, depending on
`default_provider`.

Set these in your shell before starting, or pass them with `docker run -e`. Never commit
them to a file — `.env` and `*.secret` files are excluded by `.gitignore`.

---

## sys.config

### beamclaw_core

```erlang
{beamclaw_core, [
    %% Which LLM provider to use by default.
    %% Supported: openrouter | openai
    {default_provider, openrouter},

    %% Per-provider configuration. api_key must be {env, "VAR"}.
    {providers, [
        {openrouter, #{
            api_key  => {env, "OPENROUTER_API_KEY"},
            base_url => "https://openrouter.ai/api/v1",
            model    => "anthropic/claude-sonnet-4-5"
        }},
        {openai, #{
            api_key  => {env, "OPENAI_API_KEY"},
            base_url => "https://api.openai.com/v1",
            model    => "gpt-4o"
        }}
    ]},

    %% Agentic loop tuning.
    {agentic_loop, #{
        %% Maximum number of tool call iterations per turn before giving up.
        max_tool_iterations      => 10,
        %% Trigger context compaction when history tokens exceed this
        %% percentage of the model's context window. Token estimation
        %% uses byte_size/4 (~4 chars per token).
        compaction_threshold_pct => 80,
        %% After compaction, keep recent messages whose cumulative tokens
        %% fit within this percentage of the context window.
        compaction_target_pct    => 40,
        %% Progressive streaming: send chunks of this many characters.
        stream_chunk_size        => 80,
        %% Pre-compaction memory flush: fire a hidden LLM turn before compacting
        %% to let the agent save durable memories to workspace files.
        memory_flush             => true,
        %% Auto-context: BM25 search of MEMORY.md before each LLM call.
        %% Injects top matching snippets as context. Off by default to
        %% avoid unnecessary token usage.
        auto_context             => false,
        %% Maximum number of auto-context snippets to inject per turn.
        auto_context_limit       => 3
    }},

    %% Default autonomy level for new sessions.
    %% read_only  — tools that write/execute require approval; approval is auto-denied
    %%              for destructive tools (bash, terminal, write_file).
    %% supervised — destructive tools require explicit approval per tool call.
    %% full       — all tools run without approval prompts.
    {autonomy_level, supervised},

    %% How long an idle session is retained (seconds). 0 = never expire.
    {session_ttl_seconds, 3600},

    %% Default agent workspace. Sessions use this agent unless overridden
    %% via --agent flag (TUI), agent_id request field (HTTP/WS), or
    %% BEAMCLAW_AGENT env var.
    {default_agent, <<"default">>},

    %% Session persistence: store history in Mnesia across restarts.
    %% true  — history survives VM restarts (default)
    %% false — in-memory only; history lost on restart
    {session_persistence, true},

    %% Session sharing across channels.
    %% shared      — same user + same agent = same session regardless of channel (default)
    %% per_channel — separate session per channel (legacy behaviour)
    {session_sharing, shared},

    %% How often to scan for expired sessions (ms). Default: 5 minutes.
    {session_cleanup_interval_ms, 300000}
]}
```

### beamclaw_mcp

```erlang
{beamclaw_mcp, [
    %% List of MCP server definitions.
    %% Each entry: {ServerName :: atom(), Config :: map()}
    {servers, [
        %% Example: filesystem MCP server via npx
        %% {filesystem, #{
        %%     command => "npx",
        %%     args    => ["-y", "@modelcontextprotocol/server-filesystem", "/workspace"]
        %% }}
    ]}
]}
```

Each configured server is started as a `bc_mcp_server` gen_server that owns a stdio port.
Tools discovered via `tools/list` are registered in `bc_mcp_registry` and made available
to the agentic loop.

### beamclaw_tools

```erlang
{beamclaw_tools, [
    %% Brave Search API configuration for the web_search built-in tool.
    %% The tool is always registered but returns a helpful error at execute
    %% time if the API key is not configured.
    {web_search, #{
        api_key     => {env, "BRAVE_API_KEY"},   %% required for web search
        max_results => 10                         %% upper bound for count parameter
    }}
]}
```

| Key | Type | Default | Description |
|---|---|---|---|
| `web_search.api_key` | `{env, Var}` | — | Brave Search API key. Get one at [brave.com/search/api](https://brave.com/search/api/) |
| `web_search.max_results` | integer | `10` | Maximum number of results the tool can return |

### beamclaw_gateway

```erlang
{beamclaw_gateway, [
    %% HTTP gateway configuration.
    {http, #{
        port => 18800   %% TCP port for Cowboy listener; override with BEAMCLAW_PORT env var
    }},

    %% Channel configuration.
    {channels, [
        {telegram, #{
            token => {env, "TELEGRAM_BOT_TOKEN"},
            mode  => long_poll,  %% or: webhook
            %% Access control policy for direct messages.
            %% pairing   — unknown users get a code; blocked until approved (default)
            %% allowlist — unknown users silently dropped; no codes issued
            %% open      — no access control (not recommended)
            dm_policy  => pairing,
            %% Static allowlist of Telegram user IDs (binaries).
            %% Merged with the pairing store — users here are always allowed.
            allow_from => [],
            %% Photo/vision support.
            photo => #{
                enabled        => true,       %% set false to ignore photos
                max_size_bytes => 5242880     %% 5 MB; photos over this are skipped
            },
            %% Voice message transcription (speech-to-text).
            %% Uses Groq Whisper (OpenAI-compatible /audio/transcriptions API).
            voice => #{
                enabled              => true,
                max_duration_seconds => 120,          %% skip voice > 2 min
                stt_base_url => "https://api.groq.com/openai/v1",
                stt_api_key  => {env, "GROQ_API_KEY"},
                stt_model    => "whisper-large-v3-turbo"
            }
        }},
        {tui, #{
            enabled => true      %% set false in Docker (sys.docker.config)
        }}
    ]}
]}
```

### beamclaw_memory

```erlang
{beamclaw_memory, [
    %% Memory backend.
    %% ets    — ephemeral in-process ETS table; lost on restart (default).
    %% mnesia — persistent Mnesia table (disc_copies when schema exists,
    %%           ram_copies in dev/test).
    {backend, ets},

    %% Embedding configuration for semantic search.
    %% When enabled and an API key is set, memory entries and workspace
    %% text are embedded for vector similarity search. When disabled or
    %% unconfigured, search degrades gracefully to BM25 keyword-only.
    {embedding, #{
        enabled    => true,
        model      => {env, "BEAMCLAW_EMBEDDING_MODEL"},   %% default: text-embedding-3-small
        base_url   => {env, "BEAMCLAW_EMBEDDING_URL"},     %% default: https://api.openai.com/v1
        api_key    => {env, "BEAMCLAW_EMBEDDING_API_KEY"}, %% required for semantic search
        dimensions => 1536                                  %% embedding vector size
    }},

    %% Search tuning parameters.
    {search, #{
        %% Hybrid score weights (must sum to 1.0).
        vector_weight     => 0.6,
        bm25_weight       => 0.4,
        %% Minimum hybrid score to include a result.
        min_score         => 0.1,
        %% Default number of results returned by search actions.
        default_limit     => 5,
        %% Text chunking for embedding: words per chunk and overlap.
        chunk_size        => 200,
        chunk_overlap     => 50,
        %% Workspace files to index for search_all action.
        workspace_files   => [<<"MEMORY.md">>, <<"SOUL.md">>, <<"IDENTITY.md">>,
                              <<"USER.md">>, <<"TOOLS.md">>, <<"AGENTS.md">>],
        %% Number of past daily logs to include in search_all.
        daily_log_lookback => 7
    }}
]}
```

To enable persistent memory, switch to `{backend, mnesia}` and ensure Mnesia schema has
been created on the node:

```erlang
mnesia:create_schema([node()]).
```

This is typically done in a release start hook or setup script. In dev/test shells the
backend automatically falls back to `ram_copies`.

### beamclaw_sandbox

```erlang
{beamclaw_sandbox, [
    %% Master switch — sandbox features require Docker.
    %% When false (default), bc_tool_exec is not registered and all
    %% sandbox features are disabled with zero overhead.
    %% When running BeamClaw in Docker, the host Docker socket must be
    %% mounted (-v /var/run/docker.sock:/var/run/docker.sock) for sandbox
    %% to work. See docs/running.md "Sandbox in Docker deployments".
    {enabled, false},

    %% Docker image used for sandbox containers.
    %% Build with: beamclaw sandbox build
    {docker_image, "beamclaw-sandbox:latest"},

    %% Sandbox scope: how containers are shared.
    %% session — one container per session (default)
    %% agent   — one container per agent
    %% shared  — single global container
    {scope, session},

    %% Script execution timeout (seconds).
    {timeout_seconds, 60},

    %% Orphan container reaper sweep interval (milliseconds).
    %% The reaper periodically scans for beamclaw-sbx-* containers
    %% not tracked in the registry and kills/removes them.
    {reaper_interval_ms, 60000},

    %% Docker resource limits.
    {memory_limit, "512m"},
    {cpu_limit, "1.0"},

    %% Container network mode.
    %% none   — no network access (most secure, default)
    %% bridge — Docker bridge network
    %% host   — host network (not recommended)
    {network, none},

    %% Workspace mount mode inside container.
    %% none — no workspace access
    %% ro   — read-only (default)
    %% rw   — read-write
    {workspace_mount, ro},

    %% Maximum script output size (bytes). Output beyond this is truncated.
    {max_output_bytes, 1048576},   %% 1 MB

    %% Directory for Unix domain sockets connecting to containers.
    {bridge_socket_dir, "/tmp/beamclaw-bridges"},

    %% PII tokenization at sandbox boundary.
    {pii, #{
        enabled          => true,   %% enable bidirectional PII masking
        patterns         => [],     %% additional custom regex patterns
        tokenize_scripts => true,   %% mask secrets in scripts sent to container
        tokenize_results => true    %% mask secrets in results from container
    }},

    %% Tool access policy for bridge calls from container.
    {policy, #{
        default_action => allow,
        rules => [
            {allow, <<"read_file">>},
            {allow, <<"curl">>},
            {deny, <<"bash">>},
            {deny, <<"terminal">>}
        ]
    }},

    %% Environment variable filtering for container.
    %% Only allowlisted vars are passed; blocklisted vars are always excluded.
    {env_allowlist, [<<"PATH">>, <<"HOME">>, <<"LANG">>, <<"TERM">>]},
    {env_blocklist, [<<"OPENROUTER_API_KEY">>, <<"OPENAI_API_KEY">>,
                     <<"TELEGRAM_BOT_TOKEN">>, <<"AWS_SECRET_ACCESS_KEY">>]}
]}
```

### beamclaw_scheduler

```erlang
{beamclaw_scheduler, [
    {enabled, false},                      %% opt-in; creates bc_tool_scheduler when true
    {max_jobs_per_agent, 50},              %% max active/paused jobs per agent
    {default_autonomy, supervised},        %% autonomy level for scheduled sessions
    {max_errors, 3},                       %% consecutive failures → auto-pause
    {heartbeat, #{
        default_interval_ms => 1800000,    %% 30 min default heartbeat interval
        suppress_ok => true,               %% suppress HEARTBEAT_OK output
        active_hours => {8, 22}            %% UTC hour range (skip outside)
    }}
]}
```

| Key | Type | Default | Description |
|---|---|---|---|
| `enabled` | boolean | `false` | Enable the scheduler and register the `scheduler` tool |
| `max_jobs_per_agent` | integer | `50` | Maximum number of active/paused jobs per agent |
| `default_autonomy` | atom | `supervised` | Autonomy level for scheduled sessions |
| `max_errors` | integer | `3` | Consecutive failures before auto-pausing a job |
| `heartbeat.default_interval_ms` | integer | `1800000` | Default heartbeat interval (30 minutes) |
| `heartbeat.suppress_ok` | boolean | `true` | Suppress delivery when LLM responds `HEARTBEAT_OK` |
| `heartbeat.active_hours` | tuple | `{8, 22}` | UTC hour range for heartbeat delivery |

### beamclaw_obs

```erlang
%% Observability backends are registered at startup.
%% Currently only the OTP logger backend is active.
%% A Prometheus backend can be added as a bc_observer implementation
%% that joins the bc_obs_backends pg group — no changes to existing code needed.
{beamclaw_obs, []}
```

### kernel (OTP logger)

```erlang
{kernel, [
    {logger_level, info},
    {logger, [
        %% Console handler for foreground modes (rebar3 shell, TUI)
        {handler, default, logger_std_h, #{
            level => info,
            formatter => {logger_formatter, #{
                template => [time, " [", level, "] ", msg, "\n"]
            }}
        }},
        %% File handler for daemon mode (stdout goes nowhere with -detached)
        {handler, file, logger_std_h, #{
            level => debug,
            config => #{
                file => "/tmp/beamclaw_daemon.log",
                max_no_bytes => 5242880,       %% 5 MB per file
                max_no_files => 3              %% keep 3 rotated files
            },
            formatter => {logger_formatter, #{
                template => [time, " [", level, "] ", msg, "\n"]
            }}
        }}
    ]}
]}
```

The file handler writes to `/tmp/beamclaw_daemon.log` with automatic rotation (5 MB per
file, 3 files retained = 15 MB max disk usage). The `debug` level on the file handler
captures all trace events; the console handler stays at `info` to avoid noise in
interactive modes.

To change the log level at runtime (e.g., enable debug on the console handler):

```erlang
logger:set_handler_config(default, level, debug).
```

---

## vm.args

Key flags in `config/vm.args`:

| Flag | Default | Description |
|---|---|---|
| `-sname beamclaw` | — | Local node name (no distribution). Use `-name` for clustered deployments. |
| `-setcookie beamclaw_dev` | — | Cluster cookie. Override via `ERL_FLAGS="-setcookie $BEAMCLAW_COOKIE"`. |
| `+S auto` | auto | Scheduler threads — matches available CPU cores. |
| `+A 32` | 32 | Async I/O thread pool size. |
| `+Q 65536` | 65536 | Max open ports (file descriptors). |
| `+sbwt none` | none | Disable scheduler busy-wait (saves CPU when idle). |

---

## Docker-Specific Config (sys.docker.config)

`config/sys.docker.config` is identical to `sys.config` with these differences:

1. **TUI disabled**: `{tui, #{enabled => false}}` — stdin is not available in detached
   Docker containers.
2. **Console-only logger**: The `kernel` logger has only the console handler (no file
   handler). Docker captures stdout via its log driver, so writing to a log file inside
   the container is unnecessary.

The `docker` rebar3 profile (`rebar3 as docker release`) automatically selects this config.

---

## Skills

Skills are configured via `sys.config` under `beamclaw_core`:

```erlang
{skills, #{
    %% Per-skill overrides (optional)
    %% entries => #{<<"skill-name">> => #{enabled => false}}
}}
```

| Key | Default | Description |
|-----|---------|-------------|
| `skills` | `#{}` | Skills configuration map |

Skills are discovered from the filesystem. Use `beamclaw skills list` to see
available skills and `beamclaw skills status` for detailed requirements info.

Environment variables:
- `BEAMCLAW_HOME` — overrides the base directory for skills (default: `~/.beamclaw/`)

---

## Adding a New API Integration

When adding a new provider or external service:

1. Add its API key to `sys.config` as `{env, "NEW_SERVICE_API_KEY"}`.
2. If the key has a recognizable prefix (e.g., `xoxb-` for Slack), add a scrubber
   pattern to `bc_scrubber` in the same change.
3. Update this file with the new environment variable.
4. Update `.gitignore` if the integration produces any credential-bearing files.
