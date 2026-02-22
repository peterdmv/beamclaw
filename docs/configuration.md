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
| `BEAMCLAW_COOKIE` | No | Erlang cluster cookie (default: `beamclaw_dev`) |
| `BEAMCLAW_HOME` | No | Override workspace base directory (default: `~/.beamclaw`) |
| `BEAMCLAW_AGENT` | No | Default agent name for TUI sessions (default: `default`) |

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
        max_tool_iterations  => 10,
        %% Trigger context compaction when history exceeds this many messages.
        compaction_threshold => 50,
        %% Keep this many recent messages verbatim after compaction.
        compaction_target    => 20,
        %% Progressive streaming: send chunks of this many characters.
        stream_chunk_size    => 80
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
    {default_agent, <<"default">>}
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

### beamclaw_gateway

```erlang
{beamclaw_gateway, [
    %% HTTP gateway configuration.
    {http, #{
        port => 8080    %% TCP port for Cowboy listener
    }},

    %% Channel configuration.
    {channels, [
        {telegram, #{
            token => {env, "TELEGRAM_BOT_TOKEN"},
            mode  => long_poll   %% or: webhook
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
    {backend, ets}
]}
```

To enable persistent memory, switch to `{backend, mnesia}` and ensure Mnesia schema has
been created on the node:

```erlang
mnesia:create_schema([node()]).
```

This is typically done in a release start hook or setup script. In dev/test shells the
backend automatically falls back to `ram_copies`.

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

`config/sys.docker.config` is identical to `sys.config` with one change:

```erlang
{tui, #{enabled => false}}
```

The TUI reads from stdin, which is not available in detached Docker containers. The
`docker` rebar3 profile (`rebar3 as docker release`) automatically selects this config.

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
