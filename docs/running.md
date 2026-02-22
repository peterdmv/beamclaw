# Running BeamClaw

## The `beamclaw` CLI

The recommended way to run BeamClaw is via the `beamclaw` escript CLI, built with
`rebar3 escriptize` (see `docs/building.md`). It bundles all six OTP apps into a
single self-contained binary at `_build/default/bin/beamclaw`.

```
Usage: beamclaw <command>

Commands:
  tui              Start interactive TUI chat (default when no command given)
  start            Start gateway as background daemon
  stop             Stop running daemon
  restart          Stop then start daemon
  remote_console   Print erl -remsh command to attach a live Erlang shell
  doctor           Check environment and connectivity (OTP version, API keys, epmd)
  status           Ping running gateway HTTP health endpoint
  version          Print version
  help             Show this help
```

### Quick start — TUI

```bash
export OPENROUTER_API_KEY=sk-or-...
_build/default/bin/beamclaw tui
# or simply:
_build/default/bin/beamclaw
```

Type a message and press Enter. Use Ctrl+D (EOF) to quit.

### Daemon mode

```bash
export OPENROUTER_API_KEY=sk-or-...
_build/default/bin/beamclaw start          # start background daemon
_build/default/bin/beamclaw status         # check HTTP health
_build/default/bin/beamclaw remote_console # print remsh command
_build/default/bin/beamclaw stop           # graceful shutdown
```

Daemon IPC uses Erlang distribution; `epmd` must be available (`epmd -daemon`
if not already running). The daemon node registers as `beamclaw@<hostname>`
(short hostname from `inet:gethostname/0`).

### Viewing daemon logs

The daemon writes logs to `/tmp/beamclaw_daemon.log` via OTP's kernel logger file
handler. To follow logs in real time:

```bash
tail -f /tmp/beamclaw_daemon.log
```

Log rotation is automatic: 5 MB per file, 3 files retained. The file handler captures
`debug`-level events, including message dispatch traces for Telegram and the agentic loop.

### Remote TUI (daemon + tui)

When a daemon is already running, `beamclaw tui` auto-detects it and connects
via Erlang distribution instead of starting a second in-process gateway:

```bash
# Terminal 1: start the daemon
_build/default/bin/beamclaw start

# Terminal 2: attach a remote TUI
_build/default/bin/beamclaw tui
# Prints: BeamClaw TUI (remote) — connected to beamclaw@<hostname>
# Type messages; responses stream back from the daemon.
# Ctrl+D to disconnect.
```

Each remote TUI gets its own session (unique ID). Multiple remote TUIs can
connect simultaneously. If the daemon dies mid-conversation, the TUI prints
`[daemon disconnected]` and exits.

If no daemon is running, `beamclaw tui` falls back to the normal in-process
mode — no behaviour change from before.

### Environment variables

| Variable | Required | Description |
|---|---|---|
| `OPENROUTER_API_KEY` | Yes | LLM completions via OpenRouter |
| `OPENAI_API_KEY` | No | Alternative OpenAI provider |
| `TELEGRAM_BOT_TOKEN` | No | Enable Telegram channel |
| `BEAMCLAW_PORT` | No | Override gateway HTTP port (default: 8080) |

### Pre-flight check

```bash
_build/default/bin/beamclaw doctor
```

Checks OTP version, required env vars, `epmd` availability, and (if
`OPENROUTER_API_KEY` is set) OpenRouter API reachability.

---

## Mode 1 — Development Shell (rebar3 shell)

Best for: local development, debugging, the TUI channel.

```bash
# Export secrets — never hardcode these
export OPENROUTER_API_KEY=sk-or-...
export TELEGRAM_BOT_TOKEN=...        # omit if not using Telegram

rebar3 shell
```

The TUI channel starts automatically and reads from stdin. Type a message and press Enter
to send it to the agentic loop. Ctrl-C exits.

Hot-reload during development:

```erlang
1> reloader:reload().   % if you have a reloader plugin
2> l(bc_loop).          % reload a specific module
```

---

## Mode 2 — OTP Release (foreground or daemon)

Best for: staging, production-like testing on a host machine.

```bash
# Build the release first (see docs/building.md)
rebar3 release

# Set secrets
export OPENROUTER_API_KEY=sk-or-...
export TELEGRAM_BOT_TOKEN=...

# Start in foreground (logs to stdout)
_build/default/rel/beamclaw/bin/beamclaw foreground

# Or start as a background daemon
_build/default/rel/beamclaw/bin/beamclaw start

# Attach to a running daemon
_build/default/rel/beamclaw/bin/beamclaw remote_console

# Stop a running daemon
_build/default/rel/beamclaw/bin/beamclaw stop
```

The release reads `vm.args` and `sys.config` from the `releases/0.1.0/` directory inside
the release folder. To override settings without rebuilding, edit those files before
starting.

---

## Mode 3 — Docker

Best for: production deployment, zero Erlang installation required.

```bash
docker run -d \
  --name beamclaw \
  --restart unless-stopped \
  -e OPENROUTER_API_KEY=sk-or-... \
  -e TELEGRAM_BOT_TOKEN=...       \
  -p 8080:8080                    \
  beamclaw:latest
```

### Health check

```bash
wget -qO- http://localhost:8080/health
# {"status":"ok"}
```

The container includes a `HEALTHCHECK` that polls `/health` every 30 seconds. Docker will
mark the container unhealthy and restart it (with `--restart unless-stopped`) if the node
stops responding.

### Viewing logs

```bash
docker logs -f beamclaw
```

OTP logger output goes to stdout/stderr, captured by Docker's log driver.

### Stopping

```bash
docker stop beamclaw
docker rm beamclaw
```

---

## Channel Selection

### TUI (terminal)

Enabled by default in `sys.config` (`{tui, #{enabled => true}}`). Disabled in
`sys.docker.config` because Docker containers do not have a TTY in detached mode.

To use the TUI, run in rebar3 shell or as a foreground release with a terminal attached.

### Telegram

Set `TELEGRAM_BOT_TOKEN` and ensure the channel config in `sys.config` has
`mode => long_poll` (polling) or `mode => webhook` (requires a public HTTPS URL).

For webhook mode, configure your bot with:

```
https://api.telegram.org/bot<TOKEN>/setWebhook?url=https://your-host/webhook/telegram
```

### HTTP / WebSocket

The HTTP gateway is always enabled on port 8080 (configurable). Endpoints:

| Endpoint | Description |
|---|---|
| `GET /health` | Health check — `{"status":"ok"}` |
| `GET /metrics` | Prometheus metrics stub |
| `POST /v1/chat/completions` | OpenAI-compatible API (SSE streaming + sync) |
| `GET /ws` | WebSocket — send/receive messages |
| `POST /webhook/telegram` | Telegram webhook receiver |

---

## MCP Servers

Add external MCP servers in `sys.config` under `{beamclaw_mcp, [{servers, [...]}]}`:

```erlang
{beamclaw_mcp, [
    {servers, [
        {filesystem, #{command => "npx",
                       args    => ["-y", "@modelcontextprotocol/server-filesystem",
                                   "/home/user/workspace"]}},
        {github,     #{command => "npx",
                       args    => ["-y", "@modelcontextprotocol/server-github"],
                       env     => #{"GITHUB_PERSONAL_ACCESS_TOKEN" => {env, "GITHUB_PAT"}}}}
    ]}
]}
```

Each server entry starts a `bc_mcp_server` process that spawns the external command via
`erlang:open_port` and performs the JSON-RPC 2.0 handshake (`initialize` → `tools/list`).
Discovered tools are registered in `bc_mcp_registry` and become available to the agentic
loop alongside built-in tools.

MCP servers are restarted automatically by the supervisor (up to 5 times in 30 seconds)
if they crash.

---

## Verifying the Setup

After starting BeamClaw in any mode:

1. **Health endpoint**: `wget -qO- http://localhost:8080/health` should return `{"status":"ok"}`
2. **Log output**: look for `agent_start` and `session_start` events in the logs
3. **TUI** (shell mode): type a message — the agentic loop should respond
4. **Telegram**: send `/start` to your bot
