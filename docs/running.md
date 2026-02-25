# Running BeamClaw

## The `beamclaw` CLI

The recommended way to run BeamClaw is via the `beamclaw` escript CLI, built with
`rebar3 escriptize` (see `docs/building.md`). It bundles all six OTP apps into a
single self-contained binary at `_build/default/bin/beamclaw`.

```
Usage: beamclaw <command>

Commands:
  tui [--agent NAME]   Start interactive TUI chat (default when no command given)
  start                Start gateway as background daemon
  stop                 Stop running daemon
  restart              Stop then start daemon
  remote_console       Print erl -remsh command to attach a live Erlang shell
  agent create NAME    Create a new agent workspace
  agent list           List all agents
  agent show NAME      Show agent bootstrap files
  agent delete NAME    Delete an agent workspace
  agent rehatch NAME   Factory reset: restore all files to defaults
  sandbox status       Docker availability, image status, sandbox config
  sandbox list         Active sandbox containers
  sandbox kill ID      Force-kill a sandbox container
  sandbox build        Build sandbox Docker image
  pair [list]          List pending and approved pairing requests
  pair <channel> CODE  Approve a pending pairing request
  pair revoke CH ID    Revoke a user from a channel's allowlist
  doctor               Check environment and connectivity
  status               Ping running gateway HTTP health endpoint
  version              Print version
  help                 Show this help
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

Multiple remote TUIs can connect simultaneously. If the daemon dies mid-conversation,
the TUI prints `[daemon disconnected]` and exits.

If no daemon is running, `beamclaw tui` falls back to the normal in-process
mode — no behaviour change from before.

### Session sharing across channels

By default (`session_sharing = shared`), BeamClaw derives session IDs
deterministically from `{user_id, agent_id}`. This means the same user talking
to the same agent will share a single conversation across all channels.

**Cross-channel sharing with `BEAMCLAW_USER`**: When the `BEAMCLAW_USER`
environment variable is set, **all channels** use that value as the canonical
user identity (no channel prefix). This ensures the same session is used
whether you talk via TUI, Telegram, HTTP, or WebSocket:

```bash
# Set canonical identity on the daemon
export BEAMCLAW_USER=peter
beamclaw start

# Terminal 1: Telegram — tell agent your name
# Terminal 2: TUI — agent remembers across channels
beamclaw tui
> What's my name?
# Agent remembers from the Telegram conversation
```

When `BEAMCLAW_USER` is **not set**, each channel prefixes user IDs
independently, which means cross-channel sharing only works if the prefixed
IDs happen to match (they won't across different channel types):

| Channel | User ID format | Source |
|---------|---------------|--------|
| TUI | `local:<username>` | `USER` env → `anonymous` |
| Telegram | `tg:<telegram_user_id>` | From Telegram message `from.id` |
| HTTP | `api:<user_id>` | `X-User-Id` header or `user_id` in body |
| WebSocket | `ws:<user_id>` | `user_id` in message payload |

| `BEAMCLAW_USER` | TUI user_id | Telegram user_id | HTTP user_id |
|-----------------|-------------|-------------------|--------------|
| Not set | `local:<USER>` | `tg:<tg_id>` | `api:<header>` |
| Set to `peter` | `peter` | `peter` | `peter` |

To isolate sessions per channel (legacy behaviour), set `{session_sharing, per_channel}`
in `sys.config`.

### Session persistence

Session history is persisted to Mnesia by default (`session_persistence = true`).
Conversations survive VM restarts:

```bash
beamclaw start
BEAMCLAW_USER=alice beamclaw tui --agent default
> Remember my favourite colour is blue
# ... quit ...

beamclaw restart

BEAMCLAW_USER=alice beamclaw tui --agent default
> What's my favourite colour?
# Agent remembers: blue
```

Expired sessions are cleaned up automatically based on `session_ttl_seconds` (default: 1 hour).
Disable persistence with `{session_persistence, false}` in `sys.config`.

### Agent Management

Agents are named workspaces containing six markdown bootstrap files that define
the agent's personality, instructions, and memory. A `default` agent is created
automatically on first start.

```bash
# Create a new agent
_build/default/bin/beamclaw agent create my-assistant

# List all agents
_build/default/bin/beamclaw agent list

# Show an agent's bootstrap files
_build/default/bin/beamclaw agent show my-assistant

# Start TUI with a specific agent
_build/default/bin/beamclaw tui --agent my-assistant

# Factory reset an agent (restore all files to defaults, wipe daily logs)
_build/default/bin/beamclaw agent rehatch my-assistant

# Delete an agent (cannot delete "default")
_build/default/bin/beamclaw agent delete my-assistant
```

Edit the agent's files directly in `~/.beamclaw/agents/<name>/` to customize
its personality (SOUL.md), identity (IDENTITY.md), user context (USER.md),
tool guidance (TOOLS.md), workspace rules (AGENTS.md), and memory (MEMORY.md).

The `BEAMCLAW_AGENT` env var sets the default agent name when `--agent` is
not specified.

### Skill Management

Skills extend your agent's capabilities with domain-specific knowledge.

#### Discovering skills

```bash
beamclaw skills list           # List all discovered skills with status
beamclaw skills status         # Detailed requirements check for each skill
beamclaw skills show NAME      # Show a skill's SKILL.md content
beamclaw skills install NAME   # Install a skill's dependencies
```

#### Creating a skill

1. Create a directory: `~/.beamclaw/skills/my-skill/`
2. Add a `SKILL.md` file with frontmatter:

```markdown
---
name: my-skill
description: What this skill does
metadata: {"beamclaw": {"requires": {"bins": ["curl"]}, "install": [{"kind": "apt", "package": "curl"}]}}
---

# My Skill

Instructions for the agent...
```

#### Skill precedence

Per-agent skills override global skills with the same name:

1. Bundled (lowest): shipped with BeamClaw
2. Global: `~/.beamclaw/skills/*/SKILL.md`
3. Per-agent (highest): `~/.beamclaw/agents/<name>/skills/*/SKILL.md`

### Memory Search

The agent can search its workspace memory (MEMORY.md, daily logs, bootstrap files) using
the `workspace_memory` tool's `search` and `search_all` actions.

#### Search modes

| Mode | Description | Requirements |
|------|-------------|--------------|
| `keyword` | BM25 keyword search (TF-IDF scoring) | None (pure Erlang, always available) |
| `semantic` | Vector similarity via embeddings | `BEAMCLAW_EMBEDDING_API_KEY` must be set |
| `hybrid` | Weighted combination of BM25 + vector scores | `BEAMCLAW_EMBEDDING_API_KEY` must be set |

When semantic or hybrid mode is requested but no embedding API key is configured, search
degrades gracefully to keyword-only mode.

#### Tool actions

The agent invokes these via the `workspace_memory` tool:

- **`search`** — search MEMORY.md content. Parameters: `query` (required), `limit` (optional,
  default 5), `mode` (optional: `keyword` | `semantic` | `hybrid`, default `keyword`).
- **`search_all`** — search across all workspace files (MEMORY.md, bootstrap files, and
  recent daily logs). Same parameters as `search`.

#### Enabling semantic search

To enable semantic/hybrid search modes, set the embedding API key:

```bash
export BEAMCLAW_EMBEDDING_API_KEY=sk-...
```

Optionally configure the endpoint and model (defaults to OpenAI):

```bash
export BEAMCLAW_EMBEDDING_URL=https://api.openai.com/v1    # or Ollama, Azure, etc.
export BEAMCLAW_EMBEDDING_MODEL=text-embedding-3-small
```

Any OpenAI-compatible `/v1/embeddings` endpoint is supported. Embeddings are cached in
ETS with a 24-hour TTL to avoid redundant API calls.

#### Auto-context (optional)

When `auto_context` is enabled in `sys.config` (default: `false`), the agentic loop
performs a BM25 search of MEMORY.md before each LLM call and injects matching snippets
as context. This is off by default to keep token usage predictable:

```erlang
{agentic_loop, #{
    auto_context       => true,   %% enable auto-context injection
    auto_context_limit => 3       %% max snippets per turn
}}
```

See `docs/configuration.md` for full search tuning parameters (weights, chunk sizes,
minimum scores).

### Sandbox Management

The sandbox system provides Docker-based code execution with MCP tool bridging.
It is opt-in and requires Docker.

#### Setup

1. Enable sandbox in `sys.config`: `{enabled, true}`
2. Build the sandbox Docker image:

```bash
beamclaw sandbox build
```

3. Verify setup:

```bash
beamclaw sandbox status
```

#### CLI commands

```bash
beamclaw sandbox status      # Docker availability, image status, config
beamclaw sandbox list        # Active sandbox containers
beamclaw sandbox kill ID     # Force-kill a sandbox container
beamclaw sandbox build       # Build sandbox Docker image from priv/docker/
```

#### How it works

When enabled, the `exec` tool becomes available to the agent. The agent writes
a Python or Bash script, which runs inside a Docker container with:

- No network access (`--network none`)
- No capabilities (`--cap-drop ALL`)
- Read-only filesystem (`--read-only`)
- Memory and CPU limits
- PII tokenization at all boundary crossings
- Tool access policy enforcement

The container can discover and call BeamClaw tools (built-in + MCP) via a
Python bridge module (`beamclaw_bridge`):

```python
from beamclaw_bridge import search_tools, call_tool

# Discover available tools
tools = search_tools("names")

# Call a tool
result = call_tool("read_file", path="/workspace/data.json")
```

See `docs/configuration.md` for sandbox configuration options and
`docs/architecture.md` for the full architecture description.

#### Sandbox in Docker deployments (sibling containers)

When BeamClaw itself runs in Docker, the sandbox uses the **sibling container** pattern:
the host's Docker socket is mounted into the BeamClaw container so it can spawn sandbox
containers as peers on the host Docker daemon.

The `docker-compose.yml` already includes the necessary mounts. To enable:

1. Find your host's Docker socket GID:

```bash
stat -c '%g' /var/run/docker.sock
# e.g. 999
```

2. Set `DOCKER_GID` in `.env`:

```bash
DOCKER_GID=999
```

3. Build the sandbox image on the host (sandbox containers run on the host daemon):

```bash
docker build -t beamclaw-sandbox:latest \
  -f apps/beamclaw_sandbox/priv/docker/Dockerfile.sandbox \
  apps/beamclaw_sandbox/priv/docker/
```

4. Enable sandbox in `config/sys.docker.config`: set `{enabled, true}` in the
   `beamclaw_sandbox` section.

5. Rebuild and start:

```bash
docker compose up -d --build
```

6. Verify:

```bash
docker exec beamclaw beamclaw-ctl sandbox status
# Should show: Docker: available, Image: ok
docker exec beamclaw docker ps
# Should list running containers (confirms Docker CLI works)
```

The bridge Unix sockets (`/tmp/beamclaw-bridges/`) are shared between the BeamClaw
container and sandbox containers via a bind mount, so both can access the same sockets.

### Telegram Pairing (Access Control)

By default, BeamClaw's Telegram channel uses **pairing** to control access.
Unknown users who message the bot receive a pairing code; the bot owner
approves the code via CLI, and the user is added to a persistent allowlist.

#### Pairing flow

1. Unknown user sends a message to the bot
2. Bot replies with a pairing code:
   ```
   BeamClaw: pairing required.

   Your ID: 123456789
   Code: HJKL7M2P

   Run: beamclaw pair telegram HJKL7M2P
   ```
3. Bot owner approves: `beamclaw pair telegram HJKL7M2P`
4. User is now allowed — future messages reach the agent

#### CLI commands

```bash
beamclaw pair                       # list pending + approved (default)
beamclaw pair list                  # same as above
beamclaw pair telegram <CODE>       # approve a pending request
beamclaw pair revoke telegram <ID>  # remove user from allowlist
```

#### DM policy modes

Set `dm_policy` in the Telegram channel config (`sys.config`):

| Policy | Behaviour |
|--------|-----------|
| `pairing` | Unknown users get a code; blocked until approved **(default)** |
| `allowlist` | Unknown users silently dropped; no codes issued |
| `open` | No access control (legacy behaviour, not recommended) |

When `BEAMCLAW_USER` is set, pairing is bypassed entirely — all users
collapse to the canonical identity (single-user trust mode).

#### Hardcoded allowlist

In addition to the pairing store, you can specify a static `allow_from` list
in `sys.config`:

```erlang
{telegram, #{token => {env, "TELEGRAM_BOT_TOKEN"},
             mode => long_poll,
             dm_policy => pairing,
             allow_from => [<<"123456789">>]}}
```

Users in `allow_from` are always allowed regardless of pairing state.

#### Storage

Pairing data is stored as JSON files under `~/.beamclaw/pairing/`:

- `telegram-pending.json` — pending pairing requests (max 3, 1-hour TTL)
- `telegram-allowed.json` — approved user IDs

### Environment variables

| Variable | Required | Description |
|---|---|---|
| `OPENROUTER_API_KEY` | Yes | LLM completions via OpenRouter |
| `OPENAI_API_KEY` | No | Alternative OpenAI provider |
| `TELEGRAM_BOT_TOKEN` | No | Enable Telegram channel |
| `BEAMCLAW_PORT` | No | Override gateway HTTP port (default: 18800) |
| `BEAMCLAW_AGENT` | No | Default agent name for TUI (default: `default`) |
| `BEAMCLAW_USER` | No | Override user identity for session sharing |
| `BEAMCLAW_HOME` | No | Override workspace base directory (default: `~/.beamclaw`) |
| `BEAMCLAW_EMBEDDING_API_KEY` | No | API key for embedding service (enables semantic search) |
| `BEAMCLAW_EMBEDDING_URL` | No | Embedding API base URL (default: `https://api.openai.com/v1`) |
| `BEAMCLAW_EMBEDDING_MODEL` | No | Embedding model name (default: `text-embedding-3-small`) |

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

### Docker Compose (recommended)

Copy the example environment file and add your secrets:

```bash
cp .env.example .env
# Edit .env — at minimum set OPENROUTER_API_KEY
```

Start (or rebuild after code changes):

```bash
docker compose up -d --build
```

Stop:

```bash
docker compose down
```

Stop and remove data volume:

```bash
docker compose down -v
```

View logs:

```bash
docker compose logs -f
```

### docker run (alternative)

```bash
docker run -d \
  --name beamclaw \
  --restart unless-stopped \
  -v beamclaw_data:/home/beamclaw/.beamclaw \
  -e OPENROUTER_API_KEY=sk-or-... \
  -e TELEGRAM_BOT_TOKEN=...       \
  -p 18800:18800                    \
  beamclaw:latest
```

The named volume `beamclaw_data` persists agent workspaces (bootstrap files,
daily logs, skills) and Mnesia session history across container restarts.
Omit the `-v` flag if you don't need persistence.

### Health check

```bash
wget -qO- http://localhost:18800/health
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

### CLI control (`beamclaw-ctl`)

The Docker image includes the full `beamclaw` CLI escript as `beamclaw-ctl`.
Use it via `docker exec` to manage agents, skills, pairing, and more:

```bash
# Agent management
docker exec beamclaw beamclaw-ctl agent list
docker exec beamclaw beamclaw-ctl agent create my-assistant
docker exec beamclaw beamclaw-ctl agent show my-assistant
docker exec beamclaw beamclaw-ctl agent rehatch my-assistant
docker exec beamclaw beamclaw-ctl agent delete my-assistant

# Skills
docker exec beamclaw beamclaw-ctl skills list
docker exec beamclaw beamclaw-ctl skills status
docker exec beamclaw beamclaw-ctl skills show example-skill

# Pairing (Telegram access control)
docker exec beamclaw beamclaw-ctl pair list
docker exec beamclaw beamclaw-ctl pair telegram HJKL7M2P
docker exec beamclaw beamclaw-ctl pair revoke telegram 123456789

# Health and diagnostics
docker exec beamclaw beamclaw-ctl status
docker exec beamclaw beamclaw-ctl doctor
docker exec beamclaw beamclaw-ctl version

# Interactive TUI (requires -it for stdin)
docker exec -it beamclaw beamclaw-ctl tui --agent default
```

The release's low-level OTP commands are also available:

```bash
docker exec beamclaw /opt/beamclaw/bin/beamclaw ping
docker exec beamclaw /opt/beamclaw/bin/beamclaw rpc 'module' 'function' '[args]'
docker exec -it beamclaw /opt/beamclaw/bin/beamclaw remote_console
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

The HTTP gateway is always enabled on port 18800 (configurable). Endpoints:

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

1. **Health endpoint**: `wget -qO- http://localhost:18800/health` should return `{"status":"ok"}`
2. **Log output**: look for `agent_start` and `session_start` events in the logs
3. **TUI** (shell mode): type a message — the agentic loop should respond
4. **Telegram**: send `/start` to your bot
