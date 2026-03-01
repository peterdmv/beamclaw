# BeamClaw

A fault-tolerant, security-conscious AI agent gateway and MCP (Model Context Protocol) host
implemented in Erlang/OTP 28. BeamClaw exposes AI capabilities over Telegram, a TUI, and an
OpenAI-compatible HTTP API — with built-in tool execution, session persistence, and context
compaction.

## Features

- **Fault-tolerant sessions** — each session lives in its own OTP supervision tree; a crashing
  agentic loop restarts automatically without losing conversation history
- **LLM providers** — OpenRouter (default) and OpenAI, switchable per deployment
- **Built-in tools** — bash, terminal, curl, jq, read\_file, write\_file
- **MCP client** — connect any stdio or HTTP MCP server for additional tools
- **Channels** — Telegram (long-poll or webhook), TUI (stdin/stdout), WebSocket, HTTP SSE
- **Approval workflow** — configurable autonomy levels (`read_only`, `supervised`, `full`) with
  per-session tool allowlists
- **Context compaction** — automatic summarisation when conversation history grows long
- **Credential scrubbing** — secrets are redacted before they enter history or LLM context
- **Rate limiting** — sliding-window per client IP, ETS-backed

## Quick Start — Docker (recommended)

Requires: Docker with Compose

1. Copy the example env file and fill in your keys:

```bash
cp .env.example .env
# Edit .env with your API keys
```

2. Start BeamClaw:

```bash
docker compose up -d --build
```

3. Verify health:

```bash
wget -qO- http://localhost:18800/health
```

After code changes, rebuild and restart with the same command:

```bash
docker compose up -d --build
```

<details>
<summary>Alternative: docker run (no Compose)</summary>

```bash
docker run -d \
  --name beamclaw \
  -v beamclaw_data:/home/beamclaw/.beamclaw \
  -e OPENROUTER_API_KEY=sk-or-... \
  -e TELEGRAM_BOT_TOKEN=...      \
  -p 18800:18800                 \
  beamclaw:latest
```

</details>

See [docs/running.md](docs/running.md) for full Docker usage, channel selection, and MCP
server configuration.

## Quick Start — From Source

Requires: Erlang/OTP 28, rebar3

```bash
git clone https://github.com/peterdmv/beamclaw.git
cd beamclaw

# Set secrets in your shell (never commit these)
export OPENROUTER_API_KEY=sk-or-...
export TELEGRAM_BOT_TOKEN=...

# Compile and launch a development shell
export PATH="/home/your-user/.asdf/shims:$PATH"   # if using asdf
rebar3 shell
```

The TUI channel starts automatically in shell mode. Type a message and press Enter.

See [docs/building.md](docs/building.md) for prerequisites and build details.

## Building a Release

```bash
rebar3 release
# Output: _build/default/rel/beamclaw/
_build/default/rel/beamclaw/bin/beamclaw foreground
```

For a Docker image:

```bash
docker build -t beamclaw:latest .
```

Full instructions: [docs/building.md](docs/building.md)

## Documentation

| Document | Contents |
|---|---|
| [docs/building.md](docs/building.md) | Prerequisites, compile, test, release, Docker build |
| [docs/running.md](docs/running.md) | rebar3 shell, OTP release, Docker run, channel setup |
| [docs/configuration.md](docs/configuration.md) | All env vars, sys.config keys, MCP servers |
| [docs/architecture.md](docs/architecture.md) | Nine-app design, supervision trees, agentic loop |

The full architectural decision log lives in [DECISIONS.md](DECISIONS.md).

## Architecture (overview)

Nine OTP applications with a strictly acyclic dependency graph:

```
beamclaw_gateway → beamclaw_core → beamclaw_sandbox    → beamclaw_tools → beamclaw_obs
                                 → beamclaw_scheduler  → beamclaw_tools
                                                        → beamclaw_obs
                                 → beamclaw_mcp        → beamclaw_tools
                                 → beamclaw_memory     → beamclaw_obs
                                 → beamclaw_tools
                                 → beamclaw_obs
                 → beamclaw_obs
```

The agentic loop (`bc_loop`, a `gen_statem`) drives the conversation through states:

```
idle → compacting? → streaming → awaiting_approval? → executing_tools → streaming (loop) → idle
```

Session history is owned by a permanent `bc_session` gen_server; the loop is transient —
supervisor-restarted crashes never lose history. See [docs/architecture.md](docs/architecture.md).

## License

Apache-2.0
