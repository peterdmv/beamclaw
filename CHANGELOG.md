# Changelog

## [0.1.0] — 2026-03-05

First public release.

### Core
- Agentic loop (gen_statem) with streaming, tool execution, approval workflow
- LLM providers: OpenRouter (default), OpenAI
- Session persistence via Mnesia (survives restarts and Docker rebuilds)
- Context compaction with pre-compaction memory flush
- Credential scrubbing on all tool results
- Token-based automatic compaction trigger

### Tools
- Built-in: bash, terminal, curl, jq, read_file, write_file, delete_file
- Workspace memory (MEMORY.md, daily logs, bootstrap files, search)
- Web search (Brave Search API)
- Sandboxed code execution (Docker, with MCP tool bridge)
- Scheduled tasks and heartbeat

### Channels
- Telegram (long-poll + webhook, photo/vision, voice transcription, pairing)
- TUI (local and remote via Erlang distribution)
- HTTP API (OpenAI-compatible, SSE streaming)
- WebSocket

### Agent System
- Named agent workspaces with six bootstrap files
- Skill system (bundled + global + per-agent, BM25 auto-injection)
- Per-user agent mapping via Telegram pairing

### Search
- BM25 keyword search (pure Erlang)
- Vector semantic search (OpenAI-compatible embeddings)
- Hybrid score merging

### Infrastructure
- Nine OTP apps in rebar3 umbrella
- Docker image (Alpine, multi-stage, <100 MB)
- CLI escript (30 commands)
- 720 automated tests (683 EUnit + 37 CT)
