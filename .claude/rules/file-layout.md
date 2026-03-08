# File Layout Reference

Complete directory tree for all ten OTP apps.

---

## File Layout

```
beamclaw/
  rebar.config
  config/
    sys.config
    vm.args
  apps/
    beamclaw_obs/src/
      beamclaw_obs.app.src
      beamclaw_obs_app.erl
      beamclaw_obs_sup.erl
      bc_obs.erl              %% behaviour + emit/2 API
      bc_obs_manager.erl      %% fan-out gen_server
      bc_obs_log.erl          %% log backend
    beamclaw_memory/
      include/
        bc_memory_mnesia.hrl  %% Mnesia record (with embedding field)
      src/
        beamclaw_memory.app.src
        beamclaw_memory_app.erl
        beamclaw_memory_sup.erl
        bc_memory.erl           %% behaviour (incl. optional search/4)
        bc_memory_ets.erl
        bc_memory_mnesia.erl
        bc_bm25.erl             %% BM25 keyword search (pure function)
        bc_vector.erl           %% cosine similarity, dot product (pure function)
        bc_chunker.erl          %% text chunking for embeddings (pure function)
        bc_hybrid.erl           %% BM25 + vector score merging (pure function)
        bc_embedding.erl        %% OpenAI-compatible embedding API client
        bc_embedding_cache.erl  %% gen_server, ETS embedding cache (24h TTL)
    beamclaw_tools/src/
      beamclaw_tools.app.src
      beamclaw_tools_app.erl
      beamclaw_tools_sup.erl
      bc_tool.erl             %% behaviour
      bc_tool_registry.erl    %% gen_server, named ETS table
      bc_tool_terminal.erl
      bc_tool_bash.erl
      bc_tool_curl.erl
      bc_tool_jq.erl
      bc_tool_read_file.erl
      bc_tool_write_file.erl
      bc_tool_delete_file.erl
      bc_tool_web_search.erl        %% Brave Search API web search
      bc_tool_workspace_memory.erl  %% agent MEMORY.md + daily logs + bootstrap files + search
      bc_workspace_path.erl         %% pure path resolution + memory dir (avoids dep cycle)
    beamclaw_sandbox/
      src/
        beamclaw_sandbox.app.src
        beamclaw_sandbox_app.erl
        beamclaw_sandbox_sup.erl
        bc_sandbox_registry.erl    %% ETS: {session_id, scope} → pid; immediate cleanup on DOWN
        bc_sandbox_reaper.erl      %% gen_server: periodic orphan container cleanup
        bc_sandbox_sup.erl         %% simple_one_for_one for bc_sandbox
        bc_sandbox.erl             %% per-sandbox Docker lifecycle gen_server
        bc_sandbox_docker.erl      %% pure: Docker command arg building
        bc_sandbox_bridge.erl      %% JSON-RPC 2.0 bridge encode/decode/dispatch
        bc_sandbox_discovery.erl   %% generate /tools/ filesystem for container
        bc_tool_exec.erl           %% bc_tool behaviour: sandboxed code execution
        bc_pii_tokenizer.erl       %% gen_server: bidirectional PII masking
        bc_sandbox_policy.erl      %% pure: tool access allow/deny rules
        bc_sandbox_env.erl         %% pure: env var allowlist/blocklist filtering
        bc_sandbox_skills.erl      %% pure: save/load sandbox scripts as SKILL.md
      priv/
        docker/
          Dockerfile.sandbox       %% python:3.12-alpine sandbox image
          bridge/
            __init__.py
            beamclaw_bridge.py     %% Python bridge: search_tools, call_tool
    beamclaw_scheduler/
      include/
        bc_sched_job.hrl          %% Mnesia record for scheduled jobs
      src/
        beamclaw_scheduler.app.src
        beamclaw_scheduler_app.erl
        beamclaw_scheduler_sup.erl
        bc_sched_store.erl        %% Mnesia persistence (init_table, CRUD)
        bc_sched_runner.erl       %% Timer management gen_server
        bc_sched_executor.erl     %% Session dispatch + delivery gen_server
        bc_sched_random.erl       %% Random slot algorithm (pure)
        bc_sched_interval.erl     %% Interval string parsing (pure)
        bc_tool_scheduler.erl     %% bc_tool behaviour: scheduled tasks + heartbeat
    beamclaw_mcp/src/
      beamclaw_mcp.app.src
      beamclaw_mcp_app.erl
      beamclaw_mcp_sup.erl
      bc_mcp_registry.erl
      bc_mcp_servers_sup.erl
      bc_mcp_server.erl
    beamclaw_core/
      include/
        bc_types.hrl
        bc_session_store.hrl  %% Mnesia record for session persistence
      src/
        beamclaw_core.app.src
        beamclaw_core_app.erl
        beamclaw_core_sup.erl
        bc_provider.erl       %% behaviour
        bc_provider_openrouter.erl
        bc_provider_openai.erl
        bc_channel.erl        %% behaviour
        bc_session_registry.erl   %% session_id → pid + derive_session_id/2,3
        bc_session_store.erl      %% Mnesia-backed session persistence
        bc_session_cleaner.erl    %% periodic expired session cleanup
        bc_sessions_sup.erl
        bc_session_sup.erl
        bc_session.erl
        bc_loop.erl           %% gen_statem agentic loop
        bc_approval.erl
        bc_compactor.erl
        bc_memory_flush.erl       %% extracted pre-compaction memory flush
        bc_session_maintenance.erl  %% periodic idle/nightly/pre-expiry maintenance
        bc_user_env.erl             %% user environment context injection (time, weather, news)
        bc_scrubber.erl
        bc_thinking.erl       %% strip LLM thinking/reasoning tags
        bc_tool_parser.erl
        bc_config.erl
        bc_context.erl              %% context window usage display (pure function)
        bc_workspace_templates.erl  %% eight default bootstrap file templates
        bc_workspace.erl            %% agent workspace filesystem ops
        bc_system_prompt.erl        %% assemble bootstrap files into system messages
        bc_skill_parser.erl         %% parse SKILLS.md front-matter
        bc_skill_discovery.erl      %% discover skills from bundled + workspace
        bc_skill_eligibility.erl    %% check skill requirements (tools, MCP, env)
        bc_skill_installer.erl      %% install skill dependencies
        bc_pairing.erl              %% channel access control via pairing codes
      priv/
        skills/                     %% bundled skills
          example-skill/
          finnhub/
          nano-banana-pro/
            scripts/
              generate_image.py     %% Gemini image generation script
    beamclaw_a2a/
      include/
        bc_a2a_types.hrl          %% A2A records: a2a_task, a2a_message, a2a_status, a2a_artifact
      src/
        beamclaw_a2a.app.src
        beamclaw_a2a_app.erl
        beamclaw_a2a_sup.erl
        bc_a2a_task.erl           %% task state machine + serialization
        bc_a2a_task_manager.erl   %% gen_server: ETS-backed task store + session dispatch
        bc_a2a_server.erl         %% JSON-RPC 2.0 method dispatch (stateless)
        bc_a2a_http_h.erl         %% Cowboy handler: agent card + /a2a endpoint
        bc_a2a_agent_card.erl     %% agent card builder + serialization
        bc_channel_a2a.erl        %% stateless response routing for A2A sessions
    beamclaw_gateway/src/
      beamclaw_gateway.app.src
      beamclaw_gateway_app.erl
      beamclaw_gateway_sup.erl
      bc_rate_limiter.erl
      bc_gateway_http_sup.erl
      bc_gateway_cowboy.erl
      bc_gateway_channels_sup.erl
      bc_channel_telegram.erl
      bc_telegram_format.erl    %% pure-function markdown→Telegram HTML converter
      bc_telegram_photo.erl     %% photo extraction, download, base64 encoding
      bc_telegram_audio.erl    %% voice/audio extraction and download
      bc_stt.erl               %% speech-to-text client (Groq Whisper / OpenAI-compatible)
      bc_channel_tui.erl
      bc_http_health_h.erl
      bc_http_metrics_h.erl
      bc_http_completions_h.erl
      bc_ws_h.erl
      bc_webhook_telegram_h.erl
      bc_webhook_h.erl          %% generic webhook ingestion: POST /webhook/:source
    beamclaw_cli/src/
      beamclaw_cli.app.src
      beamclaw_cli.erl        %% escript main; 30 commands (tui/start/stop/restart/remote_console/agent create/list/show/delete/rehatch/skills list/status/show/install/scheduler list/cancel/pause/resume/pair/pair list/pair approve/pair revoke/sandbox status/list/kill/build/doctor/status/version/help)
```
