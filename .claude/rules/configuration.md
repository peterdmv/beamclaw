# Configuration Reference

Full `sys.config` and `vm.args` configuration. For quick-reference config
keys, see the Application Dependency Graph in CLAUDE.md.

---

## Configuration (`config/sys.config` + `config/vm.args`)

`bc_config:get(App, Key)` resolves `{env, "VAR"}` tuples at runtime via `os:getenv/1`.

`bc_config:canonical_user_id/0` returns the `BEAMCLAW_USER` env var as a binary,
or `undefined` if not set. When set, all channels use this value as-is (no prefix)
as the user_id, enabling cross-channel session sharing for single-user deployments.

```erlang
{kernel, [
    {logger_level, info},
    {logger, [
        {handler, default, logger_std_h, #{level => info, ...}},
        {handler, file, logger_std_h, #{
            level => debug,
            config => #{file => "/tmp/beamclaw_daemon.log",
                        max_no_bytes => 5242880, max_no_files => 3}, ...}}
    ]}
]},
{beamclaw_core, [
    {default_provider, openrouter},
    {providers, [
        {openrouter, #{api_key => {env, "OPENROUTER_API_KEY"},
                       base_url => "https://openrouter.ai/api/v1",
                       model    => "anthropic/claude-sonnet-4-5"}},
        {openai, #{api_key  => {env, "OPENAI_API_KEY"},
                   base_url => "https://api.openai.com/v1",
                   model    => "gpt-4o"}}
    ]},
    {agentic_loop, #{max_tool_iterations      => 10,
                     compaction_threshold_pct => 80,
                     compaction_target_pct    => 40,
                     compaction_provider      => openrouter,
                     compaction_model         => "moonshotai/kimi-k2.5",
                     stream_chunk_size        => 80,
                     memory_flush             => true,
                     auto_context             => false,
                     auto_context_limit       => 3}},
    {autonomy_level, supervised},
    {session_ttl_seconds, 3600},
    {default_agent, <<"default">>},
    {session_persistence, true},
    {session_sharing, shared},
    {session_cleanup_interval_ms, 300000},
    {maintenance, #{
        enabled                       => false,   %% opt-in proactive maintenance
        scan_interval_ms              => 300000,   %% 5 min scan interval
        idle_compaction_minutes       => 15,       %% min idle before compaction
        idle_compaction_threshold_pct => 20,       %% trigger: >20% of window
        idle_compaction_target_pct    => 10,       %% compact to 10% of window
        quiet_hours                   => {2, 4},   %% UTC hour range for nightly
        nightly_min_messages          => 10,       %% min history for nightly
        pre_expiry_minutes            => 10        %% flush window before TTL
    }},
    {skills, #{}},
    {user_env, #{
        enabled          => true,               %% real-time environment context
        timezone         => undefined,          %% override USER.md; IANA name string
        utc_offset_hours => undefined,          %% override; integer hours from UTC
        latitude         => 59.33,              %% for open-meteo weather (Stockholm)
        longitude        => 18.07,
        location_name    => "Stockholm",        %% display name in output
        weather          => #{enabled => true, ttl_seconds => 3600},
        news             => #{enabled => true, ttl_seconds => 3600},
        finnhub_token    => {env, "FINNHUB_TOKEN"},
        refresh_interval_ms => 1800000          %% 30 min async refresh interval
    }}
]},
{beamclaw_mcp, [
    {servers, []}
]},
{beamclaw_gateway, [
    {http, #{port => 18800}},
    {channels, [
        {telegram, #{token => {env, "TELEGRAM_BOT_TOKEN"}, mode => long_poll,
                     %% Webhook mode settings (only used when mode => webhook):
                     %% TELEGRAM_WEBHOOK_URL and TELEGRAM_WEBHOOK_SECRET are
                     %% read directly from env vars at runtime (not via config)
                     %% to avoid crashes when unset in long_poll mode.
                     dm_policy => pairing, allow_from => [],
                     photo => #{enabled => true,
                                max_size_bytes => 5242880},   %% 5 MB
                     voice => #{enabled => true,
                                max_duration_seconds => 120,
                                stt_base_url => "https://api.groq.com/openai/v1",
                                stt_api_key => {env, "GROQ_API_KEY"},
                                stt_model => "whisper-large-v3-turbo"}}},
        {tui,      #{enabled => true}}
    ]},
    {webhooks, #{
        enabled => true                        %% enable POST /webhook/:source endpoint
    }}
]},
{beamclaw_sandbox, [
    {enabled, false},                          %% opt-in; requires Docker
                                               %% In Docker: mount host socket
                                               %% -v /var/run/docker.sock:/var/run/docker.sock
    {docker_image, "beamclaw-sandbox:latest"},
    {scope, session},                          %% session | agent | shared
    {timeout_seconds, 60},
    {reaper_interval_ms, 60000},               %% orphan container sweep interval
    {memory_limit, "512m"},
    {cpu_limit, "1.0"},
    {network, none},                           %% none | bridge | host
    {workspace_mount, ro},                     %% none | ro | rw
    {max_output_bytes, 1048576},               %% 1 MB
    {bridge_socket_dir, "/tmp/beamclaw-bridges"},
    {pii, #{enabled => true, patterns => [],
            tokenize_scripts => true, tokenize_results => true}},
    {policy, #{default_action => allow,
               rules => [{allow, <<"read_file">>}, {allow, <<"curl">>},
                          {deny, <<"bash">>}, {deny, <<"terminal">>}]}},
    {env_allowlist, [<<"PATH">>, <<"HOME">>, <<"LANG">>, <<"TERM">>]},
    {env_blocklist, [<<"OPENROUTER_API_KEY">>, <<"OPENAI_API_KEY">>,
                     <<"TELEGRAM_BOT_TOKEN">>, <<"AWS_SECRET_ACCESS_KEY">>,
                     <<"GROQ_API_KEY">>, <<"TELEGRAM_WEBHOOK_SECRET">>,
                     <<"FINNHUB_TOKEN">>, <<"A2A_BEARER_TOKEN">>]}
]},
{beamclaw_tools, [
    {web_search, #{api_key => {env, "BRAVE_API_KEY"},
                   max_results => 10}}
]},
{beamclaw_scheduler, [
    {enabled, false},                      %% opt-in; creates bc_tool_scheduler when true
    {max_jobs_per_agent, 50},
    {default_autonomy, supervised},
    {max_errors, 3},                       %% consecutive failures → auto-pause
    {heartbeat, #{
        default_interval_ms => 1800000,    %% 30 min default heartbeat interval
        suppress_ok => true,               %% suppress HEARTBEAT_OK output
        active_hours => {8, 22}            %% UTC hour range (skip outside)
    }}
]},
{beamclaw_obs, []},
{beamclaw_memory, [
    {backend, ets},
    {embedding, #{enabled => true, model => "text-embedding-3-small",
                  base_url => "https://api.openai.com/v1",
                  api_key => {env, "BEAMCLAW_EMBEDDING_API_KEY"},
                  dimensions => 1536}},
    {search, #{vector_weight => 0.7, bm25_weight => 0.3,
               min_score => 0.35, default_limit => 6,
               chunk_size => 400, chunk_overlap => 80,
               workspace_files => [<<"MEMORY.md">>, ...],
               daily_log_lookback => 7}}
]},
{beamclaw_a2a, [
    {agent_card, #{
        name => <<"BeamClaw">>,
        url  => <<"http://localhost:18800">>
    }}
]}
```

`A2A_BEARER_TOKEN` env var: when set, Bearer token authentication is required on `POST /a2a`.
Agent Card advertises `bearer` scheme when token is configured; no auth advertised otherwise.

`WEBHOOK_SECRET_<SOURCE>` env vars: per-source authentication for `POST /webhook/:source`.
When `WEBHOOK_SECRET_TRADINGVIEW` is set, requests to `/webhook/tradingview` must include
the secret via one of (first match wins): `X-Webhook-Secret` header, `?secret=` query
parameter, or `"secret"` field in a JSON body. When unset, the endpoint is open (no auth
required). Source name is uppercased for the env var lookup. The `"secret"` field is
automatically stripped from JSON bodies before forwarding to the agent session.

Key `vm.args` flags:

| Flag | Value | Description |
|---|---|---|
| `-sname` | `beamclaw` | Local node name |
| `-mnesia dir` | `'"/home/beamclaw/.beamclaw/mnesia"'` | Mnesia data directory on the persistent volume. Required for Docker session persistence across container rebuilds. |
