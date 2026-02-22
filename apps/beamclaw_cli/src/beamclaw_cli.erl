%% @doc BeamClaw CLI entry point (escript).
%%
%% Usage: beamclaw [tui|start|stop|restart|remote_console|agent|skills|doctor|status|version|help]
%%
%% Built with: rebar3 escriptize
%% Output:     _build/default/bin/beamclaw
-module(beamclaw_cli).
-export([main/1]).

%% All CLI command functions terminate via halt/1 (escript pattern).
%% Dialyzer reports these as "only terminates with explicit exception" and
%% "has no local return" — both are correct and intentional for a CLI tool.
%% The unmatched application:ensure_all_started returns are also intentional.
-dialyzer([no_return, no_unused]).
-dialyzer({nowarn_function, [cmd_local_tui/1, cmd_status/0,
                             cmd_stop/0, cmd_remote_console/0, cmd_doctor/0,
                             cmd_agent_create/1, cmd_agent_list/0,
                             cmd_agent_delete/1, cmd_agent_show/1,
                             cmd_agent_rehatch/1,
                             cmd_skills_list/0, cmd_skills_status/0,
                             cmd_skills_show/1, cmd_skills_install/1,
                             spawn_daemon/0, check_openrouter_network/0]}).

-include_lib("beamclaw_core/include/bc_types.hrl").

-define(VERSION, "0.1.0").
-define(DAEMON_SNAME, beamclaw).
-define(GATEWAY_PORT, 8080).

%%--------------------------------------------------------------------
%% Entry point
%%--------------------------------------------------------------------

main([])                                  -> cmd_tui(default_agent());
main(["tui", "--agent", Name    | _])     -> cmd_tui(list_to_binary(Name));
main(["tui"                     | _])     -> cmd_tui(default_agent());
main(["start"                   | _])     -> cmd_start();
main(["stop"                    | _])     -> cmd_stop();
main(["restart"                 | _])     -> cmd_restart();
main(["remote_console"          | _])     -> cmd_remote_console();
main(["agent", "create", Name   | _])     -> cmd_agent_create(list_to_binary(Name));
main(["agent", "list"           | _])     -> cmd_agent_list();
main(["agent", "delete",  Name   | _])     -> cmd_agent_delete(list_to_binary(Name));
main(["agent", "show",    Name   | _])     -> cmd_agent_show(list_to_binary(Name));
main(["agent", "rehatch", Name   | _])     -> cmd_agent_rehatch(list_to_binary(Name));
main(["agent"                   | _])     -> cmd_agent_list();
main(["skills", "status"        | _])     -> cmd_skills_status();
main(["skills", "list"          | _])     -> cmd_skills_list();
main(["skills", "install", Name | _])     -> cmd_skills_install(list_to_binary(Name));
main(["skills", "show", Name    | _])     -> cmd_skills_show(list_to_binary(Name));
main(["skills"                  | _])     -> cmd_skills_list();
main(["doctor"                  | _])     -> cmd_doctor();
main(["status"                  | _])     -> cmd_status();
main(["version"                 | _])     -> cmd_version();
main(["help"                    | _])     -> cmd_help();
main(["--help"                  | _])     -> cmd_help();
main([Unknown                   | _]) ->
    io:format(standard_error, "beamclaw: unknown command '~s'~n", [Unknown]),
    io:format(standard_error, "Run 'beamclaw help' for usage.~n", []),
    halt(1).

%%--------------------------------------------------------------------
%% Commands
%%--------------------------------------------------------------------

%% @doc Start interactive TUI chat — auto-detects running daemon.
cmd_tui(AgentId) ->
    case try_connect_daemon() of
        connected   -> cmd_remote_tui(AgentId);
        not_running -> cmd_local_tui(AgentId)
    end.

%% @doc Start the TUI in-process (no daemon running).
cmd_local_tui(AgentId) ->
    apply_tui_config(),
    application:set_env(beamclaw_core, default_agent, AgentId,
                        [{persistent, true}]),
    case application:ensure_all_started(beamclaw_gateway) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            io:format(standard_error,
                      "beamclaw: failed to start gateway: ~p~n", [Reason]),
            halt(1)
    end,
    case whereis(bc_channel_tui) of
        undefined ->
            io:format(standard_error,
                      "beamclaw: bc_channel_tui not registered after startup~n", []),
            halt(1);
        TuiPid ->
            Ref = monitor(process, TuiPid),
            receive
                {'DOWN', Ref, process, _, _} -> halt(0)
            end
    end.

%% @doc Attach a remote TUI to a running daemon via Erlang distribution.
cmd_remote_tui(AgentId) ->
    erlang:monitor_node(daemon_node(), true),
    SessionId = generate_remote_session_id(),
    io:format("BeamClaw TUI (remote) — connected to ~s (agent: ~s)~n",
              [daemon_node(), AgentId]),
    io:format("Type a message and press Enter. Ctrl+D to disconnect.~n~n> "),
    remote_tui_loop(SessionId, AgentId).

remote_tui_loop(SessionId, AgentId) ->
    case io:get_line("") of
        eof ->
            io:format("~n[disconnected]~n"),
            halt(0);
        {error, _} ->
            io:format("~n[stdin error — disconnecting]~n"),
            halt(1);
        Line ->
            Text = string:trim(Line, trailing, "\n"),
            case Text of
                "" ->
                    io:format("> "),
                    remote_tui_loop(SessionId, AgentId);
                _ ->
                    dispatch_remote(SessionId, iolist_to_binary(Text), AgentId),
                    receive_remote_response(SessionId),
                    remote_tui_loop(SessionId, AgentId)
            end
    end.

dispatch_remote(SessionId, Text, AgentId) ->
    %% Ensure session exists on daemon.
    case rpc:call(daemon_node(), bc_session_registry, lookup, [SessionId]) of
        {error, not_found} ->
            Config = #{session_id  => SessionId,
                       user_id     => <<"remote_tui_user">>,
                       channel_id  => SessionId,
                       channel_mod => undefined,
                       agent_id    => AgentId},
            case rpc:call(daemon_node(), bc_sessions_sup, start_session, [Config]) of
                {ok, _}         -> ok;
                {badrpc, Reason} ->
                    io:format(standard_error,
                              "~nbeamclaw: RPC failed creating session: ~p~n", [Reason]),
                    halt(1);
                {error, Reason} ->
                    io:format(standard_error,
                              "~nbeamclaw: failed to create session: ~p~n", [Reason]),
                    halt(1)
            end;
        {ok, _Pid} ->
            ok;
        {badrpc, Reason} ->
            io:format(standard_error,
                      "~nbeamclaw: RPC failed: ~p~n", [Reason]),
            halt(1)
    end,
    %% Lookup session pid and dispatch the run with reply_pid = self().
    case rpc:call(daemon_node(), bc_session_registry, lookup, [SessionId]) of
        {ok, SPid} ->
            Msg = #bc_channel_message{
                session_id = SessionId,
                user_id    = <<"remote_tui_user">>,
                channel    = remote_tui,
                content    = Text,
                raw        = Text,
                ts         = erlang:system_time(millisecond),
                reply_pid  = self()
            },
            rpc:call(daemon_node(), bc_session, dispatch_run, [SPid, Msg]);
        {badrpc, Reason2} ->
            io:format(standard_error,
                      "~nbeamclaw: RPC failed dispatching run: ~p~n", [Reason2]),
            halt(1);
        {error, not_found} ->
            io:format(standard_error,
                      "~nbeamclaw: session disappeared after creation~n", []),
            halt(1)
    end.

receive_remote_response(SessionId) ->
    receive
        {bc_chunk, SessionId, Chunk} ->
            io:format("~s", [Chunk]),
            receive_remote_response(SessionId);
        {bc_done, SessionId, _Msg} ->
            io:format("~n"),
            receive_remote_response(SessionId);
        {bc_turn_complete, SessionId} ->
            io:format("> ");
        {nodedown, _Node} ->
            io:format("~n[daemon disconnected]~n"),
            halt(1)
    after 120000 ->
        io:format("~n[timeout — no response within 120s]~n> ")
    end.

%% @doc Start gateway as a background daemon (Erlang distribution IPC).
cmd_start() ->
    ensure_ctl_node(),
    case net_adm:ping(daemon_node()) of
        pong ->
            io:format("Gateway already running.~n"),
            halt(1);
        pang ->
            spawn_daemon()
    end.

%% @doc Stop a running daemon via OTP RPC.
cmd_stop() ->
    ensure_ctl_node(),
    case do_stop() of
        ok ->
            io:format("Gateway stopped.~n"),
            halt(0);
        not_running ->
            io:format("Gateway not running.~n"),
            halt(1);
        timeout ->
            io:format(standard_error,
                      "beamclaw: daemon did not stop within 10s~n", []),
            halt(1)
    end.

%% @doc Stop then start the daemon.
cmd_restart() ->
    ensure_ctl_node(),
    case do_stop() of
        ok          -> io:format("Gateway stopped.~n");
        not_running -> io:format("Gateway was not running.~n");
        timeout     ->
            io:format(standard_error,
                      "beamclaw: daemon did not stop within 10s~n", []),
            halt(1)
    end,
    spawn_daemon().

%% @doc Print the erl -remsh command to attach a live shell to the daemon.
cmd_remote_console() ->
    ensure_ctl_node(),
    case net_adm:ping(daemon_node()) of
        pang ->
            io:format("Gateway not running.~n"),
            halt(1);
        pong ->
            Cookie = atom_to_list(erlang:get_cookie()),
            io:format("Run: erl -remsh ~s -setcookie ~s~n",
                      [atom_to_list(daemon_node()), Cookie]),
            halt(0)
    end.

%% @doc Check environment and connectivity. Exits 0 if no failures, 1 otherwise.
cmd_doctor() ->
    Checks = [
        check_otp_version(),
        check_openrouter_key(),
        check_openai_key(),
        check_telegram_token(),
        check_epmd(),
        check_workspace(),
        check_skills_dir()
    ],
    AllChecks = case os:getenv("OPENROUTER_API_KEY") of
        false -> Checks;
        _     -> Checks ++ [check_openrouter_network()]
    end,
    HasFailure = lists:any(fun({Tag, _}) -> Tag =:= fail end, AllChecks),
    case HasFailure of
        true  -> halt(1);
        false -> halt(0)
    end.

%% @doc Ping the running gateway's /health endpoint.
cmd_status() ->
    application:ensure_all_started(inets),
    Port = case os:getenv("BEAMCLAW_PORT") of
        false -> ?GATEWAY_PORT;
        P     -> list_to_integer(P)
    end,
    Url = "http://localhost:" ++ integer_to_list(Port) ++ "/health",
    case httpc:request(get, {Url, []}, [{timeout, 3000}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            io:format("Gateway running at localhost:~p — ~s~n", [Port, Body]),
            halt(0);
        {ok, {{_, Code, _}, _, Body}} ->
            io:format("Gateway returned HTTP ~p: ~s~n", [Code, Body]),
            halt(1);
        {error, {failed_connect, _}} ->
            io:format("Gateway not running at localhost:~p~n", [Port]),
            halt(1);
        {error, Reason} ->
            io:format(standard_error,
                      "beamclaw: status check failed: ~p~n", [Reason]),
            halt(1)
    end.

cmd_version() ->
    io:format("beamclaw ~s~n", [?VERSION]).

%% @doc Create a new agent workspace.
cmd_agent_create(Name) ->
    case bc_workspace:create_agent(Name) of
        ok ->
            Dir = bc_workspace:agent_dir(Name),
            io:format("Agent '~s' created at ~s~n", [Name, Dir]),
            halt(0);
        {error, exists} ->
            io:format(standard_error, "beamclaw: agent '~s' already exists~n", [Name]),
            halt(1);
        {error, invalid_agent_id} ->
            io:format(standard_error,
                      "beamclaw: invalid agent name '~s' "
                      "(must match [a-z0-9_-]+)~n", [Name]),
            halt(1)
    end.

%% @doc List all agents.
cmd_agent_list() ->
    bc_workspace:ensure_default_agent(),
    Agents = bc_workspace:list_agents(),
    case Agents of
        [] ->
            io:format("No agents found.~n");
        _ ->
            io:format("Agents:~n"),
            lists:foreach(fun(Id) ->
                Label = agent_display_name(Id),
                io:format("  ~s~s~n", [Id, Label])
            end, Agents)
    end,
    halt(0).

%% @doc Delete an agent workspace.
cmd_agent_delete(Name) ->
    case bc_workspace:delete_agent(Name) of
        ok ->
            io:format("Agent '~s' deleted.~n", [Name]),
            halt(0);
        {error, cannot_delete_default} ->
            io:format(standard_error,
                      "beamclaw: cannot delete the default agent~n", []),
            halt(1);
        {error, not_found} ->
            io:format(standard_error,
                      "beamclaw: agent '~s' not found~n", [Name]),
            halt(1);
        {error, invalid_agent_id} ->
            io:format(standard_error,
                      "beamclaw: invalid agent name '~s'~n", [Name]),
            halt(1)
    end.

%% @doc Show all bootstrap files for an agent.
cmd_agent_show(Name) ->
    case bc_workspace:agent_exists(Name) of
        false ->
            io:format(standard_error,
                      "beamclaw: agent '~s' not found~n", [Name]),
            halt(1);
        true ->
            Files = bc_workspace:read_all_bootstrap_files(Name),
            Order = [<<"IDENTITY.md">>, <<"SOUL.md">>, <<"USER.md">>,
                     <<"TOOLS.md">>, <<"AGENTS.md">>, <<"BOOTSTRAP.md">>,
                     <<"MEMORY.md">>],
            lists:foreach(fun(Filename) ->
                io:format("--- ~s ---~n", [Filename]),
                case maps:get(Filename, Files, undefined) of
                    undefined -> io:format("(not found)~n");
                    Content   -> io:format("~s~n", [Content])
                end
            end, Order),
            halt(0)
    end.

%% @doc Factory reset an agent: restore all files to defaults, wipe daily logs.
cmd_agent_rehatch(Name) ->
    case bc_workspace:rehatch_agent(Name) of
        ok ->
            io:format("Agent '~s' rehatched — all files reset to defaults.~n", [Name]),
            io:format("The bootstrap ritual will run on the next conversation.~n"),
            halt(0);
        {error, not_found} ->
            io:format(standard_error, "beamclaw: agent '~s' not found~n", [Name]),
            halt(1);
        {error, invalid_agent_id} ->
            io:format(standard_error,
                      "beamclaw: invalid agent name '~s'~n", [Name]),
            halt(1)
    end.

%% @doc List all discovered skills with eligible status.
cmd_skills_list() ->
    AgentId = default_agent(),
    Skills = bc_skill_discovery:discover(AgentId),
    case Skills of
        [] ->
            io:format("No skills found.~n"),
            io:format("  Global:    ~s~n", [bc_skill_discovery:global_skills_dir()]),
            io:format("  Per-agent: ~s~n", [bc_skill_discovery:agent_skills_dir(AgentId)]);
        _ ->
            io:format("Skills:~n"),
            lists:foreach(fun(Skill) ->
                Name  = Skill#bc_skill.name,
                Desc  = case Skill#bc_skill.description of
                    undefined -> <<"">>;
                    D         -> D
                end,
                Emoji = case Skill#bc_skill.emoji of
                    undefined -> <<"">>;
                    E         -> <<E/binary, " ">>
                end,
                Eligible = bc_skill_eligibility:is_eligible(Skill),
                Status = case Eligible of
                    true  -> "[ok]  ";
                    false -> "[miss]"
                end,
                SourceTag = case Skill#bc_skill.source of
                    global       -> "(global)";
                    {agent, Aid} -> io_lib:format("(agent:~s)", [Aid])
                end,
                io:format("  ~s ~s~s~s ~s~n",
                          [Status, Emoji, Name, case Desc of <<>> -> ""; _ -> " - " ++ binary_to_list(Desc) end, SourceTag])
            end, Skills)
    end,
    halt(0).

%% @doc Show detailed requirements status for all skills.
cmd_skills_status() ->
    AgentId = default_agent(),
    Skills = bc_skill_discovery:discover(AgentId),
    case Skills of
        [] ->
            io:format("No skills found.~n");
        _ ->
            lists:foreach(fun(Skill) ->
                Name = Skill#bc_skill.name,
                io:format("~s:~n", [Name]),
                case bc_skill_eligibility:check(Skill) of
                    ok ->
                        io:format("  Status: eligible~n");
                    {missing, Details} ->
                        io:format("  Status: missing requirements~n"),
                        case maps:get(bins, Details, []) of
                            [] -> ok;
                            Bins -> io:format("  Missing binaries: ~s~n",
                                [lists:join(", ", [binary_to_list(B) || B <- Bins])])
                        end,
                        case maps:get(env, Details, []) of
                            [] -> ok;
                            Envs -> io:format("  Missing env vars: ~s~n",
                                [lists:join(", ", [binary_to_list(E) || E <- Envs])])
                        end,
                        case maps:get(os, Details, []) of
                            [] -> ok;
                            Oses -> io:format("  Required OS: ~s~n",
                                [lists:join(", ", [binary_to_list(O) || O <- Oses])])
                        end
                end,
                Specs = bc_skill_installer:available_install_specs(Skill),
                case Specs of
                    [] -> ok;
                    _  ->
                        io:format("  Install options: ~s~n",
                            [lists:join(", ", [binary_to_list(maps:get(<<"kind">>, S, <<"?">>)) || S <- Specs])])
                end,
                io:format("~n")
            end, Skills)
    end,
    halt(0).

%% @doc Show a specific skill's SKILL.md content.
cmd_skills_show(Name) ->
    AgentId = default_agent(),
    Skills = bc_skill_discovery:discover(AgentId),
    case [S || S <- Skills, S#bc_skill.name =:= Name] of
        [] ->
            io:format(standard_error, "beamclaw: skill '~s' not found~n", [Name]),
            halt(1);
        [Skill | _] ->
            io:format("--- ~s ---~n", [Skill#bc_skill.name]),
            case Skill#bc_skill.description of
                undefined -> ok;
                Desc      -> io:format("Description: ~s~n", [Desc])
            end,
            io:format("Source: ~p~n", [Skill#bc_skill.source]),
            io:format("Path: ~s~n", [Skill#bc_skill.path]),
            io:format("~n~s~n", [Skill#bc_skill.content]),
            halt(0)
    end.

%% @doc Install a skill's dependencies.
cmd_skills_install(Name) ->
    AgentId = default_agent(),
    Skills = bc_skill_discovery:discover(AgentId),
    case [S || S <- Skills, S#bc_skill.name =:= Name] of
        [] ->
            io:format(standard_error, "beamclaw: skill '~s' not found~n", [Name]),
            halt(1);
        [Skill | _] ->
            io:format("Installing dependencies for '~s'...~n", [Name]),
            case bc_skill_installer:install(Skill) of
                ok ->
                    io:format("Done.~n"),
                    halt(0);
                {error, no_install_specs} ->
                    io:format("No install specs defined for '~s'.~n", [Name]),
                    halt(1);
                {error, no_compatible_installer} ->
                    io:format("No compatible installer found for this system.~n"),
                    halt(1);
                {error, Reason} ->
                    io:format(standard_error, "Install failed: ~p~n", [Reason]),
                    halt(1)
            end
    end.

cmd_help() ->
    io:format(
        "Usage: beamclaw <command>~n~n"
        "Commands:~n"
        "  tui [--agent NAME]   Start interactive TUI chat (default)~n"
        "                       Connects to running daemon if available~n"
        "  start                Start gateway as background daemon~n"
        "  stop                 Stop running daemon~n"
        "  restart              Stop then start daemon~n"
        "  remote_console       Print command to attach live Erlang shell~n"
        "  agent create NAME    Create a new agent workspace~n"
        "  agent list           List all agents~n"
        "  agent show NAME      Show agent bootstrap files~n"
        "  agent delete NAME    Delete an agent workspace~n"
        "  agent rehatch NAME   Factory reset: restore all files to defaults~n"
        "  skills [list]        List discovered skills with status~n"
        "  skills status        Detailed requirements check for all skills~n"
        "  skills show NAME     Show a skill's SKILL.md content~n"
        "  skills install NAME  Install a skill's dependencies~n"
        "  doctor               Check environment and connectivity~n"
        "  status               Ping running gateway HTTP health endpoint~n"
        "  version              Print version~n"
        "  help                 Show this help~n~n"
        "Notes:~n"
        "  TUI: use Ctrl+D (EOF) to quit.~n"
        "  If a daemon is running (beamclaw start), tui auto-connects to it.~n"
        "  Ctrl+C shows the OTP break menu (type 'q' + Enter to exit).~n"
        "  Daemon IPC uses Erlang distribution; epmd must be available.~n~n"
        "Environment:~n"
        "  OPENROUTER_API_KEY   Required for LLM completions~n"
        "  OPENAI_API_KEY       Optional alternative provider~n"
        "  TELEGRAM_BOT_TOKEN   Optional Telegram channel~n"
        "  BEAMCLAW_PORT        Override gateway port (default: 8080)~n"
        "  BEAMCLAW_AGENT       Default agent name (default: default)~n"
        "  BEAMCLAW_HOME        Override workspace base directory~n"
    ).

%%--------------------------------------------------------------------
%% Internal: agent helpers
%%--------------------------------------------------------------------

%% @doc Resolve default agent from BEAMCLAW_AGENT env var or fallback.
default_agent() ->
    case os:getenv("BEAMCLAW_AGENT") of
        false -> <<"default">>;
        Name  -> list_to_binary(Name)
    end.

%% @doc Extract the Name field from IDENTITY.md for display, if present.
agent_display_name(AgentId) ->
    case bc_workspace:read_bootstrap_file(AgentId, <<"IDENTITY.md">>) of
        {ok, Content} ->
            case re:run(Content, "\\*\\*Name:\\*\\*\\s*(.+)",
                        [{capture, [1], binary}]) of
                {match, [Name]} ->
                    Trimmed = string:trim(Name),
                    <<" (", Trimmed/binary, ")">>;
                nomatch ->
                    <<>>
            end;
        {error, _} ->
            <<>>
    end.

%%--------------------------------------------------------------------
%% Internal: TUI embedded config
%%--------------------------------------------------------------------

apply_tui_config() ->
    application:set_env(beamclaw_core, default_provider, openrouter,
                        [{persistent, true}]),
    application:set_env(beamclaw_core, providers, [
        {openrouter, #{api_key  => {env, "OPENROUTER_API_KEY"},
                       base_url => "https://openrouter.ai/api/v1",
                       model    => "moonshotai/kimi-k2.5"}},
        {openai,     #{api_key  => {env, "OPENAI_API_KEY"},
                       base_url => "https://api.openai.com/v1",
                       model    => "gpt-4o"}}
    ], [{persistent, true}]),
    application:set_env(beamclaw_core, agentic_loop,
        #{max_tool_iterations => 10,
          compaction_threshold => 50,
          compaction_target    => 20,
          stream_chunk_size    => 80},
        [{persistent, true}]),
    application:set_env(beamclaw_core, autonomy_level, supervised,
                        [{persistent, true}]),
    application:set_env(beamclaw_core, session_ttl_seconds, 3600,
                        [{persistent, true}]),
    application:set_env(beamclaw_mcp, servers, [], [{persistent, true}]),
    application:set_env(beamclaw_gateway, http, #{port => ?GATEWAY_PORT},
                        [{persistent, true}]),
    TuiChannel = {tui, #{enabled => true}},
    Channels = case os:getenv("TELEGRAM_BOT_TOKEN") of
        false -> [TuiChannel];
        _     -> [{telegram, #{token => {env, "TELEGRAM_BOT_TOKEN"},
                               mode  => long_poll}},
                  TuiChannel]
    end,
    application:set_env(beamclaw_gateway, channels, Channels,
                        [{persistent, true}]),
    application:set_env(beamclaw_memory, backend, ets, [{persistent, true}]),
    application:set_env(beamclaw_obs, backends, [{log, #{level => info}}],
                        [{persistent, true}]).

%%--------------------------------------------------------------------
%% Internal: daemon detection
%%--------------------------------------------------------------------

%% Soft daemon detection: try to start distribution and ping the daemon.
%% Returns `connected` or `not_running` — never halts.
try_connect_daemon() ->
    case ensure_ctl_node_soft() of
        ok ->
            case net_adm:ping(daemon_node()) of
                pong -> connected;
                pang -> not_running
            end;
        {error, _} ->
            not_running
    end.

%% Like ensure_ctl_node/0 but returns ok | {error, Reason} instead of halt(1).
ensure_ctl_node_soft() ->
    Id   = integer_to_list(erlang:unique_integer([positive])),
    Name = list_to_atom("beamclaw_ctl_" ++ Id),
    case net_kernel:start([Name, shortnames]) of
        {ok, _}                       -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason}               -> {error, Reason}
    end.

%% Generate a unique session ID for remote TUI connections.
generate_remote_session_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    UUID = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
                         [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
    iolist_to_binary(["remote-tui-" | UUID]).

%%--------------------------------------------------------------------
%% Internal: daemon lifecycle
%%--------------------------------------------------------------------

%% Compute the daemon node name using the real hostname.
%% `-sname beamclaw` registers as `beamclaw@<hostname>`, not `beamclaw@localhost`.
daemon_node() ->
    {ok, Host} = inet:gethostname(),
    list_to_atom(atom_to_list(?DAEMON_SNAME) ++ "@" ++ Host).

%% Enable Erlang distribution with a unique ctl node name.
ensure_ctl_node() ->
    Id   = integer_to_list(erlang:unique_integer([positive])),
    Name = list_to_atom("beamclaw_ctl_" ++ Id),
    case net_kernel:start([Name, shortnames]) of
        {ok, _}                       -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} ->
            io:format(standard_error,
                      "beamclaw: could not enable distribution: ~p~n"
                      "  (Is epmd running? Try: epmd -daemon)~n",
                      [Reason]),
            halt(1)
    end.

do_stop() ->
    case net_adm:ping(daemon_node()) of
        pang -> not_running;
        pong ->
            rpc:call(daemon_node(), init, stop, []),
            poll_node_down(daemon_node(), 20, 500)
    end.

spawn_daemon() ->
    ErlBin     = find_erl_bin(),
    EbinPaths  = daemon_ebin_paths(),
    PaArgs     = lists:flatmap(fun(P) -> ["-pa", P] end, EbinPaths),
    ConfigFile = filename:absname("config/sys.config"),
    ConfigArgs = case filelib:is_file(ConfigFile) of
        true  -> ["-config", filename:rootname(ConfigFile)];
        false -> []
    end,
    %% Disable the TUI channel in daemon mode — no stdin available.
    %% Replace the tui entry in the channels list; preserve other channels.
    Eval = "application:load(beamclaw_gateway),"
           "Chs = application:get_env(beamclaw_gateway, channels, []),"
           "Chs2 = lists:keyreplace(tui, 1, Chs, {tui, #{enabled => false}}),"
           "application:set_env(beamclaw_gateway, channels, Chs2, [{persistent, true}]),"
           "application:ensure_all_started(beamclaw_gateway),"
           "receive after infinity -> ok end.",
    DaemonArgs = ["-detached", "-noshell", "-sname", atom_to_list(?DAEMON_SNAME)]
                 ++ PaArgs
                 ++ ConfigArgs
                 ++ ["-eval", Eval],
    erlang:open_port({'spawn_executable', ErlBin},
                     [{args, DaemonArgs}]),
    case poll_node_up(daemon_node(), 10, 500) of
        ok ->
            io:format("Gateway started. Logs: /tmp/beamclaw_daemon.log~n"),
            halt(0);
        timeout ->
            io:format(standard_error,
                      "beamclaw: daemon did not respond within 5s~n"
                      "  (Check OTP logger output for startup errors.)~n",
                      []),
            halt(1)
    end.

find_erl_bin() ->
    Candidate = filename:join([code:root_dir(), "bin", "erl"]),
    case filelib:is_file(Candidate) of
        true  -> Candidate;
        false ->
            case os:find_executable("erl") of
                false ->
                    io:format(standard_error,
                              "beamclaw: cannot find 'erl' binary~n", []),
                    halt(1);
                Path -> Path
            end
    end.

%% Derive ebin paths for the daemon process from the escript location.
%% code:get_path() returns archive-internal paths that only work inside the
%% escript process; a standalone erl cannot load from them. Instead, walk from
%% _build/default/bin/beamclaw → _build/default/lib/*/ebin.
daemon_ebin_paths() ->
    ScriptPath = filename:absname(escript:script_name()),
    %% _build/default/bin/beamclaw → _build/default/bin → _build/default
    BuildDir   = filename:dirname(filename:dirname(ScriptPath)),
    LibDir     = filename:join(BuildDir, "lib"),
    Ebins      = filelib:wildcard(filename:join([LibDir, "*", "ebin"])),
    case Ebins of
        [] ->
            io:format(standard_error,
                      "beamclaw: no ebin dirs found under ~s~n"
                      "  (Was the escript moved outside _build?)~n",
                      [LibDir]),
            halt(1);
        _ -> Ebins
    end.

poll_node_up(_Node, 0, _Interval) -> timeout;
poll_node_up(Node, Retries, Interval) ->
    case net_adm:ping(Node) of
        pong -> ok;
        pang ->
            timer:sleep(Interval),
            poll_node_up(Node, Retries - 1, Interval)
    end.

poll_node_down(_Node, 0, _Interval) -> timeout;
poll_node_down(Node, Retries, Interval) ->
    case net_adm:ping(Node) of
        pang -> ok;
        pong ->
            timer:sleep(Interval),
            poll_node_down(Node, Retries - 1, Interval)
    end.

%%--------------------------------------------------------------------
%% Internal: doctor checks
%%--------------------------------------------------------------------

check_otp_version() ->
    Release = erlang:system_info(otp_release),
    Major   = list_to_integer(hd(string:tokens(Release, "."))),
    case Major >= 28 of
        true  -> print_check(ok,   "OTP " ++ Release);
        false -> print_check(fail, "OTP " ++ Release ++ " (need >= 28)")
    end.

check_openrouter_key() ->
    case os:getenv("OPENROUTER_API_KEY") of
        false -> print_check(fail, "OPENROUTER_API_KEY not set (required)");
        _     -> print_check(ok,   "OPENROUTER_API_KEY set")
    end.

check_openai_key() ->
    case os:getenv("OPENAI_API_KEY") of
        false -> print_check(info, "OPENAI_API_KEY not set (optional)");
        _     -> print_check(ok,   "OPENAI_API_KEY set")
    end.

check_telegram_token() ->
    case os:getenv("TELEGRAM_BOT_TOKEN") of
        false -> print_check(info, "TELEGRAM_BOT_TOKEN not set (Telegram disabled)");
        _     -> print_check(ok,   "TELEGRAM_BOT_TOKEN set")
    end.

check_epmd() ->
    case os:find_executable("epmd") of
        false ->
            print_check(warn, "epmd not found on PATH "
                              "(needed for start/stop/remote_console)");
        _ ->
            print_check(ok, "epmd found")
    end.

check_workspace() ->
    BaseDir = bc_workspace:base_dir(),
    case bc_workspace:agent_exists(<<"default">>) of
        true ->
            print_check(ok, "Workspace directory: " ++ BaseDir);
        false ->
            bc_workspace:ensure_default_agent(),
            case bc_workspace:agent_exists(<<"default">>) of
                true ->
                    print_check(ok, "Workspace created: " ++ BaseDir);
                false ->
                    print_check(warn, "Cannot create workspace at " ++ BaseDir)
            end
    end.

check_skills_dir() ->
    Dir = bc_skill_discovery:global_skills_dir(),
    case filelib:is_dir(Dir) of
        true ->
            Skills = bc_skill_discovery:discover(default_agent()),
            print_check(ok, "Skills directory: " ++ Dir ++
                " (" ++ integer_to_list(length(Skills)) ++ " skills)");
        false ->
            print_check(info, "Skills directory not found: " ++ Dir)
    end.

check_openrouter_network() ->
    application:ensure_all_started(inets),
    Url = "https://openrouter.ai/api/v1/models",
    case httpc:request(get, {Url, []}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            print_check(ok,   "OpenRouter API reachable (200 OK)");
        {ok, {{_, Code, _}, _, _}} ->
            print_check(warn, "OpenRouter API returned HTTP "
                              ++ integer_to_list(Code));
        {error, Reason} ->
            print_check(fail, "OpenRouter API unreachable: "
                              ++ format_reason(Reason))
    end.

print_check(ok,   Msg) -> io:format("[ok]   ~s~n", [Msg]), {ok,   Msg};
print_check(warn, Msg) -> io:format("[warn] ~s~n", [Msg]), {warn, Msg};
print_check(fail, Msg) -> io:format("[fail] ~s~n", [Msg]), {fail, Msg};
print_check(info, Msg) -> io:format("[info] ~s~n", [Msg]), {info, Msg}.

format_reason({failed_connect, _}) ->
    "connection refused or DNS failure";
format_reason(Reason) ->
    lists:flatten(io_lib:format("~p", [Reason])).
