%% @doc Agent workspace management — filesystem operations.
%%
%% Pure functional module (no gen_server). Manages agent workspace directories
%% under ~/.beamclaw/agents/<agent-id>/, each containing seven markdown bootstrap
%% files (SOUL.md, IDENTITY.md, USER.md, TOOLS.md, MEMORY.md, AGENTS.md,
%% BOOTSTRAP.md) and a memory/ subdirectory for daily logs.
%%
%% The base directory defaults to $HOME/.beamclaw/agents/ but can be overridden
%% via the BEAMCLAW_HOME environment variable.
-module(bc_workspace).

%% CLI command functions call halt/1 which dialyzer sees as "only terminates
%% with explicit exception". The delete_dir_recursive helper intentionally
%% discards the file:del_dir result. The -spec uses term() for API stability
%% though dialyzer infers narrower types.
-dialyzer({nowarn_function, [delete_dir_recursive/1]}).

-export([base_dir/0,
         agent_dir/1,
         ensure_default_agent/0,
         create_agent/1,
         delete_agent/1,
         rehatch_agent/1,
         list_agents/0,
         agent_exists/1,
         read_bootstrap_file/2,
         write_bootstrap_file/3,
         read_all_bootstrap_files/1,
         read_daily_log/2,
         list_daily_logs/1,
         validate_agent_id/1]).

-define(MAX_BOOTSTRAP_SIZE, 20480). %% 20 KB

%% @doc Return the base directory for all agent workspaces.
-spec base_dir() -> string().
base_dir() ->
    case os:getenv("BEAMCLAW_HOME") of
        false ->
            Home = os:getenv("HOME"),
            filename:join([Home, ".beamclaw", "agents"]);
        Dir ->
            filename:join([Dir, "agents"])
    end.

%% @doc Return the directory for a specific agent.
-spec agent_dir(binary()) -> string().
agent_dir(AgentId) ->
    filename:join(base_dir(), binary_to_list(AgentId)).

%% @doc Create the default agent workspace if it doesn't exist. Idempotent.
-spec ensure_default_agent() -> ok.
ensure_default_agent() ->
    case create_agent(<<"default">>) of
        ok              -> ok;
        {error, exists} -> ok
    end.

%% @doc Create a new agent workspace with all six template files.
-spec create_agent(binary()) -> ok | {error, exists | invalid_agent_id}.
create_agent(AgentId) ->
    case validate_agent_id(AgentId) of
        {error, _} = Err -> Err;
        ok ->
            Dir = agent_dir(AgentId),
            case filelib:is_dir(Dir) of
                true -> {error, exists};
                false ->
                    %% ensure_dir creates all parents including Dir itself
                    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
                    Templates = bc_workspace_templates:all_templates(),
                    lists:foreach(fun({Filename, Content}) ->
                        Path = filename:join(Dir, binary_to_list(Filename)),
                        case filelib:is_file(Path) of
                            true  -> ok;
                            false -> ok = file:write_file(Path, Content)
                        end
                    end, Templates),
                    %% Create memory/ subdirectory for daily logs
                    MemDir = filename:join(Dir, "memory"),
                    filelib:ensure_dir(filename:join(MemDir, "dummy")),
                    ok
            end
    end.

%% @doc Factory reset: restore all bootstrap files to defaults and wipe daily logs.
%% Preserves the skills/ subdirectory.
-spec rehatch_agent(binary()) -> ok | {error, not_found | invalid_agent_id}.
rehatch_agent(AgentId) ->
    case validate_agent_id(AgentId) of
        {error, _} = Err -> Err;
        ok ->
            case agent_exists(AgentId) of
                false -> {error, not_found};
                true ->
                    Dir = agent_dir(AgentId),
                    %% Reset all 7 bootstrap files to templates
                    Templates = bc_workspace_templates:all_templates(),
                    lists:foreach(fun({Filename, Content}) ->
                        Path = filename:join(Dir, binary_to_list(Filename)),
                        ok = file:write_file(Path, Content)
                    end, Templates),
                    %% Wipe daily logs in memory/ dir
                    MemDir = filename:join(Dir, "memory"),
                    case file:list_dir(MemDir) of
                        {ok, Entries} ->
                            lists:foreach(fun(E) ->
                                file:delete(filename:join(MemDir, E))
                            end, Entries);
                        {error, _} -> ok
                    end,
                    ok
            end
    end.

%% @doc Delete an agent workspace. Refuses to delete "default".
-spec delete_agent(binary()) -> ok | {error, atom() | {no_translation, binary()}}.
delete_agent(<<"default">>) ->
    {error, cannot_delete_default};
delete_agent(AgentId) ->
    case validate_agent_id(AgentId) of
        {error, _} = Err -> Err;
        ok ->
            Dir = agent_dir(AgentId),
            case filelib:is_dir(Dir) of
                false -> {error, not_found};
                true  -> delete_dir_recursive(Dir)
            end
    end.

%% @doc List all agent IDs (directory names under base_dir).
-spec list_agents() -> [binary()].
list_agents() ->
    Base = base_dir(),
    case file:list_dir(Base) of
        {ok, Entries} ->
            Agents = [list_to_binary(E) || E <- Entries,
                       filelib:is_dir(filename:join(Base, E))],
            lists:sort(Agents);
        {error, _} ->
            []
    end.

%% @doc Check whether an agent workspace exists.
-spec agent_exists(binary()) -> boolean().
agent_exists(AgentId) ->
    filelib:is_dir(agent_dir(AgentId)).

%% @doc Read a single bootstrap file. Truncates at 20 KB.
-spec read_bootstrap_file(binary(), binary()) -> {ok, binary()} | {error, atom()}.
read_bootstrap_file(AgentId, Filename) ->
    Path = filename:join(agent_dir(AgentId), binary_to_list(Filename)),
    case file:read_file(Path) of
        {ok, Bin} when byte_size(Bin) > ?MAX_BOOTSTRAP_SIZE ->
            Truncated = binary:part(Bin, 0, ?MAX_BOOTSTRAP_SIZE),
            {ok, <<Truncated/binary, "\n[...truncated at 20KB...]">>};
        {ok, Bin} ->
            {ok, Bin};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Write content to a bootstrap file.
-spec write_bootstrap_file(binary(), binary(), binary()) -> ok | {error, term()}.
write_bootstrap_file(AgentId, Filename, Content) ->
    Path = filename:join(agent_dir(AgentId), binary_to_list(Filename)),
    file:write_file(Path, Content).

%% @doc Read all seven bootstrap files. Missing files map to undefined.
-spec read_all_bootstrap_files(binary()) -> #{binary() => binary() | undefined}.
read_all_bootstrap_files(AgentId) ->
    Files = [<<"IDENTITY.md">>, <<"SOUL.md">>, <<"USER.md">>,
             <<"TOOLS.md">>, <<"AGENTS.md">>, <<"BOOTSTRAP.md">>,
             <<"MEMORY.md">>],
    maps:from_list(lists:map(fun(F) ->
        case read_bootstrap_file(AgentId, F) of
            {ok, Content} -> {F, Content};
            {error, _}    -> {F, undefined}
        end
    end, Files)).

%% @doc Read a daily log file for a given date (<<"YYYY-MM-DD">>).
-spec read_daily_log(binary(), binary()) -> {ok, binary()} | {error, atom()}.
read_daily_log(AgentId, Date) ->
    Path = filename:join([agent_dir(AgentId), "memory",
                          binary_to_list(<<Date/binary, ".md">>)]),
    case file:read_file(Path) of
        {ok, Bin} when byte_size(Bin) > ?MAX_BOOTSTRAP_SIZE ->
            Truncated = binary:part(Bin, 0, ?MAX_BOOTSTRAP_SIZE),
            {ok, <<Truncated/binary, "\n[...truncated at 20KB...]">>};
        {ok, Bin} -> {ok, Bin};
        {error, _} -> {error, not_found}
    end.

%% @doc List available daily log files (returns filenames, newest first, max 30).
-spec list_daily_logs(binary()) -> [binary()].
list_daily_logs(AgentId) ->
    Dir = filename:join(agent_dir(AgentId), "memory"),
    case file:list_dir(Dir) of
        {ok, Entries} ->
            MdFiles = [list_to_binary(E) || E <- Entries,
                        filename:extension(E) =:= ".md"],
            lists:sublist(lists:reverse(lists:sort(MdFiles)), 30);
        {error, _} -> []
    end.

%% @doc Validate an agent ID: must match ^[a-z0-9_-]+$.
-spec validate_agent_id(binary()) -> ok | {error, invalid_agent_id}.
validate_agent_id(AgentId) when is_binary(AgentId) ->
    case re:run(AgentId, "^[a-z0-9_-]+$", [{capture, none}]) of
        match   -> ok;
        nomatch -> {error, invalid_agent_id}
    end;
validate_agent_id(_) ->
    {error, invalid_agent_id}.

%% Internal

delete_dir_recursive(Dir) ->
    case file:list_dir(Dir) of
        {ok, Entries} ->
            lists:foreach(fun(E) ->
                Path = filename:join(Dir, E),
                case filelib:is_dir(Path) of
                    true  -> delete_dir_recursive(Path);
                    false -> file:delete(Path)
                end
            end, Entries),
            file:del_dir(Dir),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
