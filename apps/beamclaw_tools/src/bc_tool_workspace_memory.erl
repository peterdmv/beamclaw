%% @doc Built-in workspace_memory tool — read/append/replace MEMORY.md.
%%
%% Allows the agent to manage its own long-term memory file. The path is
%% constructed internally from the agent_id in bc_session_ref, so no path
%% traversal is possible.
%%
%% Actions:
%%   - read:    Return current MEMORY.md content
%%   - append:  Append text to MEMORY.md
%%   - replace: Replace entire MEMORY.md content
-module(bc_tool_workspace_memory).
-behaviour(bc_tool).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

-define(MAX_READ_SIZE, 20480). %% 20 KB

definition() ->
    #{name        => <<"workspace_memory">>,
      description => <<"Read, append to, or replace the agent's long-term MEMORY.md file. "
                       "Use this to remember important facts across sessions.">>,
      parameters  => #{
          type       => object,
          properties => #{
              action => #{type => string,
                          description => <<"Action to perform: read, append, or replace">>,
                          enum => [<<"read">>, <<"append">>, <<"replace">>]},
              content => #{type => string,
                           description => <<"Text to append or replace with (ignored for read)">>}
          },
          required   => [<<"action">>]
      },
      source => builtin}.

execute(#{<<"action">> := <<"read">>}, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Path = bc_workspace_path:bootstrap_file(AgentId, <<"MEMORY.md">>),
    case file:read_file(Path) of
        {ok, Bin} when byte_size(Bin) > ?MAX_READ_SIZE ->
            Truncated = binary:part(Bin, 0, ?MAX_READ_SIZE),
            {ok, <<Truncated/binary, "\n[...truncated at 20KB...]">>};
        {ok, Bin} ->
            {ok, Bin};
        {error, enoent} ->
            {ok, <<"(MEMORY.md does not exist yet)">>};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("read error: ~p", [Reason]))}
    end;

execute(#{<<"action">> := <<"append">>, <<"content">> := Content}, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Path = bc_workspace_path:bootstrap_file(AgentId, <<"MEMORY.md">>),
    Existing = case file:read_file(Path) of
        {ok, Bin}       -> Bin;
        {error, enoent} -> <<>>
    end,
    NewContent = <<Existing/binary, "\n", Content/binary>>,
    case file:write_file(Path, NewContent) of
        ok              -> {ok, <<"Memory updated.">>};
        {error, Reason} -> {error, iolist_to_binary(io_lib:format("write error: ~p", [Reason]))}
    end;

execute(#{<<"action">> := <<"replace">>, <<"content">> := Content}, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Path = bc_workspace_path:bootstrap_file(AgentId, <<"MEMORY.md">>),
    case file:write_file(Path, Content) of
        ok              -> {ok, <<"Memory replaced.">>};
        {error, Reason} -> {error, iolist_to_binary(io_lib:format("write error: ~p", [Reason]))}
    end;

execute(#{<<"action">> := <<"append">>}, _Session, _Context) ->
    {error, <<"'content' is required for append action">>};

execute(#{<<"action">> := <<"replace">>}, _Session, _Context) ->
    {error, <<"'content' is required for replace action">>};

execute(_, _Session, _Context) ->
    {error, <<"Invalid action. Use: read, append, or replace">>}.

requires_approval() -> false.

min_autonomy() -> read_only.
