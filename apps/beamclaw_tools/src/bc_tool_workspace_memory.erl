%%
%% Copyright Péter Dimitrov 2026, All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(bc_tool_workspace_memory).
-moduledoc """
Built-in workspace_memory tool — read/append/replace MEMORY.md and daily logs.

Allows the agent to manage its own long-term memory file and daily logs.
The path is constructed internally from the agent_id in bc_session_ref,
so no path traversal is possible.

Actions:
  - read:         Return current MEMORY.md content
  - append:       Append text to MEMORY.md
  - replace:      Replace entire MEMORY.md content
  - read_daily:   Read today's daily log (or specific date)
  - append_daily: Append text to today's daily log
  - list_daily:   List available daily log files
""".
-behaviour(bc_tool).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

-define(MAX_READ_SIZE, 20480). %% 20 KB

definition() ->
    #{name        => <<"workspace_memory">>,
      description => <<"Read, append to, or replace the agent's long-term MEMORY.md file. "
                       "Also read and write daily logs (memory/YYYY-MM-DD.md).">>,
      parameters  => #{
          type       => object,
          properties => #{
              action => #{type => string,
                          description => <<"Action to perform">>,
                          enum => [<<"read">>, <<"append">>, <<"replace">>,
                                   <<"read_daily">>, <<"append_daily">>, <<"list_daily">>]},
              content => #{type => string,
                           description => <<"Text to append or replace with (for append, replace, append_daily)">>},
              date => #{type => string,
                        description => <<"Date in YYYY-MM-DD format (for read_daily/append_daily; defaults to today)">>}
          },
          required   => [<<"action">>]
      },
      source => builtin}.

%% ---- MEMORY.md actions ----

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
        {ok, Bin}  -> Bin;
        {error, _} -> <<>>
    end,
    NewContent = <<Existing/binary, "\n", Content/binary>>,
    filelib:ensure_dir(Path),
    case file:write_file(Path, NewContent) of
        ok              -> {ok, <<"Memory updated.">>};
        {error, Reason} -> {error, iolist_to_binary(io_lib:format("write error: ~p", [Reason]))}
    end;

execute(#{<<"action">> := <<"replace">>, <<"content">> := Content}, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Path = bc_workspace_path:bootstrap_file(AgentId, <<"MEMORY.md">>),
    filelib:ensure_dir(Path),
    case file:write_file(Path, Content) of
        ok              -> {ok, <<"Memory replaced.">>};
        {error, Reason} -> {error, iolist_to_binary(io_lib:format("write error: ~p", [Reason]))}
    end;

%% ---- Daily log actions ----

execute(#{<<"action">> := <<"read_daily">>} = Args, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Date = resolve_date(Args),
    Path = bc_workspace_path:daily_log_file(AgentId, Date),
    case file:read_file(Path) of
        {ok, Bin} when byte_size(Bin) > ?MAX_READ_SIZE ->
            Truncated = binary:part(Bin, 0, ?MAX_READ_SIZE),
            {ok, <<Truncated/binary, "\n[...truncated at 20KB...]">>};
        {ok, Bin} ->
            {ok, Bin};
        {error, enoent} ->
            {ok, <<"(No daily log for ", Date/binary, ")">>};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("read error: ~p", [Reason]))}
    end;

execute(#{<<"action">> := <<"append_daily">>, <<"content">> := Content} = Args, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Date = resolve_date(Args),
    Dir = bc_workspace_path:memory_dir(AgentId),
    filelib:ensure_dir(filename:join(Dir, "dummy")),
    Path = bc_workspace_path:daily_log_file(AgentId, Date),
    Existing = case file:read_file(Path) of
        {ok, Bin}  -> Bin;
        {error, _} -> <<>>
    end,
    NewContent = <<Existing/binary, "\n", Content/binary>>,
    case file:write_file(Path, NewContent) of
        ok              -> {ok, <<"Daily log updated.">>};
        {error, Reason} -> {error, iolist_to_binary(io_lib:format("write error: ~p", [Reason]))}
    end;

execute(#{<<"action">> := <<"list_daily">>}, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Dir = bc_workspace_path:memory_dir(AgentId),
    case file:list_dir(Dir) of
        {ok, Entries} ->
            MdFiles = [list_to_binary(E) || E <- Entries,
                        filename:extension(E) =:= ".md"],
            Sorted = lists:sublist(lists:reverse(lists:sort(MdFiles)), 30),
            case Sorted of
                [] -> {ok, <<"(No daily logs found)">>};
                _  -> {ok, iolist_to_binary(lists:join("\n", Sorted))}
            end;
        {error, enoent} ->
            {ok, <<"(No memory directory yet)">>};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("list error: ~p", [Reason]))}
    end;

%% ---- Error cases ----

execute(#{<<"action">> := <<"append">>}, _Session, _Context) ->
    {error, <<"'content' is required for append action">>};

execute(#{<<"action">> := <<"replace">>}, _Session, _Context) ->
    {error, <<"'content' is required for replace action">>};

execute(#{<<"action">> := <<"append_daily">>}, _Session, _Context) ->
    {error, <<"'content' is required for append_daily action">>};

execute(_, _Session, _Context) ->
    {error, <<"Invalid action. Use: read, append, replace, read_daily, append_daily, or list_daily">>}.

requires_approval() -> false.

min_autonomy() -> read_only.

%% Internal

resolve_date(#{<<"date">> := Date}) when is_binary(Date), byte_size(Date) > 0 ->
    Date;
resolve_date(_) ->
    today_date().

today_date() ->
    {Y, M, D} = date(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).
