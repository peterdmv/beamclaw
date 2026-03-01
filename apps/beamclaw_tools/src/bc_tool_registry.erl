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

-module(bc_tool_registry).
-moduledoc """
Tool registry — named gen_server backed by ETS.

Stores tool module → definition mappings.
MCP-discovered tools are also registered here (source = mcp).
""".
-behaviour(gen_server).

-export([start_link/0, register/2, lookup/1, list/0,
         list_names/0, get_definition/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB, bc_tool_registry).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc "Register a tool module. Def is the result of Mod:definition().".
-spec register(Mod :: module(), Def :: map()) -> ok.
register(Mod, Def) ->
    gen_server:cast(?MODULE, {register, Mod, Def}).

-doc "Look up a tool by name. Returns {ok, {Mod, Def}} or {error, not_found}.".
-spec lookup(Name :: binary()) -> {ok, {module(), map()}} | {error, not_found}.
lookup(Name) ->
    case ets:lookup(?TAB, Name) of
        [{Name, Mod, Def}] -> {ok, {Mod, Def}};
        []                 -> {error, not_found}
    end.

-doc "List all registered tools.".
-spec list() -> [{binary(), module(), map()}].
list() ->
    ets:tab2list(?TAB).

-doc "List just the registered tool names.".
-spec list_names() -> [binary()].
list_names() ->
    [Name || {Name, _Mod, _Def} <- ets:tab2list(?TAB)].

-doc "Get the definition for a specific tool by name.".
-spec get_definition(Name :: binary()) -> {ok, map()} | {error, not_found}.
get_definition(Name) ->
    case ets:lookup(?TAB, Name) of
        [{Name, _Mod, Def}] -> {ok, Def};
        []                  -> {error, not_found}
    end.

init([]) ->
    Tab = ets:new(?TAB, [set, named_table, public, {read_concurrency, true}]),
    %% Register built-in tools (sandbox tools register themselves on app start)
    BuiltIns = [bc_tool_terminal, bc_tool_bash, bc_tool_curl, bc_tool_jq,
                bc_tool_read_file, bc_tool_write_file, bc_tool_delete_file,
                bc_tool_workspace_memory, bc_tool_web_search],
    lists:foreach(fun(Mod) ->
        Def = Mod:definition(),
        ets:insert(Tab, {maps:get(name, Def), Mod, Def})
    end, BuiltIns),
    {ok, #{tab => Tab}}.

handle_cast({register, Mod, Def}, State) ->
    ets:insert(?TAB, {maps:get(name, Def), Mod, Def}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({lookup, Name}, _From, State) ->
    Result = case ets:lookup(?TAB, Name) of
        [{Name, Mod, Def}] -> {ok, {Mod, Def}};
        []                 -> {error, not_found}
    end,
    {reply, Result, State};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
