%% @doc Tool registry — named gen_server backed by ETS.
%%
%% Stores tool module → definition mappings.
%% MCP-discovered tools are also registered here (source = mcp).
-module(bc_tool_registry).
-behaviour(gen_server).

-export([start_link/0, register/2, lookup/1, list/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB, bc_tool_registry).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register a tool module. Def is the result of Mod:definition().
-spec register(Mod :: module(), Def :: map()) -> ok.
register(Mod, Def) ->
    gen_server:cast(?MODULE, {register, Mod, Def}).

%% @doc Look up a tool by name. Returns {ok, {Mod, Def}} or {error, not_found}.
-spec lookup(Name :: binary()) -> {ok, {module(), map()}} | {error, not_found}.
lookup(Name) ->
    case ets:lookup(?TAB, Name) of
        [{Name, Mod, Def}] -> {ok, {Mod, Def}};
        []                 -> {error, not_found}
    end.

%% @doc List all registered tools.
-spec list() -> [{binary(), module(), map()}].
list() ->
    ets:tab2list(?TAB).

init([]) ->
    Tab = ets:new(?TAB, [set, named_table, public, {read_concurrency, true}]),
    %% Register built-in tools
    BuiltIns = [bc_tool_terminal, bc_tool_bash, bc_tool_curl, bc_tool_jq,
                bc_tool_read_file, bc_tool_write_file],
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
