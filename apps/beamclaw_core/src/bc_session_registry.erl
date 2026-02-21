%% @doc Session registry — named gen_server, ETS: session_id → pid.
%%
%% register/2 uses gen_server:call so the ETS insert is guaranteed complete
%% before the caller returns. This eliminates the timer:sleep race condition
%% that would arise if channels tried to look up a session immediately after
%% start_session/1 returned.
-module(bc_session_registry).
-behaviour(gen_server).

-export([start_link/0, register/2, lookup/1, unregister/1, all/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB, bc_session_registry).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register(SessionId :: binary(), Pid :: pid()) -> ok.
register(SessionId, Pid) ->
    gen_server:call(?MODULE, {register, SessionId, Pid}).

-spec lookup(SessionId :: binary()) -> {ok, pid()} | {error, not_found}.
lookup(SessionId) ->
    case ets:lookup(?TAB, SessionId) of
        [{SessionId, Pid}] -> {ok, Pid};
        []                 -> {error, not_found}
    end.

-spec unregister(SessionId :: binary()) -> ok.
unregister(SessionId) ->
    gen_server:cast(?MODULE, {unregister, SessionId}).

-spec all() -> [{binary(), pid()}].
all() ->
    ets:tab2list(?TAB).

init([]) ->
    ets:new(?TAB, [set, named_table, public, {read_concurrency, true}]),
    {ok, #{}}.

handle_call({register, SessionId, Pid}, _From, State) ->
    ets:insert(?TAB, {SessionId, Pid}),
    erlang:monitor(process, Pid),
    {reply, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast({unregister, SessionId}, State) ->
    ets:delete(?TAB, SessionId),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    ets:match_delete(?TAB, {'_', Pid}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
