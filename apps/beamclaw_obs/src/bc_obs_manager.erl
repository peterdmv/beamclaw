%% @doc Observability fan-out manager.
%%
%% Receives cast events and forwards to all registered backends via pg process groups.
-module(bc_obs_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PG_SCOPE, bc_obs_backends).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% pg scope is started as a supervised sibling; nothing to do here.
    {ok, #{}}.

handle_cast({emit, Event}, State) ->
    Members = pg:get_members(?PG_SCOPE, backends),
    lists:foreach(fun(Pid) -> gen_server:cast(Pid, {event, Event}) end, Members),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
