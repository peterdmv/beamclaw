%% @doc Log observability backend. Implements the bc_observer behaviour.
%%
%% Emits one structured log line per event via OTP logger.
%% Log level filtering is controlled by the operator via the kernel app's
%% logger config in sys.config — no level field is stored in state.
-module(bc_obs_log).
-behaviour(gen_server).
%% Implements bc_observer backend callbacks (handle_event/2).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% bc_obs callback
-export([handle_event/2]).

-define(PG_SCOPE, bc_obs_backends).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    pg:join(?PG_SCOPE, backends, self()),
    {ok, #{}}.

handle_cast({event, Event}, State) ->
    {ok, NewState} = handle_event(Event, State),
    {noreply, NewState};
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

%% bc_obs callback
handle_event(#{type := Type, data := Data} = _Event, State) ->
    logger:info("[obs] type=~p data=~p", [Type, Data]),
    {ok, State}.
