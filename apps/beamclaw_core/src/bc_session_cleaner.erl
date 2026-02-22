%% @doc Periodic session cleanup — deletes expired stored sessions from Mnesia.
%%
%% Runs every `session_cleanup_interval_ms` (default: 5 minutes).
%% Uses the `session_ttl_seconds` config key to determine expiry.
-module(bc_session_cleaner).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_INTERVAL_MS, 300000).  %% 5 minutes
-define(DEFAULT_TTL_S, 3600).          %% 1 hour

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    IntervalMs = bc_config:get(beamclaw_core, session_cleanup_interval_ms,
                               ?DEFAULT_INTERVAL_MS),
    erlang:send_after(IntervalMs, self(), cleanup),
    {ok, #{interval_ms => IntervalMs}}.

handle_info(cleanup, #{interval_ms := IntervalMs} = State) ->
    Enabled = bc_config:get(beamclaw_core, session_persistence, true),
    case Enabled of
        true ->
            TTL = bc_config:get(beamclaw_core, session_ttl_seconds, ?DEFAULT_TTL_S),
            Count = bc_session_store:delete_expired(TTL),
            case Count > 0 of
                true ->
                    logger:info("[session_cleaner] deleted ~p expired sessions", [Count]);
                false ->
                    ok
            end;
        false ->
            ok
    end,
    erlang:send_after(IntervalMs, self(), cleanup),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
