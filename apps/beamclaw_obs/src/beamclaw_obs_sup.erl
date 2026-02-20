-module(beamclaw_obs_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(PG_SCOPE, bc_obs_backends).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% rest_for_one: a pg crash cascades to manager + log (they re-join the
    %% fresh scope); a manager crash only restarts manager; a log crash only
    %% restarts log (re-joins group in init).
    SupFlags = #{strategy => rest_for_one, intensity => 5, period => 30},
    Children = [
        #{id       => bc_obs_pg,
          start    => {pg, start_link, [?PG_SCOPE]},
          restart  => permanent,
          shutdown => 5000,
          type     => worker,
          modules  => [pg]},
        child(bc_obs_manager, worker),
        child(bc_obs_log,     worker)
    ],
    {ok, {SupFlags, Children}}.

child(Mod, Type) ->
    #{id       => Mod,
      start    => {Mod, start_link, []},
      restart  => permanent,
      shutdown => 5000,
      type     => Type,
      modules  => [Mod]}.
