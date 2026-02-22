-module(beamclaw_core_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 30},
    Children = [
        #{id       => bc_session_registry,
          start    => {bc_session_registry, start_link, []},
          restart  => permanent,
          shutdown => 5000,
          type     => worker,
          modules  => [bc_session_registry]},
        #{id       => bc_session_cleaner,
          start    => {bc_session_cleaner, start_link, []},
          restart  => permanent,
          shutdown => 5000,
          type     => worker,
          modules  => [bc_session_cleaner]},
        #{id       => bc_sessions_sup,
          start    => {bc_sessions_sup, start_link, []},
          restart  => permanent,
          shutdown => infinity,
          type     => supervisor,
          modules  => [bc_sessions_sup]}
    ],
    {ok, {SupFlags, Children}}.
