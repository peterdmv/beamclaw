-module(beamclaw_core_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = bc_session_store:init_table(),
    bc_workspace:ensure_default_agent(),
    beamclaw_core_sup:start_link().

stop(_State) ->
    ok.
