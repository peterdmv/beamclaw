-module(beamclaw_a2a_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    beamclaw_a2a_sup:start_link().

stop(_State) ->
    ok.
