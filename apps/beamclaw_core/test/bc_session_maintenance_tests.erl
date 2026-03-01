-module(bc_session_maintenance_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for bc_session_maintenance — config defaults, nightly scheduling.

config_defaults_test() ->
    %% When no maintenance config is set, get_config returns empty map
    application:unset_env(beamclaw_core, maintenance),
    Cfg = bc_session_maintenance:get_config(),
    ?assertEqual(false, maps:get(enabled, Cfg, false)).

config_enabled_test() ->
    application:set_env(beamclaw_core, maintenance, #{enabled => true}),
    Cfg = bc_session_maintenance:get_config(),
    ?assertEqual(true, maps:get(enabled, Cfg, false)),
    application:unset_env(beamclaw_core, maintenance).

nightly_during_quiet_hours_test() ->
    %% should_run_nightly returns true when within hours and not done today
    {{Y, M, D}, {Hour, _, _}} = calendar:universal_time(),
    %% Set quiet hours to include current hour
    Start = Hour,
    End = Hour + 1,
    ?assertEqual(true,
        bc_session_maintenance:should_run_nightly({Start, End}, undefined)).

nightly_skips_outside_hours_test() ->
    %% should_run_nightly returns false when outside quiet hours
    {{_Y, _M, _D}, {Hour, _, _}} = calendar:universal_time(),
    %% Set quiet hours to NOT include current hour
    Start = (Hour + 2) rem 24,
    End = (Hour + 3) rem 24,
    %% Handle wrap-around: if Start >= End the range is empty/wrapped
    case Start < End of
        true ->
            ?assertEqual(false,
                bc_session_maintenance:should_run_nightly({Start, End}, undefined));
        false ->
            %% Wrapped range: StartHour >= EndHour means the simple comparison
            %% in should_run_nightly will return false, which is what we want
            ?assertEqual(false,
                bc_session_maintenance:should_run_nightly({Start, End}, undefined))
    end.

nightly_once_per_day_test() ->
    %% should_run_nightly returns false when already done today
    {{Y, M, D}, {Hour, _, _}} = calendar:universal_time(),
    Today = {Y, M, D},
    %% Even within quiet hours, should return false if done today
    ?assertEqual(false,
        bc_session_maintenance:should_run_nightly({Hour, Hour + 1}, Today)).

nightly_different_day_reruns_test() ->
    %% should_run_nightly returns true if done on a different day
    {{Y, M, D}, {Hour, _, _}} = calendar:universal_time(),
    Yesterday = case D of
        1 -> {Y, M, 28};  %% approximate
        _ -> {Y, M, D - 1}
    end,
    ?assertEqual(true,
        bc_session_maintenance:should_run_nightly({Hour, Hour + 1}, Yesterday)).

%% Integration-style tests: scan with empty registry

scan_idle_empty_registry_test() ->
    %% Scanning with no sessions should not crash
    case whereis(bc_session_registry) of
        undefined -> {ok, _} = bc_session_registry:start_link();
        _         -> ok
    end,
    ?assertEqual(ok, bc_session_maintenance:scan_idle_sessions(#{})).

scan_pre_expiry_empty_registry_test() ->
    %% Scanning with no sessions should not crash
    application:set_env(beamclaw_core, session_ttl_seconds, 3600),
    case whereis(bc_session_registry) of
        undefined -> {ok, _} = bc_session_registry:start_link();
        _         -> ok
    end,
    ?assertEqual(ok, bc_session_maintenance:scan_pre_expiry(#{})).
