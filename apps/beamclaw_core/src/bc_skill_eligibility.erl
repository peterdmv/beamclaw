%% @doc Skill eligibility checking.
%%
%% Checks whether a skill's requirements are met on the current system.
%% Requirements are specified in metadata.beamclaw.requires:
%%   - bins:  list of binary names that must exist on PATH
%%   - env:   list of environment variable names that must be set
%%   - os:    list of OS names (e.g. ["linux", "darwin"])
%%
%% If metadata.beamclaw.always =:= true, skip all checks.
-module(bc_skill_eligibility).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([is_eligible/1, check/1]).

%% @doc Check if a skill is eligible (all requirements met).
-spec is_eligible(#bc_skill{}) -> boolean().
is_eligible(Skill) ->
    case check(Skill) of
        ok -> true;
        _  -> false
    end.

%% @doc Check requirements. Returns ok or {missing, Details}.
-spec check(#bc_skill{}) ->
    ok | {missing, #{bins => [binary()], env => [binary()], os => [binary()]}}.
check(#bc_skill{metadata = Metadata}) ->
    Beamclaw = maps:get(<<"beamclaw">>, Metadata, #{}),
    case maps:get(<<"always">>, Beamclaw, false) of
        true -> ok;
        _ ->
            Requires = maps:get(<<"requires">>, Beamclaw, #{}),
            MissingBins = check_bins(maps:get(<<"bins">>, Requires, [])),
            MissingEnv  = check_env(maps:get(<<"env">>, Requires, [])),
            MissingOs   = check_os(maps:get(<<"os">>, Requires, [])),
            case {MissingBins, MissingEnv, MissingOs} of
                {[], [], []} -> ok;
                _ ->
                    Missing = maps:from_list(
                        [{bins, MissingBins} || MissingBins =/= []] ++
                        [{env, MissingEnv}   || MissingEnv =/= []] ++
                        [{os, MissingOs}     || MissingOs =/= []]),
                    {missing, Missing}
            end
    end.

%% Internal

check_bins(Bins) when is_list(Bins) ->
    [B || B <- Bins, not bin_exists(B)];
check_bins(_) -> [].

check_env(Vars) when is_list(Vars) ->
    [V || V <- Vars, not env_set(V)];
check_env(_) -> [].

check_os(OsList) when is_list(OsList), OsList =/= [] ->
    {OsFamily, _} = os:type(),
    OsName = atom_to_binary(OsFamily),
    case lists:member(OsName, OsList) of
        true  -> [];
        false -> OsList
    end;
check_os(_) -> [].

bin_exists(Name) when is_binary(Name) ->
    os:find_executable(binary_to_list(Name)) =/= false;
bin_exists(_) -> false.

env_set(Name) when is_binary(Name) ->
    os:getenv(binary_to_list(Name)) =/= false;
env_set(_) -> false.
