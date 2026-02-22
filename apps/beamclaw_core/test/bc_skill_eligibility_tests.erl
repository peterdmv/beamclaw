%% @doc EUnit tests for bc_skill_eligibility.
-module(bc_skill_eligibility_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

make_skill(Metadata) ->
    #bc_skill{
        name = <<"test">>,
        description = <<"test skill">>,
        content = <<"content">>,
        source = global,
        metadata = Metadata,
        path = "/tmp/test"
    }.

%% ---- no requirements = eligible ----

no_requirements_test() ->
    Skill = make_skill(#{}),
    ?assert(bc_skill_eligibility:is_eligible(Skill)).

%% ---- always flag bypasses checks ----

always_flag_test() ->
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"always">> => true,
        <<"requires">> => #{<<"bins">> => [<<"nonexistent_binary_xyz">>]}
    }}),
    ?assert(bc_skill_eligibility:is_eligible(Skill)).

%% ---- bins check: existing binary ----

bins_existing_test() ->
    %% "sh" should always exist
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"requires">> => #{<<"bins">> => [<<"sh">>]}
    }}),
    ?assert(bc_skill_eligibility:is_eligible(Skill)).

%% ---- bins check: missing binary ----

bins_missing_test() ->
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"requires">> => #{<<"bins">> => [<<"nonexistent_binary_xyz">>]}
    }}),
    ?assertNot(bc_skill_eligibility:is_eligible(Skill)),
    {missing, Details} = bc_skill_eligibility:check(Skill),
    ?assert(lists:member(<<"nonexistent_binary_xyz">>, maps:get(bins, Details))).

%% ---- env check: set variable ----

env_set_test() ->
    os:putenv("BEAMCLAW_TEST_VAR", "value"),
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"requires">> => #{<<"env">> => [<<"BEAMCLAW_TEST_VAR">>]}
    }}),
    ?assert(bc_skill_eligibility:is_eligible(Skill)),
    os:unsetenv("BEAMCLAW_TEST_VAR").

%% ---- env check: unset variable ----

env_unset_test() ->
    os:unsetenv("BEAMCLAW_MISSING_VAR"),
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"requires">> => #{<<"env">> => [<<"BEAMCLAW_MISSING_VAR">>]}
    }}),
    ?assertNot(bc_skill_eligibility:is_eligible(Skill)),
    {missing, Details} = bc_skill_eligibility:check(Skill),
    ?assert(lists:member(<<"BEAMCLAW_MISSING_VAR">>, maps:get(env, Details))).

%% ---- os check: current os ----

os_current_test() ->
    {OsFamily, _} = os:type(),
    OsName = atom_to_binary(OsFamily),
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"requires">> => #{<<"os">> => [OsName]}
    }}),
    ?assert(bc_skill_eligibility:is_eligible(Skill)).

%% ---- os check: wrong os ----

os_wrong_test() ->
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"requires">> => #{<<"os">> => [<<"win32_fake">>]}
    }}),
    ?assertNot(bc_skill_eligibility:is_eligible(Skill)).

%% ---- check returns detailed missing info ----

detailed_missing_test() ->
    os:unsetenv("BEAMCLAW_DETAIL_VAR"),
    Skill = make_skill(#{<<"beamclaw">> => #{
        <<"requires">> => #{
            <<"bins">> => [<<"nonexistent_xyz">>],
            <<"env">> => [<<"BEAMCLAW_DETAIL_VAR">>]
        }
    }}),
    {missing, Details} = bc_skill_eligibility:check(Skill),
    ?assert(maps:is_key(bins, Details)),
    ?assert(maps:is_key(env, Details)).
