%% @doc EUnit tests for bc_skill_parser.
-module(bc_skill_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

%% ---- valid parsing ----

valid_skill_test() ->
    Bin = <<"---\n"
            "name: test-skill\n"
            "description: A test skill\n"
            "homepage: https://example.com\n"
            "---\n"
            "# Test Skill\n\n"
            "This is the skill content.\n">>,
    {ok, Skill} = bc_skill_parser:parse(Bin, global),
    ?assertEqual(<<"test-skill">>, Skill#bc_skill.name),
    ?assertEqual(<<"A test skill">>, Skill#bc_skill.description),
    ?assertEqual(<<"https://example.com">>, Skill#bc_skill.homepage),
    ?assert(binary:match(Skill#bc_skill.content, <<"Test Skill">>) =/= nomatch),
    ?assertEqual(global, Skill#bc_skill.source).

%% ---- with JSON metadata ----

metadata_test() ->
    Bin = <<"---\n"
            "name: meta-skill\n"
            "description: Has metadata\n"
            "metadata: {\"beamclaw\": {\"emoji\": \"🔧\", \"requires\": {\"bins\": [\"jq\"]}}}\n"
            "---\n"
            "Content here.\n">>,
    {ok, Skill} = bc_skill_parser:parse(Bin, {agent, <<"my-agent">>}),
    ?assertEqual(<<"meta-skill">>, Skill#bc_skill.name),
    ?assertEqual({agent, <<"my-agent">>}, Skill#bc_skill.source),
    ?assertMatch(#{<<"beamclaw">> := _}, Skill#bc_skill.metadata).

%% ---- emoji extraction ----

emoji_extraction_test() ->
    Bin = <<"---\n"
            "name: emoji-skill\n"
            "metadata: {\"beamclaw\": {\"emoji\": \"fire\"}}\n"
            "---\n"
            "Content.\n">>,
    {ok, Skill} = bc_skill_parser:parse(Bin, global),
    ?assertEqual(<<"fire">>, Skill#bc_skill.emoji).

%% ---- missing name ----

missing_name_test() ->
    Bin = <<"---\n"
            "description: No name field\n"
            "---\n"
            "Content.\n">>,
    ?assertEqual({error, missing_name}, bc_skill_parser:parse(Bin, global)).

%% ---- no frontmatter ----

no_frontmatter_test() ->
    Bin = <<"# Just markdown\n\nNo frontmatter here.\n">>,
    ?assertEqual({error, no_frontmatter}, bc_skill_parser:parse(Bin, global)).

%% ---- invalid metadata JSON ----

invalid_metadata_json_test() ->
    Bin = <<"---\n"
            "name: bad-json\n"
            "metadata: {not valid json}\n"
            "---\n"
            "Content.\n">>,
    {ok, Skill} = bc_skill_parser:parse(Bin, global),
    ?assertEqual(<<"bad-json">>, Skill#bc_skill.name),
    %% Metadata should be empty map on parse failure
    ?assertEqual(#{}, Skill#bc_skill.metadata).

%% ---- minimal skill (name only) ----

minimal_skill_test() ->
    Bin = <<"---\n"
            "name: minimal\n"
            "---\n"
            "Just content.\n">>,
    {ok, Skill} = bc_skill_parser:parse(Bin, global),
    ?assertEqual(<<"minimal">>, Skill#bc_skill.name),
    ?assertEqual(undefined, Skill#bc_skill.description),
    ?assertEqual(undefined, Skill#bc_skill.homepage),
    ?assertEqual(undefined, Skill#bc_skill.emoji).

%% ---- path stored ----

path_test() ->
    Bin = <<"---\nname: pathed\n---\nContent.\n">>,
    {ok, Skill} = bc_skill_parser:parse(Bin, global, "/tmp/skills/foo/SKILL.md"),
    ?assertEqual("/tmp/skills/foo/SKILL.md", Skill#bc_skill.path).

%% ---- unclosed frontmatter ----

unclosed_frontmatter_test() ->
    Bin = <<"---\nname: oops\nNo closing delimiter.\n">>,
    ?assertEqual({error, no_closing_delimiter}, bc_skill_parser:parse(Bin, global)).
