%%
%% Copyright Péter Dimitrov 2026, All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(bc_skill_parser_tests).
-moduledoc "EUnit tests for bc_skill_parser.".

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

%% ---- bundled finnhub skill ----

bundled_finnhub_test() ->
    SkillFile = filename:join([code:priv_dir(beamclaw_core), "skills",
                               "finnhub", "SKILL.md"]),
    {ok, Bin} = file:read_file(SkillFile),
    {ok, Skill} = bc_skill_parser:parse(Bin, global, SkillFile),
    ?assertEqual(<<"finnhub">>, Skill#bc_skill.name),
    ?assertNotEqual(undefined, Skill#bc_skill.description),
    ?assertMatch(#{<<"beamclaw">> := #{<<"requires">> := _}},
                 Skill#bc_skill.metadata),
    %% Verify requires has env and bins
    #{<<"beamclaw">> := #{<<"requires">> := Reqs}} = Skill#bc_skill.metadata,
    ?assert(lists:member(<<"FINNHUB_TOKEN">>, maps:get(<<"env">>, Reqs))),
    ?assert(lists:member(<<"curl">>, maps:get(<<"bins">>, Reqs))),
    ?assert(lists:member(<<"jq">>, maps:get(<<"bins">>, Reqs))).

%% ---- bundled nano-banana-pro skill ----

bundled_nano_banana_pro_test() ->
    SkillFile = filename:join([code:priv_dir(beamclaw_core), "skills",
                               "nano-banana-pro", "SKILL.md"]),
    {ok, Bin} = file:read_file(SkillFile),
    {ok, Skill} = bc_skill_parser:parse(Bin, global, SkillFile),
    ?assertEqual(<<"nano-banana-pro">>, Skill#bc_skill.name),
    ?assertEqual(<<"https://ai.google.dev/">>, Skill#bc_skill.homepage),
    ?assertMatch(#{<<"beamclaw">> := #{<<"requires">> := _}},
                 Skill#bc_skill.metadata),
    #{<<"beamclaw">> := #{<<"requires">> := Reqs}} = Skill#bc_skill.metadata,
    ?assert(lists:member(<<"uv">>, maps:get(<<"bins">>, Reqs))),
    ?assert(lists:member(<<"GEMINI_API_KEY">>, maps:get(<<"env">>, Reqs))),
    %% Content should contain {baseDir} template (not yet resolved)
    ?assert(binary:match(Skill#bc_skill.content, <<"{baseDir}">>) =/= nomatch).
