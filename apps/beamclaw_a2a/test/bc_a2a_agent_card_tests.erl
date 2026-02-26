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

-module(bc_a2a_agent_card_tests).

-include_lib("eunit/include/eunit.hrl").

generate_default_test() ->
    Card = bc_a2a_agent_card:generate(),
    
    % Check required fields
    ?assert(is_map(Card)),
    ?assert(maps:is_key(name, Card)),
    ?assert(maps:is_key(description, Card)),
    ?assert(maps:is_key(url, Card)),
    ?assert(maps:is_key(version, Card)),
    ?assert(maps:is_key(capabilities, Card)),
    ?assert(maps:is_key(skills, Card)),
    ?assert(maps:is_key(defaultInputModes, Card)),
    ?assert(maps:is_key(defaultOutputModes, Card)),
    
    % Check version
    ?assertEqual(<<"0.3.0">>, maps:get(version, Card)),
    
    % Check capabilities structure
    Capabilities = maps:get(capabilities, Card),
    ?assert(is_map(Capabilities)),
    ?assertEqual(true, maps:get(streaming, Capabilities)),
    ?assertEqual(false, maps:get(pushNotifications, Capabilities)),
    
    % Check default modes
    ?assertEqual([<<"text/plain">>], maps:get(defaultInputModes, Card)),
    ?assertEqual([<<"text/plain">>], maps:get(defaultOutputModes, Card)),
    
    % Check skills is a list
    ?assert(is_list(maps:get(skills, Card))).

generate_with_overrides_test() ->
    Overrides = #{
        name => <<"Test Agent">>,
        description => <<"Test Description">>
    },
    Card = bc_a2a_agent_card:generate(Overrides),
    
    ?assertEqual(<<"Test Agent">>, maps:get(name, Card)),
    ?assertEqual(<<"Test Description">>, maps:get(description, Card)),
    ?assertEqual(<<"0.3.0">>, maps:get(version, Card)).

url_construction_test() ->
    Card = bc_a2a_agent_card:generate(),
    Url = maps:get(url, Card),
    
    ?assert(is_binary(Url)),
    ?assert(byte_size(Url) > 0),
    % Should end with /a2a
    ?assertEqual(<<"/a2a">>, binary:part(Url, byte_size(Url) - 4, 4)).

skills_structure_test() ->
    Card = bc_a2a_agent_card:generate(),
    Skills = maps:get(skills, Card),
    
    ?assert(is_list(Skills)),
    ?assert(length(Skills) > 0),
    
    % Check first skill structure
    [FirstSkill | _] = Skills,
    ?assert(is_map(FirstSkill)),
    ?assert(maps:is_key(name, FirstSkill)),
    ?assert(maps:is_key(description, FirstSkill)).