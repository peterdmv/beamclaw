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

-module(bc_a2a_agent_card).
-moduledoc """
Agent Card generation for A2A protocol.

Builds the Agent Card JSON from BeamClaw configuration, describing the agent's
capabilities, skills, and endpoint information according to the A2A specification.
""".

-export([generate/0, generate/1]).

%% @doc Generate agent card with default configuration.
generate() ->
    generate(#{}).

%% @doc Generate agent card with custom overrides.
generate(Overrides) when is_map(Overrides) ->
    BaseCard = #{
        name => get_agent_name(),
        description => get_agent_description(),
        url => get_agent_url(),
        version => <<"0.3.0">>,
        capabilities => #{
            streaming => true,
            pushNotifications => false
        },
        skills => get_agent_skills(),
        defaultInputModes => [<<"text/plain">>],
        defaultOutputModes => [<<"text/plain">>]
    },
    maps:merge(BaseCard, Overrides).

%% Internal functions

get_agent_name() ->
    case application:get_env(beamclaw_core, agent_name) of
        {ok, Name} when is_list(Name) -> list_to_binary(Name);
        {ok, Name} when is_binary(Name) -> Name;
        _ -> <<"BeamClaw Agent">>
    end.

get_agent_description() ->
    case application:get_env(beamclaw_core, agent_description) of
        {ok, Desc} when is_list(Desc) -> list_to_binary(Desc);
        {ok, Desc} when is_binary(Desc) -> Desc;
        _ -> <<"Agentic LLM system built with Erlang/OTP">>
    end.

get_agent_url() ->
    % Get the base URL from gateway configuration
    GatewayConfig = case application:get_env(beamclaw_gateway, http) of
        {ok, Config} -> Config;
        _ -> #{}
    end,
    
    Port = maps:get(port, GatewayConfig, 18800),
    Host = maps:get(host, GatewayConfig, <<"localhost">>),
    
    % Check for environment override
    ActualPort = case os:getenv("BEAMCLAW_PORT") of
        false -> Port;
        P -> list_to_integer(P)
    end,
    
    Protocol = case maps:get(ssl, GatewayConfig, false) of
        true -> <<"https">>;
        false -> <<"http">>
    end,
    
    iolist_to_binary([Protocol, <<"://">>, Host, <<":">>, integer_to_binary(ActualPort), <<"/a2a">>]).

get_agent_skills() ->
    % Try to derive skills from available tools/config
    % This is a simplified implementation - in practice, this would
    % query the actual BeamClaw tools and capabilities
    DefaultSkills = [
        #{
            name => <<"conversation">>,
            description => <<"Natural language conversation and reasoning">>
        },
        #{
            name => <<"tool_execution">>,
            description => <<"Execute various tools and external commands">>
        },
        #{
            name => <<"memory_access">>,
            description => <<"Access and manage conversation memory">>
        }
    ],
    
    % In the future, this could query beamclaw_tools for available tools
    % and dynamically build the skills list
    case application:get_env(beamclaw_a2a, extra_skills) of
        {ok, ExtraSkills} when is_list(ExtraSkills) ->
            DefaultSkills ++ ExtraSkills;
        _ ->
            DefaultSkills
    end.