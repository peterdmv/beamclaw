%% @doc A2A Agent Card — discovery metadata.
%%
%% Published at `GET /.well-known/agent.json` per the A2A protocol specification.
%% Describes the agent's identity, capabilities, skills, and service endpoint.
-module(bc_a2a_agent_card).

-export([build/0, to_json/1]).

-record(agent_card, {
    name                 :: binary(),
    description          :: binary(),
    url                  :: binary(),
    version              :: binary(),
    capabilities         :: map(),
    skills               :: [map()],
    default_input_modes  :: [binary()],
    default_output_modes :: [binary()],
    provider             :: map() | undefined,
    documentation_url    :: binary() | undefined,
    authentication       :: map() | undefined
}).

%% @doc Build the agent card from application config.
-spec build() -> #agent_card{}.
build() ->
    Config = bc_config:get(beamclaw_a2a, agent_card, #{}),
    #agent_card{
        name = maps:get(name, Config, <<"BeamClaw">>),
        description = maps:get(description, Config,
            <<"A fault-tolerant AI agent gateway on the BEAM with A2A interoperability.">>),
        url = maps:get(url, Config, <<"http://localhost:18800">>),
        version = <<"0.1.0">>,
        capabilities = #{
            streaming => maps:get(streaming, Config, true),
            push_notifications => maps:get(push_notifications, Config, false),
            state_transition_history => true
        },
        skills = maps:get(skills, Config, default_skills()),
        default_input_modes = [<<"text/plain">>],
        default_output_modes = [<<"text/plain">>],
        provider = #{
            organization => <<"BeamClaw">>,
            url => <<"https://github.com/peterdmv/beamclaw">>
        },
        documentation_url = <<"https://github.com/peterdmv/beamclaw">>,
        authentication = undefined
    }.

%% @doc Serialize agent card to JSON-compatible map.
-spec to_json(#agent_card{}) -> map().
to_json(#agent_card{} = C) ->
    reject_undefined(#{
        <<"name">>              => C#agent_card.name,
        <<"description">>       => C#agent_card.description,
        <<"url">>               => C#agent_card.url,
        <<"version">>           => C#agent_card.version,
        <<"capabilities">>      => #{
            <<"streaming">>              => maps:get(streaming, C#agent_card.capabilities),
            <<"pushNotifications">>      => maps:get(push_notifications, C#agent_card.capabilities),
            <<"stateTransitionHistory">> => maps:get(state_transition_history, C#agent_card.capabilities)
        },
        <<"skills">>            => [skill_to_json(S) || S <- C#agent_card.skills],
        <<"defaultInputModes">> => C#agent_card.default_input_modes,
        <<"defaultOutputModes">>=> C#agent_card.default_output_modes,
        <<"provider">>          => provider_to_json(C#agent_card.provider),
        <<"documentationUrl">>  => C#agent_card.documentation_url,
        <<"authentication">>    => undefined
    }).

%% --- Internal ---

default_skills() ->
    [#{
        id => <<"general-assistant">>,
        name => <<"General Assistant">>,
        description => <<"A general-purpose AI assistant with file, shell, and MCP tools.">>,
        tags => [<<"general">>, <<"coding">>, <<"files">>],
        examples => [<<"Read and summarise this file">>, <<"Run this shell command">>]
    }].

skill_to_json(S) ->
    #{
        <<"id">>          => maps:get(id, S),
        <<"name">>        => maps:get(name, S),
        <<"description">> => maps:get(description, S),
        <<"tags">>        => maps:get(tags, S),
        <<"examples">>    => maps:get(examples, S)
    }.

provider_to_json(undefined) -> undefined;
provider_to_json(P) ->
    reject_undefined(#{
        <<"organization">> => maps:get(organization, P),
        <<"url">>          => maps:get(url, P, undefined)
    }).

reject_undefined(Map) when is_map(Map) ->
    maps:filter(fun(_K, V) -> V =/= undefined end, Map).
