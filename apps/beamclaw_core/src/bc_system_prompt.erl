%% @doc System prompt assembly from agent workspace bootstrap files.
%%
%% Reads the six markdown files from the agent's workspace directory and
%% converts them into system-role messages prepended to the conversation
%% history before each LLM call. Assembled fresh every call so that
%% MEMORY.md updates mid-session are picked up immediately.
-module(bc_system_prompt).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([assemble/1]).

%% @doc Assemble system messages from an agent's bootstrap files.
%% Returns messages in order: IDENTITY → SOUL → USER → TOOLS → AGENTS → MEMORY.
%% Files that are missing or empty/whitespace-only are skipped.
%% If the agent doesn't exist, returns a single fallback system message.
-spec assemble(binary()) -> [#bc_message{}].
assemble(AgentId) ->
    case bc_workspace:agent_exists(AgentId) of
        false ->
            [fallback_message()];
        true ->
            Files = bc_workspace:read_all_bootstrap_files(AgentId),
            %% Ordered: IDENTITY → SOUL → USER → TOOLS → AGENTS → MEMORY
            Order = [<<"IDENTITY.md">>, <<"SOUL.md">>, <<"USER.md">>,
                     <<"TOOLS.md">>, <<"AGENTS.md">>, <<"MEMORY.md">>],
            Msgs = lists:filtermap(fun(Filename) ->
                case maps:get(Filename, Files, undefined) of
                    undefined -> false;
                    Content   ->
                        case is_blank(Content) of
                            true  -> false;
                            false ->
                                MsgContent = <<"[", Filename/binary, "]\n", Content/binary>>,
                                {true, #bc_message{
                                    id      = generate_id(),
                                    role    = system,
                                    content = MsgContent,
                                    ts      = 0
                                }}
                        end
                end
            end, Order),
            case Msgs of
                [] -> [fallback_message()];
                _  -> Msgs
            end
    end.

%% Internal

fallback_message() ->
    #bc_message{
        id      = generate_id(),
        role    = system,
        content = <<"You are a helpful assistant.">>,
        ts      = 0
    }.

is_blank(Bin) ->
    Trimmed = string:trim(Bin),
    Trimmed =:= <<>> orelse Trimmed =:= "".

generate_id() ->
    <<N:128>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("sys-~32.16.0b", [N])).
