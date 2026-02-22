%% @doc Default bootstrap file templates for agent workspaces.
%%
%% Pure data module. Each template provides the initial content for one of
%% the six markdown files in an agent workspace directory.
-module(bc_workspace_templates).

-export([template/1, all_templates/0]).

%% Dialyzer infers a narrower binary type from the literal filenames in
%% all_templates/0; the spec uses binary() for API clarity.
-dialyzer({nowarn_function, [all_templates/0]}).

%% @doc Return the default content for a given bootstrap filename.
-spec template(binary()) -> binary().
template(<<"SOUL.md">>) ->
    <<"# Soul\n\n"
      "You are a helpful, thoughtful assistant. You communicate clearly and\n"
      "concisely, adapting your tone to the user's needs. You are honest about\n"
      "uncertainty and ask clarifying questions when the request is ambiguous.\n">>;
template(<<"IDENTITY.md">>) ->
    <<"# Identity\n\n"
      "- **Name:** Assistant\n"
      "- **Type:** General-purpose\n"
      "- **Emoji:** \n">>;
template(<<"USER.md">>) ->
    <<"# User\n\n"
      "No user profile configured yet. The agent will learn about the user\n"
      "over time and this file can be updated with preferences and context.\n">>;
template(<<"TOOLS.md">>) ->
    <<"# Tools\n\n"
      "This file provides guidance on how the agent should use its available\n"
      "tools. Update it with environment-specific instructions, preferred\n"
      "workflows, or tool restrictions.\n">>;
template(<<"MEMORY.md">>) ->
    <<"# Memory\n\n"
      "Long-term memory for this agent. The agent can read and update this\n"
      "file to remember important facts across sessions.\n">>;
template(<<"AGENTS.md">>) ->
    <<"# Agents\n\n"
      "Workspace guidelines and conventions. Update this file with rules\n"
      "that should apply to all sessions using this agent workspace.\n">>;
template(_) ->
    <<>>.

%% @doc Return all template filenames with their default content.
-spec all_templates() -> [{binary(), binary()}].
all_templates() ->
    Files = [<<"SOUL.md">>, <<"IDENTITY.md">>, <<"USER.md">>,
             <<"TOOLS.md">>, <<"MEMORY.md">>, <<"AGENTS.md">>],
    [{F, template(F)} || F <- Files].
