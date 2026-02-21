%% @doc Built-in read_file tool — reads a file path and returns its content.
%%
%% Requires no approval and runs at read_only autonomy, allowing the agent to
%% inspect files (documentation, configs, source) without an approval prompt.
-module(bc_tool_read_file).
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"read_file">>,
      description => <<"Read the contents of a file and return them as text.">>,
      parameters  => #{
          type       => object,
          properties => #{
              path => #{type => string, description => <<"Absolute or relative path to the file">>}
          },
          required   => [<<"path">>]
      },
      source => builtin}.

execute(#{<<"path">> := Path}, _Session, _Context) ->
    case file:read_file(binary_to_list(Path)) of
        {ok, Bin}       -> {ok, Bin};
        {error, Reason} -> {error, iolist_to_binary(atom_to_list(Reason))}
    end.

requires_approval() -> false.

min_autonomy() -> read_only.
