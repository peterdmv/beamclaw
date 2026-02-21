%% @doc Built-in write_file tool — writes content to a file path (create or overwrite).
%%
%% Requires approval because it is a destructive operation (overwrites without
%% confirmation). Runs at supervised autonomy minimum.
-module(bc_tool_write_file).
-behaviour(bc_tool).

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

definition() ->
    #{name        => <<"write_file">>,
      description => <<"Write text content to a file, creating or overwriting it.">>,
      parameters  => #{
          type       => object,
          properties => #{
              path    => #{type => string, description => <<"Absolute or relative path to the file">>},
              content => #{type => string, description => <<"Text content to write">>}
          },
          required   => [<<"path">>, <<"content">>]
      },
      source => builtin}.

execute(#{<<"path">> := Path, <<"content">> := Content}, _Session, _Context) ->
    case file:write_file(binary_to_list(Path), Content) of
        ok              -> {ok, <<"ok">>};
        {error, Reason} -> {error, iolist_to_binary(atom_to_list(Reason))}
    end.

requires_approval() -> true.

min_autonomy() -> supervised.
