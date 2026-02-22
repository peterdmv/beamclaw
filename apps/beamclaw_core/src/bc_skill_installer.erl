%% @doc Skill installer — runs install specs from SKILL.md metadata.
%%
%% Reads metadata.beamclaw.install array from a skill's metadata and
%% attempts installation using the first compatible installer.
%%
%% Supported kinds: apt, brew, npm, pip, download.
-module(bc_skill_installer).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([install/1, available_install_specs/1]).

%% @doc Attempt to install a skill's dependencies.
-spec install(#bc_skill{}) -> ok | {error, term()}.
install(#bc_skill{metadata = Metadata}) ->
    Beamclaw = maps:get(<<"beamclaw">>, Metadata, #{}),
    Specs = maps:get(<<"install">>, Beamclaw, []),
    case Specs of
        [] -> {error, no_install_specs};
        _ when is_list(Specs) ->
            Compatible = filter_compatible(Specs),
            case Compatible of
                [] -> {error, no_compatible_installer};
                [Spec | _] -> run_install(Spec)
            end;
        _ -> {error, invalid_install_specs}
    end.

%% @doc Return install specs that are compatible with the current system.
-spec available_install_specs(#bc_skill{}) -> [map()].
available_install_specs(#bc_skill{metadata = Metadata}) ->
    Beamclaw = maps:get(<<"beamclaw">>, Metadata, #{}),
    Specs = maps:get(<<"install">>, Beamclaw, []),
    case is_list(Specs) of
        true  -> filter_compatible(Specs);
        false -> []
    end.

%% Internal

filter_compatible(Specs) ->
    [S || S <- Specs, is_map(S), is_compatible(S)].

is_compatible(#{<<"kind">> := <<"apt">>}) ->
    os:find_executable("apt-get") =/= false;
is_compatible(#{<<"kind">> := <<"brew">>}) ->
    os:find_executable("brew") =/= false;
is_compatible(#{<<"kind">> := <<"npm">>}) ->
    os:find_executable("npm") =/= false;
is_compatible(#{<<"kind">> := <<"pip">>}) ->
    os:find_executable("pip") =/= false orelse
    os:find_executable("pip3") =/= false;
is_compatible(#{<<"kind">> := <<"download">>}) ->
    os:find_executable("curl") =/= false;
is_compatible(_) ->
    false.

run_install(#{<<"kind">> := <<"apt">>, <<"package">> := Pkg}) ->
    run_cmd("sudo apt-get install -y " ++ binary_to_list(Pkg));
run_install(#{<<"kind">> := <<"brew">>, <<"formula">> := Formula}) ->
    run_cmd("brew install " ++ binary_to_list(Formula));
run_install(#{<<"kind">> := <<"npm">>, <<"package">> := Pkg}) ->
    run_cmd("npm install -g " ++ binary_to_list(Pkg));
run_install(#{<<"kind">> := <<"pip">>, <<"package">> := Pkg}) ->
    PipBin = case os:find_executable("pip3") of
        false -> "pip";
        _     -> "pip3"
    end,
    run_cmd(PipBin ++ " install " ++ binary_to_list(Pkg));
run_install(#{<<"kind">> := <<"download">>, <<"url">> := Url, <<"target">> := Target}) ->
    ExpandedTarget = expand_home(binary_to_list(Target)),
    ok = filelib:ensure_dir(ExpandedTarget),
    run_cmd("curl -L -o " ++ ExpandedTarget ++ " " ++ binary_to_list(Url) ++
            " && chmod +x " ++ ExpandedTarget);
run_install(_) ->
    {error, invalid_install_spec}.

run_cmd(Cmd) ->
    io:format("  Running: ~s~n", [Cmd]),
    Output = os:cmd(Cmd),
    io:format("  ~s~n", [Output]),
    ok.

expand_home("~/" ++ Rest) ->
    Home = os:getenv("HOME"),
    filename:join(Home, Rest);
expand_home(Path) ->
    Path.
