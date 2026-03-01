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

-module(bc_tool_delete_file_tests).
-moduledoc "EUnit tests for bc_tool_delete_file.".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamclaw_core/include/bc_types.hrl").

session_ref() ->
    #bc_session_ref{
        session_id  = <<"test-session">>,
        user_id     = <<"test-user">>,
        session_pid = self(),
        autonomy    = supervised,
        agent_id    = <<"test-agent">>
    }.

%% ---- definition ----

definition_test() ->
    Def = bc_tool_delete_file:definition(),
    ?assertEqual(<<"delete_file">>, maps:get(name, Def)),
    Params = maps:get(parameters, Def),
    ?assertEqual([<<"path">>], maps:get(required, Params)),
    ?assertEqual(true, bc_tool_delete_file:requires_approval()),
    ?assertEqual(supervised, bc_tool_delete_file:min_autonomy()).

%% ---- delete existing file ----

delete_existing_file_test() ->
    TmpFile = "/tmp/beamclaw_delete_test_" ++
              integer_to_list(erlang:unique_integer([positive])),
    ok = file:write_file(TmpFile, <<"test content">>),
    ?assert(filelib:is_regular(TmpFile)),
    {ok, <<"ok">>} = bc_tool_delete_file:execute(
        #{<<"path">> => list_to_binary(TmpFile)}, session_ref(), #{}),
    ?assertNot(filelib:is_regular(TmpFile)).

%% ---- delete nonexistent file (idempotent) ----

delete_nonexistent_file_test() ->
    Path = <<"/tmp/beamclaw_delete_test_nonexistent_",
             (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    {ok, Msg} = bc_tool_delete_file:execute(
        #{<<"path">> => Path}, session_ref(), #{}),
    ?assert(binary:match(Msg, <<"already deleted">>) =/= nomatch).
