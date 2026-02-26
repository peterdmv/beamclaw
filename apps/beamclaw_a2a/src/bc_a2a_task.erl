%% @doc A2A Task — state machine and serialization.
%%
%% Tasks progress through a defined lifecycle:
%%   submitted → working → input_required → completed | failed | canceled | rejected
-module(bc_a2a_task).

-include("bc_a2a_types.hrl").

-export([new/1, new/2, new/3,
         transition/2, transition/3,
         add_artifact/2,
         is_terminal/1,
         to_json/1,
         from_json/1]).

-export([message_to_json/1]).

-define(TERMINAL_STATES, [completed, failed, canceled, rejected]).

%% --- Construction ---

-spec new(#a2a_message{}) -> #a2a_task{}.
new(Message) ->
    new(generate_id(), Message, #{}).

-spec new(binary(), #a2a_message{}) -> #a2a_task{}.
new(Id, Message) ->
    new(Id, Message, #{}).

-spec new(binary(), #a2a_message{}, map()) -> #a2a_task{}.
new(Id, Message, Metadata) ->
    Now = iso8601_now(),
    #a2a_task{
        id = Id,
        context_id = maps:get(<<"contextId">>, Metadata, undefined),
        status = #a2a_status{state = submitted, message = undefined, timestamp = Now},
        history = [Message],
        artifacts = [],
        metadata = Metadata
    }.

%% --- State Transitions ---

-spec transition(#a2a_task{}, a2a_task_state()) ->
    {ok, #a2a_task{}} | {error, invalid_transition}.
transition(Task, NewState) ->
    transition(Task, NewState, undefined).

-spec transition(#a2a_task{}, a2a_task_state(), #a2a_message{} | undefined) ->
    {ok, #a2a_task{}} | {error, invalid_transition}.
transition(#a2a_task{status = #a2a_status{state = Current}} = Task, NewState, Message) ->
    case valid_transition(Current, NewState) of
        true ->
            Now = iso8601_now(),
            NewHistory = case Message of
                undefined -> Task#a2a_task.history;
                _         -> Task#a2a_task.history ++ [Message]
            end,
            {ok, Task#a2a_task{
                status = #a2a_status{state = NewState, message = Message, timestamp = Now},
                history = NewHistory
            }};
        false ->
            {error, invalid_transition}
    end.

%% --- Artifacts ---

-spec add_artifact(#a2a_task{}, #a2a_artifact{}) -> #a2a_task{}.
add_artifact(#a2a_task{artifacts = Arts} = Task, Artifact) ->
    Indexed = Artifact#a2a_artifact{index = length(Arts)},
    Task#a2a_task{artifacts = Arts ++ [Indexed]}.

%% --- Queries ---

-spec is_terminal(#a2a_task{}) -> boolean().
is_terminal(#a2a_task{status = #a2a_status{state = State}}) ->
    lists:member(State, ?TERMINAL_STATES).

%% --- Serialization ---

-spec to_json(#a2a_task{}) -> map().
to_json(#a2a_task{} = T) ->
    reject_undefined(#{
        <<"id">>         => T#a2a_task.id,
        <<"contextId">>  => T#a2a_task.context_id,
        <<"status">>     => status_to_json(T#a2a_task.status),
        <<"history">>    => [message_to_json(M) || M <- T#a2a_task.history],
        <<"artifacts">>  => [artifact_to_json(A) || A <- T#a2a_task.artifacts],
        <<"metadata">>   => T#a2a_task.metadata
    }).

-spec from_json(map()) -> #a2a_task{}.
from_json(Json) ->
    StatusJson = maps:get(<<"status">>, Json, #{}),
    #a2a_task{
        id = maps:get(<<"id">>, Json),
        context_id = maps:get(<<"contextId">>, Json, undefined),
        status = #a2a_status{
            state = binary_to_existing_atom(maps:get(<<"state">>, StatusJson, <<"submitted">>)),
            message = parse_message(maps:get(<<"message">>, StatusJson, undefined)),
            timestamp = maps:get(<<"timestamp">>, StatusJson, iso8601_now())
        },
        history = [parse_message(M) || M <- maps:get(<<"history">>, Json, [])],
        artifacts = [parse_artifact(A) || A <- maps:get(<<"artifacts">>, Json, [])],
        metadata = maps:get(<<"metadata">>, Json, #{})
    }.

%% --- JSON helpers ---

-spec message_to_json(#a2a_message{}) -> map().
message_to_json(#a2a_message{role = Role, parts = Parts, metadata = Meta}) ->
    reject_undefined(#{
        <<"role">>     => atom_to_binary(Role),
        <<"parts">>    => [part_to_json(P) || P <- Parts],
        <<"metadata">> => Meta
    }).

%% --- Internal ---

valid_transition(submitted,      working)        -> true;
valid_transition(submitted,      canceled)       -> true;
valid_transition(submitted,      rejected)       -> true;
valid_transition(working,        completed)      -> true;
valid_transition(working,        failed)         -> true;
valid_transition(working,        canceled)       -> true;
valid_transition(working,        input_required) -> true;
valid_transition(input_required, working)        -> true;
valid_transition(input_required, canceled)       -> true;
valid_transition(_,              _)              -> false.

status_to_json(#a2a_status{state = State, message = Msg, timestamp = Ts}) ->
    reject_undefined(#{
        <<"state">>     => atom_to_binary(State),
        <<"message">>   => case Msg of undefined -> undefined; _ -> message_to_json(Msg) end,
        <<"timestamp">> => Ts
    }).

part_to_json(#{type := text, text := Text}) ->
    #{<<"type">> => <<"text">>, <<"text">> => Text};
part_to_json(#{type := file, file := File}) ->
    #{<<"type">> => <<"file">>, <<"file">> => File};
part_to_json(#{type := data, data := Data}) ->
    #{<<"type">> => <<"data">>, <<"data">> => Data}.

artifact_to_json(#a2a_artifact{name = Name, description = Desc, parts = Parts, index = Idx}) ->
    reject_undefined(#{
        <<"name">>        => Name,
        <<"description">> => Desc,
        <<"parts">>       => [part_to_json(P) || P <- Parts],
        <<"index">>       => Idx
    }).

parse_message(undefined) -> undefined;
parse_message(null) -> undefined;
parse_message(Json) when is_map(Json) ->
    #a2a_message{
        role = binary_to_existing_atom(maps:get(<<"role">>, Json, <<"user">>)),
        parts = [parse_part(P) || P <- maps:get(<<"parts">>, Json, [])],
        metadata = maps:get(<<"metadata">>, Json, #{})
    }.

parse_part(#{<<"type">> := <<"text">>, <<"text">> := Text}) ->
    #{type => text, text => Text};
parse_part(#{<<"type">> := <<"file">>, <<"file">> := File}) ->
    #{type => file, file => File};
parse_part(#{<<"type">> := <<"data">>, <<"data">> := Data}) ->
    #{type => data, data => Data}.

parse_artifact(Json) ->
    #a2a_artifact{
        name = maps:get(<<"name">>, Json, undefined),
        description = maps:get(<<"description">>, Json, undefined),
        parts = [parse_part(P) || P <- maps:get(<<"parts">>, Json, [])],
        index = maps:get(<<"index">>, Json, 0)
    }.

generate_id() ->
    Bytes = crypto:strong_rand_bytes(8),
    list_to_binary(lists:flatten(
        [io_lib:format("~2.16.0b", [B]) || <<B>> <= Bytes]
    )).

iso8601_now() ->
    calendar:system_time_to_rfc3339(erlang:system_time(second), [{offset, "Z"}]).

reject_undefined(Map) when is_map(Map) ->
    maps:filter(fun(_K, V) -> V =/= undefined end, Map).

binary_to_existing_atom(B) when is_binary(B) ->
    erlang:binary_to_existing_atom(B, utf8).
