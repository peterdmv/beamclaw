%% @doc A2A JSON-RPC 2.0 server handler.
%%
%% Implements the A2A protocol operations:
%%   message/send  — send a message to the agent, creating a task
%%   tasks/get     — retrieve task status and history
%%   tasks/cancel  — cancel a running task
%%   tasks/list    — list tasks with filters
%%
%% Transport-agnostic: processes decoded JSON maps, returns JSON-encodable maps.
-module(bc_a2a_server).

-include("bc_a2a_types.hrl").

-export([handle_request/1, agent_card/0]).

%% @doc Handle a JSON-RPC 2.0 request (already decoded from JSON).
-spec handle_request(map()) -> map() | undefined.
handle_request(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method, <<"id">> := Id} = Req) ->
    Params = maps:get(<<"params">>, Req, #{}),
    case dispatch(Method, Params) of
        {ok, Result}              -> json_rpc_response(Id, Result);
        {error, Code, Message}    -> json_rpc_error(Id, Code, Message);
        {error, Code, Message, D} -> json_rpc_error(Id, Code, Message, D)
    end;
handle_request(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := _}) ->
    %% Notification (no id) — acknowledge silently
    undefined;
handle_request(_) ->
    json_rpc_error(null, -32600, <<"Invalid Request">>).

%% @doc Return the agent card as a JSON-compatible map.
-spec agent_card() -> map().
agent_card() ->
    bc_a2a_agent_card:to_json(bc_a2a_agent_card:build()).

%% --- Method dispatch ---

dispatch(<<"message/send">>, Params) ->
    Message = parse_inbound_message(Params),
    Metadata = maps:get(<<"metadata">>, Params, #{}),
    case bc_a2a_task_manager:create_task(Message, Metadata) of
        {ok, Task}       -> {ok, bc_a2a_task:to_json(Task)};
        {error, Reason}  -> {error, -32603, iolist_to_binary(
            io_lib:format("Failed to create task: ~p", [Reason]))}
    end;

dispatch(<<"tasks/get">>, #{<<"id">> := TaskId} = Params) ->
    case bc_a2a_task_manager:get_task(TaskId) of
        {ok, Task} ->
            Result = bc_a2a_task:to_json(Task),
            HistLen = maps:get(<<"historyLength">>, Params, undefined),
            Result2 = case HistLen of
                undefined -> Result;
                N when is_integer(N) ->
                    History = maps:get(<<"history">>, Result, []),
                    Result#{<<"history">> => lists:nthtail(
                        max(0, length(History) - N), History)}
            end,
            {ok, Result2};
        {error, not_found} ->
            {error, -32001, <<"Task not found">>, #{<<"taskId">> => TaskId}}
    end;
dispatch(<<"tasks/get">>, _) ->
    {error, -32602, <<"Missing required parameter: id">>};

dispatch(<<"tasks/cancel">>, #{<<"id">> := TaskId}) ->
    case bc_a2a_task_manager:cancel_task(TaskId) of
        {ok, Task}                 -> {ok, bc_a2a_task:to_json(Task)};
        {error, not_found}         -> {error, -32001, <<"Task not found">>,
                                            #{<<"taskId">> => TaskId}};
        {error, invalid_transition}-> {error, -32002, <<"Task is not cancelable">>}
    end;
dispatch(<<"tasks/cancel">>, _) ->
    {error, -32602, <<"Missing required parameter: id">>};

dispatch(<<"tasks/list">>, Params) ->
    Opts = #{
        context_id => maps:get(<<"contextId">>, Params, undefined),
        status     => parse_status(maps:get(<<"status">>, Params, undefined)),
        limit      => maps:get(<<"pageSize">>, Params, 50)
    },
    Tasks = bc_a2a_task_manager:list_tasks(Opts),
    {ok, #{<<"tasks">> => [bc_a2a_task:to_json(T) || T <- Tasks]}};

dispatch(Method, _) ->
    {error, -32601, <<"Method not found: ", Method/binary>>}.

%% --- Helpers ---

parse_inbound_message(Params) ->
    Msg = maps:get(<<"message">>, Params, #{}),
    Parts = [parse_part(P) || P <- maps:get(<<"parts">>, Msg, []),
             P =/= undefined],
    #a2a_message{
        role = user,
        parts = Parts,
        metadata = maps:get(<<"metadata">>, Msg, #{})
    }.

parse_part(#{<<"type">> := <<"text">>, <<"text">> := Text}) ->
    #{type => text, text => Text};
parse_part(#{<<"type">> := <<"file">>, <<"file">> := File}) ->
    #{type => file, file => File};
parse_part(#{<<"type">> := <<"data">>, <<"data">> := Data}) ->
    #{type => data, data => Data};
parse_part(_) ->
    undefined.

parse_status(undefined) -> undefined;
parse_status(S) when is_binary(S) ->
    try binary_to_existing_atom(S, utf8)
    catch _:_ -> undefined
    end.

json_rpc_response(Id, Result) ->
    #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id, <<"result">> => Result}.

json_rpc_error(Id, Code, Message) ->
    json_rpc_error(Id, Code, Message, undefined).

json_rpc_error(Id, Code, Message, Data) ->
    Error0 = #{<<"code">> => Code, <<"message">> => Message},
    Error = case Data of
        undefined -> Error0;
        _         -> Error0#{<<"data">> => Data}
    end,
    #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id, <<"error">> => Error}.
