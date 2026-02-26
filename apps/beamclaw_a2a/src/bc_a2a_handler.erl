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

-module(bc_a2a_handler).
-moduledoc """
A2A protocol HTTP handler.

Implements the A2A (Agent2Agent) JSON-RPC 2.0 over HTTP protocol.

To integrate with bc_gateway_cowboy.erl, add these routes:
    {"/a2a", bc_a2a_handler, []},
    {"/.well-known/agent.json", bc_a2a_handler, []}

Supported operations:
- GET /.well-known/agent.json → returns agent card
- POST /a2a → JSON-RPC 2.0 dispatch:
  - message/send → creates/resumes a task, routes to bc_session
  - tasks/get → returns task state
  - tasks/cancel → cancels task
  - tasks/list → lists tasks (optionally filtered by contextId)
""".

-behaviour(cowboy_handler).

-export([init/2]).

init(Req0 = #{method := <<"GET">>, path := <<"/.well-known/agent.json">>}, State) ->
    handle_agent_card(Req0, State);
init(Req0 = #{method := <<"POST">>, path := <<"/a2a">>}, State) ->
    handle_jsonrpc(Req0, State);
init(Req0, State) ->
    % Method not allowed or path not found
    Req1 = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, 
                           jsx:encode(#{error => <<"Method not allowed">>}), Req0),
    {ok, Req1, State}.

%% Handle agent card requests
handle_agent_card(Req0, State) ->
    AgentCard = bc_a2a_agent_card:generate(),
    Json = jsx:encode(AgentCard),
    Req1 = cowboy_req:reply(200, 
                           #{<<"content-type">> => <<"application/json">>},
                           Json, Req0),
    {ok, Req1, State}.

%% Handle JSON-RPC 2.0 requests
handle_jsonrpc(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    try jsx:decode(Body, [return_maps]) of
        JsonReq when is_map(JsonReq) ->
            Response = process_jsonrpc_request(JsonReq),
            Json = jsx:encode(Response),
            Req2 = cowboy_req:reply(200, 
                                   #{<<"content-type">> => <<"application/json">>},
                                   Json, Req1),
            {ok, Req2, State};
        _ ->
            ErrorResponse = jsonrpc_error(null, -32700, <<"Parse error">>, undefined),
            Json = jsx:encode(ErrorResponse),
            Req2 = cowboy_req:reply(400,
                                   #{<<"content-type">> => <<"application/json">>},
                                   Json, Req1),
            {ok, Req2, State}
    catch
        error:_ ->
            ErrorResponse = jsonrpc_error(null, -32700, <<"Parse error">>, undefined),
            Json = jsx:encode(ErrorResponse),
            Req2 = cowboy_req:reply(400,
                                   #{<<"content-type">> => <<"application/json">>},
                                   Json, Req1),
            {ok, Req2, State}
    end.

%% Process JSON-RPC request
process_jsonrpc_request(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method, <<"id">> := Id} = Req) ->
    Params = maps:get(<<"params">>, Req, #{}),
    
    case Method of
        <<"message/send">> ->
            handle_message_send(Id, Params);
        <<"tasks/get">> ->
            handle_task_get(Id, Params);
        <<"tasks/cancel">> ->
            handle_task_cancel(Id, Params);
        <<"tasks/list">> ->
            handle_task_list(Id, Params);
        _ ->
            jsonrpc_error(Id, -32601, <<"Method not found">>, undefined)
    end;
process_jsonrpc_request(#{<<"id">> := Id}) ->
    jsonrpc_error(Id, -32600, <<"Invalid Request">>, undefined);
process_jsonrpc_request(_) ->
    jsonrpc_error(null, -32600, <<"Invalid Request">>, undefined).

%% Handle message/send - create or resume a task
handle_message_send(Id, Params) ->
    case validate_message_send_params(Params) of
        {ok, ValidatedParams} ->
            case execute_message_send(ValidatedParams) of
                {ok, Result} ->
                    jsonrpc_success(Id, Result);
                {error, Reason} ->
                    jsonrpc_error(Id, -32603, <<"Internal error">>, Reason)
            end;
        {error, Reason} ->
            jsonrpc_error(Id, -32602, <<"Invalid params">>, Reason)
    end.

%% Handle tasks/get - retrieve task state
handle_task_get(Id, Params) ->
    case maps:get(<<"taskId">>, Params, undefined) of
        undefined ->
            jsonrpc_error(Id, -32602, <<"Invalid params">>, <<"Missing taskId">>);
        TaskId ->
            case bc_a2a_task:get(TaskId) of
                {ok, TaskRecord} ->
                    TaskMap = bc_a2a_task:task_to_map(TaskRecord),
                    jsonrpc_success(Id, bc_a2a_types:task(TaskMap));
                {error, not_found} ->
                    jsonrpc_error(Id, -32603, <<"Internal error">>, <<"Task not found">>)
            end
    end.

%% Handle tasks/cancel - cancel a task
handle_task_cancel(Id, Params) ->
    case maps:get(<<"taskId">>, Params, undefined) of
        undefined ->
            jsonrpc_error(Id, -32602, <<"Invalid params">>, <<"Missing taskId">>);
        TaskId ->
            case bc_a2a_task:cancel(TaskId) of
                ok ->
                    jsonrpc_success(Id, #{success => true});
                {error, not_found} ->
                    jsonrpc_error(Id, -32603, <<"Internal error">>, <<"Task not found">>)
            end
    end.

%% Handle tasks/list - list tasks
handle_task_list(Id, Params) ->
    ContextId = maps:get(<<"contextId">>, Params, undefined),
    Tasks = bc_a2a_task:list(ContextId),
    jsonrpc_success(Id, #{tasks => [bc_a2a_types:task(Task) || Task <- Tasks]}).

%% Validate message/send parameters
validate_message_send_params(Params) ->
    case maps:get(<<"message">>, Params, undefined) of
        undefined ->
            {error, <<"Missing message">>};
        Message ->
            % Validate message structure
            case validate_message_structure(Message) of
                ok ->
                    TaskId = maps:get(<<"taskId">>, Params, undefined),
                    ContextId = maps:get(<<"contextId">>, Params, undefined),
                    {ok, #{message => Message, taskId => TaskId, contextId => ContextId}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Validate message structure
validate_message_structure(#{<<"role">> := Role, <<"parts">> := Parts}) 
    when is_binary(Role), is_list(Parts) ->
    % TODO: Add more detailed validation of parts
    ok;
validate_message_structure(_) ->
    {error, <<"Invalid message structure">>}.

%% Execute message/send operation
execute_message_send(#{message := Message, taskId := TaskId, contextId := ContextId}) ->
    case TaskId of
        undefined ->
            % Create new task
            NewTaskId = bc_a2a_task:create(Message, ContextId),
            bc_a2a_task:update_status(NewTaskId, <<"working">>, <<"Processing message">>),
            
            % TODO: Route to bc_session for actual processing
            % For now, just simulate completion
            bc_a2a_task:update_status(NewTaskId, <<"completed">>, <<"Message processed">>),
            
            case bc_a2a_task:get(NewTaskId) of
                {ok, TaskRecord} ->
                    TaskMap = bc_a2a_task:task_to_map(TaskRecord),
                    {ok, bc_a2a_types:task(TaskMap)};
                Error ->
                    Error
            end;
        _ ->
            % Resume existing task
            case bc_a2a_task:get(TaskId) of
                {ok, _Task} ->
                    bc_a2a_task:add_message(TaskId, Message),
                    bc_a2a_task:update_status(TaskId, <<"working">>, <<"Processing additional message">>),
                    
                    % TODO: Route to bc_session for actual processing
                    bc_a2a_task:update_status(TaskId, <<"completed">>, <<"Message processed">>),
                    
                    case bc_a2a_task:get(TaskId) of
                        {ok, UpdatedTaskRecord} ->
                            UpdatedTaskMap = bc_a2a_task:task_to_map(UpdatedTaskRecord),
                            {ok, bc_a2a_types:task(UpdatedTaskMap)};
                        Error ->
                            Error
                    end;
                {error, not_found} ->
                    {error, <<"Task not found">>}
            end
    end.

%% JSON-RPC response constructors
jsonrpc_success(Id, Result) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    }.

jsonrpc_error(Id, Code, Message, Data) ->
    Error = case Data of
        undefined ->
            #{<<"code">> => Code, <<"message">> => Message};
        _ ->
            #{<<"code">> => Code, <<"message">> => Message, <<"data">> => Data}
    end,
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => Error
    }.