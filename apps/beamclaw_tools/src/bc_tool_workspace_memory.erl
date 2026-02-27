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

-module(bc_tool_workspace_memory).
-moduledoc """
Built-in workspace_memory tool — manage MEMORY.md, daily logs, and bootstrap files.

Allows the agent to manage its own long-term memory file, daily logs, and
bootstrap files (IDENTITY.md, USER.md, SOUL.md, TOOLS.md, AGENTS.md).
The path is constructed internally from the agent_id in bc_session_ref,
so no path traversal is possible.

Actions:
  - read:             Return current MEMORY.md content
  - append:           Append text to MEMORY.md
  - replace:          Replace entire MEMORY.md content
  - read_daily:       Read today's daily log (or specific date)
  - append_daily:     Append text to today's daily log
  - list_daily:       List available daily log files
  - read_bootstrap:   Read a bootstrap file by name (requires `file` parameter)
  - update_bootstrap: Replace a bootstrap file's content (requires `file` + `content`)
  - delete_bootstrap: Delete a bootstrap file (requires `file` parameter)
  - search:           BM25/hybrid keyword search across MEMORY.md and daily logs
  - search_all:       Unified search across workspace files AND structured memory
""".
-behaviour(bc_tool).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([definition/0, execute/3, requires_approval/0, min_autonomy/0]).

-define(MAX_READ_SIZE, 20480). %% 20 KB

definition() ->
    #{name        => <<"workspace_memory">>,
      description => <<"Manage the agent's long-term MEMORY.md, daily logs, and bootstrap files. "
                       "Use read/append/replace for MEMORY.md, read_daily/append_daily/list_daily "
                       "for daily logs, read_bootstrap/update_bootstrap for identity and config "
                       "files (IDENTITY.md, USER.md, SOUL.md, TOOLS.md, AGENTS.md), "
                       "delete_bootstrap to remove a bootstrap file (e.g. BOOTSTRAP.md after setup), "
                       "and search to find relevant content across all memory sources.">>,
      parameters  => #{
          type       => object,
          properties => #{
              action => #{type => string,
                          description => <<"Action to perform">>,
                          enum => [<<"read">>, <<"append">>, <<"replace">>,
                                   <<"read_daily">>, <<"append_daily">>, <<"list_daily">>,
                                   <<"read_bootstrap">>, <<"update_bootstrap">>,
                                   <<"delete_bootstrap">>,
                                   <<"search">>, <<"search_all">>]},
              content => #{type => string,
                           description => <<"Text to append or replace with "
                                            "(for append, replace, append_daily, update_bootstrap)">>},
              date => #{type => string,
                        description => <<"Date in YYYY-MM-DD format (for read_daily/append_daily; defaults to today)">>},
              file => #{type => string,
                        description => <<"Bootstrap filename for read_bootstrap/update_bootstrap/delete_bootstrap. "
                                         "Allowed: IDENTITY.md, USER.md, SOUL.md, TOOLS.md, AGENTS.md "
                                         "(delete_bootstrap also allows BOOTSTRAP.md)">>},
              query => #{type => string,
                         description => <<"Search query (required for search action)">>},
              limit => #{type => integer,
                         description => <<"Max results to return (for search; default 6)">>},
              mode => #{type => string,
                        description => <<"Search mode: keyword (BM25 only, no API call), "
                                         "semantic (vector only), or hybrid (default)">>}
          },
          required   => [<<"action">>]
      },
      source => builtin}.

%% ---- MEMORY.md actions ----

execute(#{<<"action">> := <<"read">>}, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Path = bc_workspace_path:bootstrap_file(AgentId, <<"MEMORY.md">>),
    case file:read_file(Path) of
        {ok, Bin} when byte_size(Bin) > ?MAX_READ_SIZE ->
            Truncated = binary:part(Bin, 0, ?MAX_READ_SIZE),
            {ok, <<Truncated/binary, "\n[...truncated at 20KB...]">>};
        {ok, Bin} ->
            {ok, Bin};
        {error, enoent} ->
            {ok, <<"(MEMORY.md does not exist yet)">>};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("read error: ~p", [Reason]))}
    end;

execute(#{<<"action">> := <<"append">>, <<"content">> := Content}, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Path = bc_workspace_path:bootstrap_file(AgentId, <<"MEMORY.md">>),
    Existing = case file:read_file(Path) of
        {ok, Bin}  -> Bin;
        {error, _} -> <<>>
    end,
    NewContent = <<Existing/binary, "\n", Content/binary>>,
    filelib:ensure_dir(Path),
    case file:write_file(Path, NewContent) of
        ok              -> {ok, <<"Memory updated.">>};
        {error, Reason} -> {error, iolist_to_binary(io_lib:format("write error: ~p", [Reason]))}
    end;

execute(#{<<"action">> := <<"replace">>, <<"content">> := Content}, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Path = bc_workspace_path:bootstrap_file(AgentId, <<"MEMORY.md">>),
    filelib:ensure_dir(Path),
    case file:write_file(Path, Content) of
        ok              -> {ok, <<"Memory replaced.">>};
        {error, Reason} -> {error, iolist_to_binary(io_lib:format("write error: ~p", [Reason]))}
    end;

%% ---- Daily log actions ----

execute(#{<<"action">> := <<"read_daily">>} = Args, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Date = resolve_date(Args),
    Path = bc_workspace_path:daily_log_file(AgentId, Date),
    case file:read_file(Path) of
        {ok, Bin} when byte_size(Bin) > ?MAX_READ_SIZE ->
            Truncated = binary:part(Bin, 0, ?MAX_READ_SIZE),
            {ok, <<Truncated/binary, "\n[...truncated at 20KB...]">>};
        {ok, Bin} ->
            {ok, Bin};
        {error, enoent} ->
            {ok, <<"(No daily log for ", Date/binary, ")">>};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("read error: ~p", [Reason]))}
    end;

execute(#{<<"action">> := <<"append_daily">>, <<"content">> := Content} = Args, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Date = resolve_date(Args),
    Dir = bc_workspace_path:memory_dir(AgentId),
    filelib:ensure_dir(filename:join(Dir, "dummy")),
    Path = bc_workspace_path:daily_log_file(AgentId, Date),
    Existing = case file:read_file(Path) of
        {ok, Bin}  -> Bin;
        {error, _} -> <<>>
    end,
    NewContent = <<Existing/binary, "\n", Content/binary>>,
    case file:write_file(Path, NewContent) of
        ok              -> {ok, <<"Daily log updated.">>};
        {error, Reason} -> {error, iolist_to_binary(io_lib:format("write error: ~p", [Reason]))}
    end;

execute(#{<<"action">> := <<"list_daily">>}, Session, _Context) ->
    AgentId = Session#bc_session_ref.agent_id,
    Dir = bc_workspace_path:memory_dir(AgentId),
    case file:list_dir(Dir) of
        {ok, Entries} ->
            MdFiles = [list_to_binary(E) || E <- Entries,
                        filename:extension(E) =:= ".md"],
            Sorted = lists:sublist(lists:reverse(lists:sort(MdFiles)), 30),
            case Sorted of
                [] -> {ok, <<"(No daily logs found)">>};
                _  -> {ok, iolist_to_binary(lists:join("\n", Sorted))}
            end;
        {error, enoent} ->
            {ok, <<"(No memory directory yet)">>};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("list error: ~p", [Reason]))}
    end;

%% ---- Bootstrap file actions ----

execute(#{<<"action">> := <<"read_bootstrap">>, <<"file">> := File}, Session, _Context) ->
    case validate_bootstrap_file(File) of
        {ok, Name} ->
            AgentId = Session#bc_session_ref.agent_id,
            Path = bc_workspace_path:bootstrap_file(AgentId, Name),
            case file:read_file(Path) of
                {ok, Bin} when byte_size(Bin) > ?MAX_READ_SIZE ->
                    Truncated = binary:part(Bin, 0, ?MAX_READ_SIZE),
                    {ok, <<Truncated/binary, "\n[...truncated at 20KB...]">>};
                {ok, Bin} ->
                    {ok, Bin};
                {error, enoent} ->
                    {ok, <<"(", Name/binary, " does not exist yet)">>};
                {error, Reason} ->
                    {error, iolist_to_binary(io_lib:format("read error: ~p", [Reason]))}
            end;
        {error, Msg} ->
            {error, Msg}
    end;

execute(#{<<"action">> := <<"update_bootstrap">>, <<"file">> := File,
          <<"content">> := Content}, Session, _Context) ->
    case validate_bootstrap_file(File) of
        {ok, Name} ->
            AgentId = Session#bc_session_ref.agent_id,
            Path = bc_workspace_path:bootstrap_file(AgentId, Name),
            filelib:ensure_dir(Path),
            case file:write_file(Path, Content) of
                ok              -> {ok, <<Name/binary, " updated.">>};
                {error, Reason} -> {error, iolist_to_binary(io_lib:format("write error: ~p", [Reason]))}
            end;
        {error, Msg} ->
            {error, Msg}
    end;

%% ---- Delete bootstrap file action ----

execute(#{<<"action">> := <<"delete_bootstrap">>, <<"file">> := File}, Session, _Ctx) ->
    case validate_delete_bootstrap_file(File) of
        {ok, Name} ->
            AgentId = Session#bc_session_ref.agent_id,
            Path = bc_workspace_path:bootstrap_file(AgentId, Name),
            case file:delete(Path) of
                ok -> {ok, <<"Deleted ", Name/binary>>};
                {error, enoent} -> {ok, <<"File already absent: ", Name/binary>>};
                {error, Reason} ->
                    {error, iolist_to_binary(
                        io_lib:format("Failed to delete ~s: ~p", [Name, Reason]))}
            end;
        {error, Msg} -> {error, Msg}
    end;

execute(#{<<"action">> := <<"delete_bootstrap">>}, _Session, _Context) ->
    {error, <<"'file' is required for delete_bootstrap action">>};

%% ---- Search action ----

execute(#{<<"action">> := <<"search">>, <<"query">> := Query} = Args, Session, _Context)
  when is_binary(Query), byte_size(Query) > 0 ->
    AgentId = Session#bc_session_ref.agent_id,
    Limit = maps:get(<<"limit">>, Args, 6),
    LimitInt = if is_integer(Limit) -> min(Limit, 20); true -> 6 end,
    Mode = resolve_search_mode(Args),
    Chunks = collect_search_chunks(AgentId),
    case Chunks of
        [] ->
            {ok, <<"No memory content available to search.">>};
        _ ->
            Ranked = run_search(Query, Chunks, Mode, AgentId, LimitInt),
            case Ranked of
                [] ->
                    {ok, <<"No results found for: ", Query/binary>>};
                _ ->
                    ChunkMap = maps:from_list(Chunks),
                    Lines = lists:map(fun({Source, Score}) ->
                        Text = maps:get(Source, ChunkMap, <<>>),
                        Pct = float_to_binary(Score * 100, [{decimals, 1}]),
                        Snippet = truncate_snippet(Text, 200),
                        <<Source/binary, " (", Pct/binary, "%)\n", Snippet/binary>>
                    end, Ranked),
                    {ok, iolist_to_binary(lists:join(<<"\n\n---\n\n">>, Lines))}
            end
    end;

execute(#{<<"action">> := <<"search">>}, _Session, _Context) ->
    {error, <<"'query' is required for search action">>};

%% ---- Search all (workspace + structured memory) ----

execute(#{<<"action">> := <<"search_all">>, <<"query">> := Query} = Args, Session, _Context)
  when is_binary(Query), byte_size(Query) > 0 ->
    AgentId = Session#bc_session_ref.agent_id,
    Limit = maps:get(<<"limit">>, Args, 6),
    LimitInt = if is_integer(Limit) -> min(Limit, 20); true -> 6 end,
    %% Search all bootstrap files (MEMORY.md, IDENTITY.md, etc.) + daily logs
    BootstrapChunks = collect_bootstrap_chunks(AgentId),
    DailyChunks = collect_daily_chunks(AgentId),
    AllChunks = BootstrapChunks ++ DailyChunks,
    case AllChunks of
        [] ->
            {ok, <<"No content available to search.">>};
        _ ->
            Ranked = bc_bm25:rank(Query, AllChunks),
            case Ranked of
                [] ->
                    {ok, <<"No results found for: ", Query/binary>>};
                _ ->
                    Top = lists:sublist(Ranked, LimitInt),
                    ChunkMap = maps:from_list(AllChunks),
                    Lines = lists:map(fun({Source, Score}) ->
                        Text = maps:get(Source, ChunkMap, <<>>),
                        Pct = float_to_binary(Score * 100, [{decimals, 1}]),
                        Snippet = truncate_snippet(Text, 200),
                        <<Source/binary, " (", Pct/binary, "%)\n", Snippet/binary>>
                    end, Top),
                    {ok, iolist_to_binary(lists:join(<<"\n\n---\n\n">>, Lines))}
            end
    end;

execute(#{<<"action">> := <<"search_all">>}, _Session, _Context) ->
    {error, <<"'query' is required for search_all action">>};

%% ---- Error cases ----

execute(#{<<"action">> := <<"append">>}, _Session, _Context) ->
    {error, <<"'content' is required for append action">>};

execute(#{<<"action">> := <<"replace">>}, _Session, _Context) ->
    {error, <<"'content' is required for replace action">>};

execute(#{<<"action">> := <<"append_daily">>}, _Session, _Context) ->
    {error, <<"'content' is required for append_daily action">>};

execute(#{<<"action">> := <<"read_bootstrap">>}, _Session, _Context) ->
    {error, <<"'file' is required for read_bootstrap action">>};

execute(#{<<"action">> := <<"update_bootstrap">>, <<"file">> := _}, _Session, _Context) ->
    {error, <<"'content' is required for update_bootstrap action">>};

execute(#{<<"action">> := <<"update_bootstrap">>}, _Session, _Context) ->
    {error, <<"'file' and 'content' are required for update_bootstrap action">>};

execute(_, _Session, _Context) ->
    {error, <<"Invalid action. Use: read, append, replace, read_daily, append_daily, "
              "list_daily, read_bootstrap, update_bootstrap, delete_bootstrap, "
              "search, or search_all">>}.

requires_approval() -> false.

min_autonomy() -> read_only.

%% Internal

-define(ALLOWED_BOOTSTRAP_FILES,
    [<<"IDENTITY.md">>, <<"USER.md">>, <<"SOUL.md">>,
     <<"TOOLS.md">>, <<"AGENTS.md">>, <<"HEARTBEAT.md">>]).

validate_bootstrap_file(File) when is_binary(File) ->
    case lists:member(File, ?ALLOWED_BOOTSTRAP_FILES) of
        true  -> {ok, File};
        false -> {error, <<"Invalid bootstrap file '", File/binary, "'. "
                           "Allowed: IDENTITY.md, USER.md, SOUL.md, TOOLS.md, AGENTS.md, HEARTBEAT.md">>}
    end;
validate_bootstrap_file(_) ->
    {error, <<"'file' must be a string">>}.

-define(DELETABLE_BOOTSTRAP_FILES,
    [<<"IDENTITY.md">>, <<"USER.md">>, <<"SOUL.md">>,
     <<"TOOLS.md">>, <<"AGENTS.md">>, <<"HEARTBEAT.md">>, <<"BOOTSTRAP.md">>]).

validate_delete_bootstrap_file(File) when is_binary(File) ->
    case lists:member(File, ?DELETABLE_BOOTSTRAP_FILES) of
        true -> {ok, File};
        false ->
            Allowed = lists:join(<<", ">>, ?DELETABLE_BOOTSTRAP_FILES),
            {error, iolist_to_binary(
                [<<"Invalid file for delete_bootstrap. Allowed: ">>,
                 Allowed])}
    end;
validate_delete_bootstrap_file(_) ->
    {error, <<"'file' parameter must be a string">>}.

resolve_date(#{<<"date">> := Date}) when is_binary(Date), byte_size(Date) > 0 ->
    Date;
resolve_date(_) ->
    today_date().

today_date() ->
    {Y, M, D} = date(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

%% Collect all searchable chunks from workspace files and daily logs.
collect_search_chunks(AgentId) ->
    MemoryChunks = collect_memory_chunks(AgentId),
    DailyChunks = collect_daily_chunks(AgentId),
    MemoryChunks ++ DailyChunks.

collect_memory_chunks(AgentId) ->
    Path = bc_workspace_path:bootstrap_file(AgentId, <<"MEMORY.md">>),
    case file:read_file(Path) of
        {ok, Bin} when byte_size(Bin) > 0 ->
            Paragraphs = split_paragraphs(Bin),
            lists:filtermap(fun({Idx, Para}) ->
                case string:trim(Para) of
                    <<>> -> false;
                    Trimmed ->
                        Source = iolist_to_binary(
                            io_lib:format("[MEMORY.md:para ~B]", [Idx])),
                        {true, {Source, Trimmed}}
                end
            end, lists:zip(lists:seq(1, length(Paragraphs)), Paragraphs));
        _ ->
            []
    end.

collect_daily_chunks(AgentId) ->
    Dir = bc_workspace_path:memory_dir(AgentId),
    case file:list_dir(Dir) of
        {ok, Entries} ->
            MdFiles = [F || F <- lists:sort(lists:reverse(Entries)),
                        filename:extension(F) =:= ".md"],
            %% Only look back 7 days worth of logs
            Recent = lists:sublist(MdFiles, 7),
            lists:filtermap(fun(Filename) ->
                FilePath = filename:join(Dir, Filename),
                case file:read_file(FilePath) of
                    {ok, Bin} when byte_size(Bin) > 0 ->
                        Source = iolist_to_binary(
                            io_lib:format("[~s]", [filename:rootname(Filename)])),
                        {true, {Source, Bin}};
                    _ ->
                        false
                end
            end, Recent);
        {error, _} ->
            []
    end.

split_paragraphs(Bin) ->
    re:split(Bin, <<"\\n\\n+">>, [{return, binary}]).

collect_bootstrap_chunks(AgentId) ->
    SearchCfg = application:get_env(beamclaw_memory, search, #{}),
    Files = maps:get(workspace_files, SearchCfg,
                     [<<"MEMORY.md">>, <<"IDENTITY.md">>, <<"USER.md">>,
                      <<"SOUL.md">>, <<"TOOLS.md">>, <<"AGENTS.md">>]),
    lists:flatmap(fun(Filename) ->
        Path = bc_workspace_path:bootstrap_file(AgentId, Filename),
        case file:read_file(Path) of
            {ok, Bin} when byte_size(Bin) > 0 ->
                Source = iolist_to_binary(io_lib:format("[~s]", [Filename])),
                [{Source, Bin}];
            _ ->
                []
        end
    end, Files).

truncate_snippet(Text, MaxWords) ->
    Words = re:split(Text, <<"\\s+">>, [{return, binary}]),
    case length(Words) > MaxWords of
        true ->
            Kept = lists:sublist(Words, MaxWords),
            <<(iolist_to_binary(lists:join(<<" ">>, Kept)))/binary, "...">>;
        false ->
            Text
    end.

resolve_search_mode(#{<<"mode">> := <<"keyword">>})  -> keyword;
resolve_search_mode(#{<<"mode">> := <<"semantic">>})  -> semantic;
resolve_search_mode(#{<<"mode">> := <<"hybrid">>})    -> hybrid;
resolve_search_mode(_)                                -> hybrid.

run_search(Query, Chunks, keyword, _AgentId, Limit) ->
    BM25Scores = bc_bm25:rank(Query, Chunks),
    lists:sublist(BM25Scores, Limit);
run_search(Query, Chunks, Mode, AgentId, Limit) ->
    BM25Scores = bc_bm25:rank(Query, Chunks),
    %% Try to get vector scores if embedding is configured
    VectorScores = case bc_embedding:is_configured() andalso Mode =/= keyword of
        true  -> compute_vector_scores(Query, Chunks, AgentId);
        false -> []
    end,
    SearchCfg = application:get_env(beamclaw_memory, search, #{}),
    bc_hybrid:merge(BM25Scores, #{
        vector_scores => VectorScores,
        vector_weight => maps:get(vector_weight, SearchCfg, 0.7),
        bm25_weight   => maps:get(bm25_weight, SearchCfg, 0.3),
        min_score     => maps:get(min_score, SearchCfg, 0.35),
        limit         => Limit
    }).

compute_vector_scores(Query, Chunks, AgentId) ->
    %% Embed the query
    case bc_embedding:embed(Query) of
        {ok, QueryVec} ->
            %% Get embeddings for all chunks (cached or computed)
            ChunkEmbeddings = get_chunk_embeddings(Chunks, AgentId),
            [{Source, bc_vector:cosine_similarity(QueryVec, Vec)}
             || {Source, Vec} <- ChunkEmbeddings, Vec =/= undefined];
        {error, _} ->
            []
    end.

get_chunk_embeddings(Chunks, AgentId) ->
    %% Check cache first, batch-embed missing ones
    {Cached, Missing} = lists:partition(fun({Source, Text}) ->
        Hash = content_hash(Text),
        case bc_embedding_cache:get(AgentId, Source, Hash) of
            {ok, _} -> true;
            miss    -> false
        end
    end, Chunks),
    CachedResults = [{Source, element(2, bc_embedding_cache:get(AgentId, Source, content_hash(Text)))}
                     || {Source, Text} <- Cached],
    MissingResults = case Missing of
        [] -> [];
        _ ->
            Texts = [Text || {_, Text} <- Missing],
            case bc_embedding:embed_batch(Texts) of
                {ok, Vectors} ->
                    Pairs = lists:zip(Missing, Vectors),
                    lists:map(fun({{Source, Text}, Vec}) ->
                        Hash = content_hash(Text),
                        bc_embedding_cache:put(AgentId, Source, Hash, Vec),
                        {Source, Vec}
                    end, Pairs);
                {error, _} ->
                    [{Source, undefined} || {Source, _} <- Missing]
            end
    end,
    CachedResults ++ MissingResults.

content_hash(Text) ->
    crypto:hash(sha256, Text).
