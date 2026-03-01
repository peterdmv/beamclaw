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

-module(bc_context).
-moduledoc """
Context window usage display.

Pure-function module — no gen_server, no state. Gathers context window usage
data from session history, system prompt, and tool registry, then formats it
as ANSI text (TUI), plain text (Telegram fallback), SVG, or PNG.

Token estimation: byte_size(Content) div 4 (~4 chars/token approximation).
""".

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([gather/1, format_text/1, format_text/2, format_telegram/1,
         render_svg/1, render_png/1]).

%% Exported for testing
-export([estimate_tokens/1, context_window/1, format_size/1]).

%% ---- Layer 1: Gather raw data ----

-spec gather(map()) -> map().
gather(#{agent_id := AgentId, history := History}) ->
    %% Get model name from provider config
    Model = get_model_name(),
    Window = context_window(Model),

    %% Categorize system messages
    SystemMsgs = bc_system_prompt:assemble(AgentId),
    {BootstrapFiles, DailyTokens, SkillTokens} = categorize_system_messages(SystemMsgs),

    BootstrapTokens = lists:foldl(fun({_, T}, Acc) -> Acc + T end, 0, BootstrapFiles),

    %% Tool definitions
    ToolTokens = estimate_tool_tokens(),

    %% Conversation history (non-system messages only)
    MessageTokens = estimate_history_tokens(History),

    %% Compaction buffer estimate
    LoopCfg = bc_config:get(beamclaw_core, agentic_loop, #{}),
    CompactionTarget = maps:get(compaction_target, LoopCfg, 20),
    %% Estimate avg message size from history, or use a default
    AvgMsgTokens = case length(History) of
        0 -> 200;
        N -> max(200, MessageTokens div N)
    end,
    CompactionBuffer = CompactionTarget * AvgMsgTokens,

    Total = BootstrapTokens + DailyTokens + SkillTokens + ToolTokens + MessageTokens,
    FreeSpace = max(0, Window - Total - CompactionBuffer),

    #{model            => Model,
      context_window   => Window,
      total            => Total,
      bootstrap_files  => BootstrapFiles,
      bootstrap_tokens => BootstrapTokens,
      daily_tokens     => DailyTokens,
      skill_tokens     => SkillTokens,
      tool_tokens      => ToolTokens,
      message_tokens   => MessageTokens,
      compaction_buffer => CompactionBuffer,
      free_space       => FreeSpace,
      message_count    => length(History),
      categories       => [
          {<<"Bootstrap files">>, BootstrapTokens},
          {<<"Daily logs">>, DailyTokens},
          {<<"Skills">>, SkillTokens},
          {<<"Tool definitions">>, ToolTokens},
          {<<"Messages">>, MessageTokens},
          {<<"Free space">>, FreeSpace},
          {<<"Compaction buffer">>, CompactionBuffer}
      ]}.

%% ---- Layer 2a: Format as text ----

-spec format_text(map()) -> binary().
format_text(Info) ->
    format_text(Info, #{}).

-spec format_text(map(), map()) -> binary().
format_text(Info, Opts) ->
    Ansi = maps:get(ansi, Opts, false),
    #{model := Model, context_window := Window, total := Total,
      categories := Categories, bootstrap_files := BootstrapFiles,
      compaction_buffer := CompBuffer} = Info,

    Pct = case Window of 0 -> 0; _ -> (Total * 100) div Window end,
    UsedWithBuffer = Total + CompBuffer,

    %% Build the 100-cell grid
    Grid = build_grid(Info),

    %% Color maps for ANSI
    Colors = #{
        bootstrap  => "\e[38;5;244m",  %% Gray
        daily      => "\e[38;5;174m",  %% Pink
        skills     => "\e[38;5;220m",  %% Yellow
        tools      => "\e[38;5;135m",  %% Purple
        messages   => "\e[38;5;75m",   %% Blue
        free       => "\e[38;5;246m",  %% Light gray
        compaction => "\e[38;5;240m",  %% Dark gray
        reset      => "\e[39m"
    },

    %% Format header
    Header = <<"Context Usage">>,

    %% Format the grid rows with legend on the right
    ModelLine = iolist_to_binary([Model, " \xC2\xB7 ",
                                  format_size(UsedWithBuffer), "/",
                                  format_size(Window), " tokens (",
                                  integer_to_list(Pct), "%)"]),

    LegendLines = format_legend(Categories, Window, Ansi, Colors),

    %% Build grid rows (10 rows of 10 cells)
    GridRows = build_grid_rows(Grid, Ansi, Colors),

    %% Right-side annotations for each row
    RightSide = [
        ModelLine,                           %% Row 0
        <<>>,                                %% Row 1
        <<>>,                                %% Row 2
        <<"Estimated usage by category">>,   %% Row 3
        lists:nth(1, LegendLines),           %% Row 4 — Bootstrap
        lists:nth(2, LegendLines),           %% Row 5 — Daily
        lists:nth(3, LegendLines),           %% Row 6 — Skills
        lists:nth(4, LegendLines),           %% Row 7 — Tools
        lists:nth(5, LegendLines),           %% Row 8 — Messages
        lists:nth(6, LegendLines),           %% Row 9 — Free
        lists:nth(7, LegendLines)            %% extra — Compaction (below grid)
    ],

    %% Combine grid + right side
    FullLines = [Header, <<>> |
        lists:zipwith(fun(Row, Right) ->
            case Right of
                <<>> -> Row;
                _ -> iolist_to_binary([Row, "   ", Right])
            end
        end, GridRows, lists:sublist(RightSide, 10))
    ],

    %% Add compaction legend line below the grid
    CompLine = lists:nth(11, RightSide),
    FullLines2 = FullLines ++ [iolist_to_binary([
        "                      ", CompLine
    ])],

    %% Bootstrap file listing
    BootstrapSection = case BootstrapFiles of
        [] -> [];
        _ ->
            [<<>>, <<"Bootstrap files">> |
             [iolist_to_binary([
                 <<"\xe2\x94\x94 ">>, Name, <<": ">>, format_size(Tokens), <<" tokens">>
             ]) || {Name, Tokens} <- BootstrapFiles]]
    end,

    iolist_to_binary(lists:join(<<"\n">>, FullLines2 ++ BootstrapSection)).

%% ---- Layer 2b: Format as Telegram HTML (emoji grid) ----

-spec format_telegram(map()) -> binary().
format_telegram(Info) ->
    #{model := Model, context_window := Window, total := Total,
      categories := Categories, bootstrap_files := BootstrapFiles,
      compaction_buffer := CompBuffer} = Info,

    Pct = case Window of 0 -> 0; _ -> (Total * 100) div Window end,
    UsedWithBuffer = Total + CompBuffer,

    Grid = build_grid(Info),

    %% Header
    ModelBin = case Model of
        M when is_list(M) -> list_to_binary(M);
        M when is_binary(M) -> M
    end,
    Header = iolist_to_binary([
        <<"<b>">>, <<"\xf0\x9f\x93\x8a">>, <<" Context Usage</b>\n">>,
        bc_telegram_format:escape_html(ModelBin), <<"\n">>,
        format_size(UsedWithBuffer), <<"/">>, format_size(Window),
        <<" tokens (">>, integer_to_list(Pct), <<"%)">>
    ]),

    %% Grid: 10×10 emoji, no spaces between cells
    GridRows = build_telegram_grid_rows(Grid),
    GridBlock = iolist_to_binary(lists:join(<<"\n">>, GridRows)),

    %% Legend
    Legend = telegram_legend(Categories, Window),
    LegendBlock = iolist_to_binary(lists:join(<<"\n">>, Legend)),

    %% Bootstrap files
    BootstrapBlock = case BootstrapFiles of
        [] -> <<>>;
        _ ->
            BootLines = [iolist_to_binary([
                <<"<code>">>, <<"\xe2\x94\x94 ">>,
                bc_telegram_format:escape_html(Name),
                <<": ">>, format_size(Tokens), <<" tokens</code>">>
            ]) || {Name, Tokens} <- BootstrapFiles],
            iolist_to_binary([
                <<"\n\n<b>Bootstrap files</b>\n">>,
                iolist_to_binary(lists:join(<<"\n">>, BootLines))
            ])
    end,

    iolist_to_binary([Header, <<"\n\n">>, GridBlock, <<"\n\n">>,
                      LegendBlock, BootstrapBlock]).

build_telegram_grid_rows(Grid) ->
    Rows = chunk_list(Grid, 10),
    lists:map(fun(Row) ->
        iolist_to_binary([telegram_cell_emoji(C) || C <- Row])
    end, Rows).

telegram_cell_emoji(bootstrap)  -> <<"\xf0\x9f\x9f\xa4">>;  %% 🟤
telegram_cell_emoji(daily)      -> <<"\xf0\x9f\x94\xb4">>;  %% 🔴
telegram_cell_emoji(skills)     -> <<"\xf0\x9f\x9f\xa1">>;  %% 🟡
telegram_cell_emoji(tools)      -> <<"\xf0\x9f\x9f\xa3">>;  %% 🟣
telegram_cell_emoji(messages)   -> <<"\xf0\x9f\x94\xb5">>;  %% 🔵
telegram_cell_emoji(free)       -> <<"\xe2\xac\x9c">>;       %% ⬜
telegram_cell_emoji(compaction) -> <<"\xe2\x9a\xab">>.       %% ⚫

telegram_legend(Categories, Window) ->
    lists:map(fun({CatName, CatTokens}) ->
        CatPct = case Window of 0 -> 0; _ -> (CatTokens * 100) div Window end,
        CatKey = category_key(CatName),
        Emoji = telegram_cell_emoji(CatKey),
        iolist_to_binary([Emoji, <<" ">>, CatName, <<": ">>,
                          format_size(CatTokens), <<" (">>,
                          integer_to_list(CatPct), <<"%)">>])
    end, Categories).

%% ---- Layer 2c: Format as SVG ----

-spec render_svg(map()) -> binary().
render_svg(Info) ->
    #{model := Model, context_window := Window, total := Total,
      categories := Categories, bootstrap_files := BootstrapFiles,
      compaction_buffer := CompBuffer} = Info,

    Pct = case Window of 0 -> 0; _ -> (Total * 100) div Window end,
    UsedWithBuffer = Total + CompBuffer,

    Grid = build_grid(Info),

    %% SVG color palette (dark theme)
    SvgColors = [
        {bootstrap,  <<"#808080">>},  %% Gray
        {daily,      <<"#e8a0a0">>},  %% Pink
        {skills,     <<"#e0c060">>},  %% Yellow
        {tools,      <<"#b080e0">>},  %% Purple
        {messages,   <<"#60a0e0">>},  %% Blue
        {free,       <<"#404060">>},  %% Dark blue-gray
        {compaction, <<"#303050">>}   %% Darker
    ],

    %% Title
    Title = <<"  <text x=\"20\" y=\"25\" fill=\"#e0e0e0\" font-size=\"16\">Context Usage</text>\n">>,

    %% Model + summary text (below title)
    ModelText = iolist_to_binary([
        "  <text x=\"20\" y=\"50\" fill=\"#a0a0a0\" font-size=\"13\">",
        svg_escape(Model), " \xC2\xB7 ",
        format_size(UsedWithBuffer), "/", format_size(Window),
        " (", integer_to_list(Pct), "%)",
        "</text>\n"
    ]),

    %% Category legend (below model text)
    LegendSvg = lists:map(fun({Idx, {CatName, CatTokens}}) ->
        CatPct = case Window of 0 -> 0; _ -> (CatTokens * 100) div Window end,
        CatKey = category_key(CatName),
        CatColor = proplists:get_value(CatKey, SvgColors, <<"#808080">>),
        CatY = 75 + Idx * 20,
        CatSymbol = category_symbol(CatKey),
        iolist_to_binary([
            "  <text x=\"20\" y=\"", integer_to_list(CatY),
            "\" fill=\"", CatColor, "\" font-size=\"12\">",
            CatSymbol, " ", svg_escape(binary_to_list(CatName)), ": ",
            format_size(CatTokens), " (", integer_to_list(CatPct), "%)",
            "</text>\n"
        ])
    end, lists:zip(lists:seq(0, length(Categories) - 1), Categories)),

    %% Grid cells (10×10, 30×30px each, 5px gap, below legend)
    CellSize = 30,
    Gap = 5,
    StartX = 20,
    StartY = 230,
    GridSvg = lists:map(fun(Idx) ->
        Row = Idx div 10,
        Col = Idx rem 10,
        X = StartX + Col * (CellSize + Gap),
        Y = StartY + Row * (CellSize + Gap),
        CellType = lists:nth(Idx + 1, Grid),
        Color = proplists:get_value(CellType, SvgColors, <<"#404060">>),
        iolist_to_binary([
            "  <rect x=\"", integer_to_list(X),
            "\" y=\"", integer_to_list(Y),
            "\" width=\"", integer_to_list(CellSize),
            "\" height=\"", integer_to_list(CellSize),
            "\" rx=\"4\" fill=\"", Color, "\"/>\n"
        ])
    end, lists:seq(0, 99)),

    %% Grid bottom: StartY + 10 rows * (30 + 5) - 5 = 230 + 345 = 575
    GridBottom = StartY + 10 * (CellSize + Gap) - Gap,

    %% Bootstrap file listing (below grid)
    BootstrapSvg = case BootstrapFiles of
        [] -> [];
        _ ->
            BootHeaderY = GridBottom + 20,
            BootHeader = iolist_to_binary([
                "  <text x=\"20\" y=\"", integer_to_list(BootHeaderY),
                "\" fill=\"#a0a0a0\" font-size=\"12\">Bootstrap files</text>\n"
            ]),
            BootLines = lists:map(fun({Idx, {Name, Tokens}}) ->
                BY = BootHeaderY + 16 + Idx * 16,
                iolist_to_binary([
                    "  <text x=\"30\" y=\"", integer_to_list(BY),
                    "\" fill=\"#707090\" font-size=\"11\">\xe2\x94\x94 ",
                    svg_escape(binary_to_list(Name)), ": ",
                    format_size(Tokens), " tokens</text>\n"
                ])
            end, lists:zip(lists:seq(0, length(BootstrapFiles) - 1), BootstrapFiles)),
            [BootHeader | BootLines]
    end,

    %% Adjust canvas height based on content
    BaseHeight = GridBottom + 30,
    CanvasHeight = case BootstrapFiles of
        [] -> BaseHeight;
        _  -> BaseHeight + 20 + 16 + length(BootstrapFiles) * 16
    end,
    SvgHeaderFinal = iolist_to_binary([
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"720\" height=\"",
        integer_to_list(CanvasHeight),
        "\"\n     style=\"background:#1a1a2e; font-family:monospace\">\n"
    ]),

    SvgFooter = <<"</svg>\n">>,

    iolist_to_binary([
        SvgHeaderFinal, Title, ModelText | LegendSvg
    ] ++ GridSvg ++ BootstrapSvg ++ [SvgFooter]).

%% ---- Layer 3: SVG → PNG ----

-spec render_png(map()) -> {ok, binary()} | {error, term()}.
render_png(Info) ->
    Svg = render_svg(Info),
    TmpBase = "/tmp/bc_context_" ++ integer_to_list(erlang:unique_integer([positive])),
    SvgPath = TmpBase ++ ".svg",
    PngPath = TmpBase ++ ".png",
    try
        ok = file:write_file(SvgPath, Svg),
        Cmd = "rsvg-convert -o " ++ PngPath ++ " " ++ SvgPath,
        case os:cmd(Cmd ++ " 2>&1; echo $?") of
            Result ->
                Lines = string:split(Result, "\n", all),
                NonEmpty = [L || L <- Lines, L =/= "", L =/= []],
                ExitCode = case NonEmpty of
                    [] -> "unknown";
                    _  -> string:trim(lists:last(NonEmpty))
                end,
                case ExitCode of
                    "0" ->
                        case file:read_file(PngPath) of
                            {ok, PngBin} -> {ok, PngBin};
                            {error, R}   -> {error, {read_png, R}}
                        end;
                    _ ->
                        {error, {rsvg_convert, Result}}
                end
        end
    catch
        _:Reason -> {error, Reason}
    after
        file:delete(SvgPath),
        file:delete(PngPath)
    end.

%% ---- Internal: Token estimation ----

-spec estimate_tokens(binary() | undefined) -> non_neg_integer().
estimate_tokens(undefined) -> 0;
estimate_tokens(<<>>) -> 0;
estimate_tokens(Bin) when is_binary(Bin) -> byte_size(Bin) div 4;
estimate_tokens(_) -> 0.

%% ---- Internal: Context window lookup ----

-spec context_window(string() | binary()) -> pos_integer().
context_window(Model) when is_binary(Model) ->
    context_window(binary_to_list(Model));
context_window("anthropic/claude-sonnet-4-5" ++ _) -> 200000;
context_window("anthropic/claude-opus-4-6" ++ _)   -> 200000;
context_window("anthropic/claude-" ++ _)            -> 200000;
context_window("gpt-4o" ++ _)                       -> 128000;
context_window("gpt-4" ++ _)                        -> 128000;
context_window("moonshotai/kimi" ++ _)               -> 256000;
context_window("google/gemini" ++ _)                 -> 1000000;
context_window("deepseek/" ++ _)                     -> 128000;
context_window(_)                                    -> 128000.

%% ---- Internal: Model name from config ----

get_model_name() ->
    DefaultProvider = bc_config:get(beamclaw_core, default_provider, openrouter),
    Providers = bc_config:get(beamclaw_core, providers, []),
    ProvMap = proplists:get_value(DefaultProvider, Providers, #{}),
    case maps:get(model, ProvMap, undefined) of
        undefined -> "unknown";
        M when is_list(M) -> M;
        M when is_binary(M) -> binary_to_list(M)
    end.

%% ---- Internal: Categorize system messages ----

categorize_system_messages(Msgs) ->
    lists:foldl(fun(#bc_message{content = Content}, {Boot, Daily, Skill}) ->
        case Content of
            <<"[skill:", _/binary>> ->
                {Boot, Daily, Skill + estimate_tokens(Content)};
            <<"[skills:available]", _/binary>> ->
                {Boot, Daily, Skill + estimate_tokens(Content)};
            <<"[memory/", _/binary>> ->
                {Boot, Daily + estimate_tokens(Content), Skill};
            <<"[", Rest/binary>> ->
                %% Bootstrap file: [IDENTITY.md], [SOUL.md], etc.
                case binary:match(Rest, <<".md]">>) of
                    nomatch -> {Boot, Daily, Skill};
                    _ ->
                        Filename = extract_filename(Content),
                        Tokens = estimate_tokens(Content),
                        {Boot ++ [{Filename, Tokens}], Daily, Skill}
                end;
            _ ->
                {Boot, Daily, Skill}
        end
    end, {[], 0, 0}, Msgs).

extract_filename(<<"[", Rest/binary>>) ->
    case binary:match(Rest, <<"]">>) of
        {Pos, _} -> binary:part(Rest, 0, Pos);
        nomatch  -> <<"unknown">>
    end;
extract_filename(_) -> <<"unknown">>.

%% ---- Internal: Tool token estimation ----

estimate_tool_tokens() ->
    try
        Tools = bc_tool_registry:list(),
        lists:foldl(fun({Name, _Mod, Def}, Acc) ->
            %% Estimate from name + description + JSON-encoded parameters
            Desc = maps:get(description, Def, <<>>),
            Params = maps:get(parameters, Def, #{}),
            ParamsJson = jsx:encode(Params),
            Acc + estimate_tokens(Name) + estimate_tokens(Desc) +
                (byte_size(ParamsJson) div 4)
        end, 0, Tools)
    catch
        _:_ -> 0
    end.

%% ---- Internal: History token estimation ----

estimate_history_tokens(History) ->
    lists:foldl(fun(#bc_message{content = Content, tool_calls = TCs}, Acc) ->
        ContentTokens = estimate_tokens(Content),
        TCTokens = lists:foldl(fun(TC, TAcc) ->
            case TC of
                #bc_tool_call{name = N, args = Args} ->
                    TAcc + estimate_tokens(N) +
                        (byte_size(jsx:encode(Args)) div 4);
                _ when is_map(TC) ->
                    TAcc + (byte_size(jsx:encode(TC)) div 4);
                _ -> TAcc
            end
        end, 0, TCs),
        Acc + ContentTokens + TCTokens
    end, 0, History).

%% ---- Internal: Grid building ----

build_grid(#{context_window := Window, bootstrap_tokens := BootTok,
             daily_tokens := DailyTok, skill_tokens := SkillTok,
             tool_tokens := ToolTok, message_tokens := MsgTok,
             compaction_buffer := CompBuf, free_space := FreeTok}) ->
    %% Each cell = Window / 100 tokens
    CellSize = max(1, Window div 100),
    %% Fill cells in order: bootstrap, daily, skills, tools, messages, free, compaction
    Segments = [
        {bootstrap,  BootTok},
        {daily,      DailyTok},
        {skills,     SkillTok},
        {tools,      ToolTok},
        {messages,   MsgTok},
        {free,       FreeTok},
        {compaction, CompBuf}
    ],
    fill_grid(Segments, CellSize, []).

fill_grid([], _CellSize, Acc) ->
    Grid = lists:reverse(Acc),
    %% Pad or trim to exactly 100 cells
    case length(Grid) of
        N when N >= 100 -> lists:sublist(Grid, 100);
        N -> Grid ++ lists:duplicate(100 - N, free)
    end;
fill_grid([{Type, Tokens} | Rest], CellSize, Acc) ->
    Cells = max(0, (Tokens + CellSize - 1) div CellSize),  %% ceiling division
    NewAcc = lists:duplicate(Cells, Type) ++ Acc,
    fill_grid(Rest, CellSize, NewAcc).

%% ---- Internal: Grid row formatting ----

build_grid_rows(Grid, Ansi, Colors) ->
    Rows = chunk_list(Grid, 10),
    lists:map(fun(Row) ->
        Cells = lists:map(fun(CellType) ->
            Symbol = cell_symbol(CellType),
            case Ansi of
                true ->
                    Color = cell_color(CellType, Colors),
                    Reset = maps:get(reset, Colors),
                    iolist_to_binary([Color, Symbol, Reset]);
                false ->
                    Symbol
            end
        end, Row),
        iolist_to_binary(lists:join(<<" ">>, Cells))
    end, Rows).

cell_symbol(bootstrap)  -> <<"\xe2\x9b\x81">>;  %% ⛁
cell_symbol(daily)      -> <<"\xe2\x9b\x81">>;  %% ⛁
cell_symbol(skills)     -> <<"\xe2\x9b\x81">>;  %% ⛁
cell_symbol(tools)      -> <<"\xe2\x9b\x81">>;  %% ⛁
cell_symbol(messages)   -> <<"\xe2\x9b\x81">>;  %% ⛁
cell_symbol(free)       -> <<"\xe2\x9b\xb6">>;  %% ⛶
cell_symbol(compaction) -> <<"\xe2\x9b\x9d">>.  %% ⛝

cell_color(bootstrap,  C) -> maps:get(bootstrap,  C, "");
cell_color(daily,      C) -> maps:get(daily,      C, "");
cell_color(skills,     C) -> maps:get(skills,     C, "");
cell_color(tools,      C) -> maps:get(tools,      C, "");
cell_color(messages,   C) -> maps:get(messages,   C, "");
cell_color(free,       C) -> maps:get(free,       C, "");
cell_color(compaction, C) -> maps:get(compaction,  C, "").

%% ---- Internal: Legend formatting ----

format_legend(Categories, Window, Ansi, Colors) ->
    lists:map(fun({CatName, CatTokens}) ->
        CatPct = case Window of 0 -> 0; _ -> (CatTokens * 100) div Window end,
        CatKey = category_key(CatName),
        Symbol = cell_symbol(CatKey),
        case Ansi of
            true ->
                Color = cell_color(CatKey, Colors),
                Reset = maps:get(reset, Colors),
                iolist_to_binary([Color, Symbol, Reset, " ",
                                  CatName, ": ",
                                  format_size(CatTokens), " tokens (",
                                  integer_to_list(CatPct), "%)"]);
            false ->
                iolist_to_binary([Symbol, " ",
                                  CatName, ": ",
                                  format_size(CatTokens), " tokens (",
                                  integer_to_list(CatPct), "%)"])
        end
    end, Categories).

%% ---- Internal: Helpers ----

format_size(N) when N >= 1000 ->
    Whole = N div 1000,
    Frac = (N rem 1000) div 100,
    iolist_to_binary([integer_to_list(Whole), ".", integer_to_list(Frac), "k"]);
format_size(N) ->
    iolist_to_binary(integer_to_list(N)).

category_key(<<"Bootstrap files">>) -> bootstrap;
category_key(<<"Daily logs">>)      -> daily;
category_key(<<"Skills">>)          -> skills;
category_key(<<"Tool definitions">>) -> tools;
category_key(<<"Messages">>)        -> messages;
category_key(<<"Free space">>)      -> free;
category_key(<<"Compaction buffer">>) -> compaction;
category_key(_)                      -> free.

category_symbol(bootstrap)  -> <<"\xe2\x9b\x81">>;
category_symbol(daily)      -> <<"\xe2\x9b\x81">>;
category_symbol(skills)     -> <<"\xe2\x9b\x81">>;
category_symbol(tools)      -> <<"\xe2\x9b\x81">>;
category_symbol(messages)   -> <<"\xe2\x9b\x81">>;
category_symbol(free)       -> <<"\xe2\x9b\xb6">>;
category_symbol(compaction) -> <<"\xe2\x9b\x9d">>;
category_symbol(_)          -> <<"\xe2\x9b\xb6">>.

svg_escape(Text) when is_list(Text) ->
    lists:flatmap(fun
        ($<) -> "&lt;";
        ($>) -> "&gt;";
        ($&) -> "&amp;";
        ($") -> "&quot;";
        (C)  -> [C]
    end, Text);
svg_escape(Text) when is_binary(Text) ->
    list_to_binary(svg_escape(binary_to_list(Text))).

chunk_list([], _N) -> [];
chunk_list(List, N) ->
    {Chunk, Rest} = lists:split(min(N, length(List)), List),
    [Chunk | chunk_list(Rest, N)].
