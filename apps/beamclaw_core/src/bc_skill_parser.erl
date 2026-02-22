%% @doc Parser for SKILL.md files.
%%
%% SKILL.md format:
%% ```
%% ---
%% name: example-skill
%% description: One-line description
%% homepage: https://example.com
%% metadata: {"beamclaw": {"emoji": "...", "requires": {...}}}
%% ---
%%
%% # Markdown content...
%% ```
%%
%% Parsing: split on `---` delimiters, parse frontmatter as key: value lines,
%% metadata value is JSON. Everything after second `---` is the content body.
-module(bc_skill_parser).

-include_lib("beamclaw_core/include/bc_types.hrl").

-export([parse/2, parse/3]).

%% @doc Parse a SKILL.md binary into a #bc_skill{} record.
%% Source is `global` or `{agent, AgentId}`.
-spec parse(binary(), global | {agent, binary()}) ->
    {ok, #bc_skill{}} | {error, term()}.
parse(Binary, Source) ->
    parse(Binary, Source, "").

%% @doc Parse with an explicit filesystem path.
-spec parse(binary(), global | {agent, binary()}, string()) ->
    {ok, #bc_skill{}} | {error, term()}.
parse(Binary, Source, Path) ->
    case split_frontmatter(Binary) of
        {ok, FrontmatterBin, ContentBin} ->
            KV = parse_frontmatter(FrontmatterBin),
            Name = maps:get(<<"name">>, KV, undefined),
            case Name of
                undefined -> {error, missing_name};
                _ ->
                    Desc = maps:get(<<"description">>, KV, undefined),
                    Homepage = maps:get(<<"homepage">>, KV, undefined),
                    MetaRaw = maps:get(<<"metadata">>, KV, undefined),
                    Metadata = parse_metadata(MetaRaw),
                    Emoji = extract_emoji(Metadata),
                    {ok, #bc_skill{
                        name        = Name,
                        description = Desc,
                        homepage    = Homepage,
                        emoji       = Emoji,
                        content     = string:trim(ContentBin),
                        source      = Source,
                        metadata    = Metadata,
                        path        = Path
                    }}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Internal

%% Split binary on `---` delimiters. The frontmatter is between the first
%% and second `---`. Content is everything after the second `---`.
split_frontmatter(Binary) ->
    case binary:split(Binary, <<"---">>) of
        [Before, Rest] ->
            %% Before should be empty or whitespace (start of file)
            case is_blank(Before) of
                true ->
                    case binary:split(Rest, <<"---">>) of
                        [Frontmatter, Content] ->
                            {ok, Frontmatter, Content};
                        [_] ->
                            {error, no_closing_delimiter}
                    end;
                false ->
                    {error, no_opening_delimiter}
            end;
        [_] ->
            {error, no_frontmatter}
    end.

%% Parse frontmatter lines as `key: value` pairs.
parse_frontmatter(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global]),
    lists:foldl(fun(Line, Acc) ->
        Trimmed = string:trim(Line),
        case binary:split(Trimmed, <<":">>) of
            [Key, Value] when byte_size(Key) > 0 ->
                K = string:trim(Key),
                V = string:trim(Value),
                case V of
                    <<>> -> Acc;
                    _    -> maps:put(iolist_to_binary(K), iolist_to_binary(V), Acc)
                end;
            _ ->
                Acc
        end
    end, #{}, Lines).

%% Parse the metadata JSON string. Returns a map or empty map on failure.
parse_metadata(undefined) -> #{};
parse_metadata(<<>>) -> #{};
parse_metadata(JsonBin) ->
    try jsx:decode(JsonBin, [return_maps]) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    catch _:_ -> #{}
    end.

%% Extract emoji from metadata.beamclaw.emoji
extract_emoji(#{<<"beamclaw">> := #{<<"emoji">> := Emoji}}) -> Emoji;
extract_emoji(_) -> undefined.

is_blank(Bin) ->
    Trimmed = string:trim(Bin),
    Trimmed =:= <<>> orelse Trimmed =:= "".
