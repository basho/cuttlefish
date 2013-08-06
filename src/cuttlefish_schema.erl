%% -------------------------------------------------------------------
%%
%% cuttlefish_schema: slurps schema files
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(cuttlefish_schema).

-export([file/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-type errorlist() :: {error, [string()]}.

-spec file(string()) -> {
    [cuttlefish_translation:translation()], 
    [cuttlefish_mapping:mapping()]
} | error.
file(Filename) ->
    {ok, B} = file:read_file(Filename),
    %% TODO: Hardcoded utf8
    S = unicode:characters_to_list(B, utf8),
    case string(S) of 
        {error, Errors} ->
            lager:error("Error parsing schema: ~s", [Filename]),
            {error, Errors};
        Schema ->
            Schema
    end.

-spec string(string()) -> {
    [cuttlefish_translation:translation()], 
    [cuttlefish_mapping:mapping()]
} | {error, [errorlist()]}.
string(S) -> 
    case erl_scan:string(S) of
        {ok, Tokens, _} ->
            CommentTokens = erl_comment_scan:string(S),
            Schemas = parse_schema(Tokens, CommentTokens),

            {Errors, _Other} = lists:partition(fun(X) -> element(1, X) =:= error end, Schemas),

            case length(Errors) of
                0 ->
                    lists:partition(fun cuttlefish_translation:is_translation/1, Schemas);
                _ ->
                    [ [ lager:error(Str) || Str <- Strings] || {error, Strings} <- Errors],
                    {error, Errors}
            end;
        {error, {Line, erl_scan, _}, _} ->
            ErrStr = "Error scanning erlang near line " ++ integer_to_list(Line),
            lager:error(ErrStr),
            {error, [{error, [ErrStr]}]}
    end.

parse_schema(Tokens, Comments) ->
    parse_schema(Tokens, Comments, []).

%% We're done! We don't care about any comments after the last schema item
-spec parse_schema(
    [any()],
    [any()],
    [cuttlefish_translation:translation() | cuttlefish_mapping:mapping()]
    ) -> 
        [cuttlefish_translation:translation() | cuttlefish_mapping:mapping()].
parse_schema([], _LeftoverComments, Acc) ->
    lists:reverse(Acc);
parse_schema(ScannedTokens, CommentTokens, Acc) ->
    {LineNo, Tokens, TailTokens } = parse_schema_tokens(ScannedTokens),
    {Comments, TailComments} = lists:foldr(
        fun(X={CommentLineNo, _, _, Comment}, {C, TC}) -> 
            case CommentLineNo < LineNo of
                true -> {Comment ++ C, TC};
                _ -> {C, [X|TC]}
            end
        end, 
        {[], []}, 
        CommentTokens),
    %%{ Key, Default } = parse(Tokens),
    {Type, Return} = parse(Tokens),
    Attributes = comment_parser(Comments),

    Item = case Type of
        error ->
            MoreInfo = "Schema parse error near line number " ++ integer_to_list(LineNo),
            {error, [MoreInfo|Return]};
        mapping ->
            {mapping, Key, Mapping, Proplist} = Return,
            Doc = proplists:get_value(doc, Attributes, []), 
            cuttlefish_mapping:parse({mapping, Key, Mapping, [{doc, Doc}|Proplist]});
        translation ->
            cuttlefish_translation:parse(Return)
    end,
    parse_schema(TailTokens, TailComments, [Item| Acc]).

parse_schema_tokens(Scanned) -> 
    parse_schema_tokens(Scanned, []).

parse_schema_tokens(Scanned, Acc=[{dot, LineNo}|_]) ->
    {LineNo, lists:reverse(Acc), Scanned};
parse_schema_tokens([H|Scanned], Acc) ->
    parse_schema_tokens(Scanned, [H|Acc]).

-spec parse(list()) -> { mapping | translation, tuple()} | errorlist().
parse(Scanned) ->
    case erl_parse:parse_exprs(Scanned) of
        {ok, Parsed} ->
            {value, X, _} = erl_eval:exprs(Parsed,[]),
            {element(1, X), X};
        {error, {_Line, erl_parse, String}} ->
            {error, String};
        E ->
            {error, E}
    end.

comment_parser(Comments) ->
    StrippedComments = 
        lists:filter(fun(X) -> X =/= [] end, 
            [percent_stripper(C) || C <- Comments]),
    %% now, let's go annotation hunting

    AttrList = lists:foldl(
        fun(Line, Acc) ->
                case {Line, Acc} of
                    {[ $@ | T], _} ->
                        Annotation = hd(string:tokens(T, [$\s])),
                        [{list_to_atom(Annotation), [percent_stripper(T -- Annotation)] }|Acc];
                    { _, []} -> [];
                    {String, _} ->
                        [{Annotation, Strings}|T] = Acc,
                        [{Annotation, [String|Strings]}|T]
                end
            end, [], StrippedComments), 
    SortedList = lists:reverse([ {Attr, lists:reverse(Value)} || {Attr, Value} <- AttrList]),
    CorrectedList = attribute_formatter(SortedList),
    CorrectedList.

%% Just handles the @doc business
attribute_formatter([Other | T]) ->
    [ Other | attribute_formatter(T)];
attribute_formatter([]) -> [].

percent_stripper(Line) ->
    percent_stripper_r(percent_stripper_l(Line)).

percent_stripper_l([$%|T]) -> percent_stripper_l(T);
percent_stripper_l([$\s|T]) -> percent_stripper_l(T);
percent_stripper_l(Line) -> Line.

percent_stripper_r(Line) -> 
    lists:reverse(
        percent_stripper_l(
            lists:reverse(Line))).
-ifdef(TEST).

percent_stripper_test() ->
    ?assertEqual("hi!", percent_stripper("%%% hi!")),
    ?assertEqual("hi!", percent_stripper("%% hi!")),
    ?assertEqual("hi!", percent_stripper("% hi!")),
    ?assertEqual("hi!", percent_stripper(" hi!")),
    ?assertEqual("hi!", percent_stripper(" % % hi!")),
    ?assertEqual("hi!", percent_stripper("% % % hi!")),
    ?assertEqual("hi!", percent_stripper("% % % hi! % % %")),
    ok.

comment_parser_test() ->
    Comments = [
        " ",
        "%% @doc this is a sample doc",
        "%% it spans multiple lines %%",
        "",
        "%% there can be line breaks",
        "%% @datatype enum on, off",
        "%% @advanced",
        "%% @include_default name_substitution",
        "%% @mapping riak_kv.anti_entropy"
    ],
    ParsedComments = comment_parser(Comments),
    ?assertEqual(["this is a sample doc",
                  "it spans multiple lines",
                  "there can be line breaks"],
                  proplists:get_value(doc, ParsedComments)), 
    ok.

bad_file_test() -> 
    cuttlefish_lager_test_backend:bounce(),
    {error, ErrorList} = file("../test/bad_erlang.schema"),

    Logs = cuttlefish_lager_test_backend:get_logs(),
    [L1|Tail] = Logs,
    [L2|[]] = Tail,
    ?assertMatch({match, _}, re:run(L1, "Error scanning erlang near line 10")),
    ?assertMatch({match, _}, re:run(L2, "Error parsing schema: ../test/bad_erlang.schema")),

    ?assertEqual([
        {error, ["Error scanning erlang near line 10"]}
        ], ErrorList),
    ok.

parse_invalid_erlang_test() ->
    cuttlefish_lager_test_backend:bounce(),
    SchemaString = lists:flatten([
            "%% @doc some doc\n",
            "%% the doc continues!\n",
            "{mapping, \"ring_size\", \"riak_core.ring_creation_size\", [\n",
            "  {datatype, penguin}"
            "}.\n"
        ]),
    Parsed = string(SchemaString),

    [Log1, Log2, Log3] = cuttlefish_lager_test_backend:get_logs(),
    ?assertMatch({match, _}, re:run(Log1, "Schema parse error near line number 4")),
    ?assertMatch({match, _}, re:run(Log2, "syntax error before: ")),
    ?assertMatch({match, _}, re:run(Log3, "'}'")),

    ?assertEqual({error, [{error, ["Schema parse error near line number 4", "syntax error before: ","'}'"]}]}, Parsed).


parse_bad_datatype_test() ->
    cuttlefish_lager_test_backend:bounce(),
    
    SchemaString = lists:flatten([
            "%% @doc some doc\n",
            "%% the doc continues!\n",
            "{mapping, \"ring_size\", \"riak_core.ring_creation_size\", [\n",
            "  {datatype, penguin}"
            "]}.\n"
        ]),
    _Parsed = string(SchemaString),
    ?assertEqual([], cuttlefish_lager_test_backend:get_logs()).

-endif.