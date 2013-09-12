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

-export([files/1, file/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-type errorlist() :: {error, string()|[string()]}.
-type schema() :: {[cuttlefish_translation:translation()], [cuttlefish_mapping:mapping()], [cuttlefish_validator:validator()]}.
-export_type([schema/0]).

files(ListOfSchemaFiles) ->
    lists:foldl(
        fun(SchemaFile, {TranslationAcc, MappingAcc, ValidatorAcc}) ->

            case cuttlefish_schema:file(SchemaFile) of
                {error, _Errors} ->
                    %% These have already been logged. Were' not moving forward with this
                    erlang:halt(1); 
                {Translations, Mappings, Validators} ->
                    
                    NewMappings = lists:foldl(
                        fun(Mapping, NewMappingAcc) -> 
                            cuttlefish_mapping:replace(Mapping, NewMappingAcc) 
                        end, 
                        MappingAcc, 
                        Mappings), 

                    NewTranslations = lists:foldl(
                        fun(Translation, NewTranslationAcc) -> 
                            cuttlefish_translation:replace(Translation, NewTranslationAcc)
                        end, 
                        TranslationAcc, 
                        Translations), 

                    NewValidators = lists:foldl(
                        fun(Validator, NewValidatorAcc) -> 
                            cuttlefish_validator:replace(Validator, NewValidatorAcc)
                        end, 
                        ValidatorAcc, 
                        Validators),

                    {NewTranslations, NewMappings, NewValidators}
            end 
        end, 
        {[], [], []}, 
        ListOfSchemaFiles).

-spec file(string()) -> {
    [cuttlefish_translation:translation()], 
    [cuttlefish_mapping:mapping()],
    [cuttlefish_validator:validator()]
} | errorlist().
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
    [cuttlefish_mapping:mapping()],
    [cuttlefish_validator:validator()]
} | {error, [errorlist()]}.
string(S) -> 
    case erl_scan:string(S) of
        {ok, Tokens, _} ->
            CommentTokens = erl_comment_scan:string(S),
            Schemas = parse_schema(Tokens, CommentTokens),

            {Errors, _Other} = lists:partition(fun(X) -> element(1, X) =:= error end, Schemas),

            case length(Errors) of
                0 ->
                    {Translations, Mappings, Validators} = 
                        lists:foldr(
                            fun(Item, {Ts, Ms, Vs}) -> 
                                case element(1, Item) of
                                    translation ->
                                        {[Item|Ts], Ms, Vs};
                                    mapping ->
                                        {Ts, [Item|Ms], Vs};
                                    validator ->
                                        {Ts, Ms, [Item|Vs]};
                                    _ ->
                                        {Ts, Ms, Vs}
                                end 
                            end, 
                            {[],[],[]}, 
                            Schemas),
                    {cuttlefish_translation:remove_duplicates(Translations),
                     cuttlefish_mapping:remove_duplicates(Mappings),
                     cuttlefish_validator:remove_duplicates(Validators)};
                _ ->
                    [begin
                        case Desc of
                            [H|_] when is_list(H) ->
                                [ lager:error(D) || D <- Desc];
                            _ ->
                                lager:error(lists:flatten(Desc)) 
                        end
                    end || {error, Desc} <- Errors],
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
    [cuttlefish_translation:translation() | cuttlefish_mapping:mapping() | cuttlefish_validator:validator() | errorlist()]
    ) -> 
        [cuttlefish_translation:translation() | cuttlefish_mapping:mapping() | cuttlefish_validator:validator() | errorlist()].
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
    
    Item = case parse(Tokens) of
        {error, Reason} ->
            MoreInfo = "Schema parse error near line number " ++ integer_to_list(LineNo),
            {error, [MoreInfo|Reason]};
        {mapping, {mapping, Key, Mapping, Proplist}} ->
            Attributes = comment_parser(Comments),
            Doc = proplists:get_value(doc, Attributes, []), 
            cuttlefish_mapping:parse({mapping, Key, Mapping, [{doc, Doc}|Proplist]});
        {translation, Return} ->
            cuttlefish_translation:parse(Return);
        {validator, Return} ->
            cuttlefish_validator:parse(Return);
        Other ->
            {error, io_lib:format("Unknown parse return: ~p", [Other])}
    end,
    parse_schema(TailTokens, TailComments, [Item| Acc]).

parse_schema_tokens(Scanned) -> 
    parse_schema_tokens(Scanned, []).

parse_schema_tokens(Scanned, Acc=[{dot, LineNo}|_]) ->
    {LineNo, lists:reverse(Acc), Scanned};
parse_schema_tokens([H|Scanned], Acc) ->
    parse_schema_tokens(Scanned, [H|Acc]).

-spec parse(list()) -> { mapping | translation | validator, tuple()} | errorlist().
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
            "  {default, \"blue\"}, ",
            "  {datatype, penguin}"
            "]}.\n"
        ]),
    _Parsed = string(SchemaString),
    ?assertEqual([], cuttlefish_lager_test_backend:get_logs()).

files_test() ->
    {Translations, Mappings, Validations} = files(["../test/multi1.schema", "../test/multi2.schema"]),
    ?assertEqual(2, length(Mappings)),
    [M1, M2] = Mappings,
    ?assertEqual(["a","b","d"], cuttlefish_mapping:variable(M1)),
    ?assertEqual("what.ev1", cuttlefish_mapping:mapping(M1)),

    ?assertEqual(["a","b","c"], cuttlefish_mapping:variable(M2)),
    ?assertEqual("what.ev4", cuttlefish_mapping:mapping(M2)),

    ?assertEqual(2, length(Translations)),
    [T1, T2] = Translations,
    ?assertEqual("what.ev2", cuttlefish_translation:mapping(T1)),
    F1 = cuttlefish_translation:func(T1),
    ?assertEqual(1, F1(x)),

    ?assertEqual("what.ev1", cuttlefish_translation:mapping(T2)),
    F2 = cuttlefish_translation:func(T2),
    ?assertEqual(4, F2(x)),

    ?assertEqual(1, length(Validations)),
    [V1] = Validations,
    ?assertEqual("my.little.validator", cuttlefish_validator:name(V1)),
    
    ok.

-endif.
