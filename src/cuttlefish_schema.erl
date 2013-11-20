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

-export([files/1, strings/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-type errorlist() :: {error, string()|[string()]}.
-type schema() :: {[cuttlefish_translation:translation()], [cuttlefish_mapping:mapping()], [cuttlefish_validator:validator()]}.
-export_type([schema/0]).

files(ListOfSchemaFiles) ->
    merger(fun file/1, ListOfSchemaFiles).

strings(ListOfStrings) ->
    merger(fun string/1, ListOfStrings).

merger(Fun, ListOfInputs) ->
    Schema = lists:foldr(
        fun(Input, {TranslationAcc, MappingAcc, ValidatorAcc}) ->

            case Fun(Input) of
                {error, Errors} ->
                    %% These have already been logged. We're not moving forward with this
                    %% but, return them anyway so the rebar plugin can display them 
                    %% with io:format, since it doesn't have lager.
                    {error, Errors}; 
                {Translations, Mappings, Validators} ->
                    
                    NewMappings = lists:foldr(
                        fun cuttlefish_mapping:replace/2,
                        MappingAcc,
                        Mappings),

                    NewTranslations = lists:foldr(
                        fun cuttlefish_translation:replace/2,
                        TranslationAcc,
                        Translations),

                    NewValidators = lists:foldr(
                        fun cuttlefish_validator:replace/2,
                        ValidatorAcc,
                        Validators),

                    {NewTranslations, NewMappings, NewValidators}
            end 
        end, 
        {[], [], []}, 
        ListOfInputs),
    filter(Schema).

%% This filter is *ONLY* for the case of multiple mappings to a single erlang
%% app setting, *AND* there's no corresponding translation for that app setting
filter({Translations, Mappings, Validators}) ->
    Counts = count_mappings(Mappings),
    {MappingsToCheck, _} = lists:unzip(Counts),
    NewMappings = lists:foldl(
        fun(MappingName, Acc) ->
            case lists:any(
                fun(T) -> cuttlefish_translation:mapping(T) =:= MappingName end, 
                Translations) of
                false ->
                    cuttlefish_mapping:remove_all_but_first(MappingName, Acc);
                _ -> Acc
            end
        end,
        Mappings, MappingsToCheck),

    {Translations, NewMappings, Validators}.

count_mappings(Mappings) ->
    lists:foldl(
        fun(M, Acc) ->
            orddict:update_counter(cuttlefish_mapping:mapping(M), 1, Acc)
        end,
        orddict:new(),
        Mappings).

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
            cuttlefish_util:print_error("Error parsing schema: ~s", [Filename]),
            {error, Errors};
        Schema ->
            Schema
    end.

%% string/1 reads in a string and returns a tuple of lists.
%% The first is a list of Translations
%% The second is a list of Mappings
%% The third is a list of Validators
%% 
%% The big issue here is how we should preserve order in a single string,
%% which for practical purposes represents a single schema file.

%% Ugh, that means conflict resolution.
-spec string(string()) -> {
    [cuttlefish_translation:translation()], 
    [cuttlefish_mapping:mapping()],
    [cuttlefish_validator:validator()]
} | {error, [errorlist()]}.
string(S) -> 
    case erl_scan:string(S) of
        {ok, Tokens, _} ->
            CommentTokens = erl_comment_scan:string(S),
            {Translations, Mappings, Validators, Errors} = parse_schema(Tokens, CommentTokens),
            
            case length(Errors) of
                0 ->
                    {Translations, Mappings, Validators};
                _ ->
                    [begin
                        case Desc of
                            [H|_] when is_list(H) ->
                                [ cuttlefish_util:print_error(D) || D <- Desc];
                            _ ->
                                cuttlefish_util:print_error(Desc)
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
    parse_schema(Tokens, Comments, {[], [], [], []}).

-spec parse_schema(
    [any()],
    [any()],
    {[cuttlefish_translation:translation()],
     [cuttlefish_mapping:mapping()],
     [cuttlefish_validator:validator()],
     [errorlist()]}
    ) -> 
        {[cuttlefish_translation:translation()],
         [cuttlefish_mapping:mapping()],
         [cuttlefish_validator:validator()],
         [errorlist()]}.
%% We're done! We don't care about any comments after the last schema item
parse_schema([], _LeftoverComments, {TAcc, MAcc, VAcc, EAcc}) ->
    {lists:reverse(TAcc), lists:reverse(MAcc), lists:reverse(VAcc), lists:reverse(EAcc)};
parse_schema(ScannedTokens, CommentTokens, {TAcc, MAcc, VAcc, EAcc}) ->
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

    NewAcc = case parse(Tokens) of
        {error, Reason} ->
            MoreInfo = "Schema parse error near line number " ++ integer_to_list(LineNo),
            {TAcc, MAcc, VAcc, [{error, [MoreInfo|Reason]} | EAcc]};
        {mapping, {mapping, Variable, Mapping, Proplist}} ->
            Attributes = comment_parser(Comments),
            Doc = proplists:get_value(doc, Attributes, []),
            MappingSource = {mapping, Variable, Mapping, [{doc, Doc}|Proplist]},
            {TAcc, cuttlefish_mapping:parse_and_merge(MappingSource, MAcc), VAcc, EAcc};
        {translation, Return} ->
            {cuttlefish_translation:parse_and_merge(Return, TAcc), MAcc, VAcc, EAcc};
        {validator, Return} ->
            {TAcc, MAcc, cuttlefish_validator:parse_and_merge(Return, VAcc), EAcc};
        Other ->
            {TAcc, MAcc, VAcc, [{error, io_lib:format("Unknown parse return: ~p", [Other])} | EAcc]}
    end,
    parse_schema(TailTokens, TailComments, NewAcc).

parse_schema_tokens(Scanned) -> 
    parse_schema_tokens(Scanned, []).

parse_schema_tokens([], Acc=[Last|_]) ->
    %% When you've reached the end of file without encountering a dot,
    %% return the result anyway and let erl_parse produce the error.
    {element(2, Last), lists:reverse(Acc), []};
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
    lager:start(),
    %% files/1 takes a list of schemas in priority order.
    %% Loads them in reverse order, as things are overridden
    {Translations, Mappings, Validators} = files(
        [
            "../test/multi1.schema",
            "../test/multi2.schema",
            "../test/multi3.schema"
        ]),

    ?assertEqual(6, length(Mappings)),
    [M1, M2, M3, M4, M5, M6] = Mappings,

    %% Check mappings in correct order
    io:format("~p", [Mappings]),
    ?assertEqual(["top_level", "var1"], cuttlefish_mapping:variable(M1)),
    ?assertEqual(["a", "some", "var1"], cuttlefish_mapping:variable(M2)),
    ?assertEqual(["a", "some", "var2"], cuttlefish_mapping:variable(M3)),
    ?assertEqual(["a", "some", "var3"], cuttlefish_mapping:variable(M4)),
    ?assertEqual(["b", "some", "var1"], cuttlefish_mapping:variable(M5)),
    ?assertEqual(["b", "some", "var2"], cuttlefish_mapping:variable(M6)),


    %% Check correct mapping overrides
    ?assertEqual("app_a.big_var", cuttlefish_mapping:mapping(M1)),
    ?assertEqual("app_a.some_var1", cuttlefish_mapping:mapping(M2)),
    ?assertEqual("app_a.some_var", cuttlefish_mapping:mapping(M3)),
    ?assertEqual("app_a.some_var3", cuttlefish_mapping:mapping(M4)),
    ?assertEqual("app_b.some_var3", cuttlefish_mapping:mapping(M5)),
    ?assertEqual("app_b.some_var2", cuttlefish_mapping:mapping(M6)),

    ?assertEqual(6, length(Translations)),
    [T1, T2, T3, T4, T5, T6] = Translations,

    %% Check translation overrides
    AssertTran = fun(Mapping, Translation, Expected) ->

        %% Check Order
        ?assertEqual(Mapping, cuttlefish_translation:mapping(Translation)),
        %% Check Override
        F1 = cuttlefish_translation:func(Translation),
        ?assertEqual(Expected, F1(x))
    end,

    AssertTran("app_a.big_var",   T1, "tippedy top"),
    AssertTran("app_a.some_var1", T2, "a1"),
    AssertTran("app_a.some_var2", T3, "a2"),
    AssertTran("app_a.some_var3", T4, "toplevel"),
    AssertTran("app_b.some_var1", T5, "b3"),
    AssertTran("app_b.some_var2", T6, "b2"),

    %% One more time, for validators!
    ?assertEqual(5, length(Validators)),
    [V1, V2, V3, V4, V5] = Validators,

    %% Now check overrides
    AssertVal = fun(Name, Validator, Expected) ->
        %% Check Order
        ?assertEqual(Name, cuttlefish_validator:name(Validator)),
        %% Check Override
        F1 = cuttlefish_validator:func(Validator),
        ?assertEqual(Expected, F1(x))
    end,

    AssertVal("top.val", V1, false),
    AssertVal("a.validator1", V2, true),
    AssertVal("a.validator2", V3, false),
    AssertVal("b.validator1", V4, false),
    AssertVal("b.validator2", V5, true),

    ok.

strings_filtration_test() ->

    String = "{mapping, \"a.b\", \"e.k\", []}.\n"
          ++ "{mapping, \"a.c\", \"e.k\", []}.\n"
          ++ "{mapping, \"a.d\", \"e.j\", []}.\n"
          ++ "{mapping, \"a.e\", \"e.j\", []}.\n"
          ++ "{translation, \"e.j\", fun(X) -> \"1\" end}.\n"
          ++ "{mapping, \"b.a\", \"e.i\", []}.\n"
          ++ "{mapping, \"b.b\", \"e.i\", []}.\n"
          ++ "{mapping, \"b.c\", \"e.i\", []}.\n"
          ++ "{translation, \"e.i\", fun(X) -> \"1\" end}.\n",
    {Translations, Mappings, _} = strings([String]),
    ?assertEqual(2, length(Translations)),
    ?assertEqual(6, length(Mappings)),
    ?assertEqual(["a", "b"], cuttlefish_mapping:variable(hd(Mappings))),
    ?assertEqual(["b", "b"], cuttlefish_mapping:variable(lists:nth(5, Mappings))),
    ok.

-endif.
