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

-export([file/1, map/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

map(Translations, Schema, Config) ->
    DConfig = add_defaults(Config, Schema),
    Conf = transform_datatypes(DConfig, Schema),
    {DirectMappings, TranslationsToDrop} = lists:foldl(
        fun({Key, Default, Attributes}, {ConfAcc, XlatAcc}) ->
            Mapping = proplists:get_value(mapping, Attributes),
            case {
                Default =/= undefined orelse proplists:is_defined(Key, Conf), 
                proplists:is_defined(Mapping, Translations)
                } of
                {true, false} -> 
                    Tokens = string:tokens(Mapping, "."),
                    NewValue = proplists:get_value(Key, Conf),
                    {set_value(Tokens, ConfAcc, NewValue), XlatAcc};
                {true, true} -> {ConfAcc, XlatAcc};
                _ -> {ConfAcc, [Mapping|XlatAcc]}
            end
        end, 
        {[], []},
        Schema),
    
    %% Translations
    lists:foldl(
        fun({Mapping, Xlat, _}, Acc) ->
            case lists:member(Mapping, TranslationsToDrop) of
                false ->
                    %%io:format("Translation: ~s~n", [Mapping]),
                    Tokens = string:tokens(Mapping, "."),
                    NewValue = Xlat(Conf),
                    %%io:format("tyktorp(~s, ~p, ~p)~n", [Mapping, Acc, NewValue]),
                    set_value(Tokens, Acc, NewValue);
                _ ->
                    Acc
            end
        end, 
        DirectMappings, 
        Translations). 

%for each token, is it special?
%
%if yes, special processing
%if no, recurse into this with the value from the proplist and tail of tokens
%
%unless the tail of tokens is []

%% This is the last token, so things ends with replacing the proplist value.
set_value([LastToken], Acc, NewValue) ->
    {_Type, Token, _X} = token_type(LastToken),
    cuttlefish_util:replace_proplist_value(Token, NewValue, Acc); 
%% This is the case of all but the last token.
%% recurse until you hit a leaf.
set_value([HeadToken|MoreTokens], PList, NewValue) ->
    {_Type, Token, _X} = token_type(HeadToken),
    OldValue = proplists:get_value(Token, PList, []),
    cuttlefish_util:replace_proplist_value(
        Token,
        set_value(MoreTokens, OldValue, NewValue),
        PList).

%% TODO: Why is this function still here?
%% It's an important reminder that '$name' tokens aren't what you'd call "done"
%% They currently work as a place holder and it's up to the schema to do more 
%% robust handling. It might be that that's enough, but until we're sure.
%% never forget token_type/1
token_type(Token) ->
    case string:tokens(Token, "$") of
        [Token] -> { normal, list_to_atom(Token), none};
        [X] -> {named, list_to_atom(X), none}
    end.

%% TODO: keeping around for possible use in the advanced.config usecase. 
%% Priority is a nested set of proplists, but each list has only one item
%% for easy merge
%% merge([{K,V}]=Priority, Proplist) ->
%%     case proplists:get_value(K, Proplist) of
%%         undefined -> Proplist ++ Priority;
%%         Existing ->
%%             proplists:delete(K, Proplist) ++ merge(V, Existing) 
%%     end; 
%% merge([], Proplist) -> Proplist;
%% merge(Priority, []) -> Priority.


add_defaults(Conf, Schema) ->
    lists:foldl(
        fun({Key, Default, Attributes}, Acc) ->
            Match = lists:any(
                fun({K, _V}) ->
                    variable_key_match(K, Key)
                end, 
                Conf),
            %% No, then plug in the default
            FuzzyMatch = lists:member($$, Key),
            case {Match, FuzzyMatch} of
                {false, true} -> 
                    Sub = proplists:get_value(include_default, Attributes),
                    [{cuttlefish_util:variable_key_replace(Key, Sub), Default}|Acc];
                {false, false} -> [{Key, Default}|Acc];
                _ -> Acc
            end 
        end, 
        Conf, 
        lists:filter(fun({_K, Def, _A}) -> Def =/= undefined end, Schema)).

transform_datatypes(Conf, Schema) ->
    [ begin
        %% Look up mapping from schema
        {_Key, _Default, Attributes} = find_mapping(Key, Schema),
        %%Mapping = proplists:get_value(mapping, Attributes),
        {DT, _} = proplists:get_value(datatype, Attributes, {undefined, []}),
        {Key, caster(Value, DT)}
    end || {Key, Value} <- Conf].

%% Ok, this is tricky
%% There are three scenarios we have to deal with:
%% 1. The mapping is there! -> return mapping
%% 2. The mapping is not there -> error
%% 3. The mapping is there, but the key in the schema contains a $.
%%      (fuzzy match)
find_mapping(Key, Schema) ->
    {HardMappings, FuzzyMappings} =  lists:foldl(
        fun(Mapping={K, _D, _A}, {HM, FM}) -> 
            case {Key =:= K, variable_key_match(Key, K)} of
                {true, _} -> {[Mapping|HM], FM};
                {_, true} -> {HM, [Mapping|FM]};
                _ -> {HM, FM}
            end
        end,
        {[], []},
        Schema),

    case {length(HardMappings), length(FuzzyMappings)} of
        {1, _} -> hd(HardMappings);
        {0, 1} -> hd(FuzzyMappings);
        {0, 0} -> {error, io_lib:format("~s not_found", [Key])};
        {X, Y} -> {error, io_lib:format("~p hard mappings and ~p fuzzy mappings found for ~s", [X, Y, Key])}
    end.

variable_key_match(Key, KeyDef) ->
    KeyTokens = string:tokens(Key, "."),
    KeyDefTokens = string:tokens(KeyDef, "."),

    case length(KeyTokens) =:= length(KeyDefTokens) of
        true ->
            Zipped = lists:zip(KeyTokens, KeyDefTokens),
            lists:all(
                fun({X,Y}) ->
                    X =:= Y orelse hd(Y) =:= $$
                end,
                Zipped);
        _ -> false
    end.

%% I used to write java. in java, when you want to change something from
%% one datatype to another, you cast. So that's what we do here.
-spec caster(term(), atom()) -> term().
caster(X, enum) -> list_to_atom(X);
caster(X, integer) -> list_to_integer(X);
caster(X, ip) ->
    Parts = string:tokens(X, ":"),
    [Port|BackwardsIP] = lists:reverse(Parts),
    {string:join(lists:reverse(BackwardsIP), ":"), list_to_integer(Port)};
caster(X, _) -> X.

-spec file(string()) -> [{string(), any(), list()}].
file(Filename) ->
    {ok, B} = file:read_file(Filename),
    %% TODO: Hardcoded utf8
    S = unicode:characters_to_list(B, utf8),
    string(S).

-spec string(string()) -> {[{string(), fun(), list()}], [{string(), any(), list()}]}.
string(S) -> 
    {ok, Tokens, _} = erl_scan:string(S),
    CommentTokens = erl_comment_scan:string(S),
    Schemas = parse_schema(Tokens, CommentTokens),
    lists:partition(fun({_, _, Attributes}) -> proplists:is_defined(translation, Attributes) end, Schemas). 

parse_schema(Tokens, Comments) ->
    parse_schema(Tokens, Comments, []).

parse_schema([], _, Acc) ->
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
    { Key, Default } = parse(Tokens),
    Attributes = comment_parser(Comments),
    parse_schema(TailTokens, TailComments, [{Key, Default, Attributes}| Acc]).

parse_schema_tokens(Scanned) -> 
    parse_schema_tokens(Scanned, []).

parse_schema_tokens(Scanned, Acc=[{dot, LineNo}|_]) ->
    {LineNo, lists:reverse(Acc), Scanned};
parse_schema_tokens([H|Scanned], Acc) ->
    parse_schema_tokens(Scanned, [H|Acc]).

-spec parse(list()) -> {string(), any()}.
parse(Scanned) ->
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    {value, X, _} = erl_eval:exprs(Parsed,[]),
    X.

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

attribute_formatter([{translation, _}| T]) ->
    [{translation, true}| attribute_formatter(T)];
attribute_formatter([{datatype, DT}| T]) ->
    [{datatype, data_typer(DT)}| attribute_formatter(T)];
attribute_formatter([{advanced, _}| T]) ->
    [{advanced, true}| attribute_formatter(T)];
attribute_formatter([{mapping, Mapping}| T]) ->
    [{mapping, lists:flatten(Mapping)}| attribute_formatter(T)];
attribute_formatter([{include_default, NameSub}| T]) ->
    [{include_default, lists:flatten(NameSub)}| attribute_formatter(T)];
attribute_formatter([{commented, CommentValue}| T]) ->
    [{commented, lists:flatten(CommentValue)}| attribute_formatter(T)];
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

data_typer(DT) ->
    DataTypes = lists:flatten(DT),
    DataType = hd(string:tokens(DataTypes, [$\s])),
    Extra = DataTypes -- DataType,
    {list_to_atom(DataType), [ percent_stripper(T) || T <- string:tokens(Extra, [$,])] }.

-ifdef(TEST).
map_test() ->
    {Translations, Schema} = file("../test/riak.schema"),
    Conf = conf_parse:file("../test/riak.conf"),
    NewConfig = map(Translations, Schema, Conf),
    io:format("~p~n", [proplists:get_value(riak_core, NewConfig)]),

    NewRingSize = proplists:get_value(ring_creation_size, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual(32, NewRingSize),

    NewAAE = proplists:get_value(anti_entropy, proplists:get_value(riak_kv, NewConfig)), 
    ?assertEqual({on,[debug]}, NewAAE),

    NewSASL = proplists:get_value(sasl_error_logger, proplists:get_value(sasl, NewConfig)), 
    ?assertEqual(false, NewSASL),

    NewHTTP = proplists:get_value(http, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual([{"127.0.0.1", 8098}, {"10.0.0.1", 80}], NewHTTP),

    NewPB = proplists:get_value(pb, proplists:get_value(riak_api, NewConfig)), 
    ?assertEqual([{"127.0.0.1", 8087}], NewPB),

    NewHTTPS = proplists:get_value(https, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual(undefined, NewHTTPS),

    file:write_file("../generated.config",io_lib:fwrite("~p.\n",[NewConfig])),
    ok.

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
    ?assertEqual(
        [
            {doc,["this is a sample doc",
                  "it spans multiple lines",
                  "there can be line breaks"]},
            {datatype,{enum,["on","off"]}},
            {advanced, true},
            {include_default, "name_substitution"},
            {mapping, "riak_kv.anti_entropy"}
        ], ParsedComments
        ),
    ok.

caster_ip_test() ->
    ?assertEqual({"127.0.0.1", 8098}, caster("127.0.0.1:8098", ip)),
    ?assertEqual({"2001:0db8:85a3:0042:1000:8a2e:0370:7334", 8098}, caster("2001:0db8:85a3:0042:1000:8a2e:0370:7334:8098", ip)),
    ok.

find_mapping_test() ->
    Mappings = [
        {"key.with.fixed.name", 0, []},
        {"key.with.$variable.name", 1, []}
    ],
    ?assertEqual(
        {"key.with.fixed.name", 0, []}, 
        find_mapping("key.with.fixed.name", Mappings)),
    ?assertEqual(
        {"key.with.$variable.name", 1, []}, 
        find_mapping("key.with.A.name", Mappings)),
    ?assertEqual(
        {"key.with.$variable.name", 1, []}, 
        find_mapping("key.with.B.name", Mappings)),
    ?assertEqual(
        {"key.with.$variable.name", 1, []}, 
        find_mapping("key.with.C.name", Mappings)),
    ?assertEqual(
        {"key.with.$variable.name", 1, []}, 
        find_mapping("key.with.D.name", Mappings)),
    ?assertEqual(
        {"key.with.$variable.name", 1, []}, 
        find_mapping("key.with.E.name", Mappings)),
    ok.

all_the_marbles_test() ->
    %%lager:start(),
    {Translations, Schema} = file("../test/riak.schema"),
    Conf = [], %conf_parse:file("../test/riak.conf"),
    NewConfig = map(Translations, Schema, Conf),
    ?assert(is_proplist(NewConfig)),

    {ok, [AppConfig]} = file:consult("../test/default.config"),
    
    ?assert(is_proplist(AppConfig)),

    proplist_equals(AppConfig, NewConfig),
    ok.

proplist_equals(Expected, Actual) ->
    ExpectedKeys = lists:sort(proplists:get_keys(Expected)),
    ActualKeys = lists:sort(proplists:get_keys(Actual)),
    ?assertEqual(ExpectedKeys, ActualKeys), 
    [ begin 
        ExpectedValue = proplists:get_value(EKey, Expected),
        ActualValue = proplists:get_value(EKey, Actual, undefined),
        case {is_proplist(ExpectedValue), is_proplist(ActualValue)} of
            {true, true} ->
                proplist_equals(ExpectedValue, ActualValue);
            {false, false} ->
                ?assertEqual(ExpectedValue, ActualValue);
            _ ->
                ?assert(false)
        end
    end || EKey <- ExpectedKeys].

is_proplist(Proplist) when is_list(Proplist) ->
    lists:all(
        fun(X) -> 
            is_tuple(X) andalso tuple_size(X) =:= 2
        end,
        Proplist);
is_proplist(_) -> false.
-endif.