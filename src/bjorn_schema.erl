%% -------------------------------------------------------------------
%%
%% bjorn_schema: slurps schema files
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

-module(bjorn_schema).

-export([file/1, map/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% TODO: temporary
-compile(export_all).

map(Translations, Schema, Config) ->
    
    Conf = transform_datatypes(Config, Schema),
    DirectMappings = lists:foldl(
        fun({Key, _Default, Attributes}, Acc) ->
            Mapping = proplists:get_value(mapping, Attributes),
            case {proplists:is_defined(datatype, Attributes), proplists:is_defined(Mapping, Translations)} of
                {true, false} -> 
                    Tokens = string:tokens(Mapping, "."),
                    NewValue = proplists:get_value(Key, Conf),
                    tyktorp(Tokens, Acc, NewValue);
                _ -> Acc
            end
        end, 
        [], 
        Schema),
    
    lists:foldl(
        fun({Mapping, Xlat, _}, Acc) ->
            Tokens = string:tokens(Mapping, "."),
            NewValue = Xlat(Conf),
            tyktorp(Tokens, Acc, NewValue)
        end, 
        DirectMappings, 
        Translations). 

%for each token, is it special?
%
%if yes, special processing
%if no, recurse into this with the value from the proplist and tail of tokens
%
%unless the tail of tokens is []
tyktorp([LastToken], Acc, NewValue) ->
    {Type, Token, X} = token_type(LastToken),
    OldValue = proplists:get_value(Token, Acc), 
    New = case Type of
        tuple -> bjorn_util:replace_tuple_element(X, NewValue, OldValue); 
        _ -> NewValue
    end,
    bjorn_util:replace_proplist_value(Token, New, Acc); 
tyktorp([HeadToken|MoreTokens], PList, NewValue) ->
    {_Type, Token, _X} = token_type(HeadToken),
    OldValue = proplists:get_value(Token, PList, []),
    bjorn_util:replace_proplist_value(
        Token,
        tyktorp(MoreTokens, OldValue, NewValue),
        PList).

%% Keeping this around to deal with possible $ prefixed tokens
token_type(Token) ->
    case string:tokens(Token, "$") of
        [Token] -> { normal, list_to_atom(Token), none};
        [X] -> {named, list_to_atom(X), none}
    end.

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
        {0, 0} -> {error, not_found};
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
attribute_formatter([{mapping, Mapping}| T]) ->
    [{mapping, lists:flatten(Mapping)}| attribute_formatter(T)];
attribute_formatter([_Other | T]) ->
    attribute_formatter(T); %% TODO: don't throw other things away [ Other | attribute_formatter(T)]
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

    NewRingSize = proplists:get_value(ring_creation_size, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual(32, NewRingSize),

    NewAAE = proplists:get_value(anti_entropy, proplists:get_value(riak_kv, NewConfig)), 
    ?assertEqual({on,[debug]}, NewAAE),

    NewSASL = proplists:get_value(sasl_error_logger, proplists:get_value(sasl, NewConfig)), 
    ?assertEqual(false, NewSASL),

    NewHTTP = proplists:get_value(http, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual([{"127.0.0.1", 8098}, {"10.0.0.1", 80}], NewHTTP),

    file:write_file("../generated.config",io_lib:fwrite("~p.\n",[NewConfig])),
    ok.

file_test() ->
    {_, Schema} = file("../test/riak.schema"),
    ?assertEqual(7, length(Schema)),
    ?assertEqual(
        {"ring_size", 64, 
                [
                 {datatype,{integer,[]}},
                 {mapping, "riak_core.ring_creation_size"}]},
        lists:nth(1, Schema) 
        ),
    ?assertEqual(
        {"anti_entropy",on,
                [
                 {datatype,{enum,["on","off","debug"]}},
                 {mapping,"riak_kv.anti_entropy"}]},
        lists:nth(2, Schema) 
        ),
    ?assertEqual(
        { "log.console.file", "./log/console.log",
                [
                 {mapping, "lager.handlers"}
                ]},
        lists:nth(3, Schema) 
        ),
    ?assertEqual(
        { "log.error.file", "./log/error.log",
                [
                 {mapping, "lager.handlers"}
                ]},
        lists:nth(4, Schema) 
        ),
    ?assertEqual(
        { "log.syslog", off,
                [
                 {datatype,{enum,["on","off"]}},
                 {mapping, "lager.handlers"}
                ]},
        lists:nth(5, Schema) 
        ),
    ?assertEqual(
        { "sasl", off,
                [
                 {datatype,{enum,["on","off"]}},
                 {mapping, "sasl.sasl_error_logger"}
                ]},
        lists:nth(6, Schema) 
        ),
    ?assertEqual(
        { "listener.http.$name", "127.0.0.1:8098",
                [
                 {datatype,{ip,[]}},
                 {mapping, "riak_core.http"}
                ]},
        lists:nth(7, Schema) 
        ),
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
        "%% @optional",
        "%% @mapping riak_kv.anti_entropy"
    ],
    ParsedComments = comment_parser(Comments),
    ?assertEqual(
        [
          {datatype,{enum,["on","off"]}},
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
-endif.