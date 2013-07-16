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

-export([file/1, map/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.


%% TODO: Make map/2 work without Defaults
%% Defaults right now exists to provide structure for the app.config. It is a relic
%% of the time before translations. A Mapping in the schema used to describe a value's location
%% say in a tuple, but if you didn't have a mapping to element(2), we'd never know it was a 2-tuple
%% Now that we can translate, we can define what an element of the app config looks like at any level.
map({Translations, Schema, Conf}, Defaults) ->
    
    DirectMappings = lists:foldl(
        fun({Key, _Default, Attributes}, Acc) ->
            Mapping = proplists:get_value(mapping, Attributes),
            case {proplists:is_defined(datatype, Attributes), proplists:is_defined(Mapping, Translations)} of
                {true, false} -> 
                    {DT, _} = proplists:get_value(datatype, Attributes),
                    Tokens = string:tokens(Mapping, "."),
                    NewValue = caster(proplists:get_value(Key, Conf),  DT),
                    tyktorp(Tokens, Acc, NewValue);
                _ -> Acc
            end
        end, 
        Defaults, 
        Schema),

    lists:foldl(
        fun({Mapping, Xlat, _}, Acc) ->
            Tokens = string:tokens(Mapping, "."),
            NewValue = Xlat(Conf),
            tyktorp(Tokens, Acc, NewValue)
        end, 
        DirectMappings, 
        Translations). 

    %

tyktorp([LastToken], Acc, NewValue) ->
    {Type, Token, X} = token_type(LastToken),
    OldValue = proplists:get_value(Token, Acc), 
    New = case Type of
        tuple -> bjorn_util:replace_tuple_element(X, NewValue, OldValue); 
        _ -> NewValue
    end,
    bjorn_util:replace_proplist_value(Token, New, Acc); 
tyktorp([HeadToken|MoreTokens], PList, NewValue) ->
    {Type, Token, X} = token_type(HeadToken),
    OldValue = proplists:get_value(Token, PList),
    bjorn_util:replace_proplist_value(
        Token,
        tyktorp(MoreTokens, OldValue, NewValue),
        PList).

token_type(Token) ->
    case string:tokens(Token, "{}") of
        [X, N] -> { tuple, list_to_atom(X), list_to_integer(N) } ;
        [X] -> { normal, list_to_atom(X), none}
    end.
%for each token, is it special?
%
%if yes, special processing
%
%if no, recurse into this with the value from the proplist and tail of tokens
%
%unless the tail of tokens is []

%% Priority is a nested set of proplists, but each list has only one item
%% for easy merge
merge([{K,V}]=Priority, Proplist) ->
    case proplists:get_value(K, Proplist) of
        undefined -> Proplist ++ Priority;
        Existing ->
            proplists:delete(K, Proplist) ++ merge(V, Existing) 
    end; 
merge([], Proplist) -> Proplist;
merge(Priority, []) -> Priority.

caster(X, enum) -> list_to_atom(X);
caster(X, integer) -> list_to_integer(X);
caster(X, _) -> X.

-spec file(string()) -> [{string(), any(), list()}].
file(Filename) ->
    {ok, B} = file:read_file(Filename),
    %% TODO: Hardcoded utf8
    S = unicode:characters_to_list(B, utf8),
    string(S).

-spec string(string()) -> {[{string(), fun(), list()}], [{string(), any(), list()}]}.
string(S) -> 
    Lines = string:tokens(S, [$\n]),
    %% TODO: LineNos skew when file contains blank newlines
    {ok, Tokens, _} = erl_scan:string(S),
    Dots = [ LineNo || {dot, LineNo} <- Tokens],

    {_, BlowsChunks} = lists:foldl(fun(Dot, {I, Acc}) ->
            Chunk = lists:sublist(Lines, I, Dot - I + 1),  
            {Dot + 1, [Chunk|Acc]}
        end, {1, []} , Dots), 
    Chunks = lists:reverse(BlowsChunks), 
    
    KeyDefinitions = [ string:join(X, "\n") || X <- Chunks],
    Schemas = [ begin
        %%io:format("KeyDef: ~p~n", [KeyD]),
        { Key, Default } = parse(KeyD),
        
        Attributes = case erl_comment_scan:string(KeyD) of
            [] -> [];
            [{_, _, _, Comments}] ->
                comment_parser(Comments)
        end,
        
        {Key, Default, Attributes}
      end || KeyD <- KeyDefinitions],

      lists:partition(fun({_, _, Attributes}) -> proplists:is_defined(translation, Attributes) end, Schemas). 

-spec parse(string()) -> {string(), any()}.
parse(S) ->
    {ok,Scanned,_} = erl_scan:string(S),
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
    Conf = bjorn_conf_file:file("../test/riak.conf"),
    {ok, [Defaults]} = file:consult("../test/default.config"), 
    NewConfig = map({Translations, Schema, Conf}, Defaults),

    NewRingSize = proplists:get_value(ring_creation_size, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual(32, NewRingSize),

    NewAAE = proplists:get_value(anti_entropy, proplists:get_value(riak_kv, NewConfig)), 
    ?assertEqual({on,[]}, NewAAE),

    file:write_file("../generated.config",io_lib:fwrite("~p.\n",[NewConfig])),
    ok.

file_test() ->
    {_, Schema} = file("../test/riak.schema"),


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
                 {datatype,{enum,["on","off"]}},
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

    ok.

fun_file_test() ->
    {[{"fun", Fun, _}], _ } = Schema = file("../test/fun.schema"),
    
    ?assertEqual("bjorn!",
        Fun()
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
        "%% @mapping riak_kv.anti_entropy{1}"
    ],
    ParsedComments = comment_parser(Comments),
    ?assertEqual(
        [
          {datatype,{enum,["on","off"]}},
          {mapping, "riak_kv.anti_entropy{1}"}
        ], ParsedComments
        ),
    ok.

%mapper_test() ->
%   mapper("riak_kv.anti_entropy{0}")

-endif.