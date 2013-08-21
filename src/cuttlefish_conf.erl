%% -------------------------------------------------------------------
%%
%% cuttlefish_conf: handles the reading and generation of .conf files
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
-module(cuttlefish_conf).

-export([
    generate/1,
    generate_file/2,
    file/1,
    files/1]).

files(ListOfConfFiles) ->
    lists:foldl(
        fun(ConfFile, ConfAcc) -> 
            Conf = cuttlefish_conf:file(ConfFile),
            lists:foldl(
                fun({K,V}, MiniAcc) ->
                    cuttlefish_util:replace_proplist_value(K, V, MiniAcc) 
                end, 
                ConfAcc, 
                Conf) 
        end, 
        [], 
        ListOfConfFiles).

file(Filename) ->
    case conf_parse:file(Filename) of
        {error, Reason} ->
            lager:error("Could not open file (~s) for Reason ~s", [Filename, Reason]);
        Conf ->
            remove_duplicates(Conf)
    end.

%%validate(Schema, Config) ->
%%    %% For each config item, find the mapping and try converting that datertype
%%    [ begin
%%        Mapping = cuttlefish_generator:find_mapping(Key, Schema),
%%        cuttlefish_mapping:datatype(Mapping),
%%        ok 
%%    end || {Key, _Value} <- Config],
%%    ok.

generate(Schema) ->
    lists:foldl(
        fun(SchemaElement, ConfFile) ->
            ConfFile ++ generate_element(SchemaElement)
        end, [], Schema).

generate_file(Schema, Filename) ->
    ConfFileLines = generate(Schema),
    
    {ok, S} = file:open(Filename, write),
    [ begin
        io:format(S, "~s~n", [lists:flatten(Line)]) 
    end || Line <- ConfFileLines],
    file:close(S),
    ok.

generate_element(MappingRecord) ->
    Default = cuttlefish_mapping:default(MappingRecord),
    Key = cuttlefish_mapping:key(MappingRecord),
    Commented = cuttlefish_mapping:commented(MappingRecord),
    Level = cuttlefish_mapping:level(MappingRecord),
    IncDef = cuttlefish_mapping:include_default(MappingRecord),
    Datatype = cuttlefish_mapping:datatype(MappingRecord),
    %% level != basic: leave out of generated .conf file
    %% commeneted $val: insert into .conf file, but commented out with $val
    %% include_default $val:  substitute '$name' or whatever in the key for $val
    %%    e.g. {include_default, "internal"}
    %%         listener.http.$name -> listener.http.internal 

    Field = cuttlefish_util:variable_key_replace(Key, IncDef),

    case generate_element(Level, Default, Commented) of
        no ->
            [];
        commented ->
            Comments = generate_comments(MappingRecord),
            Comments ++ [lists:flatten([ "## ", Field, " = ", cuttlefish_datatypes:to_string(Commented, Datatype) ]), ""];
        default ->
            Comments = generate_comments(MappingRecord),
            Comments ++ [lists:flatten([ Field, " = ", cuttlefish_datatypes:to_string(Default, Datatype) ]), ""]  
    end.

generate_element(_, undefined, undefined) -> no;
generate_element(basic, _Default, undefined) -> default;
generate_element(basic, undefined, _Comment) -> commented;
generate_element(_Level, _Default, _Commented) -> no.

generate_comments(MappingRecord) ->
    %%io:format("DocRec: ~p~n", [cuttlefish_mapping:key(MappingRecord)]),
    Doc = cuttlefish_mapping:doc(MappingRecord),
    [ "## " ++ D || D <- Doc].

remove_duplicates(Conf) ->
    lists:foldl(
        fun({K,V}, MiniAcc) ->
            cuttlefish_util:replace_proplist_value(K, V, MiniAcc) 
        end, 
        [], 
        Conf).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-ifdef(TEST).

generate_element_test() ->

    TestSchemaElement =
        cuttlefish_mapping:parse({mapping, "ring_size", "riak_core.ring_creation_size", 
            [
             {datatype, integer},
             {commented, 64},
             {doc, ["Default ring creation size.  Make sure it is a power of 2,",
                    "e.g. 16, 32, 64, 128, 256, 512 etc"]}
            ]
        }),


    GeneratedConf = generate_element(TestSchemaElement),

    ?assertEqual(4, length(GeneratedConf)),
    ?assertEqual(
        "## Default ring creation size.  Make sure it is a power of 2,",
        lists:nth(1, GeneratedConf)
        ), 
    ?assertEqual(
        "## e.g. 16, 32, 64, 128, 256, 512 etc",
        lists:nth(2, GeneratedConf)
        ), 
    ?assertEqual(
        "## ring_size = 64",
        lists:nth(3, GeneratedConf)
        ), 
    ?assertEqual(
        "",
        lists:nth(4, GeneratedConf)
        ), 
    ok.

generate_dollar_test() ->
    TestSchemaElement =         
        cuttlefish_mapping:parse({ mapping, "listener.http.$name", "riak_core.http", [
             {datatype, ip},
             {default, "127.0.0.1:8098"},
             {mapping, "riak_core.http"},
             {include_default,"internal"}
            ]}),
    _GeneratedConf = generate_element(TestSchemaElement),

    ok.

generate_comments_test() ->
    SchemaElement = cuttlefish_mapping:parse({ mapping, "dont.care", "undefined", [
        {doc, ["Hi!", "Bye!"]}
    ]}),
    Comments = generate_comments(SchemaElement),
    ?assertEqual(["## Hi!", "## Bye!"], Comments).

duplicates_test() ->
    Conf = file("../test/multi1.conf"),
    ?assertEqual(2, length(Conf)),
    ?assertEqual("3", proplists:get_value("a.b.c", Conf)),
    ?assertEqual("1", proplists:get_value("a.b.d", Conf)),
    ok.

duplicates_multi_test() ->
    Conf = files(["../test/multi1.conf", "../test/multi2.conf"]),
    ?assertEqual(2, length(Conf)),
    ?assertEqual("4", proplists:get_value("a.b.c", Conf)),
    ?assertEqual("1", proplists:get_value("a.b.d", Conf)),
    ok.

files_one_nonent() ->
    Conf = files(["../test/multi1.conf", "../test/nonent.conf"]),
    ?assertEqual(2, length(Conf)),
    ?assertEqual("3", proplists:get_value("a.b.c", Conf)),
    ?assertEqual("1", proplists:get_value("a.b.d", Conf)),
    ok.

-endif.