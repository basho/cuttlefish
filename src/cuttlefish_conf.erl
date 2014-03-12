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
         files/1,
         is_variable_defined/2,
         pretty_datatype/1,
         pretty_doc/1]).

-type conf_pair() :: {cuttlefish_variable:variable(), any()}.
-type conf() :: [conf_pair()].
-export_type([conf_pair/0, conf/0]).

-define(FMT(F, Args), lists:flatten(io_lib:format(F, Args))).

is_variable_defined(VariableDef, Conf) ->
    lists:any(fun({X, _}) -> cuttlefish_variable:is_fuzzy_match(X, VariableDef) end, Conf).

files(ListOfConfFiles) ->
    lists:foldl(
      fun(ConfFile, ConfAcc) ->
              case cuttlefish_conf:file(ConfFile) of
                  {error, _Reason} ->
                      ConfAcc;
                  Conf ->
                      lists:foldl(
                        fun({K,V}, MiniAcc) ->
                                cuttlefish_util:replace_proplist_value(K, V, MiniAcc)
                        end,
                        ConfAcc,
                        Conf)
              end
      end,
      [],
      ListOfConfFiles).

file(Filename) ->
    case conf_parse:file(Filename) of
        {error, Reason} ->
            lager:error("Could not open file (~s) for Reason ~s", [Filename, Reason]),
            {error, Reason};
        Conf ->
            remove_duplicates(Conf)
    end.

-spec generate([cuttlefish_mapping:mapping()]) -> [string()].
generate(Mappings) ->
    lists:foldl(
      fun(Mapping, ConfFile) ->
              ConfFile ++ generate_element(Mapping)
      end, [], Mappings).

-spec generate_file([cuttlefish_mapping:mapping()], string()) -> ok.
generate_file(Mappings, Filename) ->
    ConfFileLines = generate(Mappings),

    {ok, S} = file:open(Filename, [write]),
    _ = [ begin
          io:format(S, "~s~n", [lists:flatten(Line)])
      end || Line <- ConfFileLines],
    _ = file:close(S),
    ok.

-spec generate_element(cuttlefish_mapping:mapping()) -> [string()].
generate_element(MappingRecord) ->
    Default = cuttlefish_mapping:default(MappingRecord),
    Key = cuttlefish_mapping:variable(MappingRecord),
    Commented = cuttlefish_mapping:commented(MappingRecord),
    Level = cuttlefish_mapping:level(MappingRecord),
    Hidden = cuttlefish_mapping:hidden(MappingRecord),
    IncDef = cuttlefish_mapping:include_default(MappingRecord),
    [Datatype|_] = cuttlefish_mapping:datatype(MappingRecord),
    %% level != basic OR hidden == true: leave out of generated .conf file
    %% commeneted $val: insert into .conf file, but commented out with $val
    %% include_default $val:  substitute '$name' or whatever in the key for $val
    %%    e.g. {include_default, "internal"}
    %%         listener.http.$name -> listener.http.internal

    Field = string:join(cuttlefish_variable:replace_match(Key, IncDef), "."),

    case Level of
        basic -> ok;
        Level ->
            lager:warning("{level, ~p} has been deprecated. Use 'hidden' or '{hidden, true}'", [Level])
    end,

    case generate_element(Hidden, Level, Default, Commented) of
        no ->
            [];
        commented ->
            Comments = generate_comments(MappingRecord),
            Comments ++ [lists:flatten([ "## ", Field, " = ", cuttlefish_datatypes:to_string(Commented, Datatype) ]), ""];
        default ->
            Comments = generate_comments(MappingRecord),
            Comments ++ [lists:flatten([ Field, " = ", cuttlefish_datatypes:to_string(Default, Datatype) ]), ""]
    end.
generate_element(true, _, _, _) -> no;
generate_element(false, _, undefined, undefined) -> no;
generate_element(false, basic, _Default, undefined) -> default;
generate_element(false, basic, _, _Comment) -> commented;
generate_element(false, _Level, _Default, _Commented) -> no.

-spec generate_comments(cuttlefish_mapping:mapping()) -> [string()].
generate_comments(M) ->
    DocString = cuttlefish_mapping:doc(M),

    [DefaultDT|_] = cuttlefish_mapping:datatype(M),
    Default = case cuttlefish_mapping:default(M) of
                  undefined -> [];
                  Other ->
                      [ "", ?FMT("Default: ~s", [cuttlefish_datatypes:to_string(Other, DefaultDT)]) ]
              end,

    Datatypes = ["", "Acceptable values:" |
                 [ ?FMT("  - ~s", [pretty_datatype(DT)])
                   || DT <- cuttlefish_mapping:datatype(M)]],

    Doc = DocString ++ Default ++ Datatypes,
    [ "## " ++ D || D <- Doc].

-spec pretty_datatype(cuttlefish_datatypes:datatype() |
                      cuttlefish_datatypes:extended()) -> string().
pretty_datatype(integer) -> "an integer";
pretty_datatype({enum, L}) ->
    "one of: " ++ string:join([ atom_to_list(A) || A <- L], ", ");
pretty_datatype(ip) -> "an IP/port pair, e.g. 127.0.0.1:10011";
pretty_datatype({duration, _}) -> "a time duration with units, e.g. '10s' for 10 seconds";
pretty_datatype(bytesize) -> "a byte size with units, e.g. 10GB";
pretty_datatype({integer, I}) -> "the integer " ++ integer_to_list(I);
pretty_datatype({string, S}) -> "the text \"" ++ S ++ "\"";
pretty_datatype({atom, A}) -> "the text \"" ++ atom_to_list(A) ++ "\"";
pretty_datatype({ip, {IP, Port}}) -> ?FMT("the address ~s:~p", [IP, Port]);
pretty_datatype({{duration,_}, D}) -> "the time duration " ++ D;
pretty_datatype({bytesize, B}) -> "the bytesize " ++ B;
pretty_datatype(file) -> "the path to a file";
pretty_datatype(directory) -> "the path to a directory";
pretty_datatype({file, F}) -> "the file " ++ F;
pretty_datatype({directory, D}) -> "the directory " ++ D;
pretty_datatype(flag) -> "on or off";
pretty_datatype({flag, On, Off}) when is_atom(On), is_atom(Off) ->
    ?FMT("~p or ~p", [On, Off]);
pretty_datatype({flag, {On,_}, {Off,_}}) ->
    ?FMT("~p or ~p", [On, Off]);
pretty_datatype(_) -> "text". %% string and atom


-spec pretty_doc([string()]) -> string().
pretty_doc(D) ->
    string:join(D, " ").

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

    ?assertEqual(7, length(GeneratedConf)),
    ?assertEqual(
       "## Default ring creation size.  Make sure it is a power of 2,",
       lists:nth(1, GeneratedConf)
      ),
    ?assertEqual(
       "## e.g. 16, 32, 64, 128, 256, 512 etc",
       lists:nth(2, GeneratedConf)
      ),
    ?assertEqual(
       "## ",
       lists:nth(3, GeneratedConf)
      ),
    ?assertEqual(
       "## Acceptable values:",
       lists:nth(4, GeneratedConf)
      ),
    ?assertEqual(
       "##   - an integer",
       lists:nth(5, GeneratedConf)
      ),
    ?assertEqual(
       "## ring_size = 64",
       lists:nth(6, GeneratedConf)
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
    ?assertEqual(["## Hi!", "## Bye!", "## ", "## Acceptable values:", "##   - text"], Comments).

duplicates_test() ->
    Conf = file("../test/multi1.conf"),
    ?assertEqual(2, length(Conf)),
    ?assertEqual("3", proplists:get_value(["a","b","c"], Conf)),
    ?assertEqual("1", proplists:get_value(["a","b","d"], Conf)),
    ok.

duplicates_multi_test() ->
    Conf = files(["../test/multi1.conf", "../test/multi2.conf"]),
    ?assertEqual(2, length(Conf)),
    ?assertEqual("4", proplists:get_value(["a","b","c"], Conf)),
    ?assertEqual("1", proplists:get_value(["a","b","d"], Conf)),
    ok.

files_one_nonent_test() ->
    Conf = files(["../test/multi1.conf", "../test/nonent.conf"]),
    ?assertEqual(2, length(Conf)),
    ?assertEqual("3", proplists:get_value(["a","b","c"], Conf)),
    ?assertEqual("1", proplists:get_value(["a","b","d"], Conf)),
    ok.

generate_element_level_advanced_test() ->
    cuttlefish_lager_test_backend:bounce(warning),
    assert_no_output({level, advanced}),
    [Log] = cuttlefish_lager_test_backend:get_logs(),
    ?assertMatch({match, _}, re:run(Log, "{level, advanced} has been deprecated. Use 'hidden' or '{hidden, true}'")),
    ok.

generate_element_level_intermediate_test() ->
    cuttlefish_lager_test_backend:bounce(warning),
    assert_no_output({level, intermediate}),
    [Log] = cuttlefish_lager_test_backend:get_logs(),
    ?assertMatch({match, _}, re:run(Log, "{level, intermediate} has been deprecated. Use 'hidden' or '{hidden, true}'")),
    ok.

generate_element_hidden_test() ->
    cuttlefish_lager_test_backend:bounce(warning),
    assert_no_output(hidden),
    assert_no_output({hidden, true}),
    ?assertEqual([], cuttlefish_lager_test_backend:get_logs()),
    ok.

assert_no_output(Setting) ->
    Mapping = cuttlefish_mapping:parse(
                {mapping,
                 "a", "b", [
                   {doc, ["blah"]},
                   Setting
               ]}),

    ?assertEqual([], generate_element(Mapping)).

-endif.
