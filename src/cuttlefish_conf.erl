-module(cuttlefish_conf).

-export([
    generate/1,
    generate_file/2,
    file/1]).

file(Filename) ->
    case conf_parse:file(Filename) of
        {error, Reason} ->
            lager:error("Could not open file (~s) for Reason ~s", [Filename, Reason]);
        Conf ->
            Conf
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
    file:close(S).

generate_element(MappingRecord) ->
    Default = cuttlefish_mapping:default(MappingRecord),
    Key = cuttlefish_mapping:key(MappingRecord),
    Commented = cuttlefish_mapping:commented(MappingRecord),
    Advanced = cuttlefish_mapping:advanced(MappingRecord),
    IncDef = cuttlefish_mapping:include_default(MappingRecord),
    Datatype = cuttlefish_mapping:datatype(MappingRecord),
    %% advanced: leave out of generated .conf file
    %% commeneted $val: insert into .conf file, but commented out with $val
    %% include_default $val:  substitute '$name' or whatever in the key for $val
    %%    e.g. {include_default, "internal"}
    %%         listener.http.$name -> listener.http.internal 

    Field = cuttlefish_util:variable_key_replace(Key, IncDef),

    case generate_element(Advanced, Default, Commented) of
        no ->
            [];
        commented ->
            Comments = generate_comments(MappingRecord),
            Comments ++ [lists:flatten([ "## ", Field, " = ", cuttlefish_datatypes:to_string(Commented, Datatype) ]), ""];
        default ->
            Comments = generate_comments(MappingRecord),
            Comments ++ [lists:flatten([ Field, " = ", cuttlefish_datatypes:to_string(Default, Datatype) ]), ""]  
    end.

generate_element(true, _, _) -> no;
generate_element(_, undefined, undefined) -> no;
generate_element(_, _Default, undefined) -> default;
generate_element(_, undefined, _Comment) -> commented;
generate_element(_Advanced, _Default, _Commented) -> no.

generate_comments(MappingRecord) ->
    %%io:format("DocRec: ~p~n", [cuttlefish_mapping:key(MappingRecord)]),
    Doc = cuttlefish_mapping:doc(MappingRecord),
    [ "## " ++ D || D <- Doc].

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

-endif.