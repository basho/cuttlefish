-module(cuttlefish_mapping).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-record(mapping, {
        key::string(),
        mapping::string(),
        default::term(),
        commented::term(),
        datatype = string :: atom(),
        enum::[atom()],
        advanced = false :: boolean(),
        doc = [] :: list(),
        include_default::string()
    }).

-opaque mapping() :: #mapping{}.
-export_type([mapping/0]).

-export([
    parse/1,
    is_mapping/1,
    key/1,
    mapping/1,
    default/1,
    commented/1,
    datatype/1,
    enum/1,
    advanced/1,
    doc/1,
    include_default/1
    ]).

-spec parse({mapping, string(), string(), [{atom(), any()}]}) -> mapping().
parse({mapping, Key, Mapping, Proplist}) ->
    #mapping{
        key = Key,
        default = proplists:get_value(default, Proplist),
        commented = proplists:get_value(commented, Proplist),
        mapping = Mapping,
        advanced = proplists:get_value(advanced, Proplist, false),
        datatype = proplists:get_value(datatype, Proplist, string),
        enum = proplists:get_value(enum, Proplist),
        doc = proplists:get_value(doc, Proplist, []),
        include_default = proplists:get_value(include_default, Proplist)  
    };
parse(_) -> error.

-spec is_mapping(any()) -> boolean().
is_mapping(M) ->
    is_tuple(M) andalso element(1, M) =:= mapping. 

-spec key(mapping()) -> string().
key(M) -> M#mapping.key.

-spec mapping(mapping()) -> string().
mapping(M) -> M#mapping.mapping.

-spec default(mapping()) -> term().
default(M) -> M#mapping.default.

-spec commented(mapping()) -> term().
commented(M)        -> M#mapping.commented.

-spec datatype(mapping()) -> atom().
datatype(M) -> M#mapping.datatype.

-spec enum(mapping()) -> [atom()].
enum(M) -> M#mapping.enum.

-spec advanced(mapping()) -> boolean().
advanced(M) -> M#mapping.advanced.

-spec doc(mapping()) -> [string()].
doc(M) -> M#mapping.doc.

-spec include_default(mapping()) -> string().
include_default(M) -> M#mapping.include_default.

-ifdef(TEST).

mapping_test() ->

    SampleMapping = {
        mapping,
        "conf.key",
        "erlang.key",
        [
            {advanced, true},
            {default, "default value"},
            {datatype, enum}, 
            {enum, ["on", "off"]},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {doc, ["documentation", "for feature"]}
        ]
    },

    Record = parse(SampleMapping),

    ?assertEqual("conf.key", Record#mapping.key),
    ?assertEqual("default value", Record#mapping.default),
    ?assertEqual("erlang.key", Record#mapping.mapping),
    ?assertEqual(true, Record#mapping.advanced),
    ?assertEqual(enum, Record#mapping.datatype),
    ?assertEqual(["on", "off"], Record#mapping.enum),
    ?assertEqual(["documentation", "for feature"], Record#mapping.doc),
    ?assertEqual("default_substitution", Record#mapping.include_default),

    %% funciton tests
    ?assertEqual("conf.key", key(Record)),
    ?assertEqual("default value", default(Record)),
    ?assertEqual("erlang.key", mapping(Record)),
    ?assertEqual(true, advanced(Record)),
    ?assertEqual(enum, datatype(Record)),
    ?assertEqual(["on", "off"], enum(Record)),
    ?assertEqual(["documentation", "for feature"], doc(Record)),
    ?assertEqual("default_substitution", include_default(Record)),

    ok.

-endif.