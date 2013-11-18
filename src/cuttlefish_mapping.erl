%% -------------------------------------------------------------------
%%
%% cuttlefish_mapping: models a single mapping
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
-module(cuttlefish_mapping).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-record(mapping, {
        variable::[string()],
        mapping::string(),
        default::term(),
        commented::term(),
        datatype = string :: cuttlefish_datatypes:datatype(),
        datatype_options :: any(),
        level = basic :: basic | intermediate | advanced,
        doc = [] :: list(),
        include_default = undefined :: string() | undefined,
        validators = [] :: [string()],
        priority = 0 :: integer()
    }).

-type mapping() :: #mapping{}.
-export_type([mapping/0]).

-export([
    parse/1,
    is_mapping/1,
    set_priority/2,
    priority/1,
    variable/1,
    mapping/1,
    default/1,
    commented/1,
    datatype/1,
    level/1,
    doc/1,
    include_default/1,
    replace/2,
    remove_duplicates/1,
    validators/1,
    validators/2,
    print/1
    ]).

-spec parse({mapping, string(), string(), [{atom(), any()}]}) -> mapping() | {error, list()}.
parse({mapping, Variable, Mapping, Proplist}) ->

    Datatype = case proplists:get_value(datatype, Proplist, string) of
        {enum, Enums} ->
            AtomEnums = [ begin
                case is_list(E) of
                    true -> list_to_atom(E);
                    _ -> E
                end
            end || E <- Enums ],
            {enum, AtomEnums};
        D -> D
    end,

    #mapping{
        variable = cuttlefish_util:tokenize_variable_key(Variable),
        default = proplists:get_value(default, Proplist),
        commented = proplists:get_value(commented, Proplist),
        mapping = Mapping,
        level = proplists:get_value(level, Proplist, basic),
        datatype = Datatype,
        doc = proplists:get_value(doc, Proplist, []),
        include_default = proplists:get_value(include_default, Proplist),
        validators = proplists:get_value(validators, Proplist, [])
    };
parse(X) -> {error, io_lib:format("poorly formatted input to cuttlefish_mapping:parse/1 : ~p", [X])}.


-spec set_priority(mapping(), Priority::integer()) -> mapping().
set_priority(Mapping, Prio) ->
    Mapping#mapping{priority = Prio}.

-spec priority(mapping()) -> integer().
priority(#mapping{priority = P}) ->
    P.

-spec is_mapping(any()) -> boolean().
is_mapping(M) ->
    is_tuple(M) andalso element(1, M) =:= mapping.

-spec variable(mapping()) -> [string()].
variable(M) -> M#mapping.variable.

-spec mapping(mapping()) -> string().
mapping(M) -> M#mapping.mapping.

-spec default(mapping()) -> term().
default(M) -> M#mapping.default.

-spec commented(mapping()) -> term().
commented(M)        -> M#mapping.commented.

-spec datatype(mapping()) -> cuttlefish_datatypes:datatype().
datatype(M) -> M#mapping.datatype.

-spec level(mapping()) -> basic | intermediate | advanced.
level(M) -> M#mapping.level.

-spec doc(mapping()) -> [string()].
doc(M) -> M#mapping.doc.

-spec include_default(mapping()) -> string() | undefined.
include_default(M) -> M#mapping.include_default.

-spec validators(mapping()) -> [string()].
validators(M) -> M#mapping.validators.

-spec validators(mapping(), [cuttlefish_validator:validator()]) -> [cuttlefish_validator:validator()].
validators(M, Validators) ->
    lists:foldr(fun(VName, Vs) ->
                        case lists:keyfind(VName, 2, Validators) of
                            false -> Vs;
                            V -> [V|Vs]
                        end
                end, [], M#mapping.validators).

-spec replace(mapping(), [mapping()]) -> [mapping()].
replace(Mapping, ListOfMappings) ->
    Removed = lists:filter(fun(M) -> variable(M) =/= variable(Mapping) end, ListOfMappings),
    Mapping1 = Removed ++ [Mapping],
    Mapping1.

-spec remove_duplicates([mapping()]) -> [mapping()].
remove_duplicates(Mappings) ->
    lists:foldl(
        fun(Mapping, Acc) ->
            replace(Mapping, Acc)
        end,
        [],
        Mappings).

print(Mappings) ->
    [lager:debug("[MAP] ~s.", [to_string(M)]) || M <- Mappings].

to_string(#mapping{variable = V, mapping = M}) ->
    io_lib:format("~s -> ~s", [string:join(V, "."), M]).

-ifdef(TEST).

mapping_test() ->

    SampleMapping = {
        mapping,
        "conf.key",
        "erlang.key",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {doc, ["documentation", "for feature"]},
            {validators, ["valid.the.impailer"]}
        ]
    },

    Record = parse(SampleMapping),

    ?assertEqual(["conf","key"], Record#mapping.variable),
    ?assertEqual("default value", Record#mapping.default),
    ?assertEqual("erlang.key", Record#mapping.mapping),
    ?assertEqual(advanced, Record#mapping.level),
    ?assertEqual({enum, [on, off]}, Record#mapping.datatype),
    ?assertEqual(["documentation", "for feature"], Record#mapping.doc),
    ?assertEqual("default_substitution", Record#mapping.include_default),
    ?assertEqual(["valid.the.impailer"], Record#mapping.validators),

    %% funciton tests
    ?assertEqual(["conf","key"], variable(Record)),
    ?assertEqual("default value", default(Record)),
    ?assertEqual("erlang.key", mapping(Record)),
    ?assertEqual(advanced, level(Record)),
    ?assertEqual({enum, [on, off]}, datatype(Record)),
    ?assertEqual(["documentation", "for feature"], doc(Record)),
    ?assertEqual("default_substitution", include_default(Record)),
    ?assertEqual(["valid.the.impailer"], validators(Record)),

    ok.

replace_test() ->
    Element1 = parse({
        mapping,
        "conf.key18",
        "erlang.key4",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {doc, ["documentation", "for feature"]}
        ]
    }),

    SampleMappings = [Element1,
    parse({
        mapping,
        "conf.key",
        "erlang.key1",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {doc, ["documentation", "for feature"]}
        ]
    }),
    parse({
        mapping,
        "conf.key",
        "erlang.key2",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {doc, ["documentation", "for feature"]}
        ]
    })
    ],

    Override = parse({
        mapping,
        "conf.key",
        "erlang.key",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {doc, ["documentation", "for feature"]}
        ]
    }),

    NewMappings = replace(Override, SampleMappings),
    ?assertEqual([Element1, Override], NewMappings),
    ok.


remove_duplicates_test() ->
    SampleMappings = [parse({
        mapping,
        "conf.key",
        "erlang.key1",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {doc, ["documentation", "for feature"]}
        ]
    }),
    parse({
        mapping,
        "conf.key",
        "erlang.key2",
        [
            {level, advanced},
            {default, "default value"},
            {datatype, {enum, [on, off]}},
            {commented, "commented value"},
            {include_default, "default_substitution"},
            {doc, ["documentation", "for feature"]}
        ]
    })
    ],

    NewMappings = remove_duplicates(SampleMappings),
    [_|Expected] = SampleMappings,
    ?assertEqual(Expected, NewMappings),
    ok.

validators_test() ->
    Validators = [
    cuttlefish_validator:parse({
        validator, "a", "a desc", fun(_X) -> true end
        }),
    cuttlefish_validator:parse({
        validator, "b", "b desc", fun(_X) -> true end
        }),
    cuttlefish_validator:parse({
        validator, "c", "c desc", fun(_X) -> true end
        })
    ],

    Mapping = parse({
        mapping,
        "conf.key",
        "erlang.key1",
        [
            {validators, ["a", "b"]}
        ]
    }),

    [A, B, _C] = Validators,

    ?assertEqual([A,B], validators(Mapping, Validators)),

    ok.

-endif.
