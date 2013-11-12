%% -------------------------------------------------------------------
%%
%% cuttlefish_generator: this is where the action is
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
-module(cuttlefish_generator).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([map/2, find_mapping/2, add_defaults/2]).

map(Schema, Config) ->
    map_add_defaults(Schema, Config).

map_add_defaults({_, Mappings, _} = Schema, Config) ->
    %% Config at this point is just what's in the .conf file.
    %% add_defaults/2 rolls the default values in from the schema
    lager:info("Adding Defaults"),
    DConfig = add_defaults(Config, Mappings),
    
    case contains_error(DConfig) of
        true ->
            lager:info("Error adding defaults, aborting"),
            {error, add_defaults};
        _ -> 
            map_transform_datatypes(Schema, Config, DConfig)
    end.

map_transform_datatypes({_, Mappings, _} = Schema, Config, DConfig) ->
    %% Everything in DConfig is of datatype "string", 
    %% transform_datatypes turns them into other erlang terms
    %% based on the schema
    lager:info("Applying Datatypes"),
    Conf = transform_datatypes(DConfig, Mappings),

    case contains_error(Conf) of
        true ->
            lager:info("Error transforming datatypes, aborting"),
            {error, transform_datatypes};
        _ ->
            map_validate(Schema, Config, Conf)
    end.

map_validate(Schema, Config, Conf) ->
    %% Any more advanced validators
    lager:info("Validation"),
    case run_validations(Schema, Conf) of
        true -> map_translate(Schema, Config, Conf);
        _ ->
            lager:error("Some validator failed, aborting"),
            {error, validation}
    end.

map_translate({Translations, Mappings, _Validators} = Schema, _Config, Conf) ->
    %% This fold handles 1:1 mappings, that have no cooresponding translations
    %% The accumlator is the app.config proplist that we start building from
    %% these 1:1 mappings, hence the return "DirectMappings". 
    %% It also builds a list of "TranslationsToDrop". It's basically saying that
    %% if a user didn't actually configure this setting in the .conf file and 
    %% there's no default in the schema, then there won't be enough information
    %% during the translation phase to succeed, so we'll earmark it to be skipped
    {DirectMappings, {TranslationsToMaybeDrop, TranslationsToKeep}} = lists:foldl(
        fun(MappingRecord, {ConfAcc, {MaybeDrop, Keep}}) ->
            Mapping = cuttlefish_mapping:mapping(MappingRecord),
            Default = cuttlefish_mapping:default(MappingRecord),
            Variable = cuttlefish_mapping:variable(MappingRecord),
            case {
                Default =/= undefined orelse cuttlefish_conf:is_variable_defined(Variable, Conf), 
                proplists:is_defined(Mapping, Translations)
                } of
                {true, false} -> 
                    Tokens = string:tokens(Mapping, "."),
                    NewValue = proplists:get_value(Variable, Conf),
                    {set_value(Tokens, ConfAcc, NewValue), {MaybeDrop, [Mapping|Keep]}};
                {true, true} -> {ConfAcc, {MaybeDrop, [Mapping|Keep]}};
                _ -> {ConfAcc, {[Mapping|MaybeDrop], Keep}}
            end
        end, 
        {[], {[],[]}},
        Mappings),
    lager:info("Applied 1:1 Mappings"),

    TranslationsToDrop = TranslationsToMaybeDrop -- TranslationsToKeep,
    %% The fold handles the translations. After we've build the DirecetMappings,
    %% we use that to seed this fold's accumulator. As we go through each translation
    %% we write that to the `app.config` that lives in the accumutator.
    Return = lists:foldl(
        fun(TranslationRecord, Acc) ->
            Mapping = cuttlefish_translation:mapping(TranslationRecord),
            Xlat = cuttlefish_translation:func(TranslationRecord),
            case lists:member(Mapping, TranslationsToDrop) of
                false ->
                    Tokens = string:tokens(Mapping, "."),
                    %% get Xlat arity
                    Arity = proplists:get_value(arity, erlang:fun_info(Xlat)),
                    NewValue = case Arity of 
                        1 ->
                            lager:debug("Running translation for ~s", [Mapping]),
                            try Xlat(Conf) of
                                X -> X
                            catch
                                E:R ->
                                    lager:error("Error running translation for ~s, [~p, ~p].", [Mapping, E, R])
                            end;
                        2 -> 
                            lager:debug("Running translation for ~s", [Mapping]),
                            try Xlat(Conf, Schema) of
                                X -> X
                            catch
                                E:R ->
                                    lager:error("Error running translation for ~s, [~p, ~p].", [Mapping, E, R])
                            end;
                        Other -> 
                            lager:error("~p is not a valid arity for translation fun() ~s. Try 1 or 2.", [Other, Mapping]),
                            undefined
                    end,
                    set_value(Tokens, Acc, NewValue);
                _ ->
                    lager:debug("~p in Translations to drop...", [Mapping]),
                    Acc
            end
        end, 
        DirectMappings, 
        Translations),
    lager:info("Applied Translations"),
    Return.

%for each token, is it special?
%
%if yes, special processing
%if no, recurse into this with the value from the proplist and tail of tokens
%
%unless the tail of tokens is []

%% This is the last token, so things ends with replacing the proplist value.
set_value([LastToken], Acc, NewValue) ->
    cuttlefish_util:replace_proplist_value(list_to_atom(LastToken), NewValue, Acc); 
%% This is the case of all but the last token.
%% recurse until you hit a leaf.
set_value([HeadToken|MoreTokens], PList, NewValue) ->
    Token = list_to_atom(HeadToken),
    OldValue = proplists:get_value(Token, PList, []),
    cuttlefish_util:replace_proplist_value(
        Token,
        set_value(MoreTokens, OldValue, NewValue),
        PList).

%% @doc adds default values from the schema when something's not 
%% defined in the Conf, to give a complete app.config
add_defaults(Conf, Mappings) ->
    Prefixes = get_possible_values_for_fuzzy_matches(Conf, Mappings),
    
    lists:foldl(
        fun(MappingRecord, Acc) ->
            Default = cuttlefish_mapping:default(MappingRecord),
            VariableDef = cuttlefish_mapping:variable(MappingRecord),

            IsFuzzyMatch =  lists:any(fun(X) -> hd(X) =:= $$ end, VariableDef),

            IsStrictMatch = lists:any(
                fun({K, _V}) ->
                    K =:= VariableDef
                end, 
                Conf),

            %% No, then plug in the default
            case {IsStrictMatch, IsFuzzyMatch} of
                %% Strict match means we have the setting already
                {true, false} -> Acc;
                %% If IsStrictMatch =:= false, IsFuzzyMatch =:= true, we've got a setting, but 
                %% it's part of a complex data structure.
                {false, true} ->
                    lists:foldl(
                        fun({Prefix, List}, SubAcc) -> 
                            case cuttlefish_util:variable_starts_with(VariableDef, Prefix) of 
                                true ->
                                    ToAdd = [ begin
                                        VariableToAdd = cuttlefish_util:variable_match_replace(VariableDef, V),
                                        case proplists:is_defined(VariableToAdd, Acc) of
                                            true ->
                                                no;
                                            _ ->
                                                {VariableToAdd, Default}
                                        end 
                                    end || V <- List],
                                    ToAdd2 = lists:filter(fun(X) -> X =/= no end, ToAdd), 
                                    SubAcc ++ ToAdd2;
                                _ -> SubAcc
                            end
                        end, 
                        Acc, 
                        Prefixes);
                %% If Match =:= FuzzyMatch =:= false, use the default, key not set in .conf
                {false, false} -> [{VariableDef, Default}|Acc];
                %% If Match =:= true, do nothing, the value is set in the .conf file
                _ -> 
                    %% TODO: Handle with more style and grace
                    lager:error("Both fuzzy and strict match! should not happen"),
                    [{error, io_lib:format("~p has both a fuzzy and strict match", [VariableDef])}|Acc]
            end 
        end, 
        Conf, 
        lists:filter(fun(MappingRecord) -> cuttlefish_mapping:default(MappingRecord) =/= undefined end, Mappings)).



%%%%%%%%%%%%%%%%%%%%%%%%
%% Prefixes is the thing we need for defaults of named keys
%% it looks like this:
%%
%% Prefixes: [{"riak_control.user",["user"]},
%%     {"listener.https",["internal"]},
%%     {"listener.protobuf",["internal"]},
%%     {"listener.http",["internal"]},
%%     {"multi_backend",
%%      ["bitcask_mult","leveldb_mult","leveldb_mult2","memory_mult"}]
%%%%%%%%%%%%%%%%%%%%%%%%
-spec get_possible_values_for_fuzzy_matches(cuttlefish_conf:conf(), [cuttlefish_mapping:mapping()]) -> [{string(), [string()]}].
get_possible_values_for_fuzzy_matches(Conf, Mappings) ->
    %% Get a list of all the key definitions from the schema
    %% that involve a pattern match
    FuzzyVariableDefs = 
        [ cuttlefish_mapping:variable(M) || M <- Mappings, lists:any(fun(X) -> hd(X) =:= $$ end, cuttlefish_mapping:variable(M))],

    %% Now, get all the Keys athat could match them
    FuzzyVariables = lists:foldl(
        fun({Variable, _}, FuzzyMatches) ->
            Fuzz = lists:filter(
                fun(VariableDef) -> 
                    cuttlefish_util:fuzzy_variable_match(Variable, VariableDef)
                end, 
                FuzzyVariableDefs), 
            case Fuzz of
                [] -> FuzzyMatches;
                [VD|_] -> 
                    ListOfVars = [ Var || {_, Var } <- cuttlefish_util:matches_for_variable_def(VD, [{Variable, 0}])],
                    orddict:append_list(VD, ListOfVars, FuzzyMatches)
            end
        end, 
        orddict:new(), 
        Conf), 
    
    %% PrefixesWithoutDefaults are all the names it found referenced in the Conf 
    %% proplist. It may look something like this: [{"n",["ck","ak","bk"]}]
    PrefixesWithoutDefaults = orddict:fold(
        fun(VariableDef, NameList, Acc) -> 
            {Prefix, _, _} = cuttlefish_util:split_variable_on_match(VariableDef), 
            orddict:append_list(Prefix, NameList, Acc)
        end, 
        orddict:new(), 
        FuzzyVariables),

    %% We're almost done, we need to go through the FuzzyKeyDefs again.
    %% make sure that each one starts with a Prefix. 
    %% if not *AND* the schema provides a 'include_default', add that
    %% default substitution to the list of prefixes

    %% For each FuzzyKeyDef, if it is not covered by a prefix in
    %% PrefixesWithoutDefaults, add it to DefaultsNeeded.
    DefaultsNeeded = lists:filter(
        fun(FVD) -> 
            lists:all(
                fun(P) -> 
                    string:str(FVD, P) =/= 1 
                end, 
                orddict:fetch_keys(PrefixesWithoutDefaults))
        end, 
        FuzzyVariableDefs), 

    %% This fold is our end result.
    %% For each DefaultNeeded, add the default if the schema has an "include_default"
    lists:foldl(
        fun(Needed, Acc) ->
            M = find_mapping(Needed, Mappings),
            DefaultVar = cuttlefish_mapping:include_default(M),

            case DefaultVar of
                undefined ->
                    Acc;
                _ ->
                    {Prefix, _Var, _} = cuttlefish_util:split_variable_on_match(Needed), 
                    DefaultVar = cuttlefish_mapping:include_default(M),
                    orddict:append(Prefix, DefaultVar, Acc) 
            end
            
        end, 
        PrefixesWithoutDefaults, 
        DefaultsNeeded).

-spec transform_datatypes(cuttlefish_conf:conf(), [cuttlefish_mapping:mapping()]) -> cuttlefish_conf:conf().
transform_datatypes(Conf, Mappings) ->
    lists:foldl(
        fun({Variable, Value}, Acc) ->
            %% Look up mapping from schema
            case find_mapping(Variable, Mappings) of
                {error, _} ->
                    %% So, this error message isn't so performant (s/o @argv0)
                    %% but it shouldn't happen too often, and I think it's important
                    %% to give users this feedback.

                    %% It won't prevent anything from starting, but will let you know
                    %% that you're trying to set something that has no effect
                    VarName = string:join(Variable, "."),
                    lager:warning("You've tried to set ~s, but there is no setting with that name.", [VarName]),
                    lager:warning("  it could be a typo. Did you mean one of these?"),

                    Possibilities = [ begin
                        MapVarName = string:join(cuttlefish_mapping:variable(M), "."),
                        {cuttlefish_util:levenshtein(VarName, MapVarName), MapVarName}
                    end || M <- Mappings],
                    Sorted = lists:sort(Possibilities),
                    [ lager:warning("    ~s", [T]) || {_, T} <- lists:sublist(Sorted, 3) ],
                    Acc;
                MappingRecord ->
                    DT = cuttlefish_mapping:datatype(MappingRecord),
                    case {DT, cuttlefish_datatypes:from_string(Value, DT)} of
                        {_, {error, Message}} ->
                            lager:error("Bad datatype: ~s ~s", [string:join(Variable, "."), Message]),
                            [{error, Message}|Acc];
                        {{enum, PossibleValues}, NewValue} ->
                            case lists:member(NewValue, PossibleValues) of
                                true -> [{Variable, NewValue}|Acc];
                                false ->  
                                    ErrStr = io_lib:format("Bad value: ~s for enum ~s", [NewValue, Variable]),
                                    lager:error(ErrStr),
                                    [{error, ErrStr}|Acc]
                            end;
                        {_, NewValue} -> [{Variable, NewValue}|Acc]
                    end
            end
        end, 
        [], 
        Conf). 

%% Ok, this is tricky
%% There are three scenarios we have to deal with:
%% 1. The mapping is there! -> return mapping
%% 2. The mapping is not there -> error
%% 3. The mapping is there, but the key in the schema contains a $.
%%      (fuzzy match)
find_mapping([H|_]=Variable, Mappings) when is_list(H) ->
    {HardMappings, FuzzyMappings} =  lists:foldl(
        fun(Mapping, {HM, FM}) ->
            VariableDef = cuttlefish_mapping:variable(Mapping), 
            case {Variable =:= VariableDef, cuttlefish_util:fuzzy_variable_match(Variable, VariableDef)} of
                {true, _} -> {[Mapping|HM], FM};
                {_, true} -> {HM, [Mapping|FM]};
                _ -> {HM, FM}
            end
        end,
        {[], []},
        Mappings),

    case {length(HardMappings), length(FuzzyMappings)} of
        {1, _} -> hd(HardMappings);
        {0, 1} -> hd(FuzzyMappings);
        {0, 0} -> {error, lists:flatten(io_lib:format("~s not_found", [string:join(Variable, ".")]))};
        {X, Y} -> {error, io_lib:format("~p hard mappings and ~p fuzzy mappings found for ~s", [X, Y, Variable])}
    end;
find_mapping(Variable, Mappings) ->
    find_mapping(cuttlefish_util:tokenize_variable_key(Variable), Mappings).

run_validations({_, Mappings, Validators}, Conf) ->
    Validations = [ begin
        Vs = cuttlefish_mapping:validators(M, Validators),
        Value = proplists:get_value(cuttlefish_mapping:variable(M), Conf),
        [ begin
            Validator = cuttlefish_validator:func(V),
            case {Value, Validator(Value)} of
                {undefined, _} -> true;
                {_, true} -> 
                    true;
                _ -> 
                    LogString = io_lib:format(
                        "~s invalid, ~s", 
                        [
                            cuttlefish_mapping:variable(M), 
                            cuttlefish_validator:description(V) 
                        ]),
                    lager:error(LogString),
                    false 
            end
        end || V <- Vs]

     end || M <- Mappings, 
            cuttlefish_mapping:validators(M) =/= [], 
            cuttlefish_mapping:default(M) =/= undefined orelse proplists:is_defined(cuttlefish_mapping:variable(M), Conf)
            ],
    lists:all(fun(X) -> X =:= true end, lists:flatten(Validations)). 

-spec contains_error(list()) -> boolean().
contains_error(List) ->
    lists:any(fun({error, _}) -> true; (_) -> false end, List).

-ifdef(TEST).

bad_conf_test() ->
    Conf = [
        {["integer_thing"], "thirty_two"},
        {["enum_thing"], bad_enum_value},
        {["ip_thing"], "not an IP address"}
    ],

    Mappings = [
        cuttlefish_mapping:parse({mapping, "integer_thing", "to.int", [
            {datatype, integer}
        ]}),
        cuttlefish_mapping:parse({mapping, "enum_thing", "to.enum", [
            {datatype, {enum, [on, off]}}
        ]}),
        cuttlefish_mapping:parse({mapping, "ip_thing", "to.ip", [
            {datatype, ip}
        ]})
    ],

    Translations = [
        cuttlefish_translation:parse({translation, "to.enum", fun(_ConfConf) -> whatev end}) 
    ],

    NewConfig = map({Translations, Mappings, []}, Conf),
    io:format("NewConf: ~p~n", [NewConfig]),

    ?assertEqual({error,transform_datatypes}, NewConfig), 
    ok.

add_defaults_test() ->
    %%lager:start(),
    Conf = [
        %%{["a","b","c"], "override"}, %% Specifically left out. Uncomment line to break test,
        {["a","c","d"], "override"},
        {["no","match"], "unchanged"},
        %%{"m.rk.x", "defined"}, %% since this is undefined no defaults should be created for "m",
        
        %% two matches on a name "ak" and "bk"
        {["n","ak","x"], "set_n_name_x"},
        {["n","bk","x"], "set_n_name_x2"},
        {["n","ck","y"], "set_n_name_y3"}
          
    ],

    Mappings = [
        %% First mapping, direct, not in .conf, will be default
        cuttlefish_mapping:parse({mapping, "a.b.c", "b.c", [
                {default, "q"}
            ]}),
        %% default is "l", but since "a.c.d" is in Conf, it will be "override"
        cuttlefish_mapping:parse({mapping, "a.c.d", "c.d", [
                {default, "l"}
            ]}),
        cuttlefish_mapping:parse({mapping, "m.$name.x", "some.proplist", [
                {default, "m_name_x"}
            ]}),
        cuttlefish_mapping:parse({mapping, "n.$name.x", "some.proplist", [
            {default, "n_name_x"}
        ]}),
        cuttlefish_mapping:parse({mapping, "n.$name.y", "some.proplist", [
            {default, "n_name_y"}
        ]}),
        cuttlefish_mapping:parse({mapping, "o.$name.z", "some.proplist", [
            {default, "o_name_z"},
            {include_default, "blue"}
        ]})
    ],

    DConf = add_defaults(Conf, Mappings),
    io:format("DConf: ~p~n", [DConf]),
    ?assertEqual(10, length(DConf)),
    ?assertEqual("q",               proplists:get_value(["a","b","c"], DConf)),
    ?assertNotEqual("l",            proplists:get_value(["a","c","d"], DConf)),
    ?assertEqual("override",        proplists:get_value(["a","c","d"], DConf)),
    ?assertEqual("unchanged",       proplists:get_value(["no","match"], DConf)),
    ?assertEqual("set_n_name_x",    proplists:get_value(["n","ak","x"], DConf)),
    ?assertEqual("set_n_name_x2",   proplists:get_value(["n","bk","x"], DConf)),
    ?assertEqual("n_name_x",        proplists:get_value(["n","ck","x"], DConf)),
    ?assertEqual("n_name_y",        proplists:get_value(["n","ak","y"], DConf)),
    ?assertEqual("n_name_y",        proplists:get_value(["n","bk","y"], DConf)),
    ?assertEqual("set_n_name_y3",   proplists:get_value(["n","ck","y"], DConf)),
    ?assertEqual("o_name_z",        proplists:get_value(["o","blue","z"], DConf)),
    ok.

map_test() ->
    lager:start(),
    Schema = cuttlefish_schema:file("../test/riak.schema"),
    
    Conf = conf_parse:file("../test/riak.conf"),

    NewConfig = map(Schema, Conf),
    
    NewRingSize = proplists:get_value(ring_creation_size, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual(32, NewRingSize),

    NewAAE = proplists:get_value(anti_entropy, proplists:get_value(riak_kv, NewConfig)), 
    ?assertEqual({on,[debug]}, NewAAE),

    NewSASL = proplists:get_value(sasl_error_logger, proplists:get_value(sasl, NewConfig)), 
    ?assertEqual(false, NewSASL),

    NewHTTP = proplists:get_value(http, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual([{"10.0.0.1", 80}, {"127.0.0.1", 8098}], NewHTTP),

    NewPB = proplists:get_value(pb, proplists:get_value(riak_api, NewConfig)), 
    ?assertEqual([{"127.0.0.1", 8087}], NewPB),

    NewHTTPS = proplists:get_value(https, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual(undefined, NewHTTPS),
    ok.

find_mapping_test() ->
    lager:start(),
    Mappings = [
        cuttlefish_mapping:parse({mapping, "variable.with.fixed.name", "", [{ default, 0}]}),
        cuttlefish_mapping:parse({mapping, "variable.with.$matched.name", "",  [{ default, 1}]})
    ],
    io:format("Mappings: ~p~n", [Mappings]),
    
    ?assertEqual(
        ["variable","with","fixed","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","fixed","name"], Mappings))
        ),

    ?assertEqual(
        0,
        cuttlefish_mapping:default(find_mapping(["variable","with","fixed","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","$matched","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","A","name"], Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","A","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","$matched","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","B","name"], Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","B","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","$matched","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","C","name"], Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","C","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","$matched","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","D","name"], Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","D","name"], Mappings))
        ),

    ?assertEqual(
        ["variable","with","$matched","name"],
        cuttlefish_mapping:variable(find_mapping(["variable","with","E","name"], Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","E","name"], Mappings))
        ),

    %% Test variable name with dot
    ?assertEqual(
        {error, "variable.with.E.F.name not_found"},
        find_mapping(["variable","with","E","F","name"], Mappings)
        ),
    %% Test variable name with escaped dot
    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping(["variable","with","E.F","name"], Mappings))
        ),

    ok.

validation_test() ->

    Pid = self(),

    Mappings = [cuttlefish_mapping:parse(
        {mapping, "a", "b.c", [{validators, ["a"]}, {datatype, {enum, [true, false]}}]}
        )
    ],

    Validators = [cuttlefish_validator:parse(
        {validator, "a", "error msg", fun(X) -> Pid ! X, true end}
        ) 
    ],

    Conf = [
        {["a"], true}
    ],

    AppConf = map({[], Mappings, Validators}, Conf),

    receive
        X ->
            ?assert(X)
    after
        1000 ->
            ?assert(false)
    end,

    ?assertEqual([{b, [{c, true}]}], AppConf),
    ok.

-endif.
