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

-export([map/2, find_mapping/2]).

map({Translations, Mappings, Validators} = Schema, Config) ->
    %% Config at this point is just what's in the .conf file.
    %% add_defaults/2 rolls the default values in from the schema
    DConfig = add_defaults(Config, Mappings),

    %% Everything in DConfig is of datatype "string", 
    %% transform_datatypes turns them into other erlang terms
    %% based on the schema
    Conf = transform_datatypes(DConfig, Mappings),
            
    %% Any more advanced validators
    [ begin
        Vs = cuttlefish_mapping:validators(M, Validators),
        [ begin
            Value = proplists:get_value(cuttlefish_mapping:variable(M), Conf),
            Validator = cuttlefish_validator:func(V),
            case {Value, Validator(Value)} of
                {undefined, _} -> ok;
                {_, true} -> 
                    true;
                _ -> 
                    cuttlefish_message_handler:error(
                        "~s invalid, ~s", 
                        [
                            cuttlefish_mapping:variable(M), 
                            cuttlefish_validator:description(V) 
                        ]) 
            end
        end || V <- Vs]

     end || M <- Mappings, cuttlefish_mapping:validators(M) =/= []],

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
            Key = cuttlefish_mapping:variable(MappingRecord),
            case {
                Default =/= undefined orelse proplists:is_defined(Key, Conf), 
                proplists:is_defined(Mapping, Translations)
                } of
                {true, false} -> 
                    Tokens = string:tokens(Mapping, "."),
                    NewValue = proplists:get_value(Key, Conf),
                    {set_value(Tokens, ConfAcc, NewValue), {MaybeDrop, [Mapping|Keep]}};
                {true, true} -> {ConfAcc, {MaybeDrop, [Mapping|Keep]}};
                _ -> {ConfAcc, {[Mapping|MaybeDrop], Keep}}
            end
        end, 
        {[], {[],[]}},
        Mappings),
    TranslationsToDrop = TranslationsToMaybeDrop -- TranslationsToKeep,
    %% The fold handles the translations. After we've build the DirecetMappings,
    %% we use that to seed this fold's accumulator. As we go through each translation
    %% we write that to the `app.config` that lives in the accumutator.
    lists:foldl(
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
                            Xlat(Conf);
                        2 -> 
                            Xlat(Conf, Schema);
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
        Translations). 

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
                    lager:error("Both fuzzy and strict match! should not happen")
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
            case length(Fuzz) of
                0 -> FuzzyMatches;
                _ -> 
                    VD = hd(Fuzz),
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

transform_datatypes(Conf, Mappings) ->
    lists:foldl(
        fun({Variable, Value}, Acc) ->
            %% Look up mapping from schema
            case find_mapping(Variable, Mappings) of
                {error, _} ->
                    %% TODO: Revisit this log message
                    %% this log message is also called recursively doh!
                    %%lager:warning("No setting defined for ~s", [Key]),
                    Acc;
                MappingRecord ->
                    DT = cuttlefish_mapping:datatype(MappingRecord),
                    case {DT, cuttlefish_datatypes:from_string(Value, DT)} of
                        {_, {error, Message}} ->
                            cuttlefish_message_handler:error("Bad datatype: ~s ~s", [string:join(Variable, "."), Message]),
                            Acc;
                        {enum, NewValue} ->
                            case lists:member(NewValue, cuttlefish_mapping:enum(MappingRecord)) of
                                true -> [{Variable, NewValue}|Acc];
                                false ->  
                                    cuttlefish_message_handler:error("Bad value: ~s for enum ~s", [NewValue, Variable]),
                                    Acc
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

-ifdef(TEST).

bad_conf_test() ->
    Conf = [
        {["integer_thing"], "thirty_two"},
        {["enum_thing"], "bad_enum_value"},
        {["ip_thing"], "not an IP address"}
    ],

    Mappings = [
        cuttlefish_mapping:parse({mapping, "integer_thing", "to.int", [
            {datatype, integer}
        ]}),
        cuttlefish_mapping:parse({mapping, "enum_thing", "to.enum", [
            {datatype, enum},
            {enum, [on, off]}
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

    ?assertEqual([], NewConfig), 
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
-endif.
