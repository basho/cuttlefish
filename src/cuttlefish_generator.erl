-module(cuttlefish_generator).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([map/3, find_mapping/2]).

map(Translations, Schema, Config) ->
    %% Config at this point is just what's in the .conf file.
    %% add_defaults/2 rolls the default values in from the schema
    DConfig = add_defaults(Config, Schema),

    %% Everything in DConfig is of datatype "string", 
    %% transform_datatypes turns them into other erlang terms
    %% based on the schema
    Conf = transform_datatypes(DConfig, Schema),

    %% This fold handles 1:1 mappings, that have no cooresponding translations
    %% The accumlator is the app.config proplist that we start building from
    %% these 1:1 mappings, hence the return "DirectMappings". 
    %% It also builds a list of "TranslationsToDrop". It's basically saying that
    %% if a user didn't actually configure this setting in the .conf file and 
    %% there's no default in the schema, then there won't be enough information
    %% during the translation phase to succeed, so we'll earmark it to be skipped
    {DirectMappings, TranslationsToDrop} = lists:foldl(
        fun(MappingRecord, {ConfAcc, XlatAcc}) ->
            Mapping = cuttlefish_mapping:mapping(MappingRecord),
            Default = cuttlefish_mapping:default(MappingRecord),
            Key = cuttlefish_mapping:key(MappingRecord),
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
                    NewValue = Xlat(Conf),
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

add_defaults(Conf, Schema) ->
    lists:foldl(
        fun(MappingRecord, Acc) ->
            Default = cuttlefish_mapping:default(MappingRecord),
            Key = cuttlefish_mapping:key(MappingRecord),
            Match = lists:any(
                fun({K, _V}) ->
                    cuttlefish_util:variable_key_match(K, Key)
                end, 
                Conf),
            %% No, then plug in the default
            FuzzyMatch = lists:member($$, Key),
            case {Match, FuzzyMatch} of
                {false, true} -> 
                    Sub = cuttlefish_mapping:include_default(MappingRecord),
                    [{cuttlefish_util:variable_key_replace(Key, Sub), Default}|Acc];
                {false, false} -> [{Key, Default}|Acc];
                _ -> Acc
            end 
        end, 
        Conf, 
        lists:filter(fun(MappingRecord) -> cuttlefish_mapping:default(MappingRecord) =/= undefined end, Schema)).

transform_datatypes(Conf, Schema) ->
    [ begin
        %% Look up mapping from schema
        MappingRecord = find_mapping(Key, Schema),
        DT = cuttlefish_mapping:datatype(MappingRecord),
        {Key, cuttlefish_datatypes:from_string(Value, DT)}
    end || {Key, Value} <- Conf].

%% Ok, this is tricky
%% There are three scenarios we have to deal with:
%% 1. The mapping is there! -> return mapping
%% 2. The mapping is not there -> error
%% 3. The mapping is there, but the key in the schema contains a $.
%%      (fuzzy match)
find_mapping(Key, Schema) ->
    {HardMappings, FuzzyMappings} =  lists:foldl(
        fun(Mapping, {HM, FM}) ->
            K = cuttlefish_mapping:key(Mapping), 
            case {Key =:= K, cuttlefish_util:variable_key_match(Key, K)} of
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

-ifdef(TEST).

map_test() ->
    lager:start(),
    {Translations, Schema} = cuttlefish_schema:file("../test/riak.schema"),
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
    ok.

find_mapping_test() ->
    Mappings = [
        cuttlefish_mapping:parse({mapping, "key.with.fixed.name", "", [{ default, 0}]}),
        cuttlefish_mapping:parse({mapping, "key.with.$variable.name", "",  [{ default, 1}]})
    ],

    ?assertEqual(
        "key.with.fixed.name",
        cuttlefish_mapping:key(find_mapping("key.with.fixed.name", Mappings))
        ),

    ?assertEqual(
        0,
        cuttlefish_mapping:default(find_mapping("key.with.fixed.name", Mappings))
        ),

    ?assertEqual(
        "key.with.$variable.name",
        cuttlefish_mapping:key(find_mapping("key.with.A.name", Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping("key.with.A.name", Mappings))
        ),

    ?assertEqual(
        "key.with.$variable.name",
        cuttlefish_mapping:key(find_mapping("key.with.B.name", Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping("key.with.B.name", Mappings))
        ),

    ?assertEqual(
        "key.with.$variable.name",
        cuttlefish_mapping:key(find_mapping("key.with.C.name", Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping("key.with.C.name", Mappings))
        ),

    ?assertEqual(
        "key.with.$variable.name",
        cuttlefish_mapping:key(find_mapping("key.with.D.name", Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping("key.with.D.name", Mappings))
        ),

    ?assertEqual(
        "key.with.$variable.name",
        cuttlefish_mapping:key(find_mapping("key.with.E.name", Mappings))
        ),

    ?assertEqual(
        1,
        cuttlefish_mapping:default(find_mapping("key.with.E.name", Mappings))
        ),

    ok.
-endif.
