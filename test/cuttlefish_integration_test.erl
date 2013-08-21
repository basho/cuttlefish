-module(cuttlefish_integration_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% This test generates a default .conf file from the riak.schema. view it at ../generated.conf
generated_conf_file_test() ->
    {_, Schema} = cuttlefish_schema:file("../test/riak.schema"),
    cuttlefish_conf:generate_file(Schema, "../generated.conf"),
    ok.


%% This test generates a .config file from the riak.schema. view it at ../generated.config
generated_config_file_test() ->
    {Translations, Schema} = cuttlefish_schema:file("../test/riak.schema"),
    Conf = conf_parse:file("../test/riak.conf"),
    NewConfig = cuttlefish_generator:map(Translations, Schema, Conf),
    
    file:write_file("../generated.config",io_lib:fwrite("~p.\n",[NewConfig])),
    ok.

%% Tests that the schema can generate a default app.config from nothing
all_the_marbles_test() ->
    {Translations, Schema} = cuttlefish_schema:file("../test/riak.schema"),
    Conf = [], %conf_parse:file("../test/riak.conf"),
    NewConfig = cuttlefish_generator:map(Translations, Schema, Conf),
    ?assert(is_proplist(NewConfig)),

    {ok, [AppConfig]} = file:consult("../test/default.config"),
    
    ?assert(is_proplist(AppConfig)),

    proplist_equals(AppConfig, NewConfig),
    ok.

multibackend_test() ->
    {Translations, Schema} = cuttlefish_schema:files(["../test/riak.schema", "../test/multi_backend.schema"]),
    [ begin 
        lager:info("T ~p", [cuttlefish_translation:mapping(T)]),
        Arity = proplists:get_value(arity, erlang:fun_info(cuttlefish_translation:func(T) )),
        lager:info("Arity ~p", [Arity])
    end || T <- Translations],
    Conf = [
        {"storage_backend", "multi"},
        {"multi_backend.bitcask_mult.storage_backend", "bitcask"},
        {"multi_backend.bitcask_mult.bitcask.data_root", "/path/to/dat/cask"},

        {"multi_backend.leveldb_mult.storage_backend", "leveldb"},
        {"multi_backend.leveldb_mult.leveldb.data_root", "/path/to/dat/level"},

        {"multi_backend.memory_mult.storage_backend", "memory"},
        {"multi_backend.memory_mult.memory_backend.ttl", "1d"},

        {"multi_backend.leveldb_mult2.storage_backend", "leveldb"},
        {"multi_backend.leveldb_mult.leveldb.data_root", "/path/to/dat/level2"}
    ],

    NewConfig = cuttlefish_generator:map(Translations, Schema, Conf),
    KV = proplists:get_value(riak_kv, NewConfig),
    Multi = proplists:get_value(multi_backend, KV), 

    lager:error("MultiBackendConfig: ~p", [Multi]),
    ?assert(false),
    ok. 


proplist_equals(Expected, Actual) ->
    ExpectedKeys = lists:sort(proplists:get_keys(Expected)),
    ActualKeys = lists:sort(proplists:get_keys(Actual)),
    ?assertEqual(ExpectedKeys, ActualKeys), 
    [ begin 
        ExpectedValue = proplists:get_value(EKey, Expected),
        ActualValue = proplists:get_value(EKey, Actual, undefined),
        case {is_proplist(ExpectedValue), is_proplist(ActualValue)} of
            {true, true} ->
                proplist_equals(ExpectedValue, ActualValue);
            {false, false} ->
                ?assertEqual(ExpectedValue, ActualValue);
            _ ->
                ?assert(false)
        end
    end || EKey <- ExpectedKeys].

is_proplist(Proplist) when is_list(Proplist) ->
    lists:all(
        fun(X) -> 
            is_tuple(X) andalso tuple_size(X) =:= 2
        end,
        Proplist);
is_proplist(_) -> false.