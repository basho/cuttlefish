-module(cuttlefish_integration_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% This test generates a default .conf file from the riak.schema. view it at ../generated.conf
generated_conf_file_test() ->
	{_, Schema} = cuttlefish_schema:file("../test/riak.schema"),
	ConfFileLines = cuttlefish_conf:generate(Schema),

	{ok, S} = file:open("../generated.conf", write),
	[ begin
        io:format("~p~n", [lists:flatten(Line)]),
        io:format(S, "~s~n", [lists:flatten(Line)]) 
    end || Line <- ConfFileLines],
	file:close(S).


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