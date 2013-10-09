-module(cuttlefish_unit).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

generate_templated_config(FileName, Conf, Context) ->
    {ok, Bin} = file:read_file(FileName), 
    %% Stolen from rebar_templater:render/2
    %% Be sure to escape any double-quotes before rendering...
    ReOpts = [global, {return, list}],
    Str0 = re:replace(Bin, "\\\\", "\\\\\\", ReOpts),
    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),

    %% the mustache module is only available in the context of a rebar run.
    SchemaString = case code:ensure_loaded(mustache) of
        {module, mustache} ->
            mustache:render(Str1, dict:from_list(Context));
        _ ->
            io:format("mustache module not loaded. this test can only be run in a rebar context~n")
    end,
    generate_config(string, SchemaString, Conf).

-spec generate_config(atom(), string(), list()) -> list().
generate_config(string, SchemaString, Conf) ->
    Schema = cuttlefish_schema:string(SchemaString),
    cuttlefish_generator:map(Schema, Conf);

generate_config(file, SchemaFile, Conf) ->
    generate_config(SchemaFile, Conf).

-spec generate_config(string(), list()) -> list().
generate_config(SchemaFile, Conf) ->
    Schema = cuttlefish_schema:file(SchemaFile),
    cuttlefish_generator:map(Schema, Conf).

assert_config(Config, KVCPath, Value) ->
    ?assertEqual(Value, kvc:path(KVCPath, Config)).
