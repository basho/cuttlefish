-module(cuttlefish_unit).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

generate_config(SchemaFile, Conf) ->
    Schema = cuttlefish_schema:file(SchemaFile),
    cuttlefish_generator:map(Schema, Conf).

assert_config(Config, KVCPath, Value) ->
    ?assertEqual(Value, kvc:path(KVCPath, Config)).
