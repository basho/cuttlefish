-module(cuttlefish_unit).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

generate_templated_config(FileName, Conf, Context) ->
    case lists:all(fun(X) -> not is_list(X) end, FileName) of
        true -> %% it's a single depth list, aka string
            SchemaString = render_template(FileName, Context),
            generate_config(string, SchemaString, Conf);
        _ -> %% It's a list of lists, aka multiple strings
            SchemaStrings = [render_template(F, Context) || F <- FileName],
            generate_config(strings, SchemaStrings, Conf)
    end.

render_template(FileName, Context) ->
    {ok, Bin} = file:read_file(FileName),
    %% Stolen from rebar_templater:render/2
    %% Be sure to escape any double-quotes before rendering...
    ReOpts = [global, {return, list}],
    Str0 = re:replace(Bin, "\\\\", "\\\\\\", ReOpts),
    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),

    %% the mustache module is only available in the context of a rebar run.
    case code:ensure_loaded(mustache) of
        {module, mustache} ->
            mustache:render(Str1, dict:from_list(Context));
        _ ->
            io:format("mustache module not loaded. this test can only be run in a rebar context~n")
    end.

-spec generate_config(atom(), [string()]|string(), list()) -> list().
generate_config(strings, SchemaStrings, Conf) ->
    Schema = cuttlefish_schema:strings(SchemaStrings),
    cuttlefish_generator:map(Schema, Conf);

generate_config(string, SchemaString, Conf) ->
    Schema = cuttlefish_schema:strings([SchemaString]),
    cuttlefish_generator:map(Schema, Conf);

generate_config(file, SchemaFile, Conf) ->
    generate_config(SchemaFile, Conf).

-spec generate_config(string(), list()) -> list().
generate_config(SchemaFile, Conf) ->
    Schema = cuttlefish_schema:files([SchemaFile]),
    cuttlefish_generator:map(Schema, Conf).

assert_config(Config, KVCPath, Value) ->
    ActualValue = case kvc:path(KVCPath, Config) of
        [] -> %% if KVC can't find it, it returns []
            undefined;
        X -> X
    end,
    ?assertEqual({KVCPath, Value}, {KVCPath, ActualValue}).

-spec dump_to_file(any(), string()) -> ok.
dump_to_file(ErlangTerm, Filename) ->
    {ok, S} = file:open(Filename, [write,append]),
    io:format(S, "~p~n", [ErlangTerm]),
    file:close(S),
    ok.
