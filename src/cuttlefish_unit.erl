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

check_config_for_errors({error, Phase, {error, Errors}}) ->
    %% What if Config is an error? It'd be nice to know what that was
    cuttlefish_error:print("Error in phase: ~s", [Phase]),
    [ cuttlefish_error:print(E) || E <- Errors],
    ?assert(false);
check_config_for_errors(_) ->
    ok.

assert_config(Config, Path, Value) ->
    check_config_for_errors(Config),
    ActualValue = case path(cuttlefish_variable:tokenize(Path), Config) of
        {error, bad_nesting} ->
            ?assert(false);
        notset ->
            ?assert(false);
        {ok, X} -> X
    end,
    ?assertEqual({Path, Value}, {Path, ActualValue}).

assert_not_configured(Config, Path) ->
    check_config_for_errors(Config),
    ActualValue = case path(cuttlefish_variable:tokenize(Path), Config) of
        {error, bad_nesting} ->
            ?assert(false);
        {ok, _} ->
            ?assert(false);
        notset -> undefined
    end,
    ?assertEqual({Path, undefined}, {Path, ActualValue}).

-spec path(cuttlefish_variable:variable(),
           [{ string() | atom() | binary() , term()}]) ->
                  {ok, any()} | notset | {error, bad_nesting}.
path(_, []) ->
    {error, bad_nesting};
path(_, undefined) ->
    notset;
path([Last], Proplist) ->
    case lists:dropwhile(key_no_match(Last), Proplist) of
        [] -> notset;
        [{_, V}|_] -> {ok, V}
    end;
path([H|T], Proplist) when is_list(H)->
    case path([H], Proplist) of
        {ok, SmallerProplist} ->
            path(T, SmallerProplist);
        Other ->
            Other
    end.

-spec key_no_match(string()) -> fun((atom() | string() | binary()) -> boolean()).
key_no_match(Key) ->
    fun({E, _}) when is_atom(E) -> E =/= list_to_atom(Key);
       ({E, _}) when is_list(E) -> E =/= Key;
       ({E, _}) when is_binary(E) -> E =/= list_to_binary(Key);
       (_) -> true
    end.

-spec dump_to_file(any(), string()) -> ok.
dump_to_file(ErlangTerm, Filename) ->
    {ok, S} = file:open(Filename, [write,append]),
    io:format(S, "~p~n", [ErlangTerm]),
    file:close(S),
    ok.

-ifdef(TEST).



path_test() ->
    ?assertEqual(
       {ok, "disable"},
       path(["vm_args", "-smp"], [{vm_args, [{'-smp', "disable"}]}])),
    ok.

-endif.
