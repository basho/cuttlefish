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

-spec path(cuttlefish_variable:variable(), [proplists:property()]) ->
                  {ok, any()} | notset | {error, bad_nesting}.
path(_, []) ->
    {error, bad_nesting};
path(_, undefined) ->
    notset;
path([Last], Proplist) ->
    case is_defined(Last, Proplist) of
        true ->
            {ok, get_value(Last, Proplist)};
        _ ->
            notset
    end;
path([H|T], Proplist) ->
    path(T, get_value(H, Proplist)).

-spec is_defined(string(), [proplists:property()]) -> boolean().
is_defined(K, Props) ->
    proplists:is_defined(K, Props) orelse
    proplists:is_defined(list_to_atom(K), Props) orelse
    proplists:is_defined(list_to_binary(K), Props).

-spec get_value(string(), [proplists:property()]) -> any().
get_value(K, Props) ->
    get_values([list_to_atom(K), K, list_to_binary(K)], Props).

get_values([], _) -> undefined;
get_values([K|T], Props) ->
    case proplists:get_value(K, Props) of
        undefined ->
            get_values(T, Props);
        V -> V
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
