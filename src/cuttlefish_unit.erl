-module(cuttlefish_unit).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

generate_templated_config(FileName, Conf, Context) ->
    generate_templated_config(FileName, Conf, Context, {[], [], []}).

generate_templated_config(FileName, Conf, Context, PreexistingSchema) ->
    RenderedSchemas = case lists:all(fun(X) -> not is_list(X) end, FileName) of
        true -> %% it's a single depth list, aka string
            [{ cuttlefish_schema:string_fun_factory(), render_template(FileName, Context)}];
        _ -> %% It's a list of lists, aka multiple strings
            [{ cuttlefish_schema:string_fun_factory(), render_template(F, Context)} || F <- FileName]
    end,
    Schema = cuttlefish_schema:merger(RenderedSchemas ++ [ { fun(_, _) -> PreexistingSchema end, ""} ]),
    cuttlefish_generator:map(Schema, Conf).

render_template(FileName, Context) ->
    {ok, Bin} = file:read_file(FileName),
    %% Stolen from rebar_templater:render/2
    %% Be sure to escape any double-quotes before rendering...
    ReOpts = [global, {return, list}],
    Str0 = re:replace(Bin, "\\\\", "\\\\\\", ReOpts),
    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),

    %% the mustache module is only available in the context of a rebar run.
    case {code:ensure_loaded(mustache), code:ensure_loaded(rebar_mustache)} of
        {{module, mustache}, _} ->
            mustache:render(Str1, dict:from_list(Context));
        {_, {module, rebar_mustache}} ->
            rebar_mustache:render(Str1, dict:from_list(Context));
        _ ->
            io:format("mustache and/or rebar_mustache module not loaded. "
                      "This test can only be run in a rebar context.~n")
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

assert_valid_config({error, Phase, {error, Errors}}) ->
    %% What if Config is an error? It'd be nice to know what that was
    cuttlefish_error:print("Error in phase: ~s", [Phase]),
    _ = [ cuttlefish_error:print(E) || E <- Errors],
    ?assert(false);
assert_valid_config(List) when is_list(List) ->
    ok;
assert_valid_config(_) ->
    ?assert(false).

assert_config({error, _, _}=Config, _, _) ->
    assert_valid_config(Config);
assert_config(Config, Path, Value) ->
    ActualValue = case path(cuttlefish_variable:tokenize(Path), Config) of
        {error, bad_nesting} ->
            ?assertEqual({Path, Value}, {Path, nesting_error});
        notset ->
            ?assertEqual({Path, Value}, {Path, notset});
        {ok, X} -> X
    end,
    ?assertEqual({Path, Value}, {Path, ActualValue}).

assert_not_configured({error, _, _}=Config, _) ->
    assert_valid_config(Config);
assert_not_configured(Config, Path) ->
    ActualValue = case path(cuttlefish_variable:tokenize(Path), Config) of
        {error, bad_nesting} ->
            ?assert(false);
        {ok, _} ->
            ?assert(false);
        notset -> undefined
    end,
    ?assertEqual({Path, undefined}, {Path, ActualValue}).

%% @doc Asserts that the generated configuration is in error.
assert_error(Config) ->
    ?assertMatch({error, _, {error, _}}, Config).

%% @doc Asserts that the generated configuration is in error, with the
%% error occurring in a specific phase.
assert_error_in_phase(Config, Phase) when is_atom(Phase) ->
    ?assertMatch({error, Phase, {error, _}}, Config).

%% @doc Asserts that the generated configuration is in error, and the
%% given error message was emitted by the given phase.
assert_error(Config, Phase, Message) ->
    assert_error_in_phase(Config, Phase),
    assert_error_message(Config, Message).

%% @doc Asserts that the generated configuration is in error and has
%% the given error messages.
assert_errors(Config, [H|_]=Messages) when is_list(H) ->
    [ assert_error_message(Config, Message) || Message <- Messages ].

%% @doc Asserts that the generated configuration is in error, with
%% errors occuring in the given phase and containing the given
%% messages.
assert_errors(Config, Phase, [H|_]=Messages) when is_list(H) ->
    assert_error_in_phase(Config, Phase),
    [ assert_error_message(Config, Message) || Message <- Messages ].

%% @doc Asserts that the generated configuration is in error and
%% contains the given error message.
assert_error_message(Config, Message) ->
    assert_error(Config),
    {error, Messages} = element(3, Config),
    ?assert(lists:member({error, Message}, Messages)).


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
    _ = file:close(S),
    ok.

-ifdef(TEST).

path_test() ->
    ?assertEqual(
       {ok, "disable"},
       path(["vm_args", "-smp"], [{vm_args, [{'-smp', "disable"}]}])),
    ok.

multiple_schema_generate_templated_config_test() ->
    lager:start(),
    Context = [
        {mustache, "mustache"}
              ],
    PrereqSchema = {[], [
        cuttlefish_mapping:parse(
        {mapping, "c", "app.c", [
            {default, "/c"}
                                ]})
                        ], []},

    Config = cuttlefish_unit:generate_templated_config("../test/sample_mustache.schema", [], Context, PrereqSchema),
    lager:error("~p", [Config]),
    assert_config(Config, "app_a.setting_b", "/c/mustache/a.b"),
    ok.

-endif.
