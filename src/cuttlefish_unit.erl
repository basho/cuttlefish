%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013-2017 Basho Technologies, Inc.
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

%%
%% @doc Support for unit testing.
%% @end
-module(cuttlefish_unit).

%% Documented and/or Active API
-export([
    assert_config/3,
    assert_error/1,
    assert_error/3,
    assert_error_in_phase/2,
    assert_error_message/2,
    assert_errors/2,
    assert_errors/3,
    assert_not_configured/2,
    assert_valid_config/1,
    generate_config/2,
    generate_templated_config/3,
    generate_templated_config/4,
    lib_priv_dir/1,
    lib_test_dir/1
]).

%% Historically Exported by -compile(export_all).
%% Commented out unless/until somebody screams.
%% None of these are used anywhere in Riak, nor are they mentioned in any
%% documentation.
%%
%%    chase_message/3,
%%    dump_to_file/2,
%%    generate_config/3,
%%    key_no_match/1,
%%    path/2,
%%    render_template/2

% I'm pretty sure these should be defined somewhere else, but where?
% These types are a rough guess, there doesn't seem to be an explicit
% statement on their specifications.
-export_type([
    mustache_ctx/0,
    mustache_def/0,
    mustache_key/0,
    mustache_val/0
]).
% Various mustache modules [claim to] accept the mustache_key() types.
% The type of mustache_val() could certainly be narrower, but to what?
-type mustache_ctx()  :: [mustache_def()].
-type mustache_def()  :: {mustache_key(), mustache_val()}.
-type mustache_key()  :: atom() | binary() | string().
-type mustache_val()  :: term().

% This file uses eunit's assert macros so inclusion is unconditional.
% However, eunit.hrl defines TEST by default, and we don't want that, so
% override the default behavior if not actually running eunit (or ct).
-ifndef(TEST).
-ifndef(NOTEST).
-define(NOTEST, true).
-endif.
-endif.
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

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

-spec generate_config(string(), list()) -> list().
generate_config(SchemaFile, Conf) ->
    Schema = cuttlefish_schema:files([SchemaFile]),
    cuttlefish_generator:map(Schema, Conf).

assert_valid_config(Config) ->
    case Config of
        List when is_list(List) ->
            ok;
        {error, Phase, {errorlist, Errors}} ->
            erlang:exit({assert_valid_config_failed,
                         [{phase, Phase},
                          {errorlist, Errors}]});
        Other ->
            erlang:exit({assert_valid_config_failed,
                         [{bad_value, Other}]})
    end.

assert_config(Config, Path, Value) ->
    ok = assert_valid_config(Config),
    ActualValue = case path(cuttlefish_variable:tokenize(Path), Config) of
        {error, bad_nesting} ->
            ?assertEqual({Path, Value}, {Path, nesting_error});
        notset ->
            ?assertEqual({Path, Value}, {Path, notset});
        {ok, X} ->
            X
    end,
    ?assertEqual({Path, Value}, {Path, ActualValue}).

assert_not_configured(Config, Path) ->
    ok = assert_valid_config(Config),
    case path(cuttlefish_variable:tokenize(Path), Config) of
        {error, bad_nesting} ->
            erlang:exit({assert_not_configured_failed,
                         [{bad_nesting, Path},
                          {config, Config}]});
        {ok, Value} ->
            erlang:exit({assert_not_configured_failed,
                         [{key, Path},
                          {configured_to, Value},
                          {config, Config}]});
        notset -> ok
    end.

%% @doc Asserts that the generated configuration is in error.
assert_error(Config) ->
    ?assertMatch({error, _, {errorlist, _}}, Config).

%% @doc Asserts that the generated configuration is in error, with the
%% error occurring in a specific phase.
assert_error_in_phase(Config, Phase) when is_atom(Phase) ->
    ?assertMatch({error, Phase, {errorlist, _}}, Config).

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
%% contains an error tuple that translates to the given error message
assert_error_message(Config, Message) ->
    ok = assert_error(Config),
    {errorlist, Errors} = element(3, Config),
    chase_message(Message, Errors, Errors).

-spec lib_priv_dir(Module :: module()) -> string() | false.
%%
%% @doc Returns the path to an application's working "priv" directory.
%%
%% Module MUST be compiled from a file in one of the application's main source
%% directories, generally the project's "src" directory.
%% Module's code IS NOT explicitly loaded by this operation.
%%
%% The returned path is to the "priv" directory in the working instance of the
%% application, which may be in a number of places in different Rebar versions.
%%
%% `false' is returned if the path cannot be determined, or does not exist,
%% or is not a directory.
%%
%% Logically, this is analogous to
%% ```
%%  filename:join(
%%      filename:dirname(filename:dirname(code:which(Module))),
%%      "priv" )
%% '''
%%
lib_priv_dir(Module) ->
    lib_sub_dir(Module, "priv").

-spec lib_test_dir(Module :: module()) -> string() | false.
%%
%% @doc Returns the path to an application's working "test" directory.
%%
%% Module MUST be compiled from a file in one of the application's main source
%% directories, generally the project's "src" directory.
%% Module's code IS NOT explicitly loaded by this operation.
%%
%% The returned path is to the "test" directory in the working instance of the
%% application, which may be in a number of places in different Rebar versions.
%%
%% `false' is returned if the path cannot be determined, or does not exist,
%% or is not a directory.
%%
%% Logically, this is analogous to
%% ```
%%  filename:join(
%%      filename:dirname(filename:dirname(code:which(Module))),
%%      "test" )
%% '''
%%
lib_test_dir(Module) ->
    lib_sub_dir(Module, "test").

%% ===================================================================
%% Historically Exported
%%
%% Everything in this section should be moved to either the Public API
%% or Internal section or, in the cases of commented-out dead code,
%% deleted.
%% ===================================================================

chase_message(Message, [], Errors) ->
    erlang:exit({assert_error_message_failed,
        [{expected, Message}, {actual, Errors}] });

chase_message(Message, [{error, ErrorTerm}|T], Errors) ->
    case lists:flatten(cuttlefish_error:xlate(ErrorTerm)) of
        Message ->
            ok;
        _ ->
            chase_message(Message, T, Errors)
    end.

%%-spec dump_to_file(any(), string()) -> ok.
%%dump_to_file(ErlangTerm, Filename) ->
%%    {ok, S} = file:open(Filename, [write,append]),
%%    io:format(S, "~p~n", [ErlangTerm]),
%%    _ = file:close(S),
%%    ok.

%%-spec generate_config(atom(), [string()]|string(), list()) -> list().
%%generate_config(strings, SchemaStrings, Conf) ->
%%    Schema = cuttlefish_schema:strings(SchemaStrings),
%%    cuttlefish_generator:map(Schema, Conf);
%%
%%generate_config(string, SchemaString, Conf) ->
%%    Schema = cuttlefish_schema:strings([SchemaString]),
%%    cuttlefish_generator:map(Schema, Conf);
%%
%%generate_config(file, SchemaFile, Conf) ->
%%    generate_config(SchemaFile, Conf).

-spec key_no_match(string()) -> fun((atom() | string() | binary()) -> boolean()).
key_no_match(Key) ->
    fun({E, _}) when is_atom(E) -> E =/= list_to_atom(Key);
        ({E, _}) when is_list(E) -> E =/= Key;
        ({E, _}) when is_binary(E) -> E =/= list_to_binary(Key);
        (_) -> true
    end.

-spec path(
        cuttlefish_variable:variable(),
        [{ string() | atom() | binary() , term()}])
            -> {ok, any()} | notset | {error, bad_nesting}.
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

-spec render_template(FileName :: file:name_all(), Context :: mustache_ctx())
            -> string().
render_template(FileName, Context) ->
    %% The mustache module may only be available in the context of a rebar run.
    case find_mustache() of
        false ->
            erlang:error(
                "No suitable mustache module loaded. "
                "Run this test in a rebar context.");
        Mod ->
            {ok, Bin} = file:read_file(FileName),
            render_template(Mod, Bin, Context)
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec render_template(
        Mustache    :: module(),
        Template    :: binary(),
        Context     :: mustache_ctx())
            -> string().
%
% Even where we're matching on the module, always use the Module variable so
% xref and dialyzer don't complain. If running in Rebar there will be a
% suitable module available at runtime, but usually there won't be an explicit
% project dependency on one.
%
% Unicode support is sketchy, as it is throughout cuttlefish. Someday...
%
render_template(bbmustache = Mustache, Template, Context) ->
    % It's unclear whether there could be supplementary UTF-8 bytes that could
    % be misinterpreted as relevant characters by the scanner, but as noted
    % above we're not putting much effort into Unicode for now.
    % The render call raises an error if there's a problem, so no need to
    % check return pattern.
    unicode:characters_to_list(
        Mustache:render(Template, mustache_context(Mustache, Context)));

render_template(Mustache, Template, Context) ->
    % Previous versions of this file escaped the template, but that actually
    % seems to break things I've tested, so it's just converted to a list.
    Data = case unicode:characters_to_list(Template, utf8) of
        Utf8 when erlang:is_list(Utf8) ->
            Utf8;
        _ ->
            % Not legal UTF-8, treat it as an 8-bit ISO-8859 encoding, which
            % will always succeed but _may_ be improperly mapped if it's not
            % specifically Latin-1. However, all of the punctuation characters
            % the scanner cares about should be fine.
            unicode:characters_to_list(Template, latin1)
    end,
    Mustache:render(Data, mustache_context(Mustache, Context)).

-spec find_mustache() -> module() | false.
%
% Finds a module that is likely to be a mustache implementation that
% render_template/3 knows how to use.
% Once identified, the module is cached in the process environment, so it'll
% be lost when the process goes away, which should coincide with eunit test
% setup/teardown when the available modules might change.
%
find_mustache() ->
    Key = {?MODULE, mustache_module},
    case erlang:get(Key) of
        undefined ->
            Ret = find_mustache([bbmustache, mustache, rebar_mustache]),
            _ = erlang:put(Key, Ret),
            Ret;
        Val ->
            Val
    end.

-spec find_mustache(Mods :: [module()]) -> module() | false.
%
% Let find_mustache/0 call this, not much use anywhere else.
%
find_mustache([Mod | Mods]) ->
    case code:ensure_loaded(Mod) of
        {module, _} ->
            case erlang:function_exported(Mod, render, 2) of
                true ->
                    Mod;
                _ ->
                    find_mustache(Mods)
            end;
        _ ->
            find_mustache(Mods)
    end;
find_mustache([]) ->
    false.

-spec mustache_context(Module :: module(), Context :: mustache_ctx()) -> term().
%
% Returns an appropriate mapping context for the mustache Module.
%
mustache_context(bbmustache, Context) ->
    % Despite what the docs say, bbmustache seems to need the keys to be
    % strings. This _could_ be limited to the pre-OTP-17 code, but we need R16
    % compatibility during the transition so I'm not going to sweat it.
    [mkey_string(Elem) || Elem <- Context];
mustache_context(rebar_mustache, Context) ->
    dict:from_list(Context);
mustache_context(_, Context) ->
    Context.

-spec mkey_string(Elem :: mustache_def()) -> {string(), mustache_val()}.
%
% Ensure the Key of the specified Elem is a string.
% Guards are ordered by assumed likelihood.
%
mkey_string({Key, Val}) when erlang:is_atom(Key) ->
    {erlang:atom_to_list(Key), Val};
mkey_string({Key, _} = Elem) when erlang:is_list(Key) ->
    Elem;
mkey_string({Key, Val}) when erlang:is_binary(Key) ->
    {erlang:binary_to_list(Key), Val}.

-spec lib_sub_dir(Module :: module(), SubDir :: string()) -> string() | false.
%
% Find the specified sub-directory of the application containing Module.
% We use the definitive code:get_object_code/1 initially, because we're often
% going to be running in a test scenario in which code:which/1 is going to
% return `cover_compiled', after which we'd have to resort to
% code:get_object_code/1 anyway.
%
lib_sub_dir(Module, SubDir) ->
    case code:get_object_code(Module) of
        {Module, _, Beam} ->
            Lib = filename:dirname(filename:dirname(Beam)),
            Dir = filename:join(Lib, SubDir),
            case filelib:is_dir(Dir) of
                true ->
                    Dir;
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

path_test() ->
    ?assertEqual(
        {ok, "disable"},
        path(["vm_args", "-smp"], [{vm_args, [{'-smp', "disable"}]}])).

multiple_schema_generate_templated_config_test() ->
    lager:start(),
    Context = [{mustache, "mustache"}],
    PrereqSchema = {
        [],
        [cuttlefish_mapping:parse({mapping, "c", "app.c", [{default, "/c"}]})],
        [] },

    TestDir = lib_test_dir(?MODULE),
    ?assertNotEqual(false, TestDir),
    Schema = filename:join(TestDir, "sample_mustache.schema"),

    Config = cuttlefish_unit:generate_templated_config(
        Schema, [], Context, PrereqSchema),

    lager:error("~p", [Config]),
    assert_config(Config, "app_a.setting_b", "/c/mustache/a.b"),
    ok.

-endif.
