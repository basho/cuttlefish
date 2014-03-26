%% -------------------------------------------------------------------
%%
%% cuttlefish_generator: this is where the action is
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
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
-module(cuttlefish_error).

-type error() :: {error, string()|[string()]}.
-type errorlist() :: {error, [error()]}.
-export_type([error/0, errorlist/0]).

-export([
        contains_error/1,
        is_error/1,
        filter/1,
        errorlist_maybe/1,
        print/1,
        print/2,
        format/1,
        format/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-spec contains_error(list()) -> boolean().
contains_error(List) ->
    lists:any(fun is_error/1, List).

-spec is_error(any()) -> boolean().
is_error({error, _}) -> true;
is_error(_) -> false.

-spec filter(list()) -> errorlist().
filter(List) ->
    {error, lists:filter(fun is_error/1, List)}.

-spec errorlist_maybe(any()) -> any().
errorlist_maybe(List) when is_list(List) ->
    case filter(List) of
        {error, []} ->
            List;
        Errorlist ->
            Errorlist
    end;
errorlist_maybe(AnythingElse) -> AnythingElse.

-spec print(string(), [any()]) -> ok.
print(FormatString, Args) ->
    print(io_lib:format(FormatString, Args)).

-spec print(string() | error()) -> ok.
print({error, List}) ->
    print(lists:flatten(List));
print(String) ->
    case lager:error("~s", [String]) of
        {error, lager_not_running} ->
            io:format("~s~n", [String]),
            ok;
        ok -> ok
    end.

-spec format(io:format()) -> error().
format(Str) -> format(Str, []).

-spec format(io:format(), list()) -> error().
format(Str, List) ->
    {error, lists:flatten(io_lib:format(Str, List))}.

-ifdef(TEST).

is_error_test() ->
    ?assert(is_error({error, "oh no!"})),
    ?assert(not(is_error("just an innocent string... I mean a list... I mean... argh, erlang"))),
    ok.

contains_error_test() ->
    ?assert(contains_error(["hi", {error, "hi!"}, "bye"])),
    ?assert(not(contains_error(["hi", "I'm not an error", "bye"]))),
    ok.

filter_test() ->
    ?assertEqual({error, []}, filter(["hi", "what even is an error?", "bye"])),
    ?assertEqual({error, [{error, "etoomanythings"}]},
                 filter(["hi", {error, "etoomanythings"}, "bye"])),
    ok.

errorlist_maybe_test() ->
    ?assertEqual(atom, errorlist_maybe(atom)),
    ?assertEqual(12, errorlist_maybe(12)),
    %% Fool you! "string" is a list!, but doesn't contain an error()
    ?assertEqual("string", errorlist_maybe("string")),

    ?assertEqual(
       {error, [{error, "etoomanythings"}]},
       errorlist_maybe(["hi", {error, "etoomanythings"}, "bye"])),
    ?assertEqual(
       ["hi", "what even is an error?", "bye"],
       errorlist_maybe(["hi", "what even is an error?", "bye"])),
    ok.

format_test() ->
    ?assertEqual({error, "Hi!"}, format("Hi!")),
    ?assertEqual({error, "Error: 17"}, format("Error: ~p", [17])),
    ok.

-endif.
