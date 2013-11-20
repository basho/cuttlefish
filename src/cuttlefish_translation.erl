%% -------------------------------------------------------------------
%%
%% cuttlefish_translation: models a cuttlefish translation
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
-module(cuttlefish_translation).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-record(translation, {
    mapping::string(),
    func::fun()
    }).
-type translation() :: #translation{}.
-export_type([translation/0]).

-export([
    parse/1,
    parse_and_merge/2,
    is_translation/1,
    mapping/1, 
    func/1,
    replace/2]).

-spec parse({translation, string(), fun()}) -> translation() | {error, list()}.
parse({translation, Mapping, Fun}) ->
    #translation{
        mapping = Mapping,
        func = Fun
    };
parse(X) -> {error, io_lib:format("poorly formatted input to cuttlefish_translation:parse/1 : ~p", [X])}.

%% This assumes it's run as part of a foldl over new schema elements
%% in which case, there's only ever one instance of a key in the list
%% so keyreplace works fine.
-spec parse_and_merge(
    tuple(), [translation()]) -> [translation()].
parse_and_merge({translation, Mapping, _} = TranslationSource, Translations) ->
    NewTranslation = parse(TranslationSource),

    case lists:keyfind(Mapping, #translation.mapping, Translations) of
        false ->
            [ NewTranslation | Translations];
        _OldMapping ->
            lists:keyreplace(Mapping, #translation.mapping, Translations, NewTranslation) 
    end.

-spec is_translation(any()) -> boolean().
is_translation(T) -> is_tuple(T) andalso element(1, T) =:= translation.

-spec mapping(translation()) -> string().
mapping(T)  -> T#translation.mapping.

-spec func(translation()) -> fun().
func(T)     -> T#translation.func.

-spec replace(translation(), [translation()]) -> [translation()].
replace(Translation, ListOfTranslations) ->
    Exists = lists:keymember(mapping(Translation), #translation.mapping, ListOfTranslations),
    case Exists of
        true ->
            lists:keyreplace(mapping(Translation), #translation.mapping, ListOfTranslations, Translation);
        _ ->
            [Translation | ListOfTranslations]
    end.

-ifdef(TEST).

parse_test() ->
    TranslationDataStruct = {
        translation,
        "mapping",
        fun(X) -> X*2 end
    },

    Translation = parse(TranslationDataStruct),

    ?assertEqual("mapping", Translation#translation.mapping),
    F = Translation#translation.func,
    ?assertEqual(4, F(2)),
    ok.


getter_test() ->
    Translation = #translation{
        mapping = "mapping",
        func = fun(X) -> X*2 end
    },

    ?assertEqual("mapping", mapping(Translation)),

    Fun = func(Translation),
    ?assertEqual(4, Fun(2)),
    ok.


replace_test() ->
    Element1 = #translation{
        mapping = "mapping18",
        func = fun(X) -> X*2 end
    },

    SampleTranslations = [Element1,
    #translation{
        mapping = "mapping1",
        func = fun(X) -> X*4 end
    }
    ],

    Override = #translation{
        mapping = "mapping1",
        func = fun(X) -> X*5 end
    },

    NewTranslations = replace(Override, SampleTranslations),
    ?assertEqual([Element1, Override], NewTranslations),
    ok.

-endif.
