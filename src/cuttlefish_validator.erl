%% -------------------------------------------------------------------
%%
%% cuttlefish_validator: models a cuttlefish validator
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
-module(cuttlefish_validator).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-record(validator, {
    name::string(),
    description::string(),
    func::fun()
    }).
-opaque validator() :: #validator{}.
-export_type([validator/0]).

-export([
    parse/1,
    is_validator/1,
    name/1,
    description/1, 
    func/1,
    replace/2,
    remove_duplicates/1]).

-spec parse({validator, string(), fun()}) -> validator() | {error, list()}.
parse({validator, Name, Description, Fun}) ->
    #validator{
        name = Name,
        description = Description,
        func = Fun
    };
parse(X) -> {error, io_lib:format("poorly formatted input to cuttlefish_validator:parse/1 : ~p", [X])}.


-spec is_validator(any()) -> boolean().
is_validator(V) -> is_tuple(V) andalso element(1, V) =:= validator.

-spec name(validator()) -> string().
name(V) -> V#validator.name.

-spec description(validator()) -> string().
description(V) -> V#validator.description.

-spec func(validator()) -> fun().
func(V) -> V#validator.func.

-spec replace(validator(), [validator()]) -> [validator()].
replace(Validator, ListOfValidators) ->
    Removed = lists:filter(fun(T) -> name(T) =/= name(Validator) end, ListOfValidators), 
    Removed ++ [Validator].

-spec remove_duplicates([validator()]) -> [validator()].
remove_duplicates(Validators) ->
    lists:foldl(
        fun(Validator, Acc) ->
            replace(Validator, Acc)
        end, 
        [], 
        Validators). 

-ifdef(TEST).

parse_test() ->
    ValidatorDataStruct = {
        validator,
        "name",
        "description",
        fun(X) -> X*2 end
    },

    Validator = parse(ValidatorDataStruct),

    ?assertEqual("name", Validator#validator.name),
    ?assertEqual("description", Validator#validator.description),
    F = Validator#validator.func,
    ?assertEqual(4, F(2)),
    ok.


getter_test() ->
    Validator = #validator{
        name = "name",
        description = "description",
        func = fun(X) -> X*2 end
    },

    ?assertEqual("name", name(Validator)),
    ?assertEqual("description", description(Validator)),

    Fun = func(Validator),
    ?assertEqual(4, Fun(2)),
    ok.


replace_test() ->
    Element1 = #validator{
        name = "name18",
        description = "description18",
        func = fun(X) -> X*2 end
    },

    SampleValidators = [Element1,
    #validator{
        name = "name1",
        description = "description1",
        func = fun(X) -> X*3 end
    },
    #validator{
        name = "name1",
        description = "description1",
        func = fun(X) -> X*4 end
    }
    ],

    Override = #validator{
        name = "name1",
        description = "description1",
        func = fun(X) -> X*5 end
    },

    NewValidators = replace(Override, SampleValidators),
    ?assertEqual([Element1, Override], NewValidators),
    ok.

remove_duplicates_test() ->
    SampleValidators = [
    #validator{
        name = "name1",
        description = "description1",
        func = fun(X) -> X*3 end
    },
    #validator{
        name = "name1",
        description = "description1",
        func = fun(X) -> X*4 end
    }
    ],

    NewValidators = remove_duplicates(SampleValidators),
    [_|Expected] = SampleValidators,
    ?assertEqual(Expected, NewValidators),
    ok.

-endif.
