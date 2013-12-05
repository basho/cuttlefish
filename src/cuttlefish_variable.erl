%% -------------------------------------------------------------------
%%
%% cuttlefish_variable: handles both variable and variable definitions
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
-module(cuttlefish_variable).

-type variable() :: [string()].
-export_type([variable/0]).

-export([tokenize/1]).

-export([split_variable_on_match/1]).

%% @doc like string:tokens(Key, "."), but if the dot was escaped
%% i.e. \\., don't tokenize that
-spec tokenize(string()) ->  [string()].
tokenize(Key) ->
    tokenize(Key, "", []).

tokenize([$\\, $. |Rest], Part, Acc) ->
    tokenize(Rest, [$. |Part], Acc);
tokenize([$. |Rest], Part, Acc) ->
    tokenize(Rest, "", [lists:reverse(Part)|Acc]);
tokenize([], "", Acc) ->
    lists:reverse(Acc);
tokenize([], Part, Acc) ->
    lists:reverse([lists:reverse(Part)|Acc]);
tokenize([Char|Rest], Part, Acc) ->
    tokenize(Rest, [Char|Part], Acc).



%% @doc split a key definition into:
%% * Prefix: Things before the $var
%% * Var: The $var itself
%% * Suffix: Things after the $var
-spec split_variable_on_match([string()]) -> {[string()], string(), [string()]}.
split_variable_on_match(Variable) ->
    {PrefixToks, MatchGroup, SuffixToks} = lists:foldl(
        fun(T, {Prefix, MatchGroup, Suffix}) ->
            case {T, MatchGroup} of
                {[$$|_], []} -> {Prefix, T, Suffix};
                {_, []} -> {[T|Prefix], MatchGroup, Suffix};
                {_, _} -> {Prefix, MatchGroup, [T|Suffix]}
            end
        end,
        {[], [], []},
        Variable),
    {
        lists:reverse(PrefixToks),
        MatchGroup,
        lists:reverse(SuffixToks)
    }.