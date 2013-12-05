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

-export([
	 tokenize/1,
	 split_on_match/1,
	 replace_match/2,
	 extract_first_match/2,
	 fuzzy_matches/2,
	 is_fuzzy_match/2]).

%% @doc like string:tokens(Key, "."), but if the dot was escaped
%% i.e. \\., don't tokenize that
-spec tokenize(string()) ->  variable().
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
-spec split_on_match(variable()) ->  {variable(), string(), variable()}.
split_on_match(Variable) ->
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

%% @doc replaces the $var in Key with Sub
-spec replace_match(variable(), string()) -> variable().
replace_match(Variable, Sub) ->
    [ begin
        case {H, Sub} of
            {$$, undefined} -> T;
            {$$, Sub} -> Sub;
            _ -> Tok
        end
    end || [H|T]=Tok <- Variable].

-spec extract_first_match(variable(), variable()) ->
                          nomatch  | [{string(), string()}].
%% If the lengths are equal, try to pair up a fuzzy segment with its match.
extract_first_match(VariableDef, Variable) when length(VariableDef) == length(Variable) ->
    extract_first_match(VariableDef, Variable, nomatch);
%% This could never match because they are different lengths.
extract_first_match(_,_) -> nomatch.

%% We have a perfect match, or no match at all, so return the result.
extract_first_match([], [], Result) when is_list(Result) ->
    %% If the Result is 'nomatch', the last function clause will be
    %% the only one that matches.
    lists:reverse(Result);
%% We found the first fuzzy segment, grab the binding of the segment.
extract_first_match([[$$|_]=Fuzzy|VariableDef], [Value|Variable], nomatch) ->
    extract_first_match(VariableDef, Variable, [{Fuzzy, Value}]);
%% We found a fuzzy segment and already have a match, so just recurse.
extract_first_match([[$$|_]=Fuzzy|VariableDef], [Value|Variable], Result) ->
    extract_first_match(VariableDef, Variable, [{Fuzzy, Value}|Result]);
%% We found two segments that are static and equal.
extract_first_match([X|VariableDef], [X|Variable], Result) ->
    extract_first_match(VariableDef, Variable, Result);
%% Something else happened, so we don't match!
extract_first_match(_,_,_) -> nomatch.

%% @doc given a KeyDef "a.b.$c.d", what are the possible values for $c
%% in the set of Keys in Conf = [{Key, Value}]?
-spec fuzzy_matches(variable(), cuttlefish_conf:conf()) -> 
                       [{string(), any()}].
fuzzy_matches(VariableDef, Conf) ->
    lists:foldl(
        fun({Variable, _}, Acc) ->
            case extract_first_match(VariableDef, Variable) of
                nomatch ->
                    Acc;
                [Match|_] ->
                    [Match|Acc]
            end
        end, [], Conf).

%% @doc could this fixed Key be a match for the variable key KeyDef?
%% e.g. could a.b.$var.d =:= a.b.c.d?
-spec is_fuzzy_match(variable(), variable()) ->  boolean().
is_fuzzy_match(Variable, VariableDef) ->
    case length(Variable) =:= length(VariableDef) of
        true ->
            Zipped = lists:zip(Variable, VariableDef),
            lists:all(
                fun({X,Y}) ->
                    X =:= Y orelse hd(Y) =:= $$
                end,
                Zipped);
        _ -> false
    end.
