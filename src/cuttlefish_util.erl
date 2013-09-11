%% -------------------------------------------------------------------
%%
%% cuttlefish_util: various cuttlefish utility functions
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
-module(cuttlefish_util).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([
    conf_get_value/2,
    replace_proplist_value/3,
    variable_starts_with/2,
    filter_by_variable_starts_with/2,
    split_variable_on_match/1,
    variable_match_replace/2,
    fuzzy_variable_match/2,
    matches_for_variable_def/2,
    tokenize_variable_key/1,
    numerify/1,
    ceiling/1]).

%% @doc it's a wrapper for proplists:get_value, a convenience function
%% for schema writers to not have to use [] notation for varibales
-spec conf_get_value(string()|[string()], [{[string()], any()}]) -> any().
conf_get_value([H|_T]=Variable, ConfigProplist) when is_list(H) ->
    proplists:get_value(Variable, ConfigProplist);
conf_get_value(Variable, ConfigProplist) ->
    conf_get_value(tokenize_variable_key(Variable), ConfigProplist). 

%% @doc replace the element in a proplist
-spec replace_proplist_value(atom() | string(), any(), [{string(), any()}]) -> [{string(), any()}].
replace_proplist_value(Key, Value, Proplist) ->
    proplists:delete(Key, Proplist) ++ [{Key, Value}].

variable_starts_with([HPrefix|Variable], [HPrefix|TPrefix]) ->
    variable_starts_with(Variable, TPrefix);
variable_starts_with(_Variable, []) ->
    true;
variable_starts_with(_,_) -> false.

%% @doc For Proplist, return the subset of the proplist that starts
%% with "Key"
-spec filter_by_variable_starts_with(string()|[string()], [{[string()], any()}]) -> [{[string()], any()}]. 
filter_by_variable_starts_with([H|_T]=Prefix, Proplist) when is_list(H) ->
    [ T || {Key,_}=T <- Proplist, lists:prefix(Prefix, Key) ];
filter_by_variable_starts_with(StringPrefix, Proplist) ->
    filter_by_variable_starts_with(tokenize_variable_key(StringPrefix), Proplist).

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

%% @doc replaces the $var in Key with Sub
-spec variable_match_replace([string()], string()) -> [string()].
variable_match_replace(Variable, Sub) ->
    [ begin 
        case {hd(Tok), Sub} of
            {$$, undefined} -> Tok;
            {$$, Sub} -> Sub;
            _ -> Tok
        end
    end || Tok <- Variable]. 

%% @doc could this fixed Key be a match for the variable key KeyDef?
%% e.g. could a.b.$var.d =:= a.b.c.d? 
-spec fuzzy_variable_match([string()], [string()]) -> boolean().
fuzzy_variable_match(Variable, VariableDef) ->
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

%% @doc like string:tokens(Key, "."), but if the dot was escaped 
%% i.e. \\., don't tokenize that
-spec tokenize_variable_key(string()) -> [string()].
tokenize_variable_key(Key) ->
    tokenize_variable_key(Key, "", []).

tokenize_variable_key([$\\, $.|Rest], Part, Acc) ->
    tokenize_variable_key(Rest, [$.|Part], Acc);
tokenize_variable_key([$.|Rest], Part, Acc) ->
    tokenize_variable_key(Rest, "", [lists:reverse(Part)|Acc]);
tokenize_variable_key([], Part, Acc) ->
    lists:reverse([lists:reverse(Part)|Acc]);
tokenize_variable_key([Char|Rest], Part, Acc) ->
    tokenize_variable_key(Rest, [Char|Part], Acc).

%% @doc given a KeyDef "a.b.$c.d", what are the possible values for $c
%% in the set of Keys in Conf = [{Key, Value}]?
-spec matches_for_variable_def([string()], [{[string()], any()}]) -> [string()].
matches_for_variable_def(VariableDef, Conf) ->
    lists:foldl(
        fun({Variable, _}, Acc) ->
            case length(Variable) =:= length(VariableDef) of
                true ->
                    Zipped = lists:zip(VariableDef, Variable),
                    Match = lists:all(
                        fun({X,Y}) ->
                            X =:= Y orelse hd(X) =:= $$
                        end,
                        Zipped),
                    case Match of
                        true ->
                            Matches = lists:filter(fun({X,Y}) ->
                                X =/= Y andalso hd(X) =:= $$
                            end, Zipped),
                            case length(Matches) > 0 of
                                true -> 
                                    [hd(Matches)|Acc];
                                _ -> Acc
                            end;
                        _ -> Acc
                    end;

                _ -> Acc
            end
        end, [], Conf). 

    
%% @doc turn a string into a number in a way I am happy with
-spec numerify(string()) -> integer()|float()|{error, string()}.
numerify([$.|_]=Num) -> numerify([$0|Num]);
numerify(String) ->
    try list_to_float(String) of
        Float -> Float
    catch
        _:_ ->
            try list_to_integer(String) of
                Int -> Int
            catch
                _:_ ->
                    {error, String}
            end
    end.

%% @doc remember when you learned about decimal places. about a minute
%% later, you learned about rounding up and down. This is rounding up.
-spec ceiling(float()) -> integer().
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

-ifdef(TEST).

replace_proplist_value_test() ->
    Proplist = [
        {"test1", 1},
        {"test2", 2},
        {"test3", 3}
    ],

    NewProplist = replace_proplist_value("test2", 8, Proplist),
    ?assertEqual(
        8,
        proplists:get_value("test2", NewProplist) 
        ),
    ok.

replace_proplist_value_when_undefined_test() ->
    Proplist = [
        {"test1", 1},
        {"test2", 2}
    ],

    NewProplist = replace_proplist_value("test3", 3, Proplist),
        ?assertEqual(
        3,
        proplists:get_value("test3", NewProplist) 
        ),
    ok.

variable_starts_with_test() ->
    ?assert(variable_starts_with([], [])),
    ?assert(variable_starts_with(["a","b","c"], ["a", "b"])),
    ?assert(not(variable_starts_with(["a", "b", "c"], ["a", "q"]))),
    ok.

filter_by_variable_starts_with_test() ->
    Proplist = [
        {["regular","key"], 1},
        {["other","normal","key"], 2},
        {["prefixed","key1"], 3},
        {["prefixed","key2"], 4},
        {["interleaved","key"], 5},
        {["prefixed","key3"], 6}
    ],

    FilteredByList = filter_by_variable_starts_with(["prefixed"], Proplist),
    ?assertEqual([
            {["prefixed","key1"], 3},
            {["prefixed","key2"], 4},
            {["prefixed","key3"], 6}
        ],
        FilteredByList),

    FilteredByString = filter_by_variable_starts_with("prefixed", Proplist),
    ?assertEqual([
            {["prefixed","key1"], 3},
            {["prefixed","key2"], 4},
            {["prefixed","key3"], 6}
        ],
        FilteredByString),
    ok.

split_variable_on_match_test() ->
    ?assertEqual({["a", "b"], "$c", ["d", "e"]}, split_variable_on_match(["a", "b", "$c", "d", "e"])),
    ?assertEqual({["a", "b", "c", "d", "e"], [], []}, split_variable_on_match(["a", "b", "c", "d", "e"])),
    ?assertEqual({[], "$a", ["b", "c", "d", "e"]}, split_variable_on_match(["$a", "b", "c", "d", "e"])),
    ok.

variable_match_replace_test() ->
    ?assertEqual(["a", "b", "c"], variable_match_replace(["a", "b", "c"], "d")),
    ?assertEqual(["a", "b", "c"], variable_match_replace(["a", "b", "c"], "e")),
    ?assertEqual(["a", "b", "c"], variable_match_replace(["a", "b", "c"], "f")),
    ?assertEqual(["a", "b", "c"], variable_match_replace(["a", "b", "c"], "g")),
    ?assertEqual(["a", "g", "c"], variable_match_replace(["a", "$b", "c"], "g")),
    ok.

fuzzy_variable_match_test() ->
    ?assert(fuzzy_variable_match(["alpha","bravo","charlie","delta"], ["alpha","bravo","charlie","delta"])),
    ?assert(fuzzy_variable_match(["alpha","bravo","anything","delta"], ["alpha","bravo","$charlie","delta"])),
    ?assertNot(fuzzy_variable_match(["alpha","bravo.anything","delta"], ["alpha","bravo","charlie","delta"])),
    ?assert(fuzzy_variable_match(["alpha","bravo","any.thing","delta"], ["alpha","bravo","$charlie","delta"])),
    ?assert(fuzzy_variable_match(["alpha","bravo","any.thing.you.need","delta"], ["alpha","bravo","$charlie","delta"])),
    ok.

matches_for_variable_def_test() ->
    Conf = [
        {["multi_backend","backend1","storage_backend"], 1},
        {["multi_backend","backend2","storage_backend"], 2},
        {["multi_backend","backend.3","storage_backend"], 3},
        {["multi_backend","backend4","storage_backend"], 4}
    ],

    Vars = proplists:get_all_values("$name", 
            matches_for_variable_def(["multi_backend","$name","storage_backend"], Conf)
    ),
    
    ?assertEqual(4, length(Vars)),
    ?assert(lists:member("backend1", Vars)),
    ?assert(lists:member("backend2", Vars)),
    ?assert(lists:member("backend.3", Vars)),
    ?assert(lists:member("backend4", Vars)),
    ?assertEqual(4, length(Vars)),

    ok.

-endif.
