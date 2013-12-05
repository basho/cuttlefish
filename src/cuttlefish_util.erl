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
    conf_get_value/3,
    replace_proplist_value/3,
    filter_by_variable_starts_with/2, variable_match_replace/2,
    fuzzy_variable_match/2,
    matches_for_variable_def/2, numerify/1,
    ceiling/1,
    levenshtein/2,
    print_error/1,
    print_error/2]).

%% @doc it's a wrapper for proplists:get_value, a convenience function
%% for schema writers to not have to use [] notation for varibales
-spec conf_get_value(string()|[string()], [{[string()], any()}]) -> any().
conf_get_value(Variable, ConfigProplist) ->
    conf_get_value(Variable, ConfigProplist, undefined).

-spec conf_get_value(string()|[string()], [{[string()], any()}], any()) -> any().
conf_get_value([H|_T]=Variable, ConfigProplist, Default) when is_list(H) ->
    proplists:get_value(Variable, ConfigProplist, Default);
conf_get_value(Variable, ConfigProplist, Default) ->
    conf_get_value(cuttlefish_variable:tokenize(Variable), ConfigProplist, Default).


%% @doc replace the element in a proplist
-spec replace_proplist_value(atom() | string(), any(), [{string(), any()}]) -> [{string(), any()}].
replace_proplist_value(Key, Value, Proplist) ->
    lists:keystore(Key, 1, Proplist, {Key, Value}).

%% @doc For Proplist, return the subset of the proplist that starts
%% with "Key"
-spec filter_by_variable_starts_with(string()|[string()], [{[string()], any()}]) -> [{[string()], any()}].
filter_by_variable_starts_with([H|_T]=Prefix, Proplist) when is_list(H) ->
    [ T || {Key,_}=T <- Proplist, lists:prefix(Prefix, Key) ];
filter_by_variable_starts_with(StringPrefix, Proplist) ->
    filter_by_variable_starts_with(cuttlefish_variable:tokenize(StringPrefix), Proplist).

%% @doc replaces the $var in Key with Sub
-spec variable_match_replace(cuttlefish_conf:variable(), string()) -> [string()].
variable_match_replace(Variable, Sub) ->
    [ begin
        case {H, Sub} of
            {$$, undefined} -> T;
            {$$, Sub} -> Sub;
            _ -> Tok
        end
    end || [H|T]=Tok <- Variable].

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

%% @doc given a KeyDef "a.b.$c.d", what are the possible values for $c
%% in the set of Keys in Conf = [{Key, Value}]?
-spec matches_for_variable_def(cuttlefish_conf:variable(), cuttlefish_conf:conf()) -> [{string(), any()}].
matches_for_variable_def(VariableDef, Conf) ->
    lists:foldl(
        fun({Variable, _}, Acc) ->
            case extract_first_match(VariableDef, Variable) of
                nomatch ->
                    Acc;
                [Match|_] ->
                    [Match|Acc]
            end
        end, [], Conf).

-spec extract_first_match(cuttlefish_conf:variable(),
                          cuttlefish_conf:variable()) ->
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

print_error(FormatString, Args) ->
    print_error(io_lib:format(FormatString, Args)).
print_error(String) ->
    case lager:error("~s", [String]) of
        {error, lager_not_running} ->
            io:format("~s~n", [String]),
            ok;
        ok -> ok
    end.

%% Levenshtein code by Adam Lindberg, Fredrik Svensson via
%% http://www.trapexit.org/String_similar_to_(Levenshtein)
%%
%%------------------------------------------------------------------------------
%% @spec levenshtein(StringA :: string(), StringB :: string()) -> integer()
%% @doc Calculates the Levenshtein distance between two strings
%% @end
%%------------------------------------------------------------------------------
levenshtein(Samestring, Samestring) -> 0;
levenshtein(String, []) -> length(String);
levenshtein([], String) -> length(String);
levenshtein(Source, Target) ->
    levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).

%% Recurses over every character in the source string and calculates a list of distances
levenshtein_rec([SrcHead|SrcTail], Target, DistList, Step) ->
    levenshtein_rec(SrcTail, Target, levenshtein_distlist(Target, DistList, SrcHead, [Step], Step), Step + 1);
levenshtein_rec([], _, DistList, _) ->
    lists:last(DistList).

%% Generates a distance list with distance values for every character in the target string
levenshtein_distlist([TargetHead|TargetTail], [DLH|DLT], SourceChar, NewDistList, LastDist) when length(DLT) > 0 ->
    Min = lists:min([LastDist + 1, hd(DLT) + 1, DLH + dif(TargetHead, SourceChar)]),
    levenshtein_distlist(TargetTail, DLT, SourceChar, NewDistList ++ [Min], Min);
levenshtein_distlist([], _, _, NewDistList, _) ->
    NewDistList.

% Calculates the difference between two characters or other values
dif(C, C) -> 0;
dif(_, _) -> 1.

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

tokenize_variable_key_test() ->
    ?assertEqual(["a", "b", "c", "d"], (cuttlefish_variable:tokenize("a.b.c.d"))),

    ?assertEqual(["a", "b.c", "d"], (cuttlefish_variable:tokenize("a.b\\.c.d"))),

    %% Covers GH #22
    ?assertEqual(
        ["listener", "http"],
         (cuttlefish_variable:tokenize("listener.http."))
    ),
    
    ok.

split_variable_on_match_test() ->
    ?assertEqual({["a", "b"], "$c", ["d", "e"]}, (cuttlefish_variable:split_variable_on_match(["a", "b", "$c", "d", "e"]))),
    ?assertEqual({["a", "b", "c", "d", "e"], [], []}, (cuttlefish_variable:split_variable_on_match(["a", "b", "c", "d", "e"]))),
    ?assertEqual({[], "$a", ["b", "c", "d", "e"]}, (cuttlefish_variable:split_variable_on_match(["$a", "b", "c", "d", "e"]))),
    ok.

variable_match_replace_test() ->
    ?assertEqual(["a", "b", "c"], variable_match_replace(["a", "b", "c"], "d")),
    ?assertEqual(["a", "b", "c"], variable_match_replace(["a", "b", "c"], "e")),
    ?assertEqual(["a", "b", "c"], variable_match_replace(["a", "b", "c"], "f")),
    ?assertEqual(["a", "b", "c"], variable_match_replace(["a", "b", "c"], "g")),
    ?assertEqual(["a", "g", "c"], variable_match_replace(["a", "$b", "c"], "g")),
    ?assertEqual(["a", "b", "c"], variable_match_replace(["a", "$b", "c"], undefined)),

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

levenshtein_test() ->
    ?assertEqual(0, levenshtein("X", "X")),
    ?assertEqual(1, levenshtein("X", "XX")),
    ?assertEqual(1, levenshtein("penguin", "penguino")),
    ?assertEqual(1, levenshtein("dtrace", "ctrace")),
    ?assertEqual(5, levenshtein("anti_entropy", "anti_entropy.tick")),
    ?assertEqual(1, levenshtein("anti_entropy", "anti-entropy")),
    ?assertEqual(4, levenshtein("", "four")),
    ?assertEqual(4, levenshtein("four", "")),
    ok.

print_error_test() ->
    case lager:error("Error") of
        {error, lager_not_running} ->
            ?assertEqual(ok, print_error("Error"));
        _ ->
            ?assert(fail)
    end,
    ok.

ceiling_test() ->
    ?assertEqual(9, ceiling(8.99999)),
    ?assertEqual(9, ceiling(8.00001)),
    ?assertEqual(9, ceiling(9.0)),
    ?assertEqual(-2, ceiling(-2.0000001)),
    ?assertEqual(-2, ceiling(-2.9999999)),
    ok.

-endif.
