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
    replace_proplist_value/3,
    filter_by_variable_starts_with/2,
    numerify/1,
    ceiling/1,
    levenshtein/2,
    print_error/1,
    print_error/2]).

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
