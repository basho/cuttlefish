%% -------------------------------------------------------------------
%%
%% cuttlefish_duration: complexity for parsing durations
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
-module(cuttlefish_duration).

-define(FORTNIGHT, 1209600000).
-define(WEEK,   604800000).
-define(DAY,    86400000).
-define(HOUR,   3600000).
-define(MINUTE, 60000).
-define(SECOND, 1000).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([parse/1, milliseconds/1, seconds/1]).

milliseconds(Millis) ->
    Units = lists:filter(fun({N, _Unit}) -> N =/= 0 end, [
        { Millis div ?WEEK,                "w"},
        {(Millis rem ?WEEK)   div ?DAY,    "d"},
        {(Millis rem ?DAY)    div ?HOUR,   "h"},
        {(Millis rem ?HOUR)   div ?MINUTE, "m"},
        {(Millis rem ?MINUTE) div ?SECOND, "s"},
        { Millis rem ?SECOND,              "ms"}
    ]),
    lists:flatten([ 
        integer_to_list(N) ++ Unit 
    || {N, Unit} <- Units]).

seconds(Seconds) ->
    milliseconds(Seconds * 1000).

parse(InputDurationString) ->
    DurationString = string:to_lower(InputDurationString),
    DurationTokens = tokens(DurationString),
    
    Millis = lists:sum([parse_token(T) || T <- DurationTokens]),
    round(Millis).

tokens(DurationString) ->
    {LastWorking, List} = lists:foldl(
        fun(Char, {Working, Acc}) ->
            case {length(string:tokens(Working, "1234567890.")), is_digit(Char)} of
                {0, _} ->
                    {Working ++ [Char], Acc};
                {_, true} ->
                    {[Char], [Working|Acc]};
                {_, false} ->
                    {Working ++ [Char], Acc}
            end
        end, 
        {[], []}, 
        DurationString),
    [LastWorking|List]. 

parse_token(T) -> parse_token_r(lists:reverse(T)).

parse_token_r([$s,$m|BackwardMillis]) ->
    MilliStr = lists:reverse(BackwardMillis),
    cuttlefish_util:numerify(MilliStr);
parse_token_r([$s|BackwardsSeconds]) ->
    SecondStr = lists:reverse(BackwardsSeconds),
    ?SECOND * cuttlefish_util:numerify(SecondStr);
parse_token_r([$m|BackwardsMinutes]) ->
    MinuteStr = lists:reverse(BackwardsMinutes),
    ?MINUTE * cuttlefish_util:numerify(MinuteStr);
parse_token_r([$h|BackwardsHours]) ->
    HourStr = lists:reverse(BackwardsHours),
    ?HOUR * cuttlefish_util:numerify(HourStr);
parse_token_r([$d|BackwardsDays]) ->
    DayStr = lists:reverse(BackwardsDays),
    cuttlefish_util:numerify(DayStr) * ?DAY;
parse_token_r([$w|BackwardsWeeks]) ->
    WeekStr = lists:reverse(BackwardsWeeks),
    cuttlefish_util:numerify(WeekStr) * ?WEEK;
parse_token_r([$f|BackwardsFortnights]) ->
    FortnightStr = lists:reverse(BackwardsFortnights),
    cuttlefish_util:numerify(FortnightStr) * ?FORTNIGHT;
parse_token_r(Error) ->
    {error, Error}.

is_digit(Char) -> 
    Char =:= $1 orelse
    Char =:= $2 orelse
    Char =:= $3 orelse
    Char =:= $4 orelse
    Char =:= $5 orelse
    Char =:= $6 orelse
    Char =:= $7 orelse
    Char =:= $8 orelse
    Char =:= $9 orelse
    Char =:= $0 orelse
    Char =:= $..
-ifdef(TEST).

milliseconds_test() ->
    ?assertEqual("500ms", milliseconds(500)),
    ?assertEqual("1s500ms", milliseconds(1500)),
    ?assertEqual("30m", milliseconds(1800000)),
    ?assertEqual("1w1d1h1m1s1ms", milliseconds(694861001)),
    ok.

seconds_test() ->
    ?assertEqual("50s", seconds(50)),
    ?assertEqual("1m1s", seconds(61)),
    ?assertEqual("30m", seconds(1800)),
    ?assertEqual("1w1d1h1m1s", seconds(694861)),
    ok.


parse_test() ->
    test_parse(500,  "500ms"),
    test_parse(500,  ".5s"),
    test_parse(1001, "1s1ms"),
    test_parse(1599, "1s599ms"),
    test_parse(1599, "1.599s"),
    test_parse(1600, "1.5999s"),
    test_parse(60000, "1m"),
    test_parse(60000, "60s"),
    test_parse(60000, "60000ms"),
    test_parse(90000, "1.5m"),
    test_parse(90000, "1m30s"),
    test_parse(90000, "1m29s1000ms"),
    test_parse(1800000, ".5h"),
    test_parse(3600000, "1h"),
    test_parse(3601000, "1h1s"),
    test_parse(3660000, "1h1m"),
    test_parse(3661000, "1h1m1s"),
    test_parse(3661001, "1h1m1s1ms"),
    test_parse(3600000, "60m"),
    test_parse(5400000, "90m"),
    test_parse(5401000, "90m1s"),
    test_parse(5401001, "90m1s1ms"),
    test_parse(5400000, "1h30m"),
    test_parse(5401000, "1h30m1s"),
    test_parse(3660000, "1h1m"),
    test_parse(86400000, "1d"),
    test_parse(86401000, "1d1s"),
    test_parse(86401001, "1d1s1ms"),
    test_parse(604800000, "1w"),
    test_parse(691200000, "1w1d"),
    test_parse(694800000, "1w1d1h"),
    test_parse(694860000, "1w1d1h1m"),
    test_parse(694861000, "1w1d1h1m1s"),
    test_parse(694861001, "1w1d1h1m1s1ms"),

    %% Weird but ok?
    test_parse(121001, "1m1s1ms1m"),
    
    %% Easter Egg
    test_parse(1904461001, "1f1w1d1h1m1s1ms"),
    ok.

test_parse(ExpectedMillis, StringToParse) ->
    ?assertEqual(ExpectedMillis, parse(StringToParse)).

-endif.