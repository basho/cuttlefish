%% -------------------------------------------------------------------
%%
%% cuttlefish_bytesize: complexity for parsing bytesizes
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

-module(cuttlefish_bytesize).

-define(KILOBYTE, 1024).
-define(MEGABYTE, 1048576).
-define(GIGABYTE, 1073741824).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([parse/1, to_string/1]).

%% @doc turns an integer of bytes into a string.
%% Will use the smallest unit to not lose precision.
%% e.g. 1024 -> 1kb, but 1025 = 1025.
-spec to_string(integer()) -> string().
to_string(Bytez) ->
    case { Bytez rem ?GIGABYTE, Bytez rem ?MEGABYTE, Bytez rem ?KILOBYTE} of
        {0, _, _} ->
            integer_to_list(Bytez div ?GIGABYTE) ++ "gb";
        {_, 0, _} ->
            integer_to_list(Bytez div ?MEGABYTE) ++ "mb";
        {_, _, 0} ->
            integer_to_list(Bytez div ?KILOBYTE) ++ "kb";
        _ ->
            integer_to_list(Bytez)
    end.

%% @doc the reverse of to_string/1. turns "1kb" into 1024.
-spec parse(string()) -> integer().
parse(String) ->
    try
        case lists:reverse(String) of
            [$b,$k|BSize] -> cuttlefish_util:numerify(lists:reverse(BSize)) * ?KILOBYTE;
            [$b,$m|BSize] -> cuttlefish_util:numerify(lists:reverse(BSize)) * ?MEGABYTE;
            [$b,$g|BSize] -> cuttlefish_util:numerify(lists:reverse(BSize)) * ?GIGABYTE;
            BSize -> cuttlefish_util:numerify(lists:reverse(BSize))
        end of
        Size -> Size
    catch
        _:_ -> error
    end.


-ifdef(TEST).
to_string_test() ->
    ?assertEqual("1kb", to_string(1024)),
    ?assertEqual("2kb", to_string(2048)),
    ?assertEqual("20", to_string(20)),
    ?assertEqual("10mb", to_string(10485760)),
    ok.

parse_test() ->
    ?assertEqual(1024, parse("1kb")),
    ?assertEqual(2048, parse("2kb")),
    ?assertEqual(20, parse("20")),
    ?assertEqual(10485760, parse("10mb")),
    ?assertEqual(error, parse("10mb10kb")),
    ok.

-endif.