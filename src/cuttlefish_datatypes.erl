%% -------------------------------------------------------------------
%%
%% cuttlefish_datatypes: handles datatypes in cuttlefish schemas
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
-module(cuttlefish_datatypes).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-type datatype() :: integer |
                    string |
                    file |
                    directory |
                    flag |
                    {flag, atom(), atom()} |
                    {flag, {atom(), term()}, {atom(), term()}} |
                    {enum, [atom()]} |
                    ip |
                    {duration, cuttlefish_duration:time_unit() } |
                    bytesize.
-type extended() :: { integer, integer() } |
                    { string, string() } |
                    { file, file:filename() } |
                    { directory, file:filename() } |
                    { atom, atom() } |
                    { ip, { string(), integer() } } |
                    { {duration, cuttlefish_duration:time_unit() }, string() } |
                    { bytesize, string() }.
-type datatype_list() :: [ datatype() | extended() ].

-export_type([datatype/0, extended/0, datatype_list/0]).

-export([
         is_supported/1,
         is_extended/1,
         is_valid_list/1,
         from_string/2,
         to_string/2
]).

-spec is_supported(any()) -> boolean().
is_supported(integer) -> true;
is_supported(string) -> true;
is_supported(file) -> true;
is_supported(directory) -> true;
is_supported(flag) -> true;
is_supported({flag, On, Off}) when is_atom(On), is_atom(Off) -> true;
is_supported({flag, {On, _}, {Off, _}}) when is_atom(On), is_atom(Off) -> true;
is_supported(atom) -> true;
is_supported({enum, E}) when is_list(E) -> true;
is_supported(ip) -> true;
is_supported({duration, f}) -> true;
is_supported({duration, w}) -> true;
is_supported({duration, d}) -> true;
is_supported({duration, h}) -> true;
is_supported({duration, m}) -> true;
is_supported({duration, s}) -> true;
is_supported({duration, ms}) -> true;
is_supported(bytesize) -> true;
is_supported(_) -> false.

-spec is_extended(any()) -> boolean().
is_extended({integer, I}) when is_integer(I) -> true;
is_extended({string, S}) when is_list(S) -> true;
is_extended({atom, A}) when is_atom(A) -> true;
is_extended({file, F}) when is_list(F) -> true;
is_extended({directory, D}) when is_list(D) -> true;
is_extended({ip, {IP, Port}}) when is_list(IP) andalso is_integer(Port) -> true;
is_extended({{duration, f}, D}) when is_list(D) -> true;
is_extended({{duration, w}, D}) when is_list(D) -> true;
is_extended({{duration, d}, D}) when is_list(D) -> true;
is_extended({{duration, g}, D}) when is_list(D) -> true;
is_extended({{duration, m}, D}) when is_list(D) -> true;
is_extended({{duration, s}, D}) when is_list(D) -> true;
is_extended({{duration, ms}, D}) when is_list(D) -> true;
is_extended({bytesize, B}) when is_list(B) -> true;
is_extended(_) -> false.

-spec is_valid_list(any()) -> boolean().
is_valid_list(NotList) when not is_list(NotList) ->
    false;
is_valid_list([]) -> false;
is_valid_list(List) ->
    lists:all(fun(X) ->
                  is_supported(X)
                      orelse
                  is_extended(X)
              end, List).

-spec to_string(term(), datatype()) -> string() | cuttlefish_error:error().
to_string(Atom, atom) when is_list(Atom) -> Atom;
to_string(Atom, atom) when is_atom(Atom) -> atom_to_list(Atom);

to_string(Integer, integer) when is_integer(Integer) -> integer_to_list(Integer);
to_string(Integer, integer) when is_list(Integer) -> Integer;

to_string({IP, Port}, ip) when is_list(IP), is_integer(Port) -> IP ++ ":" ++ integer_to_list(Port);
to_string(IPString, ip) when is_list(IPString) -> IPString;

to_string(Enum, {enum, _}) when is_list(Enum) -> Enum;
to_string(Enum, {enum, _}) when is_atom(Enum) -> atom_to_list(Enum);

to_string(Duration, {duration, _}) when is_list(Duration) -> Duration;
to_string(Duration, {duration, Unit}) when is_integer(Duration) -> cuttlefish_duration:to_string(Duration, Unit);

to_string(Bytesize, bytesize) when is_list(Bytesize) -> Bytesize;
to_string(Bytesize, bytesize) when is_integer(Bytesize) -> cuttlefish_bytesize:to_string(Bytesize);

to_string(String, string) when is_list(String) -> String;

to_string(File, file) when is_list(File) -> File;

to_string(Directory, directory) when is_list(Directory) -> Directory;

to_string(Flag, flag) when is_atom(Flag) -> cuttlefish_flag:to_string(Flag, flag);
to_string(Flag, {flag, _, _}=Type) -> cuttlefish_flag:to_string(Flag, Type);

%% The Pokemon Clause: Gotta Catch 'em all!
to_string(X, InvalidDatatype) ->
    {error, lists:flatten(io_lib:format("Tried to convert ~p, an invalid datatype ~p to_string.", [X, InvalidDatatype]))}.

-spec from_string(term(), datatype()) -> term() | cuttlefish_error:error().
from_string(Atom, atom) when is_atom(Atom) -> Atom;
from_string(String, atom) -> list_to_atom(String);

from_string(Atom, {enum, Enum}) when is_atom(Atom) ->
    case lists:member(Atom, Enum) of
        true -> Atom;
        _ -> {error, lists:flatten(io_lib:format("~p is not a valid enum value, acceptable values are ~p.", [Atom, Enum]))}
    end;
from_string(String, {enum, Enum}) -> from_string(list_to_atom(String), {enum, Enum});

from_string(Integer, integer) when is_integer(Integer) -> Integer;
from_string(String, integer) when is_list(String) ->
    try list_to_integer(String) of
        X -> X
    catch
        _:_ -> {error, lists:flatten(io_lib:format("~p can't be converted to an integer", [String]))}
    end;

from_string({IP, Port}, ip) when is_list(IP), is_integer(Port) -> {IP, Port};
from_string(String, ip) ->
    try begin
        Parts = string:tokens(String, ":"),
        [Port|BackwardsIP] = lists:reverse(Parts),
        {string:join(lists:reverse(BackwardsIP), ":"), list_to_integer(Port)}
    end of
        X -> X
    catch
        _:_ -> {error, lists:flatten(io_lib:format("~p cannot be converted into an IP", [String]))}
    end;

from_string(Duration, {duration, _}) when is_integer(Duration) -> Duration;
from_string(Duration, {duration, Unit}) when is_list(Duration) -> cuttlefish_duration:parse(Duration, Unit);

from_string(Bytesize, bytesize) when is_integer(Bytesize) -> Bytesize;
from_string(Bytesize, bytesize) when is_list(Bytesize) -> cuttlefish_bytesize:parse(Bytesize);

from_string(String, string) when is_list(String) -> String;

from_string(File, file) when is_list(File) -> File;

from_string(Directory, directory) when is_list(Directory) -> Directory;

from_string(Flag, flag) when is_list(Flag) -> cuttlefish_flag:parse(Flag);
from_string(Flag, {flag, _, _}=Type) when is_list(Flag) -> cuttlefish_flag:parse(Flag, Type);

from_string(Thing, InvalidDatatype) ->
   {error, lists:flatten(io_lib:format("Tried to convert ~p, an invalid datatype ~p from_string.", [Thing, InvalidDatatype]))}.

-ifdef(TEST).

to_string_atom_test() ->
    ?assertEqual("split_the", to_string(split_the, atom)),
    ?assertEqual("split_the", to_string("split_the", atom)).

to_string_integer_test() ->
    ?assertEqual("32", to_string(32, integer)),
    ?assertEqual("32", to_string("32", integer)).

to_string_ip_test() ->
    ?assertEqual("127.0.0.1:8098", to_string("127.0.0.1:8098", ip)),
    ?assertEqual("127.0.0.1:8098", to_string({"127.0.0.1", 8098}, ip)).

to_string_enum_test() ->
    ?assertEqual("true", to_string("true", {enum, [true, false]})),
    ?assertEqual("true", to_string(true, {enum, [true, false]})).

to_string_string_test() ->
    ?assertEqual("string", to_string("string", string)).

to_string_duration_test() ->
    ?assertEqual("1w", to_string("1w", {duration, s})),
    ?assertEqual("1w", to_string(604800000, {duration, ms})).

to_string_bytesize_test() ->
    ?assertEqual("1GB", to_string(1073741824, bytesize)),
    ?assertEqual("1GB", to_string("1GB", bytesize)).

to_string_unsupported_datatype_test() ->
    ?assertEqual({error, "Tried to convert \"Something\", an invalid datatype unsupported_datatype to_string."}, to_string("Something", unsupported_datatype)).

from_string_atom_test() ->
    ?assertEqual(split_the, from_string(split_the, atom)),
    ?assertEqual(split_the, from_string("split_the", atom)).

from_string_integer_test() ->
    ?assertEqual(32, from_string(32, integer)),
    ?assertEqual(32, from_string("32", integer)),
    ?assertEqual({error, "\"thirty_two\" can't be converted to an integer"}, from_string("thirty_two", integer)),
    ok.

from_string_ip_test() ->
    ?assertEqual({"127.0.0.1", 8098}, from_string("127.0.0.1:8098", ip)),
    ?assertEqual(
        {"2001:0db8:85a3:0042:1000:8a2e:0370:7334", 8098},
        from_string("2001:0db8:85a3:0042:1000:8a2e:0370:7334:8098", ip)),

    ?assertEqual(
        {error, "\"This is not an IP\" cannot be converted into an IP"},
        from_string("This is not an IP", ip)
        ),
    ok.

from_string_enum_test() ->
    ?assertEqual({error, "a is not a valid enum value, acceptable values are [b,c]."}, from_string(a, {enum, [b, c]})),
    ?assertEqual(true, from_string("true", {enum, [true, false]})),
    ?assertEqual(true, from_string(true, {enum, [true, false]})).

from_string_duration_test() ->
    %% more examples in the the cuttlefish_duration tests
    ?assertEqual(1100, from_string("1s100ms", {duration, ms})),
    ?assertEqual(1100, from_string(1100, {duration, ms})),
    ok.

from_string_duration_secs_test() ->
    %% more examples in the the cuttlefish_duration tests
    %% also rounds up for smaller units
    ?assertEqual(2, from_string("1s100ms", {duration, s})),
    ?assertEqual(2, from_string(2, {duration, s})),
    ok.

from_string_string_test() ->
    ?assertEqual("string", from_string("string", string)).

from_string_unsupported_datatype_test() ->
    ?assertEqual({error, "Tried to convert \"string\", an invalid datatype unsupported_datatype from_string."}, from_string("string", unsupported_datatype)).

is_supported_test() ->
    ?assert(is_supported(integer)),
    ?assert(is_supported(string)),
    ?assert(is_supported(atom)),
    ?assert(is_supported(file)),
    ?assert(is_supported(directory)),
    ?assert(is_supported({enum, [one, two, three]})),
    ?assert(not(is_supported({enum, not_a_list}))),
    ?assert(is_supported(ip)),
    ?assert(is_supported({duration, f})),
    ?assert(is_supported({duration, w})),
    ?assert(is_supported({duration, d})),
    ?assert(is_supported({duration, h})),
    ?assert(is_supported({duration, m})),
    ?assert(is_supported({duration, s})),
    ?assert(is_supported({duration, ms})),
    ?assert(is_supported(bytesize)),
    ?assert(not(is_supported(some_unsupported_type))),
    ok.

is_extended_test() ->
    ?assertEqual(true, is_extended({integer, 10})),
    ?assertEqual(true, is_extended({integer, -10})),
    ?assertEqual(false, is_extended({integer, "ten"})),

    ?assertEqual(true, is_extended({string, "string"})),
    ?assertEqual(false, is_extended({string, string})),
    ?assertEqual(false, is_extended({string, 10})),

    ?assertEqual(true, is_extended({atom, atom})),
    ?assertEqual(false, is_extended({atom, "atom"})),
    ?assertEqual(false, is_extended({atom, 10})),

    ?assertEqual(true, is_extended({file, "/tmp/foo.txt"})),
    ?assertEqual(true, is_extended({file, ""})),
    ?assertEqual(false, is_extended({file, this})),

    ?assertEqual(true, is_extended({directory, "/tmp/foo.txt"})),
    ?assertEqual(true, is_extended({directory, ""})),
    ?assertEqual(false, is_extended({directory, this})),

    ?assertEqual(true, is_extended({ip, {"1.2.3.4", 1234}})),
    ?assertEqual(false, is_extended({ip, {1234, 1234}})),
    ?assertEqual(false, is_extended({ip, {"1.2.3.4", "1234"}})),

    ?assertEqual(true, is_extended({{duration, f}, "10f"})),
    ?assertEqual(true, is_extended({{duration, w}, "10f"})),
    ?assertEqual(true, is_extended({{duration, d}, "10f"})),
    ?assertEqual(true, is_extended({{duration, g}, "10f"})),
    ?assertEqual(true, is_extended({{duration, m}, "10f"})),
    ?assertEqual(true, is_extended({{duration, s}, "10f"})),
    ?assertEqual(true, is_extended({{duration, ms}, "10ms"})),
    ?assertEqual(true, is_extended({bytesize, "10GB"})),

    ok.

-endif.
