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

-opaque datatype() :: integer | string | enum | ip | duration | duration_secs | bytesize.
-export_type([datatype/0]).

-export([supported/0, from_string/2, to_string/2]).

supported() ->
    [
        integer,
        string,
        enum,
        ip,
        duration,
        duration_secs,
        bytesize
    ].

-spec to_string(term(), datatype()) -> string().
to_string(Integer, integer) when is_integer(Integer) -> integer_to_list(Integer);
to_string(Integer, integer) when is_list(Integer) -> Integer;

to_string({IP, Port}, ip) when is_list(IP), is_integer(Port) -> IP ++ ":" ++ integer_to_list(Port);
to_string(IPString, ip) when is_list(IPString) -> IPString;

to_string(Enum, enum) when is_list(Enum) -> Enum; 
to_string(Enum, enum) when is_atom(Enum) -> atom_to_list(Enum);

to_string(Duration, duration) when is_list(Duration) -> Duration;
to_string(Duration, duration) when is_integer(Duration) -> cuttlefish_duration:milliseconds(Duration);

to_string(Duration, duration_secs) when is_list(Duration) -> Duration;
to_string(Duration, duration_secs) when is_integer(Duration) -> cuttlefish_duration:seconds(Duration);

to_string(Bytesize, bytesize) when is_list(Bytesize) -> Bytesize;
to_string(Bytesize, bytesize) when is_integer(Bytesize) -> cuttlefish_bytesize:to_string(Bytesize);

to_string(String, string) when is_list(String) -> String;

%% The Pokemon Clause: Gotta Catch 'em all!
to_string(X, InvalidDatatype) ->
    lager:error("Tried to convert ~p, an invalid datatype ~p to_string.", [X, InvalidDatatype]),
    error. 

-spec from_string(term(), datatype()) -> term().
from_string(Atom, enum) when is_atom(Atom) -> Atom;
from_string(String, enum) -> list_to_atom(String);

from_string(Integer, integer) when is_integer(Integer) -> Integer; 
from_string(String, integer) when is_list(String) -> list_to_integer(String);

from_string({IP, Port}, ip) when is_list(IP), is_integer(Port) -> {IP, Port}; 
from_string(X, ip) ->
    Parts = string:tokens(X, ":"),
    [Port|BackwardsIP] = lists:reverse(Parts),
    {string:join(lists:reverse(BackwardsIP), ":"), list_to_integer(Port)};

from_string(Duration, duration) when is_integer(Duration) -> Duration;
from_string(Duration, duration) when is_list(Duration) -> cuttlefish_duration:parse(Duration); 

from_string(Duration, duration_secs) when is_integer(Duration) -> Duration;
from_string(Duration, duration_secs) when is_list(Duration) -> ceiling(cuttlefish_duration:parse(Duration) / 1000); 

from_string(Bytesize, bytesize) when is_integer(Bytesize) -> Bytesize;
from_string(Bytesize, bytesize) when is_list(Bytesize) -> cuttlefish_bytesize:parse(Bytesize); 

from_string(String, string) when is_list(String) -> String;
from_string(Thing, InvalidDatatype) ->
   lager:error("Tried to convert ~p, an invalid datatype ~p from_string.", [Thing, InvalidDatatype]),
   error.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

-ifdef(TEST).

to_string_integer_test() ->
    ?assertEqual("32", to_string(32, integer)),
    ?assertEqual("32", to_string(32, integer)).

to_string_ip_test() ->
    ?assertEqual("127.0.0.1:8098", to_string("127.0.0.1:8098", ip)),
    ?assertEqual("127.0.0.1:8098", to_string({"127.0.0.1", 8098}, ip)).

to_string_enum_test() ->
    ?assertEqual("true", to_string("true", enum)),
    ?assertEqual("true", to_string(true, enum)).

to_string_string_test() ->
    ?assertEqual("string", to_string("string", string)).

from_string_integer_test() ->
    ?assertEqual(32, from_string(32, integer)),
    ?assertEqual(32, from_string("32", integer)).

from_string_ip_test() ->
    ?assertEqual({"127.0.0.1", 8098}, from_string("127.0.0.1:8098", ip)),
    ?assertEqual(
        {"2001:0db8:85a3:0042:1000:8a2e:0370:7334", 8098}, 
        from_string("2001:0db8:85a3:0042:1000:8a2e:0370:7334:8098", ip)),
    ok.

from_string_enum_test() ->
    ?assertEqual(true, from_string("true", enum)),
    ?assertEqual(true, from_string(true, enum)).

from_string_duration_test() ->
    %% more examples in the the cuttlefish_duration tests
    ?assertEqual(1100, from_string("1s100ms", duration)),
    ?assertEqual(1100, from_string(1100, duration)),
    ok.

from_string_duration_secs_test() ->
    %% more examples in the the cuttlefish_duration tests
    %% also rounds up for smaller units
    ?assertEqual(2, from_string("1s100ms", duration_secs)),
    ?assertEqual(2, from_string(2, duration_secs)),
    ok.

from_string_string_test() ->
    ?assertEqual("string", from_string("string", string)).

-endif.