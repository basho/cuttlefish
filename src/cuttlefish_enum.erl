%% -------------------------------------------------------------------
%%
%% cuttlefish_enum: datatype for simple enum settings with
%%   customizable names and values
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
-module(cuttlefish_enum).

-type enum() :: {enum, [{atom()|string(), term()}]}.
-type strict_enum() :: {enum, [{string(), term()}]}.
-export_type([enum/0, strict_enum/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([
         to_string/2,
         parse/2
]).

-define(FMT(F, A), lists:flatten(io_lib:format(F, A))).

-spec to_string(string() | atom(), enum()
           ) -> string() | cuttlefish_error:error().
to_string(Value, {enum, RawEnum}) ->
    FriendlEnum = assuage_enum(RawEnum),
    case to_string_by_value(Value, {enum, FriendlEnum}) of
        {K, _} -> atom_to_list_maybe(K);
        false ->
            to_string_by_key(
                atom_to_list_maybe(Value),
                {enum, FriendlEnum})
    end.

-spec to_string_by_value(term(), strict_enum()) -> term().
to_string_by_value(Value, {enum, FriendlEnum}) ->
    lists:keyfind(Value, 2, FriendlEnum).

-spec to_string_by_key(atom() | string(), strict_enum()) -> string().
to_string_by_key(Key, {enum, FriendlEnum}) ->
    case lists:keyfind(Key, 1, FriendlEnum) of
        {K, _} -> K;
        false ->
            to_error(Key, {enum, FriendlEnum})
    end.

-spec parse(term(), enum()) -> term() | cuttlefish_error:error().
parse(Value, {enum, RawEnum}) ->
    FriendlEnum = assuage_enum(RawEnum),
    case parse_by_key( atom_to_list_maybe(Value), {enum, FriendlEnum}) of
        {_Key, Val} ->
            Val;
        false ->
            parse_by_value(Value, {enum, FriendlEnum})
    end.

-spec parse_by_key(atom() | string() | term(), strict_enum()) ->
    {string(), term()} | false.
parse_by_key(Key, {enum, FriendlEnum}) ->
    lists:keyfind(Key, 1, FriendlEnum).

-spec parse_by_value(term(), strict_enum()) -> term() | cuttlefish_error:error().
parse_by_value(Value, {enum, FriendlEnum}) ->
    case lists:keyfind(Value, 2, FriendlEnum) of
        false ->
            to_error(Value, {enum, FriendlEnum});
        {_Key, Value} ->
            Value
    end.

-spec to_error(atom() | string()| term(), strict_enum()) -> cuttlefish_error:error().
to_error(Value, {enum, FriendlEnum}) ->
    Acceptable = [Key || {Key, _} <- FriendlEnum],
    {error,
     ?FMT("~p is not a valid enum value, acceptable values are ~p.",
          [atom_to_list_maybe(Value), Acceptable])}.

-spec atom_to_list_maybe(term()) -> term().
atom_to_list_maybe(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
atom_to_list_maybe(Other) -> Other.

assuage_enum(Enum) ->
    assuage_enum(Enum, []).
assuage_enum([], FriendlEnum) ->
    lists:reverse(FriendlEnum);
%% If the head is a 2-tuple; yay!
assuage_enum([{Key, Value}|EnumTail], FriendlEnum) when is_atom(Key) ->
    assuage_enum(EnumTail, [ { cuttlefish_datatypes:to_string(Key, atom), Value } | FriendlEnum ]);
assuage_enum([{Key, Value}|EnumTail], FriendlEnum) when is_list(Key) ->
    assuage_enum(EnumTail, [ { Key, Value } | FriendlEnum ]);
%% If the head is just a string or atom, fall here.
assuage_enum([Key|EnumTail], FriendlEnum) when is_atom(Key) ->
    assuage_enum(EnumTail, [
            { cuttlefish_datatypes:to_string(Key, atom), Key } | FriendlEnum]);
assuage_enum([Key|EnumTail], FriendlEnum) when is_list(Key) ->
    assuage_enum(EnumTail, [{Key, Key} | FriendlEnum]);
assuage_enum([BadTuple|_], _) when is_tuple(BadTuple) ->
    {error, ?FMT("An enum element's tuple must be a 2-tuple and the first element must be an atom or a string. The value was: ~w", [BadTuple])};
assuage_enum([ErroneousItem|_], _) ->
    {error, ?FMT("An enum element needs to be a 2-tuple, a string, or an atom, but was: ~w", [ErroneousItem])}.

-ifdef(TEST).

parse_test() ->
    ?assertEqual(1, parse("one", {enum, [{"one", 1}, two]})),
    ?assertEqual(two, parse("two", {enum, [{"one", 1}, two]})),
    ok.

assuage_enum_test() ->
    ?assertEqual([{"true", true}, {"false", false}],
                 assuage_enum([true, false])),
    ?assertEqual([{"true", "true"}, {"false", "false"}],
                 assuage_enum(["true", "false"])),
    ?assertEqual([{"one", 1}, {"two", 2}],
                 assuage_enum([{one, 1}, {"two", 2}])),
    ?assertEqual([{"off", off}, {"on", "On"}],
                 assuage_enum([off, {on, "On"}])),
    ok.

assuage_enum_error_test() ->
    ?assertEqual(
       {error, "An enum element's tuple must be a 2-tuple and the first element must be an atom or a string. The value was: {one,two,three}"},
       assuage_enum([{one, two, three}, oops])
    ),
    ?assertEqual(
       {error, "An enum element needs to be a 2-tuple, a string, or an atom, but was: 7"},
       assuage_enum([oops, 7])
    ),
    ok.

-endif.
