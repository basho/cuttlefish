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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([
         to_string/2,
         parse/2
]).

-define(FMT(F, A), lists:flatten(io_lib:format(F, A))).

to_string(Atom, {enum, _}) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Value, {enum, _}) ->
    Value.

-spec parse(string() | atom(),
            {enum, [ atom() | string() | {atom()|string(), term()} ]}
           ) -> term() | cuttlefish_error:error().
parse(AtomValue, Enum) when is_atom(AtomValue) ->
    parse(atom_to_list(AtomValue), Enum);
parse(StrValue, {enum, RawEnum}) ->
    FriendlEnum = assuage_enum(RawEnum),
    case lists:keyfind(StrValue, 1, FriendlEnum) of
        false ->
            Acceptable = [Key || {Key, _} <- FriendlEnum],
            {error, ?FMT("~p is not a valid enum value, acceptable values are ~p.", [StrValue, Acceptable])};
        {_Key, Value} ->
            Value
    end.

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
