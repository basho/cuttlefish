%% -------------------------------------------------------------------
%%
%% cuttlefish_datatypes_eqc: EQC tests for cuttlefish
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
-module(cuttlefish_datatypes_eqc).
-compile(export_all).

-ifdef(EQC).

-define(NUM_TESTS, 1000).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").


eqc_test_() ->
    [?_assert(prop_helper() =:= true)].

prop_helper() ->
    prop_helper(?NUM_TESTS).

prop_helper(NumTests) ->
    eqc:quickcheck(eqc:numtests(NumTests, prop_valid_to_string())).


%% generators
gen_non_blank_string() ->
    not_empty(list(gen_lower_char())).

gen_maybe_blank_string() ->
    ?LET(X, list(gen_lower_char()), X).

%% Generate a lower 7-bit ACSII character that should not cause any problems
%% with utf8 conversion.
gen_lower_char() ->
    choose(16#20, 16#7f).

not_empty(G) ->
    ?SUCHTHAT(X, G, X /= [] andalso X /= <<>>).

gen_atom() ->
 ?LET(S, gen_maybe_blank_string(), list_to_atom(S)).

gen_atom_datatype() ->
    ?LET(X, oneof([gen_atom(), gen_maybe_blank_string()]), {atom, X}).

gen_int_as_list() ->
    ?LET(X, largeint(), integer_to_list(X)).

gen_integer_datatype() ->
    ?LET(X, oneof([int(), largeint(), gen_int_as_list()]), {integer, X}).

gen_ip_pair() ->
    ?LET(IP, gen_maybe_blank_string(),
         ?LET(Port, int(), {IP, Port})).

gen_ip_datatype() ->
    ?LET(X, oneof([gen_ip_pair(), gen_non_blank_string()]), {ip, X}).

gen_enum_datatype() ->
    ?LET(X, oneof([gen_atom(), gen_maybe_blank_string()]), {{enum,foo}, X}).

gen_bytesize_datatype() ->
    ?LET(X, oneof([gen_int_as_list(), int(), largeint()]), {bytesize, X}).

gen_string_datatype() ->
    ?LET(X, gen_maybe_blank_string(), {string, X}).

gen_filename_datatype() ->
    ?LET(X, gen_maybe_blank_string(), {file, X}).

gen_dirname_datatype() ->
    ?LET(X, gen_maybe_blank_string(), {directory, X}).

gen_flag_on_off() ->
  elements(["on", "off"]).

gen_simple_flag_datatype() ->
    ?LET(Flag, gen_flag_on_off(),
            {flag, Flag}).

gen_complex_flag_datatype() ->
    ?LET(A, gen_maybe_blank_string(),
    ?LET(AV, gen_maybe_blank_string(),
    ?LET(B, gen_maybe_blank_string(),
    ?LET(BV, gen_maybe_blank_string(),
         {flag, {flag, {A, AV}, {B, BV}}})))).

gen_duration_uom() ->
    elements([f, w, d, h, m, s, ms]).

gen_duration_datatype() ->
    ?LET(X, oneof([gen_maybe_blank_string(), int(), largeint(), real()]),
         ?LET(Unit, gen_duration_uom(),
              {{duration, Unit}, X})).

gen_to_string_input() ->
    oneof([gen_atom_datatype(),
           gen_integer_datatype(),
           gen_ip_datatype(),
           gen_enum_datatype(),
           gen_duration_datatype(),
           gen_bytesize_datatype(),
           gen_string_datatype(),
           gen_filename_datatype(),
           gen_simple_flag_datatype(),
           gen_complex_flag_datatype()
           ]).


%% this EQC test doesn't have lots of "brains"
%% basically, does to_string/2 throw an exception?
prop_valid_to_string() ->
    ?FORALL(Gen, gen_to_string_input(),
              begin
                {Type, Inputs} = Gen,
                case cuttlefish_datatypes:to_string(Inputs, Type) of
                  X when is_list(X) -> true;
                  {error, Y} ->
                    %% make sure we get the same error
                    Y ==
                      lists:flatten(
                         io_lib:format( "Tried to convert ~p, an invalid datatype ~p to_string.", [Inputs, Type]));
                  _ -> false
                end
              end).

-endif.

