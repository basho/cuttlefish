%% -------------------------------------------------------------------
%%
%% cuttlefish_effective: handles datatypes in cuttlefish schemas
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
-module(cuttlefish_effective).

-define(FMT(F,A), lists:flatten(io_lib:format(F,A))).

-export([build/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-spec build(cuttlefish_conf:conf(), cuttlefish_schema:schema(), [proplists:property()]) -> [string()].
build(Conf, {_Translations, Mappings, _Validators} = _Schema, AdvConfig) ->
    EffectiveConfig = lists:reverse(lists:sort(cuttlefish_generator:add_defaults(Conf, Mappings))),
    %% EffectiveConfig is a list of { [string()], term()}

    KeysToHateOn = process_advanced(Mappings, AdvConfig),

    lists:foldl(
        fun({Var, Value}, Acc) ->
            Variable = string:join(Var, "."),

            IsHater =
                lists:any(
                    fun(X) ->
                        cuttlefish_variable:is_fuzzy_match(Var, X)
                    end,
                    KeysToHateOn
                    ),

            Line = try ?FMT("~s = ~s", [Variable, Value]) of
                X -> X
            catch
                _:_ ->
                    %% I hate that I had to do this, 'cause you know...
                    %% Erlang and Strings, but actually this is ok because
                    %% sometimes there are going to be weird tuply things
                    %% in here, so always good to fall back on ~p.
                    %% honestly, I think this try should be built into io:format
                    ?FMT("~s = ~p", [Variable, Value])
            end,
            case IsHater of
                true ->
                    [?FMT("## ~s was overridden in advanced.config", [Variable]),
                     "## " ++ Line] ++ Acc;
                _ -> [Line | Acc]
            end
        end,
        [],
        EffectiveConfig
    ).

-spec process_advanced([cuttlefish_mapping:mapping()], [proplists:property()]) -> [cuttlefish_variable:variable()].
process_advanced(Mappings, AdvancedConfig) ->
    AdvKeys = proplist_to_kvcpaths(AdvancedConfig),
    lists:foldl(
        fun(M, Acc) ->
            case lists:member(cuttlefish_mapping:mapping(M), AdvKeys) of
                true ->
                    [cuttlefish_mapping:variable(M)|Acc];
                _ -> Acc
            end
        end,
        [],
        Mappings).

proplist_to_kvcpaths(Proplist) ->
    proplist_to_kvcpaths("", Proplist).
proplist_to_kvcpaths(Prefix, Proplist) ->
    NewPrefix = case Prefix of
        "" -> "";
        _ -> Prefix ++ "."
    end,
    lists:foldl(fun(K, Acc) ->
            KeyedPrefix = NewPrefix ++ atom_to_list(K),
            R = proplist_to_kvcpaths(
                KeyedPrefix,
                proplists:get_value(K, Proplist)),
            case R of
                [] ->
                    [KeyedPrefix|Acc];
                _ ->
                    Acc ++ R
            end
        end,
        [],
        keys_if_you_got_em(Proplist)
    ).

%% For EffectiveConfig
%% 1. build a list of "leaf" key combos for AdvConfig
%% 2. Search mappings for possible matches
%% 3. If match, comment out

keys_if_you_got_em(Proplist) when is_list(Proplist) ->
    proplists:get_keys(Proplist);
keys_if_you_got_em(_) -> [].

-ifdef(TEST).

process_advanced_test() ->
    Mappings = [
        cuttlefish_mapping:parse(
            {mapping, "thing.1", "a.b.c", []}
        )
    ],
    AdvConfig = [{a, [{b, [{c, ""}, {d, ""}]}]}],
    KeysToWatch = process_advanced(Mappings, AdvConfig),
    ?assertEqual(["a.b.c"], KeysToWatch),
    ok.

proplist_to_kvcpath_test() ->
    Proplist = [{a, [
                    {b, [
                        {c, "x"}
                        ]},
                    {d, [
                        {e, "y"}
                        ]},
                    {f , "z"}
                    ]
                },
                {g, "q"}],
    Paths = proplist_to_kvcpaths(Proplist),
    ?assertEqual([
        "a.b.c",
        "a.d.e",
        "a.f",
        "g"
    ], Paths),
    ok.

proplists_to_kvcpath_riak_core_test() ->
    Proplist = [{riak_core,[
        {ring_creation_size,128},
        {cluster_mgr, {"127.0.0.1", 9080 } }
    ]}],
    Paths = proplist_to_kvcpaths(Proplist),
    ?assertEqual([
        "riak_core.ring_creation_size",
        "riak_core.cluster_mgr"
    ], Paths),
    ok.

-endif.
