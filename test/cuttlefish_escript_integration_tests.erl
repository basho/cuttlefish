%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014-2017 Basho Technologies, Inc.
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

-module(cuttlefish_escript_integration_tests).

-include_lib("eunit/include/eunit.hrl").

escript_utf8_test() ->
    cuttlefish_lager_test_backend:bounce(error),

    ?assertThrow(stop_deactivate, cuttlefish_escript:main(lists:flatten([
        "-d ", cuttlefish_test_util:fixtures_file("escript_utf8_test/generated.config"),
        " -s ", cuttlefish_test_util:fixtures_file("escript_utf8_test/lib"),
        " -e ", cuttlefish_test_util:fixtures_file("escript_utf8_test/etc"),
        " -c ", cuttlefish_test_util:fixtures_file("escript_utf8_test/etc/utf8.conf"),
        " generate"
    ]))),
    [Log] = cuttlefish_lager_test_backend:get_logs(),
    ?assertMatch({match, _}, re:run(Log, "utf8.conf: Error converting value on line #1 to latin1")),
    ok.

advanced_config_format_test() ->
    cuttlefish_lager_test_backend:bounce(error),

    ?assertThrow(stop_deactivate, cuttlefish_escript:main(lists:flatten([
        "-d ", cuttlefish_test_util:fixtures_file("acformat/generated.config"),
        " -s ", cuttlefish_test_util:fixtures_file("acformat/lib"),
        " -e ", cuttlefish_test_util:fixtures_file("acformat/etc"),
        " -c ", cuttlefish_test_util:fixtures_file("acformat/etc/acformat.conf"),
        " generate"
    ]))),
    [Log] = cuttlefish_lager_test_backend:get_logs(),
    ?assertMatch({match, _}, re:run(Log, "Error parsing "
        ++ cuttlefish_test_util:fixtures_file("acformat/etc/advanced.config")
        ++ ", incorrect format: \\[\\[a\\],\\[b\\]\\]")),
    ok.

escript_prune_test_() ->
    {timeout, 20, [
                   escript_prune("-m 3", 3),
                   escript_prune("", 3), %% default
                   escript_prune("-m 6", 6)
                  ]}.

escript_prune(DashM, ExpectedMax) ->
    %% Empty workspace
    GenCfg = cuttlefish_test_util:fixtures_file("escript_prune_test/generated.config"),

    case file:list_dir(GenCfg) of
        {ok, FilenamesToDelete} ->
            [ file:delete(filename:join(GenCfg, F)) || F <- FilenamesToDelete ];
        _ -> ok
    end,

    {_, _, T} = lists:foldl(
        fun(Counter, {PrevConfigs, PrevVMArgs, Tests}) ->
            io:format("Running iteration: ~p", [Counter]),
            %% Timer to keep from generating more than one file per second
            timer:sleep(1100),
            cuttlefish_escript:main(lists:flatten([
                "-d ", GenCfg,
                " -s ", cuttlefish_test_util:fixtures_file("escript_prune_test/lib"),
                " -e ", cuttlefish_test_util:fixtures_file("escript_prune_test/etc"),
                $\s, DashM, " generate"
            ])),

            AppConfigs = lists:sort(filelib:wildcard("app.*.config", GenCfg)),
            VMArgs = lists:sort(filelib:wildcard("vm.*.args", GenCfg)),

            {AppConfigs,
             VMArgs,

             [?_assert(length(AppConfigs) =< ExpectedMax),
              ?_assert(length(VMArgs) =< ExpectedMax),
              compare_lists(PrevConfigs, AppConfigs),
              compare_lists(PrevVMArgs, VMArgs) | Tests]}
        end,
        {[], [], []},
        lists:seq(1,10)),
    T.

%% This function is asserting that Previous is the tail of Current OR
%% that the tail of Previous is equal to the first length(Previous)
%% elements of Current
compare_lists(Previous, Current) when (length(Previous) +1) =:= length(Current) ->
    compare_lists([stub|Previous], Current);
compare_lists([_|PTail] = Previous, Current) when length(Previous) =:= length(Current) ->
    NewPrevious = PTail ++ [lists:last(Current)],
    ?_assertEqual(NewPrevious, Current);
compare_lists(_Previous, _Current) ->
    ?_assert(false).
