-module(cuttlefish_escript_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

escript_prune_test_() ->
    {timeout, 20, [
                   escript_prune("-m 3", 3),
                   escript_prune("", 3), %% default
                   escript_prune("-m 6", 6)
                  ]}.

escript_prune(DashM, ExpectedMax) ->
    %% Empty workspace
    case file:list_dir("../test_fixtures/escript_prune_test/generated.config") of
        {ok, FilenamesToDelete} ->
            [ file:delete(filename:join(["../test_fixtures/escript_prune_test/generated.config",F])) || F <- FilenamesToDelete ];
        _ -> ok
    end,

    {_, _, T} = lists:foldl(
        fun(Counter, {PrevConfigs, PrevVMArgs, Tests}) ->
            io:format("Running iteration: ~p", [Counter]),
            %% Timer to keep from generating more than one file per second
            timer:sleep(1100),
            cuttlefish_escript:main(
              "-d ../test_fixtures/escript_prune_test/generated.config "
              "-s ../test_fixtures/escript_prune_test/lib "
              "-e ../test_fixtures/escript_prune_test/etc "
              ++ DashM ++ " generate"
            ),

            AppConfigs =
                lists:sort(
                    filelib:wildcard("app.*.config",
                                     "../test_fixtures/escript_prune_test/generated.config")),
            VMArgs =
                lists:sort(
                    filelib:wildcard("vm.*.args",
                                     "../test_fixtures/escript_prune_test/generated.config")),

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
