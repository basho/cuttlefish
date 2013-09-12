-module(cuttlefish_vmargs).

-export([stringify/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% @doc turns a proplist into a list of strings suitable for vm.args files
-spec stringify([{any(), string()}]) -> [string()].
stringify(VMArgsProplist) ->
    [ lists:flatten(io_lib:format("~s ~s", [K, V]))  || {K, V} <- VMArgsProplist ].

-ifdef(TEST).

stringify_test() ->
    VMArgsProplist = [ 
      {'-name', "dev1@127.0.0.1"},
      {'-setcookie', 'riak'},
      {'-smp',"enable"},
      {'+W',"w"},
      {'+K',"true"},
      {'+A',"64"},
      {'-env ERL_MAX_PORTS',"64000"},
      {'-env ERL_FULLSWEEP_AFTER',"0"},
      {'-env ERL_CRASH_DUMP',"./log/erl_crash.dump"},
      {'-env ERL_MAX_ETS_TABLES',"256000"},
      {'+P', "256000"}
    ],

    VMArgs = stringify(VMArgsProplist),

    Expected = [
        "-name dev1@127.0.0.1",
        "-setcookie riak",
        "-smp enable",
        "+W w",
        "+K true",
        "+A 64",
        "-env ERL_MAX_PORTS 64000",
        "-env ERL_FULLSWEEP_AFTER 0",
        "-env ERL_CRASH_DUMP ./log/erl_crash.dump",
        "-env ERL_MAX_ETS_TABLES 256000",
        "+P 256000"
    ],
    [ ?assertEqual(E, V) || {E, V} <- lists:zip(Expected, VMArgs)],
    ok.

-endif.
