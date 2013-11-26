-module(erlang_vm_schema_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


%% basic schema test will check to make sure that all defaults from the schema
%% make it into the generated app.config
basic_schema_test() ->
    %% The defaults are defined in ../priv/riak_kv.schema and multi_backend.schema.
    %% they are the files under test.
    Config = cuttlefish_unit:generate_templated_config(
        ["../priv/erlang_vm.schema"], [], context()),

    cuttlefish_unit:assert_config(Config, "vm_args.-smp", "enable"),
    cuttlefish_unit:assert_config(Config, "vm_args.+W", "w"),
    cuttlefish_unit:assert_config(Config, "vm_args.+K", "true"),
    cuttlefish_unit:assert_config(Config, "vm_args.-name", "node@host"),
    cuttlefish_unit:assert_config(Config, "vm_args.-setcookie", "erlang"),
    cuttlefish_unit:assert_config(Config, "vm_args.+A", "64"),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_MAX_PORTS", "64000"),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_FULLSWEEP_AFTER", "0"),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_CRASH_DUMP", "dump"),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_MAX_ETS_TABLES", "256000"),
    cuttlefish_unit:assert_config(Config, "vm_args.+P", "256000"),
    cuttlefish_unit:assert_config(Config, "vm_args.+zdbbl", undefined),
    cuttlefish_unit:assert_config(Config, "vm_args.+sfwi", undefined),
    ok.

override_schema_test() ->
    %% Conf represents the riak.conf file that would be read in by cuttlefish.
    %% this proplists is what would be output by the conf_parse module
    Conf = [
        {["erlang", "smp"], "disable"},
        {["erlang", "W"], "i"},
        {["erlang", "K"], "false"},
        {["nodename"], "mynode@myhost"},
        {["distributed_cookie"], "riak"},
        {["erlang", "async_threads"], "22"},
        {["erlang", "max_ports"], "32000"},
        {["erlang", "fullsweep_after"], "1"},
        {["erlang", "crash_dump"], "place"},
        {["erlang", "max_ets_tables"], "128000"},
        {["erlang", "process_limit"], "128001"},
        {["erlang", "zdbbl"], 1024},
        {["erlang", "sfwi"], "500"}
    ],

    Config = cuttlefish_unit:generate_templated_config(
        ["../priv/erlang_vm.schema"], Conf, context()),
    lager:debug("Conf: ~p", [Config]),

    cuttlefish_unit:assert_config(Config, "vm_args.-smp", "disable"),
    cuttlefish_unit:assert_config(Config, "vm_args.+W", "i"),
    cuttlefish_unit:assert_config(Config, "vm_args.+K", "false"),
    cuttlefish_unit:assert_config(Config, "vm_args.-name", "mynode@myhost"),
    cuttlefish_unit:assert_config(Config, "vm_args.-setcookie", "riak"),
    cuttlefish_unit:assert_config(Config, "vm_args.+A", "22"),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_MAX_PORTS", "32000"),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_FULLSWEEP_AFTER", "1"),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_CRASH_DUMP", "place"),
    cuttlefish_unit:assert_config(Config, "vm_args.-env ERL_MAX_ETS_TABLES", "128000"),
    cuttlefish_unit:assert_config(Config, "vm_args.+P", "128001"),
    cuttlefish_unit:assert_config(Config, "vm_args.+zdbbl", 1),
    cuttlefish_unit:assert_config(Config, "vm_args.+sfwi", "500"),
    ok.

%% this context() represents the substitution variables that rebar will use during the build process.
%% riak_core's schema file is written with some {{mustache_vars}} for substitution during packaging
%% cuttlefish doesn't have a great time parsing those, so we perform the substitutions first, because
%% that's how it would work in real life.
context() ->
    [
        {node, "node@host"},
        {crash_dump, "dump"}
    ].
