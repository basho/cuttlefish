%% -------------------------------------------------------------------
%%
%% cuttlefish_schema_test:  tests file slurping. 
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

%% We're in our own module because this is a sizeable test.
-module(cuttlefish_schema_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

file_test() ->
    ExpectedSchema = [
        {"ring_size", undefined, 
                [
                 {datatype,{integer,[]}},
                 {mapping, "riak_core.ring_creation_size"},
                 {commented, "64"}]},
        {"anti_entropy", "on",
                [
                 {datatype,{enum,["on","off","debug"]}},
                 {mapping,"riak_kv.anti_entropy"}]},
        { "log.console.file", "./log/console.log",
                [
                 {mapping, "lager.handlers"}
                ]},
        { "log.error.file", "./log/error.log",
                [
                 {mapping, "lager.handlers"}
                ]},
        { "log.syslog", "off",
                [
                 {datatype,{enum,["on","off"]}},
                 {mapping, "lager.handlers"}
                ]},
        { "sasl", "off",
                [
                 {datatype,{enum,["on","off"]}},
                 {mapping, "sasl.sasl_error_logger"}
                ]},
        { "listener.http.$name", "127.0.0.1:8098",
                [
                 {datatype,{ip,[]}},
                 {mapping, "riak_core.http"},
                 {include_default,"internal"}
                ]},
        { "listener.protobuf.$name", "127.0.0.1:8087",
                [
                 {datatype,{ip,[]}},
                 {mapping, "riak_api.pb"},
                 {include_default,"internal"}
                ]},
        { "protobuf.backlog", undefined,
                [
                 {mapping, "riak_api.pb_backlog"},
                 {datatype,{integer,[]}},
                 {commented, "64"}
                ]},
        { "ring.state_dir", "./data/ring",
                [
                 {mapping, "riak_core.ring_state_dir"}
                ]},
        { "listener.https.$name", undefined,
                [
                 {datatype,{ip,[]}},
                 {mapping, "riak_core.https"},
                 {include_default,"internal"},
                 {commented,"127.0.0.1:8098"}
                ]},
        { "ssl.certfile", undefined,
                [
                 {mapping, "riak_core.ssl.certfile"},
                 {commented,"./etc/cert.pem"}
                ]},
        { "ssl.keyfile", undefined,
                [
                 {mapping, "riak_core.ssl.keyfile"},
                 {commented,"./etc/key.pem"}
                ]},
        { "handoff.port", "8099",
                [
                 {datatype, {integer, []}},
                 {mapping, "riak_core.handoff_port"}
                ]},
        { "handoff.ssl.certfile", undefined,
                [
                 {mapping, "riak_core.handoff_ssl_options.certfile"},
                 {commented,"/tmp/erlserver.pem"}
                ]},
        { "handoff.ssl.keyfile", undefined,
                [
                 {mapping, "riak_core.handoff_ssl_options.keyfile"}
                ]},
        { "dtrace", "off",
                [
                 {datatype, {enum, ["on", "off"]}},
                 {mapping, "riak_core.dtrace_support"}
                ]},
        { "platform_bin_dir", "./bin",
                [
                 {mapping, "riak_core.platform_bin_dir"}
                ]},
        { "platform_data_dir", "./data",
                [
                 {mapping, "riak_core.platform_data_dir"}
                ]},
        { "platform_etc_dir", "./etc",
                [
                 {mapping, "riak_core.platform_etc_dir"}
                ]},
        { "platform_lib_dir", "./lib",
                [
                 {mapping, "riak_core.platform_lib_dir"}
                ]},
        { "platform_log_dir", "./log",
                [
                 {mapping, "riak_core.platform_log_dir"}
                ]},
        { "search", "off",
                [
                 {datatype, {enum, ["on", "off"]}},
                 {mapping, "riak_search.enabled"}
                ]},
        { "bitcask.io_mode", "erlang",
                [
                 {datatype, {enum, ["erlang", "nif"]}},
                 {mapping, "bitcask.io_mode"}
                ]},
        { "bitcask.data_root", "./data/bitcask",
                [
                 {mapping, "bitcask.data_root"}
                ]},
        { "leveldb.data_root", "./data/leveldb",
                [
                 {mapping, "eleveldb.data_root"}
                ]},
        { "merge_index.data_root", "./data/merge_index",
                [
                 {mapping, "merge_index.data_root"}
                ]},
        { "merge_index.buffer_rollover_size", "1048576",
                [
                 {datatype, {integer,[]}},
                 {mapping, "merge_index.buffer_rollover_size"}
                ]},
        { "merge_index.max_compact_segments", "20",
                [
                 {datatype, {integer,[]}},
                 {mapping, "merge_index.max_compact_segments"}
                ]},
        {"log.crash.file", "./log/crash.log",
                [
                 {mapping, "lager.crash_log"}
                ]},
        {"log.crash.msg_size", "65536", 
                [
                 {datatype, {integer, []}},
                 {mapping, "lager.crash_log_msg_size"}
                ]},
        {"log.crash.size", "10485760", 
                [
                 {datatype, {integer, []}},
                 {mapping, "lager.crash_log_size"}
                ]},
        {"log.crash.date", "$D0", 
                [
                 {mapping, "lager.crash_log_date"}
                ]},
        {"log.crash.count", "5", 
                [
                 {datatype, {integer, []}},
                 {mapping, "lager.crash_log_count"}
                ]},
        {"log.error.redirect", "on", 
                [
                 {datatype, {enum, ["on", "off"]}},
                 {mapping, "lager.error_logger_redirect"}
                ]},
        {"log.error.messages_per_second", "100", 
                [
                 {datatype, {integer, []}},
                 {mapping, "lager.error_logger_hwm"}
                ]},
        {"storage_backend", "bitcask",
                [
                 {datatype, {enum, ["bitcask", "leveldb", "memory", "multi"]}},
                 {mapping, "riak_kv.storage_backend"}
                ]},
        {"raw_name", undefined, 
                [
                 {mapping, "riak_kv.raw_name"},
                 {commented, "riak"}
                ]},
        {"anti_entropy.build_limit.number", "1", [
                {datatype, {integer, []}},
                {mapping, "riak_kv.anti_entropy_build_limit"}
        ]},
        {"anti_entropy.build_limit.per_timespan", "3600000", 
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.anti_entropy_build_limit"}
        ]},
        {"anti_entropy.expire", "604800000", [
                {datatype, {integer, []}},
                {mapping, "riak_kv.anti_entropy_expire"}
        ]},
        {"anti_entropy.concurrency", "2", 
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.anti_entropy_concurrency"}
        ]},
        {"anti_entropy.tick", "15000", 
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.anti_entropy_tick"}
        ]},
        {"anti_entropy.data_dir", "./data/anti_entropy",
        [
                {mapping, "riak_kv.anti_entropy_data_dir"}
        ]},
        {"anti_entropy.write_buffer_size", "4194304", 
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.anti_entropy_leveldb_opts.write_buffer_size"}
        ]},
        {"anti_entropy.max_open_files", "20", 
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.anti_entropy_leveldb_opts.max_open_files"}
        ]},
        {"mapred_name", "mapred", 
        [
                {mapping, "riak_kv.mapred_name"}
        ]},
        {"mapred_2i_pipe", "on", 
        [
                {datatype, {enum, ["on", "off"]}},
                {mapping, "riak_kv.mapred_2i_pipe"}
        ]},
        {"javascript_vm.map_js_vm_count", "8", 
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.map_js_vm_count"}
        ]},
        {"javascript_vm.reduce_js_vm_count", "6",
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.reduce_js_vm_count"}
        ]},
        {"javascript_vm.hook_js_vm_count", "2", 
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.hook_js_vm_count"}
        ]},
        {"javascript_vm.max_vm_mem", "8", 
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.js_max_vm_mem"}
        ]},
        {"javascript_vm.thread_stack", "16",
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.js_thread_stack"}
        ]},
        {"javascript_vm.source_dir", undefined, 
        [
                {mapping, "riak_kv.js_source_dir"},
                {commented, "/tmp/js_source"}
        ]},
        {"http_url_encoding", "on",
        [
                {datatype, {enum, ["on", "off"]}},
                {mapping, "riak_kv.http_url_encoding"}
        ]},
        {"vnode_vclocks", "on",
        [
                {mapping, "riak_kv.vnode_vclocks"}
        ]},
        {"listkeys_backpressure", "on",
        [
                {datatype, {enum, ["on", "off"]}},
                {mapping, "riak_kv.listkeys_backpressure"}
        ]},
        {"fsm_limit", "50000",
        [
                {datatype, {integer, []}},
                {mapping, "riak_kv.fsm_limit"}
        ]},
        {"object_format", "v1",
        [
                {datatype, {enum, ["v0", "v1"]}},
                {mapping, "riak_kv.object_format"}
        ]},
        {"riak_sysmon.process_limit", "30", 
        [
                {datatype, {integer, []}},
                {mapping, "riak_sysmon.process_limit"}
        ]},
        {"riak_sysmon.port_limit", "2", 
        [
                {datatype, {integer, []}},
                {mapping, "riak_sysmon.port_limit"}
        ]},
        {"riak_sysmon.gc_ms_limit", "0", [
                {datatype, {integer, []}},
                {mapping, "riak_sysmon.gc_ms_limit"}
        ]},
        {"riak_sysmon.heap_word_limit", "40111000",
        [
                {datatype, {integer, []}},
                {mapping, "riak_sysmon.heap_word_limit"}
        ]},
        {"riak_sysmon.busy_port", "true", 
        [
                {datatype, {enum, ["true", "false"]}},
                {mapping, "riak_sysmon.busy_port"}
        ]},
        {"riak_sysmon.busy_dist_port", "true",
        [
                {datatype, {enum, ["true", "false"]}},
                {mapping, "riak_sysmon.busy_dist_port"}
        ]},
        {"riak_control", "off", 
        [
            {datatype, {enum, ["on", "off"]}},
            {mapping, "riak_control.enabled"}
        ]},
        {"riak_control.auth", "userlist",
        [
            {datatype, {enum,["userlist"]}},
            {mapping, "riak_control.auth"}
        ]},
        { "riak_control.user.$username.password", "pass",
        [
            {mapping, "riak_control.userlist"},
            {include_default, "user"}
        ]},
        {"riak_control.admin", "on", 
        [
            {mapping, "riak_control.admin"}
        ]}
    ],
 
    {_, Schema} = cuttlefish_schema:file("../test/riak.schema"),
    ?assertEqual(length(ExpectedSchema), length(Schema)),

    [ ?assertEqual(Expected, Actual) || {Expected, Actual} <- lists:zip(ExpectedSchema, Schema)],
    ok.
