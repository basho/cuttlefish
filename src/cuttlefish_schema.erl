%% -------------------------------------------------------------------
%%
%% cuttlefish_schema: slurps schema files
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

-module(cuttlefish_schema).

-export([file/1, map/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% TODO: temporary
-compile(export_all).

map(Translations, Schema, Config) ->
    DConfig = add_defaults(Config, Schema),
    Conf = transform_datatypes(DConfig, Schema),
    {DirectMappings, TranslationsToDrop} = lists:foldl(
        fun({Key, Default, Attributes}, {ConfAcc, XlatAcc}) ->
            Mapping = proplists:get_value(mapping, Attributes),
            case {
                Default =/= undefined orelse proplists:is_defined(Key, Conf), 
                proplists:is_defined(Mapping, Translations)
                } of
                {true, false} -> 
                    Tokens = string:tokens(Mapping, "."),
                    NewValue = proplists:get_value(Key, Conf),
                    {tyktorp(Tokens, ConfAcc, NewValue), XlatAcc};
                {true, true} -> {ConfAcc, XlatAcc};
                _ -> {ConfAcc, [Mapping|XlatAcc]}
            end
        end, 
        {[], []},
        Schema),
    
    %% Translations
    lists:foldl(
        fun({Mapping, Xlat, _}, Acc) ->
            case lists:member(Mapping, TranslationsToDrop) of
                false ->
                    %%io:format("Translation: ~s~n", [Mapping]),
                    Tokens = string:tokens(Mapping, "."),
                    NewValue = Xlat(Conf),
                    %%io:format("tyktorp(~s, ~p, ~p)~n", [Mapping, Acc, NewValue]),
                    tyktorp(Tokens, Acc, NewValue);
                _ ->
                    Acc
            end
        end, 
        DirectMappings, 
        Translations). 

%for each token, is it special?
%
%if yes, special processing
%if no, recurse into this with the value from the proplist and tail of tokens
%
%unless the tail of tokens is []
tyktorp([LastToken], Acc, NewValue) ->
    {Type, Token, X} = token_type(LastToken),
    OldValue = proplists:get_value(Token, Acc), 
    New = case Type of
        tuple -> cuttlefish_util:replace_tuple_element(X, NewValue, OldValue); 
        _ -> NewValue
    end,
    cuttlefish_util:replace_proplist_value(Token, New, Acc); 
tyktorp([HeadToken|MoreTokens], PList, NewValue) ->
    {_Type, Token, _X} = token_type(HeadToken),
    OldValue = proplists:get_value(Token, PList, []),
    cuttlefish_util:replace_proplist_value(
        Token,
        tyktorp(MoreTokens, OldValue, NewValue),
        PList).

%% Keeping this around to deal with possible $ prefixed tokens
token_type(Token) ->
    case string:tokens(Token, "$") of
        [Token] -> { normal, list_to_atom(Token), none};
        [X] -> {named, list_to_atom(X), none}
    end.

%% Priority is a nested set of proplists, but each list has only one item
%% for easy merge
%% merge([{K,V}]=Priority, Proplist) ->
%%     case proplists:get_value(K, Proplist) of
%%         undefined -> Proplist ++ Priority;
%%         Existing ->
%%             proplists:delete(K, Proplist) ++ merge(V, Existing) 
%%     end; 
%% merge([], Proplist) -> Proplist;
%% merge(Priority, []) -> Priority.

add_defaults(Conf, Schema) ->

    lists:foldl(
        fun({Key, Default, Attributes}, Acc) ->
            Match = lists:any(
                fun({K, _V}) ->
                    variable_key_match(K, Key)
                end, 
                Conf),
            %% No, then plug in the default
            FuzzyMatch = lists:member($$, Key),
            case {Match, FuzzyMatch} of
                {false, true} -> 
                    Sub = proplists:get_value(include_default, Attributes),
                    [{variable_key_replace(Key, Sub), Default}|Acc];
                {false, false} -> [{Key, Default}|Acc];
                _ -> Acc
            end 
        end, 
        Conf, 
        lists:filter(fun({_K, Def, _A}) -> Def =/= undefined end, Schema)).

transform_datatypes(Conf, Schema) ->
    [ begin
        %% Look up mapping from schema
        {_Key, _Default, Attributes} = find_mapping(Key, Schema),
        %%Mapping = proplists:get_value(mapping, Attributes),
        {DT, _} = proplists:get_value(datatype, Attributes, {undefined, []}),
        {Key, caster(Value, DT)}
    end || {Key, Value} <- Conf].

%% Ok, this is tricky
%% There are three scenarios we have to deal with:
%% 1. The mapping is there! -> return mapping
%% 2. The mapping is not there -> error
%% 3. The mapping is there, but the key in the schema contains a $.
%%      (fuzzy match)
find_mapping(Key, Schema) ->
    {HardMappings, FuzzyMappings} =  lists:foldl(
        fun(Mapping={K, _D, _A}, {HM, FM}) -> 
            case {Key =:= K, variable_key_match(Key, K)} of
                {true, _} -> {[Mapping|HM], FM};
                {_, true} -> {HM, [Mapping|FM]};
                _ -> {HM, FM}
            end
        end,
        {[], []},
        Schema),

    case {length(HardMappings), length(FuzzyMappings)} of
        {1, _} -> hd(HardMappings);
        {0, 1} -> hd(FuzzyMappings);
        {0, 0} -> {error, io_lib:format("~s not_found", [Key])};
        {X, Y} -> {error, io_lib:format("~p hard mappings and ~p fuzzy mappings found for ~s", [X, Y, Key])}
    end.

variable_key_match(Key, KeyDef) ->
    KeyTokens = string:tokens(Key, "."),
    KeyDefTokens = string:tokens(KeyDef, "."),

    case length(KeyTokens) =:= length(KeyDefTokens) of
        true ->
            Zipped = lists:zip(KeyTokens, KeyDefTokens),
            lists:all(
                fun({X,Y}) ->
                    X =:= Y orelse hd(Y) =:= $$
                end,
                Zipped);
        _ -> false
    end.

variable_key_replace(Key, Sub) ->
    KeyTokens = string:tokens(Key, "."), 
    string:join([ begin 
        case hd(Tok) of
            $$ -> Sub;
            _ -> Tok
        end
    end|| Tok <- KeyTokens], "."). 

caster(X, enum) -> list_to_atom(X);
caster(X, integer) -> list_to_integer(X);
caster(X, ip) ->
    Parts = string:tokens(X, ":"),
    [Port|BackwardsIP] = lists:reverse(Parts),
    {string:join(lists:reverse(BackwardsIP), ":"), list_to_integer(Port)};
caster(X, _) -> X.

-spec file(string()) -> [{string(), any(), list()}].
file(Filename) ->
    {ok, B} = file:read_file(Filename),
    %% TODO: Hardcoded utf8
    S = unicode:characters_to_list(B, utf8),
    string(S).

-spec string(string()) -> {[{string(), fun(), list()}], [{string(), any(), list()}]}.
string(S) -> 
    {ok, Tokens, _} = erl_scan:string(S),
    CommentTokens = erl_comment_scan:string(S),
    Schemas = parse_schema(Tokens, CommentTokens),
    lists:partition(fun({_, _, Attributes}) -> proplists:is_defined(translation, Attributes) end, Schemas). 

parse_schema(Tokens, Comments) ->
    parse_schema(Tokens, Comments, []).

parse_schema([], _, Acc) ->
    lists:reverse(Acc);
parse_schema(ScannedTokens, CommentTokens, Acc) ->
    {LineNo, Tokens, TailTokens } = parse_schema_tokens(ScannedTokens),
    {Comments, TailComments} = lists:foldr(
        fun(X={CommentLineNo, _, _, Comment}, {C, TC}) -> 
            case CommentLineNo < LineNo of
                true -> {Comment ++ C, TC};
                _ -> {C, [X|TC]}
            end
        end, 
        {[], []}, 
        CommentTokens),
    { Key, Default } = parse(Tokens),
    Attributes = comment_parser(Comments),
    parse_schema(TailTokens, TailComments, [{Key, Default, Attributes}| Acc]).

parse_schema_tokens(Scanned) -> 
    parse_schema_tokens(Scanned, []).

parse_schema_tokens(Scanned, Acc=[{dot, LineNo}|_]) ->
    {LineNo, lists:reverse(Acc), Scanned};
parse_schema_tokens([H|Scanned], Acc) ->
    parse_schema_tokens(Scanned, [H|Acc]).

-spec parse(list()) -> {string(), any()}.
parse(Scanned) ->
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    {value, X, _} = erl_eval:exprs(Parsed,[]),
    X.

comment_parser(Comments) ->
    StrippedComments = 
        lists:filter(fun(X) -> X =/= [] end, 
            [percent_stripper(C) || C <- Comments]),
    %% now, let's go annotation hunting

    AttrList = lists:foldl(
        fun(Line, Acc) ->
                case {Line, Acc} of
                    {[ $@ | T], _} ->
                        Annotation = hd(string:tokens(T, [$\s])),
                        [{list_to_atom(Annotation), [percent_stripper(T -- Annotation)] }|Acc];
                    { _, []} -> [];
                    {String, _} ->
                        [{Annotation, Strings}|T] = Acc,
                        [{Annotation, [String|Strings]}|T]
                end
            end, [], StrippedComments), 
    SortedList = lists:reverse([ {Attr, lists:reverse(Value)} || {Attr, Value} <- AttrList]),
    CorrectedList = attribute_formatter(SortedList),
    CorrectedList.

attribute_formatter([{translation, _}| T]) ->
    [{translation, true}| attribute_formatter(T)];
attribute_formatter([{datatype, DT}| T]) ->
    [{datatype, data_typer(DT)}| attribute_formatter(T)];
attribute_formatter([{mapping, Mapping}| T]) ->
    [{mapping, lists:flatten(Mapping)}| attribute_formatter(T)];
attribute_formatter([{include_default, NameSub}| T]) ->
    [{include_default, lists:flatten(NameSub)}| attribute_formatter(T)];
attribute_formatter([{commented, CommentValue}| T]) ->
    [{commented, lists:flatten(CommentValue)}| attribute_formatter(T)];
attribute_formatter([_Other | T]) ->
    attribute_formatter(T); %% TODO: don't throw other things away [ Other | attribute_formatter(T)]
attribute_formatter([]) -> [].

percent_stripper(Line) ->
    percent_stripper_r(percent_stripper_l(Line)).

percent_stripper_l([$%|T]) -> percent_stripper_l(T);
percent_stripper_l([$\s|T]) -> percent_stripper_l(T);
percent_stripper_l(Line) -> Line.

percent_stripper_r(Line) -> 
    lists:reverse(
        percent_stripper_l(
            lists:reverse(Line))).

data_typer(DT) ->
    DataTypes = lists:flatten(DT),
    DataType = hd(string:tokens(DataTypes, [$\s])),
    Extra = DataTypes -- DataType,
    {list_to_atom(DataType), [ percent_stripper(T) || T <- string:tokens(Extra, [$,])] }.

-ifdef(TEST).
map_test() ->
    {Translations, Schema} = file("../test/riak.schema"),
    Conf = conf_parse:file("../test/riak.conf"),
    NewConfig = map(Translations, Schema, Conf),
    io:format("~p~n", [proplists:get_value(riak_core, NewConfig)]),

    NewRingSize = proplists:get_value(ring_creation_size, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual(32, NewRingSize),

    NewAAE = proplists:get_value(anti_entropy, proplists:get_value(riak_kv, NewConfig)), 
    ?assertEqual({on,[debug]}, NewAAE),

    NewSASL = proplists:get_value(sasl_error_logger, proplists:get_value(sasl, NewConfig)), 
    ?assertEqual(false, NewSASL),

    NewHTTP = proplists:get_value(http, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual([{"127.0.0.1", 8098}, {"10.0.0.1", 80}], NewHTTP),

    NewPB = proplists:get_value(pb, proplists:get_value(riak_api, NewConfig)), 
    ?assertEqual([{"127.0.0.1", 8087}], NewPB),

    NewHTTPS = proplists:get_value(https, proplists:get_value(riak_core, NewConfig)), 
    ?assertEqual(undefined, NewHTTPS),

    file:write_file("../generated.config",io_lib:fwrite("~p.\n",[NewConfig])),
    ok.

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


    {_, Schema} = file("../test/riak.schema"),
    ?assertEqual(length(ExpectedSchema), length(Schema)),

    [ ?assertEqual(Expected, Actual) || {Expected, Actual} <- lists:zip(ExpectedSchema, Schema)],
    ok.

percent_stripper_test() ->
    ?assertEqual("hi!", percent_stripper("%%% hi!")),
    ?assertEqual("hi!", percent_stripper("%% hi!")),
    ?assertEqual("hi!", percent_stripper("% hi!")),
    ?assertEqual("hi!", percent_stripper(" hi!")),
    ?assertEqual("hi!", percent_stripper(" % % hi!")),
    ?assertEqual("hi!", percent_stripper("% % % hi!")),
    ?assertEqual("hi!", percent_stripper("% % % hi! % % %")),
    ok.

comment_parser_test() ->
    Comments = [
        " ",
        "%% @doc this is a sample doc",
        "%% it spans multiple lines %%",
        "",
        "%% there can be line breaks",
        "%% @datatype enum on, off",
        "%% @advanced",
        "%% @optional",
        "%% @include_default name_substitution",
        "%% @mapping riak_kv.anti_entropy"
    ],
    ParsedComments = comment_parser(Comments),
    ?assertEqual(
        [
          {datatype,{enum,["on","off"]}},
          {include_default, "name_substitution"},
          {mapping, "riak_kv.anti_entropy"}
        ], ParsedComments
        ),
    ok.

caster_ip_test() ->
    ?assertEqual({"127.0.0.1", 8098}, caster("127.0.0.1:8098", ip)),
    ?assertEqual({"2001:0db8:85a3:0042:1000:8a2e:0370:7334", 8098}, caster("2001:0db8:85a3:0042:1000:8a2e:0370:7334:8098", ip)),
    ok.

find_mapping_test() ->
    Mappings = [
        {"key.with.fixed.name", 0, []},
        {"key.with.$variable.name", 1, []}
    ],
    ?assertEqual(
        {"key.with.fixed.name", 0, []}, 
        find_mapping("key.with.fixed.name", Mappings)),
    ?assertEqual(
        {"key.with.$variable.name", 1, []}, 
        find_mapping("key.with.A.name", Mappings)),
    ?assertEqual(
        {"key.with.$variable.name", 1, []}, 
        find_mapping("key.with.B.name", Mappings)),
    ?assertEqual(
        {"key.with.$variable.name", 1, []}, 
        find_mapping("key.with.C.name", Mappings)),
    ?assertEqual(
        {"key.with.$variable.name", 1, []}, 
        find_mapping("key.with.D.name", Mappings)),
    ?assertEqual(
        {"key.with.$variable.name", 1, []}, 
        find_mapping("key.with.E.name", Mappings)),
    ok.

all_the_marbles_test() ->
    %%lager:start(),
    {Translations, Schema} = file("../test/riak.schema"),
    Conf = [], %conf_parse:file("../test/riak.conf"),
    NewConfig = map(Translations, Schema, Conf),
    ?assert(is_proplist(NewConfig)),

    {ok, [AppConfig]} = file:consult("../test/default.config"),
    
    ?assert(is_proplist(AppConfig)),

    proplist_equals(AppConfig, NewConfig),
    ok.

proplist_equals(Expected, Actual) ->
    ExpectedKeys = lists:sort(proplists:get_keys(Expected)),
    ActualKeys = lists:sort(proplists:get_keys(Actual)),
    ?assertEqual(ExpectedKeys, ActualKeys), 
    [ begin 
        ExpectedValue = proplists:get_value(EKey, Expected),
        ActualValue = proplists:get_value(EKey, Actual, undefined),
        case {is_proplist(ExpectedValue), is_proplist(ActualValue)} of
            {true, true} ->
                proplist_equals(ExpectedValue, ActualValue);
            {false, false} ->
                ?assertEqual(ExpectedValue, ActualValue);
            _ ->
                ?assert(false)
        end
    end || EKey <- ExpectedKeys].

is_proplist(Proplist) when is_list(Proplist) ->
    lists:all(
        fun(X) -> 
            is_tuple(X) andalso tuple_size(X) =:= 2
        end,
        Proplist);
is_proplist(_) -> false.
-endif.