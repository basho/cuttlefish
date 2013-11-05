%% -------------------------------------------------------------------
%%
%% cuttlefish_escript: used by sh scripts to parse configs
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
-module(cuttlefish_escript).

-define(STDOUT(Str, Args), io:format(Str ++ "~n", Args)).
-export([main/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

cli_options() ->
%% Option Name, Short Code, Long Code, Argument Spec, Help Message
[
 {help,               $h, "help",        undefined,        "Print this usage page"},
 {etc_dir,            $e, "etc_dir",     {string, "/etc"}, "etc dir"},
 {dest_dir,           $d, "dest_dir",    string,           "specifies the directory to write the config file to"},
 {dest_file,          $f, "dest_file",   {string, "app"},  "the file name to write"},
 {schema_dir,         $s, "schema_dir",  string,           "a directory containing .schema files"},
 {schema_file,        $i, "schema_file", string,           "individual schema file, will be processed in command line order, after -s"},
 {conf_file,          $c, "conf_file",   string,           "a cuttlefish conf file, multiple files allowed"},
 {app_config,         $a, "app_config",  string,           "the advanced erlangy app.config"},
 {log_level,          $l, "log_level",   {string, "info"}, "log level for cuttlefish output"},
 {print_schema,       $p, "print",       undefined,        "prints schema mappings on stderr"}
].

%% LOL! I wanted this to be halt 0, but honestly, if this escript does anything
%% except return the path to a generated config file, it should return a non-zero
%% return code
print_help() ->
    getopt:usage(cli_options(),
                 escript:script_name()),
    init:stop(1).

run_help([]) -> true;
run_help(ParsedArgs) ->
    lists:member(help, ParsedArgs).

%% @doc main method for generating erlang term config files
main(Args) ->
    application:load(lager),
    
    {ParsedArgs, _GarbageFile} = case getopt:parse(cli_options(), Args) of
        {ok, {P, H}} -> {P, H};
        _ -> print_help()
    end,

    SuggestedLogLevel = list_to_atom(proplists:get_value(log_level, ParsedArgs)),
    LogLevel = case lists:member(SuggestedLogLevel, [debug, info, notice, warning, error, critical, alert, emergency]) of
        true -> SuggestedLogLevel;
        _ -> info
    end,

    application:set_env(lager, handlers, [{lager_stderr_backend, LogLevel}]),
    application:start(lager),

    lager:debug("Cuttlefish set to debug level logging"),

    case run_help(ParsedArgs) of
        true -> print_help();
        _ -> ok
    end,

    EtcDir = proplists:get_value(etc_dir, ParsedArgs),

    {AppConfigExists, ExistingAppConfigName} = check_existence(EtcDir, "app.config"),
    {VMArgsExists, ExistingVMArgsName} = check_existence(EtcDir, "vm.args"),
    
    %% If /etc/app.config exists, use it and disable cuttlefish
    %% even though cuttlefish is awesome
    FilesToUse = case {AppConfigExists, VMArgsExists} of
        {true, true} ->
            lager:info("~s and ~s exists, disabling cuttlefish.", [ExistingAppConfigName, ExistingVMArgsName]),
            lager:info("If you'd like to know more about cuttlefish, check your local library!", []),
            lager:info(" or see http://github.com/basho/cuttlefish", []),
            {ExistingAppConfigName, ExistingVMArgsName};
        {true, false} ->
            lager:info("~s exists, generating vm.args", [ExistingAppConfigName]),
            {_, NewVMArgs} = engage_cuttlefish(ParsedArgs),
            {ExistingAppConfigName, NewVMArgs};
        {false, true} ->
            lager:info("~s exists, generating app.config", [ExistingVMArgsName]),
            {NewAppConfig, _} = engage_cuttlefish(ParsedArgs),
            {NewAppConfig, ExistingVMArgsName};
        _ ->
            lager:info("No app.config or vm.args detected in ~s, activating cuttlefish", [EtcDir]),
            engage_cuttlefish(ParsedArgs)
    end,

    case FilesToUse of
        %% this is nice and all, but currently all error paths of engage_cuttlefish end with init:stop(1)
        %% hopefully factor that to be cleaner.
        error -> 
            init:stop(1);
        {AppConf, VMArgs} ->
            %% Note: we have added a parameter '-vm_args' to this. It appears redundant
            %% but it is not! the erlang vm allows us to access all arguments to the erl
            %% command EXCEPT '-args_file', so in order to get access to this file location
            %% from within the vm, we need to pass it in twice.
            ?STDOUT(" -config ~s -args_file ~s -vm_args ~s ", [AppConf, VMArgs, VMArgs]),
            init:stop(0);
        X ->
            lager:error("Unknown Return from cuttlefish library: ~p", X),
            init:stop(1)
    end.

-spec engage_cuttlefish(list()) -> {string(), string()}.
engage_cuttlefish(ParsedArgs) ->
    EtcDir = proplists:get_value(etc_dir, ParsedArgs),
    ConfFiles = proplists:get_all_values(conf_file, ParsedArgs),
    SchemaDir = proplists:get_value(schema_dir, ParsedArgs), 
    
    SchemaDirFiles = case SchemaDir of
        undefined -> [];
        _ -> [ filename:join(SchemaDir, Filename)  || Filename <- filelib:wildcard("*.schema", SchemaDir)]
    end,
    IndividualSchemaFiles = proplists:get_all_values(schema_file, ParsedArgs),
    SchemaFiles = SchemaDirFiles ++ IndividualSchemaFiles,

    SortedSchemaFiles = lists:sort(fun(A,B) -> A > B end, SchemaFiles), 
    case length(SortedSchemaFiles) of
        0 ->
            lager:debug("No Schema files found in specified", []),
            init:stop(1);
        _ -> 
            lager:debug("SchemaFiles: ~p", [SortedSchemaFiles])
    end,

    DestinationPath = case proplists:is_defined(dest_dir, ParsedArgs) of
        false ->
            DP = filename:join(EtcDir, "generated"),
            file:make_dir(DP),
            DP;
        true ->
            DP = proplists:get_value(dest_dir, ParsedArgs),
            case filelib:ensure_dir(filename:join(DP, "weaksauce.dummy")) of
                %% filelib:ensure_dir/1 requires a dummy filename in the argument,
                %% I think that is weaksauce, hence "weaksauce.dummy" 
                ok -> 
                    DP;
                {error, E} ->
                    lager:info("Unable to create directory ~s - ~p.  Please check permissions.", [DP, E]),
                    init:stop(1)
            end
    end,
    AbsPath = case DestinationPath of
                          [$/|_] -> DestinationPath;
                          _      -> filename:join(element(2,file:get_cwd()), DestinationPath)
                      end,

    Date = calendar:local_time(),

    DestinationFilename = filename_maker(proplists:get_value(dest_file, ParsedArgs), Date, "config"),
    Destination = filename:join(AbsPath, DestinationFilename),

    DestinationVMArgsFilename = filename_maker("vm", Date, "args"),
    DestinationVMArgs = filename:join(AbsPath, DestinationVMArgsFilename),

    lager:debug("Generating config in: ~p", [Destination]),
    lager:debug("ConfFiles: ~p", [ConfFiles]),
    lager:debug("SchemaFiles: ~p", [SortedSchemaFiles]),

    Schema = cuttlefish_schema:files(SortedSchemaFiles),

    case proplists:is_defined(print_schema, ParsedArgs) of
        true ->
            print_schema(Schema);
        _ -> ok
    end,

    Conf = cuttlefish_conf:files(ConfFiles),  
    NewConfig = cuttlefish_generator:map(Schema, Conf),

    AdvancedConfigFile = filename:join(EtcDir, "advanced.config"),
    FinalConfig = case filelib:is_file(AdvancedConfigFile) of
        true ->
            lager:info("~s/advanced.config detected, overlaying proplists", [EtcDir]),
            {ok, [AdvancedConfig]} = file:consult(AdvancedConfigFile), 
            cuttlefish_advanced:overlay(NewConfig, AdvancedConfig);
        _ ->
            %% Nothing to see here, these aren't the droids you're looking for.
            NewConfig
    end, 

    FinalAppConfig = proplists:delete(vm_args, FinalConfig), 
    FinalVMArgs = cuttlefish_vmargs:stringify(proplists:get_value(vm_args, FinalConfig)),

    file:write_file(Destination,io_lib:fwrite("~p.\n",[FinalAppConfig])),
    file:write_file(DestinationVMArgs, io_lib:fwrite(string:join(FinalVMArgs, "\n"), [])),
    {Destination, DestinationVMArgs}.

-spec check_existence(string(), string()) -> {boolean(), string()}.
check_existence(EtcDir, Filename) ->
    FullName = filename:join(EtcDir, Filename), %% Barfolomew
    Exists = filelib:is_file(FullName),
    lager:info("Checking ~s exists... ~p", [FullName, Exists]),
    {Exists, FullName}.

filename_maker(Filename, Date, Extension) ->
    {{Y, M, D}, {HH, MM, SS}} = Date,
    _DestinationFilename = 
        io_lib:format("~s.~p.~s.~s.~s.~s.~s.~s", 
            [Filename, 
            Y,
            zero_pad(M), 
            zero_pad(D),
            zero_pad(HH), 
            zero_pad(MM), 
            zero_pad(SS),
            Extension
        ]).

zero_pad(Integer) ->
    S = integer_to_list(Integer),
    case Integer > 9 of 
        true -> S;
        _ -> [$0|S]
    end.

print_schema(Schema) ->
    lager:info("Printing Schema Mappings"),
    {_, Mappings, _} = Schema,

    {Max, ListOfMappings} = lists:foldr(
        fun(M, {OldMax, List}) -> 
            CandidateMax = length(cuttlefish_mapping:mapping(M)),
            NewMax = case CandidateMax > OldMax of
                true -> CandidateMax;
                _ -> OldMax
            end,
            {NewMax, [{cuttlefish_mapping:mapping(M), string:join(cuttlefish_mapping:variable(M), ".")}|List]}  
        end,
        {0, []},
        Mappings
        ),
    [
        io:format(standard_error, "~s ~s~n", 
            [string:left(M, Max+2, $\s), V])
    || {M, V} <- ListOfMappings].

-ifdef(TEST).

zero_pad_test() ->
    ?assertEqual("00", zero_pad(0)),
    ?assertEqual("01", zero_pad(1)),
    ?assertEqual("02", zero_pad(2)),
    ?assertEqual("03", zero_pad(3)),
    ?assertEqual("04", zero_pad(4)),
    ?assertEqual("05", zero_pad(5)),
    ?assertEqual("06", zero_pad(6)),
    ?assertEqual("07", zero_pad(7)),
    ?assertEqual("08", zero_pad(8)),
    ?assertEqual("09", zero_pad(9)),
    ?assertEqual("10", zero_pad(10)),
    ?assertEqual("11", zero_pad(11)),
    ?assertEqual("12", zero_pad(12)),
    ok.

-endif.
