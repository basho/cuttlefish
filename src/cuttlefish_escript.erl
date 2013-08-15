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
-define(STDERR(Str, Args), io:format(standard_error, Str ++ "~n", Args)).

-export([main/1]).

cli_options() ->
%% Option Name, Short Code, Long Code, Argument Spec, Help Message
[
 {help,               $h, "help",        undefined,              "Print this usage page"},
 {etc_dir,            $e, "etc_dir",     {string, "/etc"},       "etc dir"},
 {dest_dir,           $d, "dest_dir",    string,                 "speficies the directory to write the config file to"},
 {dest_file,          $f, "dest_file",   {string, "app.config"}, "the file name to write"},
 {schema_dir,         $s, "schema_dir",  string,                 "a directory containing .schema files"},
 {conf_file,          $c, "conf_file",   string,                 "a cuttlefish conf file, multiple files allowed"},
 {app_config,         $a, "app_config",  string,                 "the advanced erlangy app.config"}
].

%% LOL! I wanted this to be halt 0, but honestly, if this escript does anything
%% except return the path to a generated config file, it should return a non-zero
%% return code
print_help() ->
    getopt:usage(cli_options(),
                 escript:script_name()),
    halt(1).

run_help([]) -> true;
run_help(ParsedArgs) ->
    lists:member(help, ParsedArgs).

main(Args) ->
    {ParsedArgs, _GarbageFile} = case getopt:parse(cli_options(), Args) of
        {ok, {P, H}} -> {P, H};
        _ -> print_help()
    end,

    case run_help(ParsedArgs) of
        true -> print_help();
        _ -> ok
    end,
    
    %% If /etc/app.config exists, use it and disable cuttlefish
    %% even though cuttlefish is awesome
    EtcDir = proplists:get_value(etc_dir, ParsedArgs),
    case filelib:is_file(filename:join(EtcDir, "app.config")) of
        true ->
            AppConf = filename:join(EtcDir, "app.config"),
            ?STDERR("~s exists, disabling cuttlefish.", [AppConf]),
            %% TODO: placeholder to basho's cuttlefish documentation url
            ?STDERR("If you'd like to know more about cuttlefish, check your local library!", []),
            ?STDERR(" or see http://github.com/basho/cuttlefish", []),
            ?STDOUT("~s", [AppConf]),
            halt(0);
        _ ->
            %% Just keep going
            ?STDERR("no app.config detected in ~s, activating cuttlefish", [EtcDir]),
            ok
    end, 

    ConfFiles = proplists:get_all_values(conf_file, ParsedArgs),
    SchemaDir = proplists:get_value(schema_dir, ParsedArgs), 
    SchemaFiles = [ filename:join(SchemaDir, Filename)  || Filename <- filelib:wildcard("*.schema", SchemaDir)], %%proplists:get_all_values(schema_file, ParsedArgs),
    SortedSchemaFiles = lists:sort(fun(A,B) -> A > B end, SchemaFiles), 
    case length(SortedSchemaFiles) of
        0 ->
            ?STDERR("No Schema files found in specified", []),
            halt(1);
        _ -> 
            ?STDERR("SchemaFiles: ~p", [SortedSchemaFiles])
    end,

    DestinationPath = case proplists:is_defined(dest_dir, ParsedArgs) of
        false ->
            DP = filename:join(EtcDir, "generated"),
            file:make_dir(DP),
            DP;
        true ->
             proplists:get_value(dest_dir, ParsedArgs)
    end,

    DestinationFilename = proplists:get_value(dest_file, ParsedArgs),
    Destination = filename:join(DestinationPath, DestinationFilename),

    ?STDERR("Generating config in: ~p", [Destination]),
    ?STDERR("ConfFiles: ~p", [ConfFiles]),
    ?STDERR("SchemaFiles: ~p", [SortedSchemaFiles]),

    {Translations, Schema} = cuttlefish_schema:files(SortedSchemaFiles),
    Conf = cuttlefish_conf:files(ConfFiles),  
    NewConfig = cuttlefish_generator:map(Translations, Schema, Conf),

    AdvancedConfigFile = filename:join(EtcDir, "advanced.config"),
    FinalConfig = case filelib:is_file(AdvancedConfigFile) of
        true ->
            ?STDERR("~s/advanced.config detected, overlaying proplists", [EtcDir]),
            {ok, [AdvancedConfig]} = file:consult(AdvancedConfigFile), 
            cuttlefish_advanced:overlay(NewConfig, AdvancedConfig);
        _ ->
            %% Nothing to see here, these aren't the droids you're looking for.
            NewConfig
    end, 

    file:write_file(Destination,io_lib:fwrite("~p.\n",[FinalConfig])),
    {{Y, M, D}, {HH, MM, SS}} = calendar:local_time(),
    AuditConfigFile = io_lib:format("~s.~p.~p.~p.~p.~p.~p", [Destination, Y, M, D, HH, MM, SS]), 
    file:write_file(AuditConfigFile,io_lib:fwrite("~p.\n",[NewConfig])),
    ?STDOUT("~s", [Destination]),
    ok.