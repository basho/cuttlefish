-module(cuttlefish_escript).

-define(STDOUT(Str, Args), io:format(Str ++ "~n", Args)).
-define(STDERR(Str, Args), io:format(standard_error, Str ++ "~n", Args)).

-export([main/1]).

cli_options() ->
%% Option Name, Short Code, Long Code, Argument Spec, Help Message
[
 {help,               $h, "help",        undefined,              "Print this usage page"},
 {ent_dir,            $e, "etc_dir",     {string, "/etc"},       "etc dir"},
 {dest_dir,           $d, "dest_dir",    {string, "/tmp"},       "speficies the directory to write the config file to"},
 {dest_file,          $f, "dest_file",   {string, "app.config"}, "the file name to write"},
 {schema_file,        $s, "schema_file", string,                 "a cuttlefish schema file, multiple files allowed"},
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
            ?STDOUT("~s", [AppConf]),
            halt(0);
        _ ->
            %% Just keep going
            ?STDERR("no app.config detected in ~s, activating cuttlefish", [EtcDir]),
            ok
    end, 

    ConfFiles = proplists:get_all_values(conf_file, ParsedArgs),
    SchemaFiles = proplists:get_all_values(schema_file, ParsedArgs),
    DestinationPath = proplists:get_value(dest_dir, ParsedArgs),
    DestinationFilename = proplists:get_value(dest_file, ParsedArgs),
    Destination = filename:join(DestinationPath, DestinationFilename),

    ?STDERR("Generating config in: ~p", [Destination]),
    ?STDERR("ConfFiles: ~p", [ConfFiles]),
    ?STDERR("SchemaFiles: ~p", [SchemaFiles]),

    %% TODO?: Support multiple files
    {Translations, Schema} = cuttlefish_schema:file(hd(SchemaFiles)),
    Conf = cuttlefish_conf:file(hd(ConfFiles)),  
    NewConfig = cuttlefish_generator:map(Translations, Schema, Conf),

    AdvancedConfig = filename:join(EtcDir, "advanced.config"),
    FinalConfig = case filelib:is_file(AdvancedConfig) of
        true ->
            ?STDERR("~s/advanced.config detected, overlaying proplists", [EtcDir]),
            %% TODO: this should not be NewConfig
            NewConfig;
        _ ->
            %% Nothing to see here, these aren't the droids you're looking for.
            NewConfig
    end, 

    file:write_file(Destination,io_lib:fwrite("~p.\n",[FinalConfig])),
    %% todo: write out dated archived version
    %%file:write_file(filename:join(DestinationPath, DestinationFilename ++ "." ++ ),io_lib:fwrite("~p.\n",[NewConfig])),
    ?STDOUT("~s", [Destination]),
    ok.