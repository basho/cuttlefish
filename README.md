# Bjorn
Knows all about your options

## Motivation
Born of a desire to make configuraion of Riak easier on the masses, this project strives to help developers merge the idea of a user facing configuration file with that of an erlang app.config.

## What's it look like to users

Riak uses the semantic of $conf_dir/app.config for configuration. We're going to expand on that.

The same directory will ship with a `riak.conf` file, with a syntax that looks something like this:

```
ring_size = 32
anti_entropy = debug
log.error.file = /var/log/error.log
log.console.file = /var/log/console.log
log.syslog = on
```

What do those mean? As a Riak developer, you defined them in the config schema like this:

```erlang
%% example of super basic mapping
%% @doc Default ring creation size.  Make sure it is a power of 2,
%% @datatype integer
%% @mapping riak_core.ring_creation_size
{ "ring_size", 64 }.
 
%% Slightly more complex mapping with translation layer
%% @doc enable active anti-entropy subsystem
%% @datatype enum on, off
%% @mapping riak_kv.anti_entropy
{ "anti_entropy", on }.
 
%% @translation
{ "riak_kv.anti_entropy",
  fun(Conf) ->
  	Setting = proplists:get_value("anti_entropy", Conf), 
  	case Setting of
  		on -> {on, []};
  		debug -> {on, [debug]};
  		off -> {off, []};
  		_Default -> {on, []}
  	end
  end
}.
 
%% complex lager example
%% @doc location of the console log
%% @mapping lager.handlers
{ "log.console.file", "./log/console.log"}.
 
%% *gasp* notice the same @mapping!
%% @doc location of the error log
%% @mapping lager.handlers
{ "log.error.file", "./log/error.log"}.
 
%% *gasp* notice the same @mapping!
%% @doc turn on syslog
%% @datatype enum on, off
%% @mapping lager.handlers
{ "log.syslog", off}.
 
%% @translation
{ "lager.handlers",
	fun(Conf) ->
		SyslogHandler = case proplists:get_value("log.syslog", Conf) of
			on ->  {lager_syslog_backend, ["riak", daemon, info]};
			_ -> undefined
		end,
		ErrorHandler = case proplists:get_value("log.error.file", Conf) of
			undefined -> undefined;
			ErrorFilename -> {lager_file_backend, [{file, ErrorFilename}, {level, error}]}
		end,
        ConsoleHandler = case proplists:get_value("log.console.file", Conf) of
        	undefined -> undefined;
        	ConsoleFilename -> {lager_file_backend, [{file, ConsoleFilename}, {level, info}]}
        end,
        lists:filter(fun(X) -> X =/= undefined end, [SyslogHandler, ErrorHandler, ConsoleHandler]) 
	end
}.
```

These tuples break down into two types. `@mapping`s and `@translation`s. I'm not married to the names.

### @mapping
`@mapping`s describe lines in the `riak.conf` file. the `@mapping` annotation tells you where in the riak `app.config` this thing belongs. the `@mapping` location doesn't have to navigate down the `app.config` to a specific value, but if it navigates down to a complex datatype (e.g. `lager.handlers`), you'd better have an `@translation` for "lager.handlers"

It also tells you about about the `@datatype` annotation tells it how to cast values for app.config.

### @translation
Translations are made up of {string(), fun(proplist())}, The proplist is the set of all key value pairs in the `riak.conf` file. The fun/1 then uses that to build, possibly a complex datastructure, based on any number of config keys.

Here's a simple example for `anti_entropy`

```erlang
%% @translation
{ "riak_kv.anti_entropy",
  fun(Conf) ->
  	Setting = proplists:get_value("anti_entropy", Conf), 
  	case Setting of
  		on -> {on, []};
  		debug -> {on, [debug]};
  		off -> {off, []};
  		_Default -> {on, []}
  	end
  end
}.
```

All this does is translate the `riak.conf` choices "on, debug, off" into the tuples expected in the `app.config`

Here's something more complex, logging.

```erlang
%% @translation
{ "lager.handlers",
	fun(Conf) ->
		SyslogHandler = case proplists:get_value("log.syslog", Conf) of
			on ->  {lager_syslog_backend, ["riak", daemon, info]};
			_ -> undefined
		end,
		ErrorHandler = case proplists:get_value("log.error.file", Conf) of
			undefined -> undefined;
			ErrorFilename -> {lager_file_backend, [{file, ErrorFilename}, {level, error}]}
		end,
        ConsoleHandler = case proplists:get_value("log.console.file", Conf) of
        	undefined -> undefined;
        	ConsoleFilename -> {lager_file_backend, [{file, ConsoleFilename}, {level, info}]}
        end,
        lists:filter(fun(X) -> X =/= undefined end, [SyslogHandler, ErrorHandler, ConsoleHandler]) 
	end
}.
```
This describes what the entire "lager.handlers" list looks like in the `app.config` inside the the fun.

I think multibackend config is the only thing left that is more challenging than that.

*The values in riak.conf overwrite values in app.config*

## So where's app.config?

Well, there's a couple. The default app.config currently shipped with Riak will be hidden away somewhere Riak knows about. It will contain the default settings. The `riak.conf` file is currently parsed and overlaid on top of this config file.

But right now we only map two values, and some might be too complexly nested to hit in the first phase of this. This is why we are planning on adding the ability to place an `app.config` next to the `riak.conf` file, which will allow you to override the default app.config that ships with Riak, but just the places you need to. I think that multibackend configuration is going to be the poster child for this advanced configuration file.

To avoid confusion, we'll call the file that ships inside Riak as `default.config` and the file that sits next to `riak.conf` `advanced.config`. Anything with the `.config` extention means a standard erlang app.config syntax.

So, these three files are essentially merged into a generated app.config by a little vm before Riak starts. Then the Riak erlang vm will start up using the generated app.config

We'll also eventually be able to source `vm.args` from the `riak.conf` also, but baby steps.



### P.S. Names are placeholders. 
I don't intend to ship bjorn_schema:tyktorp/3; however, it wouldn't be the worst thing ;)