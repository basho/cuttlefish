# Cuttlefish
Cuttlefish is a library for erlang applications that wish to walk the fine line between erlang `app.config`s and a sysctl-like syntax. The name is a pun on the pronunciation of 'sysctl' and jokes are better explained.

## Riak Disclaimer
While this readme and test suite is riak-heavy, the fact is that this library can be used with any erlang application that wants a more universally accessible configuration syntax. Still, I built this for Riak, and it's nice to have a concrete example to work with.

## The Vision
Currently, Riak's `app.config` is **the** definitive place for configuring Riak. It's not odd for Erlang applications to be configured this way, but it is a struggle for non-Erlang programmers and automated deployment tools to manipulate these files. On the other hand, the `app.config` is a useful construct for Erlang programmers, and it is pretty coupled to OTP applications.

Cuttlefish's goal is to put a layer of abstraction on top of the `app.config` that is easier to work with outside of the erlang world. It will allow Erlang programmers to write a Schema for their application's configuration file, which is independent of the applications included in the project. The Schema is one of the more important parts of Cuttlefish, so we'll go into more detail on it below, but it is written in Erlang and defines how the non-Erlang configuration file works.

From this Schema, you can generate a default `.conf` file for your application. This will be the file that is packaged with your application as the default configuration.

The Schema is also used to generate an `app.config` that will be used to start your application. Using the Schema alone will generate all the proper defaults. Your users can make changes to the `.conf` file and those changes will overwrite the Schema's defaults.

**TODO**: We plan on adding the ability to also have an `advanced.config` which looks like the old `app.config` for anything that no Schema mapping is created for.

What does this look like for an application like Riak?

Well, the authors of Riak maintain a Schema for Riak's config. It defines all sorts of things we'll get into later. When we build Riak, Cuttlefish generates a `riak.conf` file that contains the default shipping configuration of Riak. When a script to start Riak is run, a little Erlang VM is spun up, reads the `riak.conf` file and uses Cuttlefish to combine that with the Schema and generate an `app.config`. That first vm is then stopped, and a new VM (destined to run Riak) is started with that generated `app.config`. Down the line somewhere, you may be troubleshooting some part of Riak, and the support organization at Basho may need you to manipulate a configuration setting that is not exposed by the Schema beacause it is so infrequently used. In that case, we can set that setting directly in an `advanced.config` which sits in the same directory as `riak.conf`.

I hope that gives you a good idea about how this works at a high level. Now let's get into Schemas

## The Schema

There are two types of Schema elements in Cuttlefish: `mapping` and `translation`. It's easy to tell the difference! They're bolth tuples, and the first element is an atom, either `mapping` or `translation`. You're welcome.

## Mappings

First of all, there's one annotation respected by Cuttlefish and that's `@doc`. If you write a multiline `@doc` it will be included in your generated `.conf` file. One day it could even be available programatically. We chose to make it an annotation because as Erlangers, you already know and love `@doc` AND we didn't want you to worry about multiline strings and an array of strings as a member of a proplist. This just seemed cleaner.

Aside from documentation, there is plenty going on with mappings, but the basic form is as follows:
```erlang
%% {atom(), string(), string(), proplist()}
Mapping = {mapping, ConfKey, ErlangConfMapping, Attributes}. 
```

* `element(1, Mapping) = mapping`
* `element(2, Mapping) = ConfKey` - the string key that you want this setting to have in the `.conf` file
* `element(3, Mapping) = ErlangConfMapping` - the nested location of the thing in the `app.config` that this field represents
* `element(4, Mapping) = Attributes` - other helpful things we'll go into right... about... now!

Attributes is a proplist, and let's assume you know how those work. Here are the keys in that proplist that we work with:
* `default` - This is the default value for the setting. If it's not defined, then it is not generated in the `app.config`
* `commented` - If this is defined, then when you generate a `.conf` file the documentation for this setting appears, along with the setting, but the setting is commented out and the value is set to this value.
* `datatype` - This is the datatype for the field
* `enum` - If the datatype is `enum`, then this is the set of valid values for this field.
* `advanced` - If this is set to true, this value will be in the generated `app.config`, but not the generated `.conf` file. It still can be overridden in the `.conf` file, you just have to know about it. It's a ways of adding "undocumented knobs"
* `include_default` - If there is a substitutable value in the ConfKey, in the generated `.conf` file, this value is substituted. (don't worry if that last one didn't make so much sense now)

The best way to get it, is to take a look at some examples. Let's start with riak's ring_size.

```erlang
%% example of super basic mapping
%% @doc Default ring creation size.  Make sure it is a power of 2,
%% e.g. 16, 32, 64, 128, 256, 512 etc
{mapping, "ring_size", "riak_core.ring_creation_size", [
  {datatype, integer},
  {commented, "64"}
]}.
```

First of all, comments before the `@doc` annotation are ignored, so feel free to put Schema specific comments in here as you see fit. Everything **after** the `@doc` in the comments, is part of the documentation. Cuttlefish will treat this documentation as:

```erlang
[
  "Default ring creation size.  Make sure it is a power of 2,",
  "e.g. 16, 32, 64, 128, 256, 512 etc"
]
``` 

Then, we can also see from `element(1)` that this is a mapping. `element(2)` says that it's represented by "ring_size" in the `riak.conf` file. `element(3)` says that it's "riak_core.ring_creation_size" in the `app.config`. We also know from the attibutes that it is an integer, and that it will appear in the generated `riak.conf` file with a value of 64. It just so happens that the default is also 64, but that's specified in riak_core's app.src. 

Let's talk about `element(3)` here for a minute. What that means is that there's an `app.config` out there that looks like this:

```erlang
[
  {riak_core, [
    {ring_creation_size, X}
  ]}
].
```
and that we're concerned with X.

Now, if life were as simple as 1:1 mappings like this, we'd be done. But it's not, and so we need to introduce translations.

## Lost in Translations

Actually, they're pretty easy.

A translation looks like this:

```erlang
%% {atom(), string, fun(proplist()) -> term()}
Translation = {translation, ErlangConfMapping, TranslationFunction}.
```

Let's break it down:
* `element(1, Translation) = translation` - WHAT IS THIS?! :P
* `element(2, Translation) = ErlangConfMapping` this is the same as the corresponding ErlangConfMapping in the mapping above. This is how we tie back to a mapping
* `element(2, Translation) = TranslationFunction` this is a fun() that takes in a proplist representing the `.conf` file and returns an erlang term. This erlang term will be the value of the ErlangConfMapping.

Ok, that does sound more confusing than it should. Let's take a look at one, you'll like it better in practice.

```erlang
%% Slightly more complex mapping with translation layer
%% @doc enable active anti-entropy subsystem
{mapping, "anti_entropy", "riak_kv.anti_entropy", [
  {datatype, enum},
  {enum, ["on", "off", "debug"]},
  {default, "on"}
]}.

{ translation,
  "riak_kv.anti_entropy",
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

See what's happening? First of all, you need a mapping. If you don't have one, don't bother writing a translation for it.
The mapping we defined for "anti_entropy" says that it's an enum with values "on", "off", and "debug". The configuration in the `app.config` is more complicated. Basically, it works like this:

* on - {on, []}
* off - {off, []}
* debug - {on, [debug]}

It's a relatively simple translation, but we want to spare non-Erlangers from this very Erlangy syntax. So, we give them the values "on", "off", and "debug" and the translation "translates" them into the erlang value we expect.

## The Curious Case of Lager Config

There are other cases when multiple values turn into a single `app.config` complex data structure. Take lager as an example.

```erlang
%% complex lager example
%% @doc location of the console log
{mapping, "log.console.file", "lager.handlers", [
  {default, "./log/console.log"}
]}.

%% *gasp* notice the same @mapping!
%% @doc location of the error log
{mapping, "log.error.file", "lager.handlers", [
  {default, "./log/error.log"}
]}.

%% *gasp* notice the same @mapping!
%% @doc turn on syslog
{mapping, "log.syslog", "lager.handlers", [
  {default, "off"},
  {datatype, enum},
  {enum, ["on", "off"]}
]}.

{ translation,
  "lager.handlers",
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

We define three mappings here, that have different values in the `riak.conf` file, but represent a complex list of lager handlers in the `app.config`. The solution is to have them all map to the same ErlangConfMapping, which references `lager.handlers`. When we create a translation for that, we're basically saying that "The return value of this function will be the value of {lager, [{handers, X}]}". that was a weird way of saying it, but the generated `app.config` looks like this:

```erlang
 {lager,
     [
      {handlers,
          [{lager_syslog_backend,["riak",daemon,info]},
           {lager_file_backend,[{file,"/var/log/error.log"},{level,error}]},
           {lager_file_backend,[{file,"/var/log/console.log"},{level,info}]}]},
          ]},
```

## What's it look like to users

Riak uses the semantic of $conf_dir/app.config for configuration. We're going to  replace that with a file called `riak.conf`, with a syntax that looks like this:

```
ring_size = 32
anti_entropy = debug
log.error.file = /var/log/error.log
log.console.file = /var/log/console.log
log.syslog = on
```

### High Level Syntax Definition
* sysctl-like syntax
* X = Y, split on first `=`
* one setting per line for easy scripting
