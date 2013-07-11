# Bjorn
Knows all about your options

## Motivation
Born of a desire to make configuraion of Riak easier on the masses, this project strives to help developers merge the idea of a user facing configuration file with that of an erlang app.config.

## What's it look like to users

Riak uses the semantic of $conf_dir/app.config for configuration. We're going to expand on that.

The same directory will ship with a `riak.conf` file, with a syntax that looks something like this:

```
anti_entropy = on
ring_size = 32
```

What do those mean? As a Riak developer, you defined them in the config schema like this:

```
%% @doc enable active anti-entropy subsystem
%% @datatype enum on, off
%% @mapping riak_kv.anti_entropy{1}
{ "anti_entropy", on}.

%% @doc Default ring creation size.  Make sure it is a power of 2,
%% @datatype integer
%% @mapping riak_core.ring_creation_size
{ "ring_size", 64}.
```

So far, the `@mapping` annotation supports dot separated proplist nesting, and this `{N}` curly brace syntax which means, "this maps to the Nth element of the tuple stored here"

The above schema is just saying that a riak.conf's "anti_entropy" key maps to the 1st element of the tuple stored in the app.config's [{riak_kv, [{anti_entropy, {on, []}}]}]. It also says that "ring_size" maps to [{riak_core, [{ring_creation_size, 64}]}].

The `@datatype` annotation tells it how to cast values for app.config.

*The values in riak.conf overwrite values in app.config*

## So where's app.config?

Well, there's a couple. The default app.config currently shipped with Riak will be hidden away somewhere Riak knows about. It will contain the default settings. The `riak.conf` file is currently parsed and overlaid on top of this config file.

But right now we only map two values, and some might be too complexly nested to hit in the first phase of this. This is why we are planning on adding the ability to place an `app.config` next to the `riak.conf` file, which will allow you to override the default app.config that ships with Riak, but just the places you need to. I think that multibackend configuration is going to be the poster child for this advanced configuration file.

To avoid confusion, we'll call the file that ships inside Riak as `default.config` and the file that sits next to `riak.conf` `advanced.config`. Anything with the `.config` extention means a standard erlang app.config syntax.

So, these three files are essentially merged into a generated app.config by a little vm before Riak starts. Then the Riak erlang vm will start up using the generated app.config

We'll also eventually be able to source `vm.args` from the `riak.conf` also, but baby steps.



### P.S. Names are placeholders. 
I don't intend to ship bjorn_schema:tyktorp/3; however, it wouldn't be the worst thing ;)