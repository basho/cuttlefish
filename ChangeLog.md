# Cuttlefish Change Log

## 3.0.0 (Mar 13, 2021)

### OTP Logger Instead of Lager

The library now uses standard OTP logger instead of Lager.

Log entries use a very similar format but the timestamp
formatting has changed.

In older releases with Lager:

```
15:12:26.054 [info] No app.config or vm.args detected in /etc, activating cuttlefish
```

In 3.0, on a cutting edge Erlang version:

```
2021-03-08T15:14:55.963768+03:00 [info] No app.config or vm.args detected in /etc, activating cuttlefish
```

GitHub issue: [#19](https://github.com/Kyorai/cuttlefish/issues/19).


## 2.7.0 (Mar 7, 2021)

### Erlang 24 Support

Cuttlefish is now compatible (builds, runs) with Erlang 24.

### Older Erlang Releases Support Dropped

Cuttlefish now supports Erlang 22 through 24. `2.6.0` was the last
release to support older Erlang versions, e.g. 18 or 19.