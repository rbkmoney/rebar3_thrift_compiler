# Rebar3 Thrift compiler [![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A rebar plugin to compile Thrift IDL files into Erlang source code and if you like to, clean after itself.

## Build

    $ rebar3 compile

## Use

Add the plugin to your `rebar.config`:

```erlang
{plugins, [
    {rebar3_thrift_compiler, {git, "https://github.com/rbkmoney/rebar3_thrift_compiler.git", {tag, "0.1"}}}
]}.
```

Then just call your plugin directly in an existing application:

```
$ rebar3 thrift compile -I ./proto -- left_pad_service.thrift
===> Fetching rebar3_thrift_compiler
===> Compiling rebar3_thrift_compiler
$ ls -1 ./src
left_pad_service_types.erl
left_pad_service_thrift.erl
...
$ ls -1 ./include
left_pad_service_constants.hrl
left_pad_service_types.hrl
...
$ rebar3 thrift clean
$ ls -1 ./src
...
$ ls -1 ./include
...
```

Alternatively you can hook plugin calls via `rebar.config`, just add these to the mix:

```erlang
{thrift_compiler_opts, [
    {in_files, ["left_pad_service.thrift"]}
]}.

{provider_hooks, [
    {pre, [
        {compile, {thrift, compile}},
        {clean, {thrift, clean}}
    ]}
]}.
```

Full list of options you can change to your taste, you can always fire `rebar3 help thrift ...` to see it:

```erlang
{thrift_compiler_opts, [
    % Directory where *.thrift files are located, relative to application root
    {in_dir, "proto"},
    % Explicit list of files to compile instead of every single *.thrift file found inside the `in_dir`
    {in_files, ["file1.thrift", ...]},
    % Directory to put all generated *.erl files into, relative to application output directory
    {out_erl_dir, "src"},
    % Directory to put all generated *.hrl files into, relative to application output directory
    {out_hrl_dir, "include"},
    % Generator (with arbitrary flags) to use when compiling *.thrift files
    {gen, "erl:legacy_names"}
]}.
```
