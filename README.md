# FlowLang

[![Lines of Code](https://sloc.xyz/gitlab/flowlang/flowlang/)](https://gitlab.com/flowlang/flowlang/)
[![Pipeline Status](https://gitlab.com/flowlang/flowlang/badges/main/pipeline.svg)](https://gitlab.com/flowlang/flowlang/)

A programming language inspired by Golang's Goroutines and Channels.

It is a compiled, garbage collected language implemented in Zig.

It is Ahead-Of-Time compiled, but ships with a Runtime in the same executable.

## Features

- Garbage Collected
- Bytecode VM with comparable performance to Python (at the moment)
- Extensions written in Zig
- Type-Safety

## Planned Features

- Multiple GCs
- Concurrency, similar to Go's Goroutines
- JIT-Compiler
- WASM Compiler/Runtime

## Try it out!

```flow
print("Hello World!");
```

```flow
// Bigger example
```


Take a look at the `example/` directory. There you can find `src/main.flow` as a
basic starting point to get familiar with the syntax of flow. Additionally, you
can look at the different test cases in `tests/cases/` to get a feel of how some
of the stuff behaves.

If you are using `nix`, you can start an ad-hoc shell environment including
`flowc` with this command:

```bash
nix shell gitlab:flowlang/flowlang
```

Otherwise, you need to install Flow from source, by cloning this Repository and
running `zig build`. After that, the compiler is located at
`zig-out/bin/compiler`.

### Extensions

Flow supports extensions for the runtime and compiler. That means, you can write
Zig Modules to implement performance critical tasks and export them to be
globally available functions in Flow-Land. For an example on how to write an
extension yourself, you can look at [betterAdd](https://gitlab.com/flowlang/betterAdd).

For an in-depth explanation and tutorial look at the [Extension
Documentation](/docs/Extensions.md).

