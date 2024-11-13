# Flow / FlowVM (`.flow/.fvm`)

A language that is highly inspired by Golang's Goroutines and Channels.

It will be a compiled, garbage collected language implemented in Zig. 

It is primarily a personal project to learn how to implement concurrency modules on top of threads,
as well as how to make and design a toy language from scratch.

## Features and Details

Non-exhaustive list of some of the biggest and most important features planned:

- Easy, Colorless concurrency model
- Gargabe Collection
- Byte-Code VM shipped in the final executable
- Uses the Zig Buildsystem for Compilation
- Extendable VM (`.fvm`-Files)
- Type-Safe

## Syntax and Grammar

TODO:

- types
- primitives
- variables, constants
- blocks
- compile errors
- if/else
- loops (while)
- function
- closures (php inspired syntax)
- channels
- async-/go-like keyword to execute a function concurrently
- arrays/lists
- structs/objects
- methods (?), would be just syntax sugar for functions with a special parameter
- VM Extensions
- Error Handling (as Values? special syntax like Zig?)

## Compile Steps

The compilation Process will be split into different steps

1. Lexing/Tokenizing
2. AST
3. Different IR with optimisations and more infos
4. Byte Code

