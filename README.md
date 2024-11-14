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

### Types

- bool
- int
- float
- string
- channel
- struct (later)
- array (later)

### Literals

- Digits/Numbers (`1`, `1234`, `12.34`)
- String Literals (`"String"`) 
- `true`/`false`
- `null`

### Operators

```
// arithmetic
+, -, *, /

// Comparison
==, <=, >=, != 

// Logical
and, or
```

### Control Structures

Blocks:
```
{
    // This is a block
}
```

Conditionals:
```
if (condition) block

---

if (condition) block
else block
```

Loops:

```
while (condition) block

---

for (initializer;condition;continue-expr) block
```

Functions:

```
func identifier(parameters) returntype block

---

// closure / anonymous function
func(parameters) use (closure-values) returntype block

---

// method on a struct
meth identifier(parameters) returntype block
```

Channel:

```
channel identifier(options)

--- 

// (chn is a channel "object")

// reading
chn -> variable

// writing
chn <- value
```

### Async function-call

```
flow call()
```

### Errors

`err` is a keyword, which itself is the value of the current error in a catch block. It can also be
used to create or reference an Error

```
func() catch block

try func()
// this is the same as:
func() catch return err

//Errors must be specified in the return type with an Exclamation Mark (`!`)
func thisMayFail() !void block

//To create an error:
var myError = err.MyErrorName
if (myError == err.MyErrorName) block
```


### TODO:

- types
- primitives
- variables, constants
- blocks
- compile errors
- conditional
- loops (while)
- function
- closures (php inspired syntax)
- channels
- async-/go-like keyword to execute a function concurrently
- arrays/lists
- structs/objects
- methods, would be just syntax sugar for functions with a special parameter
- VM Extensions
- Error Handling (as Values? special syntax like Zig?)

## Compile Steps

The compilation Process will be split into different steps

1. Lexing/Tokenizing
2. AST
3. Different IR with optimisations and more infos
4. Byte Code

