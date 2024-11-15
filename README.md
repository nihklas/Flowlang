# Flow / FlowVM (`.flow/.fvm`)

A language that is highly inspired by Golang's Goroutines and Channels.

It will be a compiled, garbage collected language implemented in Zig. 

It is primarily a personal project to learn how to implement concurrency models on top of threads,
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

[Current Grammar](https://bnfplayground.pauliankline.com/?bnf=%3CDIGIT%3E%20%3A%3A%3D%20%5B0-9%5D%0A%3CALPHA%3E%20%3A%3A%3D%20%5Ba-z%5D%20%7C%20%5BA-Z%5D%20%7C%20%22_%22%0A%3CNUMBER%3E%20%3A%3A%3D%20%3CDIGIT%3E%20%20(%22.%22%20%3CDIGIT%3E%20)%3F%0A%3CIDENTIFIER%3E%20%3A%3A%3D%20%3CALPHA%3E%20(%3CALPHA%3E%20%7C%20%3CDIGIT%3E)*%0A%2F*%20Any%20character%20between%20the%20quotes%20*%2F%0A%3CSTRING%3E%20%3A%3A%3D%20%22%5C%22%22%20(%5Ba-z%5D%20%7C%20%5BA-Z%5D%20%7C%20%5B0-9%5D)*%20%22%5C%22%22%0A%3CBOOL%3E%20%3A%3A%3D%20%22true%22%20%7C%20%22false%22%0A%3CNULL%3E%20%3A%3A%3D%20%22null%22%0A%3CWS%3E%20%3A%3A%3D%20(%22%20%22%20%7C%20%22%5Cn%22)%2B%0A%0A%3Cprimary%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%7C%20%3CSTRING%3E%20%7C%20%3CNUMBER%3E%20%7C%20%3CBOOL%3E%20%7C%20%3CNULL%3E%20%7C%20%22this%22%20%7C%20%22(%22%20%3Cexpression%3E%20%22)%22%0A%3Ccall%3E%20%3A%3A%3D%20%3Cprimary%3E%0A%3Cunary%3E%20%3A%3A%3D%20(%22!%22%20%7C%20%22-%22)%3F%20%3Ccall%3E%0A%3Cfactor%3E%20%3A%3A%3D%20%3Cunary%3E%20%3CWS%3E%3F%20((%22%2F%22%20%7C%20%22*%22)%20%3CWS%3E%3F%20%3Cunary%3E)*%0A%3Cterm%3E%20%3A%3A%3D%20%3Cfactor%3E%20%3CWS%3E%3F%20((%22%20%22%20%7C%20%22-%22)%20%3CWS%3E%3F%20%3Cfactor%3E)*%0A%3Ccomparison%3E%20%3A%3A%3D%20%3Cterm%3E%20%3CWS%3E%3F%20((%22%3C%22%20%7C%20%22%3C%3D%22%20%7C%20%22%3E%3D%22%20%7C%20%22%3E%22)%20%3CWS%3E%3F%20%3Cterm%3E)*%0A%3Cequality%3E%20%3A%3A%3D%20%3Ccomparison%3E%20%3CWS%3E%3F%20((%22%3D%3D%22%20%7C%20%22!%3D%22)%20%3CWS%3E%3F%20%3Ccomparison%3E)*%0A%3Cand%3E%20%3A%3A%3D%20%3Cequality%3E%20(%3CWS%3E%20%22and%22%20%3CWS%3E%20%3Cequality%3E)*%0A%3Cor%3E%20%3A%3A%3D%20%3Cand%3E%20(%3CWS%3E%20%22or%22%20%3CWS%3E%20%3Cand%3E)*%0A%3Cassignment%3E%20%3A%3A%3D%20(%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%22%3D%22%20%3CWS%3E%3F%20%3Cassignment%3E)%20%7C%20%3Cor%3E%0A%0A%3Cexpression%3E%20%3A%3A%3D%20%3Cassignment%3E%0A%0A%3Cstatement%3E%20%3A%3A%3D%20%3CexprStatement%3E%20%0A%09%7C%20%3CblockStatement%3E%20%0A%20%20%20%20%7C%20%3CwhileStatement%3E%20%0A%20%20%20%20%7C%20%3CifStatement%3E%20%0A%20%20%20%20%7C%20%3CreturnStatement%3E%0A%0A%3CexprStatement%3E%20%3A%3A%3D%20%3Cexpression%3E%20%22%3B%22%0A%3CblockStatement%3E%20%3A%3A%3D%20%22%7B%22%20%3CWS%3E%3F%20%3CexprStatement%3E*%20%3CWS%3E%3F%20%22%7D%22%0A%3CwhileStatement%3E%20%3A%3A%3D%20%22while%22%20%3CWS%3E%3F%20%22(%22%20%3CWS%3E%3F%20%3Cexpression%3E%20%3CWS%3E%3F%20%22)%22%20%3CWS%3E%3F%20%3Cstatement%3E%0A%3CifStatement%3E%20%3A%3A%3D%20%22if%22%20%3CWS%3E%3F%20%22(%22%20%3CWS%3E%3F%20%3Cexpression%3E%20%3CWS%3E%3F%20%22)%22%20%3CWS%3E%3F%20%3Cstatement%3E%20%3CWS%3E%3F%20(%22else%22%20%3CWS%3E%3F%20%3Cstatement%3E)%3F%0A%3CreturnStatement%3E%20%3A%3A%3D%20%22return%22%20%3CWS%3E%20%3Cexpression%3E%20%22%3B%22%0A%0A%3Cdeclaration%3E%20%3A%3A%3D%20%3Cstatement%3E%0A%0A%3CtypeHint%3E%20%3A%3A%3D%20%22%3A%22%20%3CWS%3E%3F%20%3CIDENTIFIER%3E%0A%3CvarDeclaration%3E%20%3A%3A%3D%20(%22var%22%20%7C%20%22const%22)%20%3CWS%3E%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%3CtypeHint%3E%3F%20%3CWS%3E%3F%20(%22%3D%22%20%3CWS%3E%3F%20%3Cexpression%3E)%3F%20%22%3B%22&name=)

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

// Unary
!, -
```

### Variables / Constants

```
var x: int = 1;
var y: int;
var z = 1;

const a: string = "hello";
const b: string;
const c = "world";
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

