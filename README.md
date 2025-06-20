# FlowLang

[![Lines of Code](https://sloc.xyz/gitlab/flowlang/flowlang/)](https://gitlab.com/flowlang/flowlang/)
[![Pipeline Status](https://gitlab.com/flowlang/flowlang/badges/main/pipeline.svg)](https://gitlab.com/flowlang/flowlang/)

A language that is highly inspired by Golang's Goroutines and Channels.

It will be a compiled, garbage collected language implemented in Zig. 

It is primarily a personal project to learn how to implement concurrency models on top of threads,
as well as how to make and design a toy language from scratch.

## Features and Details

Non-exhaustive list of some of the biggest and most important features planned:

- Easy, Colorless concurrency model
- Gargabe Collection
- Byte-Code VM shipped in the final executable
- Extendable VM (through Zig Modules)
- Type-Safe

## Try it out!

The file `example/src/main.flow` shows the current possible things that the flow compiler supports.

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

You can fetch the module via Zig's package manager:

```bash
zig fetch --save git+https://gitlab.com/flowlang/betteradd
```

You also need a Zig file, where you re-export all the functions you need. That
way, you can also import a Module, but only use a single function, and the rest
of the Module will not be included in the compiled output. The `exports.zig` of
the example looks simply like this:

```zig
const lib = @import("betterAdd");

pub const betterAdd = lib.betterAdd;
```

This `exports.zig` includes all modules exports the functions that are needed.
You can define different names for these functions in here as well, to avoid
naming collisions.

Here is the `build.zig` to build the compiler with the extension enabled.

```zig
const std = @import("std");
const flow = @import("flow");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const betterAdd = b.dependency("betterAdd", .{});

    const flow_dep = b.dependency("flow", .{});

    const compiler = flow.buildCompiler(b, flow_dep.builder, .{
        .target = target,
        .optimize = optimize,
        .extensions = .{
            .modules = &.{
                .{ .name = "betterAdd", .module = betterAdd.module("betterAdd") },
            },
            .exports_file = b.path("exports.zig"),
        },
    });

    b.installArtifact(compiler);
}
```

To add more extensions, you just have to add them as a dependency via `zig fetch`,
re-export the functions you want to use and add another entry in the `.modules`
array.

## Syntax and Grammar

[Current Grammar](https://bnfplayground.pauliankline.com/?bnf=%3CDIGIT%3E%20%3A%3A%3D%20%5B0-9%5D%0A%3CALPHA%3E%20%3A%3A%3D%20%5Ba-z%5D%20%7C%20%5BA-Z%5D%20%7C%20%22_%22%0A%3CNUMBER%3E%20%3A%3A%3D%20%3CDIGIT%3E*%20%20(%22.%22%20%3CDIGIT%3E%2B%20)%3F%0A%3CIDENTIFIER%3E%20%3A%3A%3D%20%3CALPHA%3E%20(%3CALPHA%3E%20%7C%20%3CDIGIT%3E)*%0A%2F*%20Any%20character%20between%20the%20quotes%20*%2F%0A%3CSTRING%3E%20%3A%3A%3D%20%22%5C%22%22%20(%5Ba-z%5D%20%7C%20%5BA-Z%5D%20%7C%20%5B0-9%5D)*%20%22%5C%22%22%0A%3CBOOL%3E%20%3A%3A%3D%20%22true%22%20%7C%20%22false%22%0A%3CNULL%3E%20%3A%3A%3D%20%22null%22%0A%2F*%20Whitespace%20gets%20ignored%20for%20the%20most%20part%2C%20this%20rule%20is%20needed%20for%20this%20Editor%20*%2F%0A%3CWS%3E%20%3A%3A%3D%20(%22%20%22%20%7C%20%22%5Cn%22)%2B%0A%2F*%20There%20is%20no%20EOF%20Symbol%20in%20this%20Editor%20*%2F%0A%3CEOF%3E%20%3A%3A%3D%20%22%20%22%0A%0A%2F*%20Utilities%20*%2F%0A%3CtypeHint%3E%20%3A%3A%3D%20%22%3A%22%20%3CWS%3E%3F%20%22%5B%5D%22%3F%20%3CIDENTIFIER%3E%0A%3CparamHint%3E%20%3A%3A%3D%20%3CtypeHint%3E%20%3CWS%3E%3F%20%22channel%22%3F%0A%3Carguments%3E%20%3A%3A%3D%20%3Cexpression%3E%20(%22%2C%22%20%3CWS%3E%3F%20%3Cexpression%3E)*%0A%3Cparameters%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%3CparamHint%3E%20(%22%2C%22%20%3CWS%3E%3F%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%3CparamHint%3E)*%0A%0A%3Cprimary%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%7C%20%3CSTRING%3E%20%7C%20%3CNUMBER%3E%20%7C%20%3CBOOL%3E%20%7C%20%3CNULL%3E%20%7C%20%22this%22%20%7C%20%22(%22%20%3Cexpression%3E%20%22)%22%20%7C%20%22%5B%22%20%3Cexpression%3E*%20%22%5D%22%0A%3Ccall%3E%20%3A%3A%3D%20%3Cprimary%3E%20(%22(%22%20%3Carguments%3E%3F%20%22)%22)*%0A%3Cunary%3E%20%3A%3A%3D%20((%22!%22%20%7C%20%22-%22)%20%3Cunary%3E)%20%7C%20%3Ccall%3E%0A%3Cfactor%3E%20%3A%3A%3D%20%3Cunary%3E%20%3CWS%3E%3F%20((%22%2F%22%20%7C%20%22*%22)%20%3CWS%3E%3F%20%3Cunary%3E)*%0A%3Cterm%3E%20%3A%3A%3D%20%3Cfactor%3E%20%3CWS%3E%3F%20((%22%2B%22%20%7C%20%22-%22)%20%3CWS%3E%3F%20%3Cfactor%3E)*%0A%3Ccomparison%3E%20%3A%3A%3D%20%3Cterm%3E%20%3CWS%3E%3F%20((%22%3C%22%20%7C%20%22%3C%3D%22%20%7C%20%22%3E%3D%22%20%7C%20%22%3E%22)%20%3CWS%3E%3F%20%3Cterm%3E)*%0A%3Cequality%3E%20%3A%3A%3D%20%3Ccomparison%3E%20%3CWS%3E%3F%20((%22%3D%3D%22%20%7C%20%22!%3D%22)%20%3CWS%3E%3F%20%3Ccomparison%3E)*%0A%3Cand%3E%20%3A%3A%3D%20%3Cequality%3E%20(%3CWS%3E%20%22and%22%20%3CWS%3E%20%3Cequality%3E)*%0A%3Cor%3E%20%3A%3A%3D%20%3Cand%3E%20(%3CWS%3E%20%22or%22%20%3CWS%3E%20%3Cand%3E)*%0A%3Cconcat%3E%20%3A%3A%3D%20%3Cor%3E%20(%3CWS%3E%20%22.%22%20%3CWS%3E%20%3Cor%3E)*%0A%3Cassignment%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%22%3D%22%20%3CWS%3E%3F%20%3Cexpression%3E%0A%3Cappend%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%22%5B%5D%22%20%3CWS%3E%3F%20%22%3D%22%20%3Cexpression%3E%0A%0A%3Cexpression%3E%20%3A%3A%3D%20%3Cassignment%3E%20%7C%20%3Cappend%3E%20%7C%20%3Cconcat%3E%0A%0A%3Cstatement%3E%20%3A%3A%3D%20%3CexprStmt%3E%20%0A%20%20%20%20%7C%20%3CforStmt%3E%20%0A%20%20%20%20%7C%20%3CbreakStmt%3E%0A%20%20%20%20%7C%20%3CcontinueStmt%3E%0A%20%20%20%20%7C%20%3CifStmt%3E%20%0A%20%20%20%20%7C%20%3CreturnStmt%3E%0A%20%20%20%20%7C%20%3CchnReadStmt%3E%0A%20%20%20%20%7C%20%3CchnWriteStmt%3E%0A%09%7C%20%3Cblock%3E%20%0A%0A%3CexprStmt%3E%20%3A%3A%3D%20%3Cexpression%3E%20%22%3B%22%0A%3Cblock%3E%20%3A%3A%3D%20%22%7B%22%20%3CWS%3E%3F%20%3Cdeclaration%3E*%20%3CWS%3E%3F%20%22%7D%22%0A%3CforStmt%3E%20%3A%3A%3D%20%22for%22%20%3CWS%3E%20(%3CexprStmt%3E%20%7C%20%3CvarDecl%3E%20%7C%20%22%3B%22)%20%3CWS%3E%3F%20%3Cexpression%3E%3F%20%3CWS%3E%3F%20%22%3B%22%20%3CWS%3E%3F%20%3Cstatement%3E%3F%20%3CWS%3E%3F%20%3Cstatement%3E%0A%3CbreakStmt%3E%20%3A%3A%3D%20%22break%22%20%3CWS%3E%3F%20%22%3B%22%0A%3CcontinueStmt%3E%20%3A%3A%3D%20%22continue%22%20%3CWS%3E%3F%20%22%3B%22%0A%3CifStmt%3E%20%3A%3A%3D%20%22if%22%20%3CWS%3E%3F%20%3Cexpression%3E%20%3CWS%3E%3F%20%3Cstatement%3E%20%3CWS%3E%3F%20(%22else%22%20%3CWS%3E%3F%20%3Cstatement%3E)%3F%0A%3CreturnStmt%3E%20%3A%3A%3D%20%22return%22%20%3CWS%3E%20%3Cexpression%3E%20%22%3B%22%0A%3CchnReadStmt%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%22-%3E%22%20%3CWS%3E%3F%20%3CIDENTIFIER%3E%20%22%3B%22%0A%3CchnWriteStmt%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%22%3C-%22%20%3CWS%3E%3F%20%3Cexpression%3E%20%22%3B%22%0A%3CflowStmt%3E%20%3A%3A%3D%20%22flow%22%20%3CWS%3E%20(%3Ccall%3E%20%7C%20%3Cblock%3E)%20%3CWS%3E%3F%20%22%3B%22%0A%0A%3Cdeclaration%3E%20%3A%3A%3D%20%3Cstatement%3E%0A%09%7C%20%3CvarDecl%3E%0A%20%20%20%20%7C%20%3CchannelDecl%3E%0A%20%20%20%20%7C%20%3CfunctionDecl%3E%0A%0A%3CvarDecl%3E%20%3A%3A%3D%20(%22var%22%20%7C%20%22const%22)%20%3CWS%3E%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%3CtypeHint%3E%3F%20%3CWS%3E%3F%20(%22%3D%22%20%3CWS%3E%3F%20%3Cexpression%3E)%3F%20%22%3B%22%0A%3CchannelDecl%3E%20%3A%3A%3D%20%22channel%22%20%3CWS%3E%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%3CtypeHint%3E%20%22%3B%22%0A%3CfunctionDecl%3E%20%3A%3A%3D%20%22func%22%20(%3CWS%3E%20%3CIDENTIFIER%3E)%3F%20%3CWS%3E%3F%20%22(%22%20%3CWS%3E%3F%20%3Cparameters%3E%3F%20%3CWS%3E%3F%20%22)%22%0A%09(%3CWS%3E%3F%20%22use%22%20%3CWS%3E%3F%20%22(%22%20%3CWS%3E%3F%20%3Carguments%3E%3F%20%22)%22)%3F%0A%09%3CWS%3E%3F%20%3Cblock%3E%0A%20%20%20%0A%3Cprogram%3E%20%3A%3A%3D%20%3Cdeclaration%3E*%20%3CEOF%3E&name=)

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

// String concat
.

// shorthands for assignment
+=
-=
*=
/=
%=
.=
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
if condition block

---

if condition block
else block
```

Loops:

```
for initializer;condition;continue-expr block
```

Functions:

```
func identifier(age: int, decimal: float, name: string, flag: bool, chn: int channel) returntype block

---

// closure / anonymous function
func(parameters) use (closure-values) returntype block

---

// method on a struct
meth identifier(parameters) returntype block
```

Channel:

```
channel id: int;

--- 

// (chn is a channel "object")

// reading
chn -> variable

// writing
chn <- value
```

### Async function-call / Execution

```
flow call()

flow {
    a = 1;
}
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
if myError == err.MyErrorName block
```

### Implementation notes

Byte code is split into 3 simple sections:

- constants
- top-level functions
- main code
