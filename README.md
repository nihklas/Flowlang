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

## TODO:

- [x] Scanner
- [ ] Parser/AST
- [ ] Input Handling (File Reading, REPL?)
- [ ] SeMa

## Try it out!

The file `example/src/main.flow` shows the current possible things that the flow compiler supports.

This language uses the zig build system, so to use Flow you have to have zig installed.

You need to create a build.zig file and put the following contents:

```zig
const std = @import("std");
const flow = @import("flow");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const flow_out = flow.compile(b, .{
        .name = "flow-example",
        .source = b.path("src/main.flow"),
        .target = target,
    });

    b.installArtifact(flow_out);

    const run_flow = b.addRunArtifact(flow_out);
    const run_step = b.step("run", "Run the code directly");
    run_step.dependOn(&run_flow.step);
}
```

You can change the name and source entries. If you know your way around the buildsystem, you can of
course do some more crazy things, but this setup is enough to get you started with just Flow.

Now, to get flow itself, you need to fetch it with this:

```bash
zig fetch --save=flow git+https://gitlab.com/nihklas/flowlang.git
```

Now you can run `zig build` to compile your project and get the binary under `zig-out/bin/<binary>`
or you run the code with `zig build run` directly.

## Syntax and Grammar

[Current Grammar](https://bnfplayground.pauliankline.com/?bnf=%3CDIGIT%3E%20%3A%3A%3D%20%5B0-9%5D%0A%3CALPHA%3E%20%3A%3A%3D%20%5Ba-z%5D%20%7C%20%5BA-Z%5D%20%7C%20%22_%22%0A%3CNUMBER%3E%20%3A%3A%3D%20%3CDIGIT%3E*%20%20(%22.%22%20%3CDIGIT%3E%2B%20)%3F%0A%3CIDENTIFIER%3E%20%3A%3A%3D%20%3CALPHA%3E%20(%3CALPHA%3E%20%7C%20%3CDIGIT%3E)*%0A%2F*%20Any%20character%20between%20the%20quotes%20*%2F%0A%3CSTRING%3E%20%3A%3A%3D%20%22%5C%22%22%20(%5Ba-z%5D%20%7C%20%5BA-Z%5D%20%7C%20%5B0-9%5D)*%20%22%5C%22%22%0A%3CBOOL%3E%20%3A%3A%3D%20%22true%22%20%7C%20%22false%22%0A%3CNULL%3E%20%3A%3A%3D%20%22null%22%0A%2F*%20Whitespace%20gets%20ignored%20for%20the%20most%20part%2C%20this%20rule%20is%20needed%20for%20this%20Editor%20*%2F%0A%3CWS%3E%20%3A%3A%3D%20(%22%20%22%20%7C%20%22%5Cn%22)%2B%0A%2F*%20There%20is%20no%20EOF%20Symbol%20in%20this%20Editor%20*%2F%0A%3CEOF%3E%20%3A%3A%3D%20%22%20%22%0A%0A%2F*%20Utilities%20*%2F%0A%3CtypeHint%3E%20%3A%3A%3D%20%22%3A%22%20%3CWS%3E%3F%20%3CIDENTIFIER%3E%0A%3CparamHint%3E%20%3A%3A%3D%20%3CtypeHint%3E%20%3CWS%3E%3F%20%22channel%22%3F%0A%3Carguments%3E%20%3A%3A%3D%20%3Cexpression%3E%20(%22%2C%22%20%3CWS%3E%3F%20%3Cexpression%3E)*%0A%3Cparameters%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%3CparamHint%3E%20(%22%2C%22%20%3CWS%3E%3F%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%3CparamHint%3E)*%0A%0A%3Cprimary%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%7C%20%3CSTRING%3E%20%7C%20%3CNUMBER%3E%20%7C%20%3CBOOL%3E%20%7C%20%3CNULL%3E%20%7C%20%22this%22%20%7C%20%22(%22%20%3Cexpression%3E%20%22)%22%0A%3Ccall%3E%20%3A%3A%3D%20%3Cprimary%3E%20(%22(%22%20%3Carguments%3E%3F%20%22)%22)*%0A%3Cunary%3E%20%3A%3A%3D%20((%22!%22%20%7C%20%22-%22)%20%3Cunary%3E)%20%7C%20%3Ccall%3E%0A%3Cfactor%3E%20%3A%3A%3D%20%3Cunary%3E%20%3CWS%3E%3F%20((%22%2F%22%20%7C%20%22*%22)%20%3CWS%3E%3F%20%3Cunary%3E)*%0A%3Cterm%3E%20%3A%3A%3D%20%3Cfactor%3E%20%3CWS%3E%3F%20((%22%2B%22%20%7C%20%22-%22)%20%3CWS%3E%3F%20%3Cfactor%3E)*%0A%3Ccomparison%3E%20%3A%3A%3D%20%3Cterm%3E%20%3CWS%3E%3F%20((%22%3C%22%20%7C%20%22%3C%3D%22%20%7C%20%22%3E%3D%22%20%7C%20%22%3E%22)%20%3CWS%3E%3F%20%3Cterm%3E)*%0A%3Cequality%3E%20%3A%3A%3D%20%3Ccomparison%3E%20%3CWS%3E%3F%20((%22%3D%3D%22%20%7C%20%22!%3D%22)%20%3CWS%3E%3F%20%3Ccomparison%3E)*%0A%3Cand%3E%20%3A%3A%3D%20%3Cequality%3E%20(%3CWS%3E%20%22and%22%20%3CWS%3E%20%3Cequality%3E)*%0A%3Cor%3E%20%3A%3A%3D%20%3Cand%3E%20(%3CWS%3E%20%22or%22%20%3CWS%3E%20%3Cand%3E)*%0A%3Cconcat%3E%20%3A%3A%3D%20%3Cor%3E%20(%3CWS%3E%20%22.%22%20%3CWS%3E%20%3Cor%3E)*%0A%3Cassignment%3E%20%3A%3A%3D%20(%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%22%3D%22%20%3CWS%3E%3F%20%3Cexpression%3E)%20%7C%20%3Cconcat%3E%0A%0A%3Cexpression%3E%20%3A%3A%3D%20%3Cassignment%3E%0A%0A%3Cstatement%3E%20%3A%3A%3D%20%3CexprStmt%3E%20%0A%20%20%20%20%7C%20%3CforStmt%3E%20%0A%20%20%20%20%7C%20%3CifStmt%3E%20%0A%20%20%20%20%7C%20%3CreturnStmt%3E%0A%20%20%20%20%7C%20%3CchnReadStmt%3E%0A%20%20%20%20%7C%20%3CchnWriteStmt%3E%0A%09%7C%20%3Cblock%3E%20%0A%0A%3CexprStmt%3E%20%3A%3A%3D%20%3Cexpression%3E%20%22%3B%22%0A%3Cblock%3E%20%3A%3A%3D%20%22%7B%22%20%3CWS%3E%3F%20%3Cdeclaration%3E*%20%3CWS%3E%3F%20%22%7D%22%0A%3CforStmt%3E%20%3A%3A%3D%20%22for%22%20%3CWS%3E%20(%3CexprStmt%3E%20%7C%20%3CvarDecl%3E%20%7C%20%22%3B%22)%20%3CWS%3E%3F%20%3Cexpression%3E%3F%20%3CWS%3E%3F%20%22%3B%22%20%3CWS%3E%3F%20%3Cstatement%3E%3F%20%3CWS%3E%3F%20%3Cstatement%3E%0A%3CifStmt%3E%20%3A%3A%3D%20%22if%22%20%3CWS%3E%3F%20%3Cexpression%3E%20%3CWS%3E%3F%20%3Cstatement%3E%20%3CWS%3E%3F%20(%22else%22%20%3CWS%3E%3F%20%3Cstatement%3E)%3F%0A%3CreturnStmt%3E%20%3A%3A%3D%20%22return%22%20%3CWS%3E%20%3Cexpression%3E%20%22%3B%22%0A%3CchnReadStmt%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%22-%3E%22%20%3CWS%3E%3F%20%3CIDENTIFIER%3E%20%22%3B%22%0A%3CchnWriteStmt%3E%20%3A%3A%3D%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%22%3C-%22%20%3CWS%3E%3F%20%3Cexpression%3E%20%22%3B%22%0A%3CflowStmt%3E%20%3A%3A%3D%20%22flow%22%20%3CWS%3E%20(%3Ccall%3E%20%7C%20%3Cblock%3E)%20%3CWS%3E%3F%20%22%3B%22%0A%0A%3Cdeclaration%3E%20%3A%3A%3D%20%3Cstatement%3E%0A%09%7C%20%3CvarDecl%3E%0A%20%20%20%20%7C%20%3CchannelDecl%3E%0A%20%20%20%20%7C%20%3CfunctionDecl%3E%0A%0A%3CvarDecl%3E%20%3A%3A%3D%20(%22var%22%20%7C%20%22const%22)%20%3CWS%3E%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%3CtypeHint%3E%3F%20%3CWS%3E%3F%20(%22%3D%22%20%3CWS%3E%3F%20%3Cexpression%3E)%3F%20%22%3B%22%0A%3CchannelDecl%3E%20%3A%3A%3D%20%22channel%22%20%3CWS%3E%20%3CIDENTIFIER%3E%20%3CWS%3E%3F%20%3CtypeHint%3E%20%22%3B%22%0A%3CfunctionDecl%3E%20%3A%3A%3D%20%22func%22%20(%3CWS%3E%20%3CIDENTIFIER%3E)%3F%20%3CWS%3E%3F%20%22(%22%20%3CWS%3E%3F%20%3Cparameters%3E%3F%20%3CWS%3E%3F%20%22)%22%0A%09(%3CWS%3E%3F%20%22use%22%20%3CWS%3E%3F%20%22(%22%20%3CWS%3E%3F%20%3Carguments%3E%3F%20%22)%22)%3F%0A%09%3CWS%3E%3F%20%3Cblock%3E%0A%20%20%20%0A%3Cprogram%3E%20%3A%3A%3D%20%3Cdeclaration%3E*%20%3CEOF%3E&name=)

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
for initializer;condition;continue-expr; block
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

## Compile Steps

The compilation Process will be split into different steps

1. Lexing/Tokenizing
2. AST
3. Different IR with optimisations and more infos
4. Byte Code

