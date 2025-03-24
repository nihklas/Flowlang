# Notes

Notes I have during development, that don't yet have a better place to be.

## General Notes

- Source Code gets parsed into the AST
- Sema happens on the AST
- Symbol Table, Scope Table and Intern Pool gets created outside of Sema and passed into it
- Symbol Table holds information about types and existence of symbols
- Keys of the symbol tables gets created through combination of name and scope id
- Intern Pool holds a copy of all literal strings
- Sema checks for any compile time errors
    - Resolves Types and checks them by building the symbol table
    - Checks scope of variables, constantness etc.
    - String literals are saved into the Intern Pool
- After Compile Time checking, AST gets turned into a CFG
- CFG consists of Blocks of Instructions and Jumps between those
    - Instructions should roughly translate per line
- Special jumps are "_entry" and "_exit"
- Optimisation passes happen on the CFG
    - for example Dead-Code-Elimination
- Jumps are defined as predecessors and successors

## IR

We have the AST on the one end and Bytecode on the other end.
Lets try build a CFG (Conditional Flow Graph) from the AST.

We have the following sample code:

```flow
const some_int = 4;
const another_int = some_int;
var a_variable = "Changeable";
a_variable = "Different";

const result = someFunc(3, another_int);
print(result);

func someFunc(a: int, b: int) int {
    if a < b {
        return a;
    }

    print("Some statement without value");

    return a + b;
}
```

A CFG is consisted of Blocks of Instructions (Nodes in the Graph), and
Jumps/Branches (Edges of the Graph). The instructions should be closer to
bytecode but still more high level to generally encode on roughly 1
instruction-per-loc.

The above code will have 4 blocks:

```flow
// entry
const some_int = 4;
const another_int = some_int;
var a_variable = "Changeable";
a_variable = "Different";
const result = someFunc(3, another_int);
print(result);

// function_1
// Condition and branching, does not translate literally to source code
if a < b

// conditional_1_true
return a;

// conditional_1_false
print("Some statement without value");
return a + b;
```

The Blocks are made up of instructions. Instructions are simple structs defined
as follows:

```zig
const CFGInstruction = struct {
    kind: InstructionKind,
    extra: u32,

    const InstructionKind = enum {
        //...
    };
};
```

Instruction Kind will be filled with all needed instructions, extra will hold
any extra information needed, like index into the symbol table, small integer
and float literals.

