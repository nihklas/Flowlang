## Compiler

- [ ] `Compiler.zig`
    - [ ] Restructure code, traversing code should all have separate functions
    - [ ] Specify unreachable paths
- [ ] `Parser.zig`
    - [ ] Go through each function and see what can be improved
    - [ ] `matchOneOf`-function?
- [ ] `Scanner.zig`
    - [ ] OOMs aren't errors, just panic
- [ ] `Sema.zig`
    - [ ] Restructure code, split up every stmt and expr type into function, so
      they look similar to the other traversing code
    - [ ] Better helper functions for errors
- [ ] `ast.zig`
    - [ ] Sort functions and members
    - [ ] helper function?

## Runtime

- [ ] `VM.zig`
    - [ ] Extract many of the similar functions out
    - [ ] Mark unreachable state as unreachable, instead of returning error
    - [ ] Crash on OOM and any other unexpected error, but with nicer message

## Shared

- [ ] `BytecodeDumper.zig`
    - [ ] extract a lot of common things into functions
