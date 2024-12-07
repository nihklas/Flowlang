test "Compiler" {
    _ = @import("compiler/Compiler.zig");
    _ = @import("compiler/Parser.zig");
    _ = @import("compiler/Scanner.zig");
    _ = @import("compiler/Sema.zig");
    _ = @import("compiler/Token.zig");
    _ = @import("compiler/ast.zig");
    _ = @import("compiler/main.zig");
}

test "Runtime" {
    _ = @import("runtime/GC.zig");
    _ = @import("runtime/VM.zig");
    _ = @import("runtime/main.zig");
}

test "Shared" {
    _ = @import("shared/debug/BytecodeDumper.zig");
    _ = @import("shared/builtins.zig");
    _ = @import("shared/byte_code.zig");
    _ = @import("shared/definitions.zig");
    _ = @import("shared/root.zig");
    _ = @import("shared/stack.zig");
}
