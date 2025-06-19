test "Compiler" {
    _ = @import("compiler/Compiler.zig");
    _ = @import("compiler/Parser.zig");
    _ = @import("compiler/Scanner.zig");
    _ = @import("compiler/Sema.zig");
    _ = @import("compiler/ir/Token.zig");
    _ = @import("compiler/ir/ast.zig");
    _ = @import("compiler/main.zig");
}

test "Runtime" {
    _ = @import("runtime/gc/Simple.zig");
    _ = @import("runtime/gc/MultiThreaded.zig");
    _ = @import("runtime/VM.zig");
    _ = @import("runtime/main.zig");
}
