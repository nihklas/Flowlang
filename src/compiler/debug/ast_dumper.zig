pub fn dump(writer: anytype, program: []*ast.Stmt) void {
    _ = writer; // autofix
    _ = program; // autofix
}

const std = @import("std");
const ast = @import("../ast.zig");
