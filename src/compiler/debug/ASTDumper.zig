pub fn dump(writer: anytype, program: []const *ast.Stmt) void {
    _ = writer; // autofix
    _ = program; // autofix
}

const std = @import("std");
const ast = @import("../ast.zig");
