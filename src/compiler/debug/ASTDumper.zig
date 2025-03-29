pub fn dump(writer: anytype, program: []const *ast.Stmt) !void {
    for (program) |stmt| {
        try dumpStmt(writer, stmt, 0);
    }
}

fn dumpStmt(writer: anytype, stmt: *const ast.Stmt, depth: usize) !void {
    try writeIndent(writer, depth);
    switch (stmt.*) {
        .expr => {
            try writer.writeAll("[Expression Stmt]\n");
            try dumpExpr(writer, stmt.expr.expr, depth + 1);
        },
        else => std.debug.panic("{s}-Stmt is not supported in printing yet", .{@tagName(stmt.*)}),
    }
}

fn dumpExpr(writer: anytype, expr: *const ast.Expr, depth: usize) !void {
    try writeIndent(writer, depth);
    switch (expr.*) {
        .binary => |binary| {
            try writer.print("(Binary Expr '{s}')\n", .{binary.op.lexeme});
            try dumpExpr(writer, binary.lhs, depth + 1);
            try dumpExpr(writer, binary.rhs, depth + 1);
        },
        .literal => |literal| {
            try writer.print("(Literal Expr '{s}')\n", .{literal.token.lexeme});
        },
        else => std.debug.panic("{s}-Expr is not supported in printing yet", .{@tagName(expr.*)}),
    }
}

fn writeIndent(writer: anytype, depth: usize) !void {
    if (depth == 0) return;
    for (0..depth * DEPTH_DISTANCE) |i| {
        if (i % DEPTH_DISTANCE == 0) {
            try writer.writeByte('|');
        } else if (i + DEPTH_DISTANCE > depth + 1) {
            try writer.writeByte('-');
        } else {
            try writer.writeByte(' ');
        }
    }
}

const DEPTH_DISTANCE = 2;
const std = @import("std");
const ast = @import("../ast.zig");
