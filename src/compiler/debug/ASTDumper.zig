pub fn dump(writer: anytype, program: []const *ast.Stmt) !void {
    for (program) |stmt| {
        try dumpStmt(writer, stmt, 0);
    }
}

fn dumpStmt(writer: anytype, stmt: *const ast.Stmt, depth: usize) !void {
    if (stmt.* != .block) {
        try writeIndent(writer, depth);
    }

    switch (stmt.*) {
        .block => |block| for (block.stmts) |inner_stmt| {
            try dumpStmt(writer, inner_stmt, depth);
        },
        .expr => {
            try writer.writeAll("[Expression Stmt]\n");
            try dumpExpr(writer, stmt.expr.expr, depth + 1);
        },
        .variable => |variable| {
            try writer.writeAll("[");
            if (variable.constant) {
                try writer.writeAll("Constant ");
            } else {
                try writer.writeAll("Variable ");
            }
            try writer.print("'{s}'", .{variable.name.lexeme});

            if (variable.type_hint) |type_hint| {
                try writer.writeAll(" ");
                for (0..type_hint.order) |_| {
                    try writer.writeAll("[]");
                }
                try writer.print("'{s}'", .{type_hint.type.lexeme});
            }

            try writer.writeAll("]\n");
            if (variable.value) |value| {
                try dumpExpr(writer, value, depth + 1);
            }
        },
        .@"if" => |if_stmt| {
            try writer.writeAll("[if Stmt]\n");

            try writeIndent(writer, depth + 1);
            try writer.writeAll("(Condition)\n");
            try dumpExpr(writer, if_stmt.condition, depth + 2);

            try writeIndent(writer, depth + 1);
            try writer.writeAll("(True Branch)\n");
            try dumpStmt(writer, if_stmt.true_branch, depth + 2);

            if (if_stmt.false_branch) |false_branch| {
                try writeIndent(writer, depth + 1);
                try writer.writeAll("(False Branch)\n");
                try dumpStmt(writer, false_branch, depth + 2);
            }
        },
        .loop => |loop| {
            try writer.writeAll("[loop Stmt]\n");

            try writeIndent(writer, depth + 1);
            try writer.writeAll("(Condition)\n");
            try dumpExpr(writer, loop.condition, depth + 2);

            if (loop.inc) |inc| {
                try writeIndent(writer, depth + 1);
                try writer.writeAll("(Increment)\n");
                try dumpStmt(writer, inc, depth + 2);
            }

            if (loop.body.len > 0) {
                try writeIndent(writer, depth + 1);
                try writer.writeAll("(Body)\n");
                for (loop.body) |body_stmt| {
                    if (body_stmt == loop.inc) continue;
                    try dumpStmt(writer, body_stmt, depth + 2);
                }
            }
        },
        .@"break" => try writer.writeAll("[Break Stmt]\n"),
        .@"continue" => try writer.writeAll("[Continue Stmt]\n"),
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
        .call => |call| {
            try writer.writeAll("(Call Expr)\n");
            try writeIndent(writer, depth + 1);
            try writer.writeAll("(Callee)\n");
            try dumpExpr(writer, call.expr, depth + 2);
            try writeIndent(writer, depth + 1);
            try writer.writeAll("(Arguments)\n");
            for (call.args) |arg| {
                try dumpExpr(writer, arg, depth + 2);
            }
        },
        .variable => |variable| {
            try writer.print("(Variable Expr '{s}')\n", .{variable.name.lexeme});
        },
        .unary => |unary| {
            try writer.print("(Unary Expr '{s}')\n", .{unary.op.lexeme});
            try dumpExpr(writer, unary.expr, depth + 1);
        },
        .assignment => |assign| {
            try writer.print("(Assignment Expr '{s}')\n", .{assign.name.lexeme});
            try dumpExpr(writer, assign.value, depth + 1);
        },
        else => std.debug.panic("{s}-Expr is not supported in printing yet", .{@tagName(expr.*)}),
    }
}

fn writeIndent(writer: anytype, depth: usize) !void {
    if (depth == 0) return;
    for (0..depth * DEPTH_DISTANCE) |i| {
        if (i % DEPTH_DISTANCE == 0) {
            try writer.writeByte('|');
        } else {
            try writer.writeByte(' ');
        }
    }
}

const DEPTH_DISTANCE = 2;
const std = @import("std");
const ast = @import("../ast.zig");
