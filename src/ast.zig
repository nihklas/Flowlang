pub const Expr = union(enum) {
    literal: struct { value: Token },
    grouping: struct { expr: *Expr },
    unary: struct { op: Token, expr: *Expr },
    binary: struct { lhs: *Expr, op: Token, rhs: *Expr },
    logical: struct { lhs: *Expr, op: Token, rhs: *Expr },
    assignment: struct { name: Token, value: *Expr },

    pub fn createLiteral(alloc: Allocator, value: Token) *Expr {
        const new_expr = alloc.create(Expr) catch @panic("OOM");
        new_expr.* = .{
            .literal = .{ .value = value },
        };
        return new_expr;
    }

    pub fn createGrouping(alloc: Allocator, expr: *Expr) *Expr {
        const new_expr = alloc.create(Expr) catch @panic("OOM");
        new_expr.* = .{
            .grouping = .{ .expr = expr },
        };
        return new_expr;
    }

    pub fn createUnary(alloc: Allocator, op: Token, expr: *Expr) *Expr {
        const new_expr = alloc.create(Expr) catch @panic("OOM");
        new_expr.* = .{
            .unary = .{ .op = op, .expr = expr },
        };
        return new_expr;
    }

    pub fn createBinary(alloc: Allocator, lhs: *Expr, op: Token, rhs: *Expr) *Expr {
        const new_expr = alloc.create(Expr) catch @panic("OOM");
        new_expr.* = .{
            .binary = .{ .lhs = lhs, .op = op, .rhs = rhs },
        };
        return new_expr;
    }

    pub fn createLogical(alloc: Allocator, lhs: *Expr, op: Token, rhs: *Expr) *Expr {
        const new_expr = alloc.create(Expr) catch @panic("OOM");
        new_expr.* = .{
            .logical = .{ .lhs = lhs, .op = op, .rhs = rhs },
        };
        return new_expr;
    }

    pub fn createAssignment(alloc: Allocator, name: Token, expr: *Expr) *Expr {
        const new_expr = alloc.create(Expr) catch @panic("OOM");
        new_expr.* = .{
            .assignment = .{ .name = name, .value = expr },
        };
        return new_expr;
    }
};

pub const Stmt = union(enum) {
    // NOTE: Only temporary, until there is support for a std library
    print: struct { expr: *Expr },
    expr: struct { expr: *Expr },
    block: struct { stmts: []const *Stmt },
    loop: struct { condition: *Expr, body: *Stmt },
    @"if": struct { condition: *Expr, true_branch: *Stmt, false_branch: ?*Stmt },
    @"return": struct { value: *Expr },
    channel_read: struct { channel: Token, result: Token },
    channel_write: struct { channel: Token, value: *Expr },
    variable: struct {
        name: Token,
        constant: bool,
        value: ?*Expr,
        // TODO: Add Type information
    },
    channel: struct {
        name: Token,
        // TODO: Add Type information
    },
    function: struct { name: Token, params: []const *Stmt, body: []const *Stmt },

    pub fn createPrint(alloc: Allocator, expr: *Expr) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .print = .{ .expr = expr },
        };
        return stmt;
    }

    pub fn createExpr(alloc: Allocator, expr: *Expr) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .expr = .{ .expr = expr },
        };
        return stmt;
    }

    pub fn createBlock(alloc: Allocator, stmts: []const *Stmt) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .block = .{ .stmts = stmts },
        };
        return stmt;
    }

    pub fn createLoop(alloc: Allocator, condition: *Expr, body: *Stmt) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .loop = .{ .condition = condition, .body = body },
        };
        return stmt;
    }

    pub fn createIf(alloc: Allocator, condition: *Expr, true_branch: *Stmt, false_branch: ?*Stmt) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .@"if" = .{ .condition = condition, .true_branch = true_branch, .false_branch = false_branch },
        };
        return stmt;
    }

    pub fn createReturn(alloc: Allocator, value: *Expr) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .@"return" = .{ .value = value },
        };
        return stmt;
    }

    pub fn createChannelRead(alloc: Allocator, channel: Token, result: Token) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .channel_read = .{ .channel = channel, .result = result },
        };
        return stmt;
    }

    pub fn createChannelWrite(alloc: Allocator, channel: Token, value: *Expr) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .channel_write = .{ .channel = channel, .value = value },
        };
        return stmt;
    }

    pub fn createVariable(alloc: Allocator, name: Token, constant: bool, value: ?*Expr) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .variable = .{
                .name = name,
                .constant = constant,
                .value = value,
            },
        };
        return stmt;
    }

    pub fn createChannel(alloc: Allocator, name: Token) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .channel = .{ .name = name },
        };
        return stmt;
    }

    pub fn createFunction(alloc: Allocator, name: Token, params: []const *Stmt, body: []const *Stmt) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .function = .{ .name = name, .params = params, .body = body },
        };
        return stmt;
    }
};

test "Expr.createLiteral" {
    const literal = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(literal);
}

test "Expr.createGrouping" {
    const literal = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(literal);

    const grouping = Expr.createGrouping(testing_alloc, literal);
    defer testing_alloc.destroy(grouping);
}

test "Expr.createUnary" {
    const literal = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(literal);

    const unary = Expr.createUnary(testing_alloc, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, literal);
    defer testing_alloc.destroy(unary);
}

test "Expr.createBinary" {
    const left = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(left);

    const right = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(right);

    const binary = Expr.createBinary(testing_alloc, left, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, right);
    defer testing_alloc.destroy(binary);
}

test "Expr.createLogical" {
    const left = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(left);

    const right = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(right);

    const binary = Expr.createLogical(testing_alloc, left, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, right);
    defer testing_alloc.destroy(binary);
}

test "Expr.createAssignment" {
    const right = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(right);

    const assignment = Expr.createAssignment(testing_alloc, .{ .type = .identifier, .lexeme = "number", .line = 1, .column = 1 }, right);
    defer testing_alloc.destroy(assignment);
}

test "Stmt.createExpr" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(expr);

    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    defer testing_alloc.destroy(expr_stmt);
}

test "Stmt.createPrint" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(expr);

    const print = Stmt.createPrint(testing_alloc, expr);
    defer testing_alloc.destroy(print);
}

test "Stmt.createBlock" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(expr);

    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    defer testing_alloc.destroy(expr_stmt);

    const block = Stmt.createBlock(testing_alloc, &.{expr_stmt});
    defer testing_alloc.destroy(block);
}

test "Stmt.createReturn" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(expr);

    const return_stmt = Stmt.createReturn(testing_alloc, expr);
    defer testing_alloc.destroy(return_stmt);
}

test "Stmt.createChannel" {
    const channel = Stmt.createChannel(testing_alloc, .{ .type = .identifier, .lexeme = "chn", .line = 1, .column = 1 });
    defer testing_alloc.destroy(channel);
}

test "Stmt.createChannelWrite" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(expr);

    const channel_write = Stmt.createChannelWrite(testing_alloc, .{ .type = .identifier, .lexeme = "chn", .line = 1, .column = 1 }, expr);
    defer testing_alloc.destroy(channel_write);
}

test "Stmt.createChannelRead" {
    const channel_read = Stmt.createChannelRead(
        testing_alloc,
        .{ .type = .identifier, .lexeme = "chn", .line = 1, .column = 1 },
        .{ .type = .identifier, .lexeme = "name", .line = 1, .column = 1 },
    );
    defer testing_alloc.destroy(channel_read);
}

test "Stmt.createLoop" {
    const condition = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(condition);

    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(expr);

    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    defer testing_alloc.destroy(expr_stmt);

    const loop = Stmt.createLoop(testing_alloc, condition, expr_stmt);
    defer testing_alloc.destroy(loop);
}

test "Stmt.createIf" {
    const condition = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(condition);

    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(expr);

    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    defer testing_alloc.destroy(expr_stmt);

    const if_stmt = Stmt.createIf(testing_alloc, condition, expr_stmt, null);
    defer testing_alloc.destroy(if_stmt);
}

test "Stmt.createVariable" {
    const condition = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(condition);

    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(expr);

    const loop = Stmt.createVariable(
        testing_alloc,
        .{ .type = .identifier, .lexeme = "name", .line = 1, .column = 1 },
        false,
        expr,
    );
    defer testing_alloc.destroy(loop);
}

test "Stmt.createFunction" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer testing_alloc.destroy(expr);

    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    defer testing_alloc.destroy(expr_stmt);

    const variable = Stmt.createVariable(testing_alloc, .{ .type = .identifier, .lexeme = "param", .line = 1, .column = 1 }, true, null);
    defer testing_alloc.destroy(variable);

    const loop = Stmt.createFunction(
        testing_alloc,
        .{ .type = .identifier, .lexeme = "name", .line = 1, .column = 1 },
        &.{variable},
        &.{expr_stmt},
    );
    defer testing_alloc.destroy(loop);
}

const testing = std.testing;
const testing_alloc = testing.allocator;

const Token = @import("Token.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
