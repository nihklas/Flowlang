const Expr = union(enum) {
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

const Stmt = union(enum) {
    // NOTE: Only temporary, until there is support for a std library
    print: struct { expr: *Expr },
    expr: struct { expr: *Expr },
    block: struct { stmts: []*Stmt },
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
    function: struct { name: Token, params: []Token, body: []*Stmt },

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

    pub fn createBlock(alloc: Allocator, stmts: []*Stmt) *Stmt {
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

    pub fn createReturn(alloc: Allocator, expr: *Expr) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .@"return" = .{ .expr = expr },
        };
        return stmt;
    }

    pub fn createChannelRead(alloc: Allocator, channel: Token, result: Token) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .channel_write = .{ .channel = channel, .result = result },
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

    pub fn createFunction(alloc: Allocator, name: Token, params: []Token, body: []*Stmt) *Stmt {
        const stmt = alloc.create(Stmt) catch @panic("OOM");
        stmt.* = .{
            .function = .{ .name = name, .params = params, .body = body },
        };
        return stmt;
    }
};

test "createLiteral" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const literal = Expr.createLiteral(alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer alloc.destroy(literal);
}

test "createGrouping" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const literal = Expr.createLiteral(alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer alloc.destroy(literal);

    const grouping = Expr.createGrouping(alloc, literal);
    defer alloc.destroy(grouping);
}

test "createUnary" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const literal = Expr.createLiteral(alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer alloc.destroy(literal);

    const unary = Expr.createUnary(alloc, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, literal);
    defer alloc.destroy(unary);
}

test "createBinary" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const left = Expr.createLiteral(alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer alloc.destroy(left);

    const right = Expr.createLiteral(alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer alloc.destroy(right);

    const binary = Expr.createBinary(alloc, left, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, right);
    defer alloc.destroy(binary);
}

test "createLogical" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const left = Expr.createLiteral(alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer alloc.destroy(left);

    const right = Expr.createLiteral(alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer alloc.destroy(right);

    const binary = Expr.createLogical(alloc, left, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, right);
    defer alloc.destroy(binary);
}

test "createAssignment" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const right = Expr.createLiteral(alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer alloc.destroy(right);

    const assignment = Expr.createAssignment(alloc, .{ .type = .identifier, .lexeme = "number", .line = 1, .column = 1 }, right);
    defer alloc.destroy(assignment);
}

const Token = @import("Token.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
