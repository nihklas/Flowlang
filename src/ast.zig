pub const Expr = union(enum) {
    literal: struct { value: Token },
    grouping: struct { expr: *Expr },
    unary: struct { op: Token, expr: *Expr },
    binary: struct { lhs: *Expr, op: Token, rhs: *Expr },
    logical: struct { lhs: *Expr, op: Token, rhs: *Expr },
    assignment: struct { name: Token, value: *Expr },

    pub fn createLiteral(alloc: Allocator, value: Token) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .literal = .{ .value = value },
        };
        return new_expr;
    }

    pub fn createGrouping(alloc: Allocator, expr: *Expr) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .grouping = .{ .expr = expr },
        };
        return new_expr;
    }

    pub fn createUnary(alloc: Allocator, op: Token, expr: *Expr) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .unary = .{ .op = op, .expr = expr },
        };
        return new_expr;
    }

    pub fn createBinary(alloc: Allocator, lhs: *Expr, op: Token, rhs: *Expr) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .binary = .{ .lhs = lhs, .op = op, .rhs = rhs },
        };
        return new_expr;
    }

    pub fn createLogical(alloc: Allocator, lhs: *Expr, op: Token, rhs: *Expr) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .logical = .{ .lhs = lhs, .op = op, .rhs = rhs },
        };
        return new_expr;
    }

    pub fn createAssignment(alloc: Allocator, name: Token, expr: *Expr) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .assignment = .{ .name = name, .value = expr },
        };
        return new_expr;
    }

    pub fn destroy(self: *Expr, alloc: Allocator) void {
        switch (self.*) {
            .literal => {},
            .grouping => |grouping| grouping.expr.destroy(alloc),
            .unary => |unary| unary.expr.destroy(alloc),
            .assignment => |assignment| assignment.value.destroy(alloc),
            .binary => |binary| {
                binary.lhs.destroy(alloc);
                binary.rhs.destroy(alloc);
            },
            .logical => |logical| {
                logical.lhs.destroy(alloc);
                logical.rhs.destroy(alloc);
            },
        }

        alloc.destroy(self);
    }

    fn create(alloc: Allocator) *Expr {
        return alloc.create(Expr) catch @panic("OOM");
    }
};

pub const Stmt = union(enum) {
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
    function: struct { name: Token, params: []*Stmt, body: []*Stmt },

    pub fn createPrint(alloc: Allocator, expr: *Expr) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .print = .{ .expr = expr },
        };
        return stmt;
    }

    pub fn createExpr(alloc: Allocator, expr: *Expr) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .expr = .{ .expr = expr },
        };
        return stmt;
    }

    pub fn createBlock(alloc: Allocator, stmts: []const *Stmt) *Stmt {
        const stmt = Stmt.create(alloc);
        const duped_stmts = alloc.dupe(*Stmt, stmts) catch @panic("OOM");
        stmt.* = .{
            .block = .{ .stmts = duped_stmts },
        };
        return stmt;
    }

    pub fn createLoop(alloc: Allocator, condition: *Expr, body: *Stmt) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .loop = .{ .condition = condition, .body = body },
        };
        return stmt;
    }

    pub fn createIf(alloc: Allocator, condition: *Expr, true_branch: *Stmt, false_branch: ?*Stmt) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .@"if" = .{ .condition = condition, .true_branch = true_branch, .false_branch = false_branch },
        };
        return stmt;
    }

    pub fn createReturn(alloc: Allocator, value: *Expr) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .@"return" = .{ .value = value },
        };
        return stmt;
    }

    pub fn createChannelRead(alloc: Allocator, channel: Token, result: Token) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .channel_read = .{ .channel = channel, .result = result },
        };
        return stmt;
    }

    pub fn createChannelWrite(alloc: Allocator, channel: Token, value: *Expr) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .channel_write = .{ .channel = channel, .value = value },
        };
        return stmt;
    }

    pub fn createVariable(alloc: Allocator, name: Token, constant: bool, value: ?*Expr) *Stmt {
        const stmt = Stmt.create(alloc);
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
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .channel = .{ .name = name },
        };
        return stmt;
    }

    pub fn createFunction(alloc: Allocator, name: Token, params: []const *Stmt, body: []const *Stmt) *Stmt {
        const stmt = Stmt.create(alloc);
        const duped_params = alloc.dupe(*Stmt, params) catch @panic("OOM");
        const duped_body = alloc.dupe(*Stmt, body) catch @panic("OOM");
        stmt.* = .{
            .function = .{ .name = name, .params = duped_params, .body = duped_body },
        };
        return stmt;
    }

    pub fn destroy(self: *Stmt, alloc: Allocator) void {
        switch (self.*) {
            .channel_read, .channel => {},
            .print => |print| print.expr.destroy(alloc),
            .expr => |expr| expr.expr.destroy(alloc),
            .block => |block| {
                for (block.stmts) |stmt| {
                    stmt.destroy(alloc);
                }
                alloc.free(block.stmts);
            },
            .loop => |loop| {
                loop.body.destroy(alloc);
                loop.condition.destroy(alloc);
            },
            .@"if" => |if_stmt| {
                if_stmt.condition.destroy(alloc);
                if_stmt.true_branch.destroy(alloc);
                if (if_stmt.false_branch) |false_branch| {
                    false_branch.destroy(alloc);
                }
            },
            .@"return" => |return_stmt| return_stmt.value.destroy(alloc),
            .channel_write => |channel_write| channel_write.value.destroy(alloc),
            .variable => |variable| if (variable.value) |value| value.destroy(alloc),
            .function => |function| {
                for (function.params) |param| {
                    param.destroy(alloc);
                }
                alloc.free(function.params);

                for (function.body) |stmt| {
                    stmt.destroy(alloc);
                }
                alloc.free(function.body);
            },
        }
        alloc.destroy(self);
    }

    fn create(alloc: Allocator) *Stmt {
        return alloc.create(Stmt) catch @panic("OOM");
    }
};

test "Expr.createLiteral" {
    const literal = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    defer literal.destroy(testing_alloc);
}

test "Expr.createGrouping" {
    const literal = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const grouping = Expr.createGrouping(testing_alloc, literal);
    defer grouping.destroy(testing_alloc);
}

test "Expr.createUnary" {
    const literal = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const unary = Expr.createUnary(testing_alloc, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, literal);
    defer unary.destroy(testing_alloc);
}

test "Expr.createBinary" {
    const left = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const right = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const binary = Expr.createBinary(testing_alloc, left, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, right);
    defer binary.destroy(testing_alloc);
}

test "Expr.createLogical" {
    const left = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const right = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const binary = Expr.createLogical(testing_alloc, left, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, right);
    defer binary.destroy(testing_alloc);
}

test "Expr.createAssignment" {
    const right = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const assignment = Expr.createAssignment(testing_alloc, .{ .type = .identifier, .lexeme = "number", .line = 1, .column = 1 }, right);
    defer assignment.destroy(testing_alloc);
}

test "Stmt.createExpr" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    defer expr_stmt.destroy(testing_alloc);
}

test "Stmt.createPrint" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const print = Stmt.createPrint(testing_alloc, expr);
    defer print.destroy(testing_alloc);
}

test "Stmt.createBlock" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    const block = Stmt.createBlock(testing_alloc, &.{expr_stmt});
    defer block.destroy(testing_alloc);
}

test "Stmt.createReturn" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const return_stmt = Stmt.createReturn(testing_alloc, expr);
    defer return_stmt.destroy(testing_alloc);
}

test "Stmt.createChannel" {
    const channel = Stmt.createChannel(testing_alloc, .{ .type = .identifier, .lexeme = "chn", .line = 1, .column = 1 });
    defer channel.destroy(testing_alloc);
}

test "Stmt.createChannelWrite" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const channel_write = Stmt.createChannelWrite(testing_alloc, .{ .type = .identifier, .lexeme = "chn", .line = 1, .column = 1 }, expr);
    defer channel_write.destroy(testing_alloc);
}

test "Stmt.createChannelRead" {
    const channel_read = Stmt.createChannelRead(
        testing_alloc,
        .{ .type = .identifier, .lexeme = "chn", .line = 1, .column = 1 },
        .{ .type = .identifier, .lexeme = "name", .line = 1, .column = 1 },
    );
    defer channel_read.destroy(testing_alloc);
}

test "Stmt.createLoop" {
    const condition = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    const loop = Stmt.createLoop(testing_alloc, condition, expr_stmt);
    defer loop.destroy(testing_alloc);
}

test "Stmt.createIf" {
    const condition = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    const if_stmt = Stmt.createIf(testing_alloc, condition, expr_stmt, null);
    defer if_stmt.destroy(testing_alloc);
}

test "Stmt.createVariable" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const loop = Stmt.createVariable(
        testing_alloc,
        .{ .type = .identifier, .lexeme = "name", .line = 1, .column = 1 },
        false,
        expr,
    );
    defer loop.destroy(testing_alloc);
}

test "Stmt.createFunction" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 });
    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    const variable = Stmt.createVariable(testing_alloc, .{ .type = .identifier, .lexeme = "param", .line = 1, .column = 1 }, true, null);
    const function = Stmt.createFunction(
        testing_alloc,
        .{ .type = .identifier, .lexeme = "name", .line = 1, .column = 1 },
        &.{variable},
        &.{expr_stmt},
    );
    defer function.destroy(testing_alloc);
}

const testing = std.testing;
const testing_alloc = testing.allocator;

const Token = @import("Token.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
