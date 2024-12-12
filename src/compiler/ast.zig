pub const Expr = union(enum) {
    pub const Literal = union(enum) {
        null: void,
        bool: bool,
        int: Integer,
        float: Float,
        string: []const u8,
    };

    literal: struct { token: Token, value: Literal },
    grouping: struct { expr: *Expr },
    unary: struct { op: Token, expr: *Expr },
    binary: struct { lhs: *Expr, op: Token, rhs: *Expr },
    logical: struct { lhs: *Expr, op: Token, rhs: *Expr },
    assignment: struct { name: Token, value: *Expr, global: bool = false, local_idx: u8 = 0 },
    append: struct { name: Token, value: *Expr, global: bool = false, local_idx: u8 = 0 },
    variable: struct { name: Token, global: bool = false, local_idx: u8 = 0 },
    call: struct { expr: *Expr, args: []*Expr },

    pub fn createLiteral(alloc: Allocator, token: Token, value: Literal) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .literal = .{ .token = token, .value = value },
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

    pub fn createAppend(alloc: Allocator, name: Token, expr: *Expr) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .append = .{ .name = name, .value = expr },
        };
        return new_expr;
    }

    pub fn createVariable(alloc: Allocator, name: Token) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .variable = .{ .name = name },
        };
        return new_expr;
    }

    pub fn createCall(alloc: Allocator, expr: *Expr, params: []*Expr) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .call = .{ .expr = expr, .args = params },
        };
        return new_expr;
    }

    pub fn destroy(self: *Expr, alloc: Allocator) void {
        defer alloc.destroy(self);
        switch (self.*) {
            .literal, .variable => {},
            .grouping => |grouping| grouping.expr.destroy(alloc),
            .unary => |unary| unary.expr.destroy(alloc),
            .assignment => |assignment| assignment.value.destroy(alloc),
            .append => |append| append.value.destroy(alloc),
            .binary => |binary| {
                binary.lhs.destroy(alloc);
                binary.rhs.destroy(alloc);
            },
            .logical => |logical| {
                logical.lhs.destroy(alloc);
                logical.rhs.destroy(alloc);
            },
            .call => |call| {
                call.expr.destroy(alloc);
                for (call.args) |expr| {
                    expr.destroy(alloc);
                }
            },
        }
    }

    pub fn getToken(self: *Expr) Token {
        return switch (self.*) {
            .literal => self.literal.token,
            .grouping => self.grouping.expr.getToken(),
            .unary => self.unary.op,
            .binary => self.binary.op,
            .logical => self.logical.op,
            .assignment => self.assignment.name,
            .append => self.append.name,
            .variable => self.variable.name,
            .call => self.call.expr.getToken(),
        };
    }

    fn create(alloc: Allocator) *Expr {
        return alloc.create(Expr) catch oom();
    }
};

pub const Stmt = union(enum) {
    expr: struct { expr: *Expr },

    block: struct { stmts: []*Stmt, local_count: usize = 0 },

    loop: struct { condition: *Expr, body: []*Stmt, inc: ?*Stmt = null },
    @"break": struct { token: Token },
    @"continue": struct { token: Token },

    @"if": struct { condition: *Expr, true_branch: *Stmt, false_branch: ?*Stmt },

    channel_read: struct { channel: Token, result: Token },
    channel_write: struct { channel: Token, value: *Expr },
    channel: struct { name: Token, type: FlowType },
    variable: struct {
        name: Token,
        constant: bool,
        value: ?*Expr,
        type_hint: ?Token,
        array: bool = false,
        global: bool = false,
        local_index: ?u8 = null,
    },

    function: struct { name: Token, ret_type: Token, params: []*Stmt, body: []*Stmt },
    @"return": struct { token: Token, value: ?*Expr },

    pub fn createExpr(alloc: Allocator, expr: *Expr) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .expr = .{ .expr = expr },
        };
        return stmt;
    }

    pub fn createBlock(alloc: Allocator, stmts: []*Stmt) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .block = .{ .stmts = stmts },
        };
        return stmt;
    }

    pub fn createLoop(alloc: Allocator, condition: *Expr, body: []*Stmt) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .loop = .{ .condition = condition, .body = body },
        };
        return stmt;
    }

    pub fn createBreak(alloc: Allocator, token: Token) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .@"break" = .{ .token = token },
        };
        return stmt;
    }

    pub fn createContinue(alloc: Allocator, token: Token) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .@"continue" = .{ .token = token },
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

    pub fn createChannel(alloc: Allocator, name: Token, type_hint: FlowType) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .channel = .{ .name = name, .type = type_hint },
        };
        return stmt;
    }

    pub fn createVariable(alloc: Allocator, name: Token, type_hint: ?Token, constant: bool, value: ?*Expr) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .variable = .{
                .name = name,
                .constant = constant,
                .value = value,
                .type_hint = type_hint,
            },
        };
        return stmt;
    }

    pub fn createFunction(alloc: Allocator, name: Token, ret_type: Token, params: []*Stmt, body: []*Stmt) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .function = .{ .name = name, .ret_type = ret_type, .params = params, .body = body },
        };
        return stmt;
    }

    pub fn createReturn(alloc: Allocator, token: Token, value: ?*Expr) *Stmt {
        const stmt = Stmt.create(alloc);
        stmt.* = .{
            .@"return" = .{ .token = token, .value = value },
        };
        return stmt;
    }

    pub fn destroy(self: *Stmt, alloc: Allocator) void {
        defer alloc.destroy(self);
        switch (self.*) {
            .channel_read, .channel, .@"break", .@"continue" => {},
            .expr => |expr| expr.expr.destroy(alloc),
            .block => |block| {
                for (block.stmts) |stmt| {
                    stmt.destroy(alloc);
                }
                alloc.free(block.stmts);
            },
            .loop => |loop| {
                for (loop.body) |body| {
                    body.destroy(alloc);
                }
                alloc.free(loop.body);

                loop.condition.destroy(alloc);
                if (loop.inc) |inc| {
                    inc.destroy(alloc);
                }
            },
            .@"if" => |if_stmt| {
                if_stmt.condition.destroy(alloc);
                if_stmt.true_branch.destroy(alloc);
                if (if_stmt.false_branch) |false_branch| {
                    false_branch.destroy(alloc);
                }
            },
            .@"return" => |return_stmt| if (return_stmt.value) |value| value.destroy(alloc),
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
    }

    fn create(alloc: Allocator) *Stmt {
        return alloc.create(Stmt) catch oom();
    }
};

test "Expr.createLiteral" {
    const literal = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    defer literal.destroy(testing_alloc);
}

test "Expr.createGrouping" {
    const literal = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const grouping = Expr.createGrouping(testing_alloc, literal);
    defer grouping.destroy(testing_alloc);
}

test "Expr.createUnary" {
    const literal = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const unary = Expr.createUnary(testing_alloc, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, literal);
    defer unary.destroy(testing_alloc);
}

test "Expr.createBinary" {
    const left = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const right = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const binary = Expr.createBinary(testing_alloc, left, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, right);
    defer binary.destroy(testing_alloc);
}

test "Expr.createLogical" {
    const left = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const right = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const binary = Expr.createLogical(testing_alloc, left, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, right);
    defer binary.destroy(testing_alloc);
}

test "Expr.createAssignment" {
    const right = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const assignment = Expr.createAssignment(testing_alloc, .{ .type = .identifier, .lexeme = "number", .line = 1, .column = 1 }, right);
    defer assignment.destroy(testing_alloc);
}

test "Expr.createVariable" {
    const variable = Expr.createVariable(testing_alloc, .{ .type = .identifier, .lexeme = "number", .line = 1, .column = 1 });
    defer variable.destroy(testing_alloc);
}

test "Stmt.createExpr" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    defer expr_stmt.destroy(testing_alloc);
}

test "Stmt.createBlock" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr_stmt = Stmt.createExpr(testing_alloc, expr);

    const stmts = try testing_alloc.alloc(*Stmt, 1);
    stmts[0] = expr_stmt;

    const block = Stmt.createBlock(testing_alloc, stmts);
    defer block.destroy(testing_alloc);
}

test "Stmt.createReturn" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const return_stmt = Stmt.createReturn(testing_alloc, .{ .type = .@"return", .lexeme = "return", .line = 1, .column = 1 }, expr);
    defer return_stmt.destroy(testing_alloc);
}

test "Stmt.createChannel" {
    const channel = Stmt.createChannel(testing_alloc, .{ .type = .identifier, .lexeme = "chn", .line = 1, .column = 1 }, .int);
    defer channel.destroy(testing_alloc);
}

test "Stmt.createChannelWrite" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
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
    const condition = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    const body = try testing_alloc.alloc(*Stmt, 1);
    body[0] = expr_stmt;
    const loop = Stmt.createLoop(testing_alloc, condition, body);
    defer loop.destroy(testing_alloc);
}

test "Stmt.createIf" {
    const condition = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    const if_stmt = Stmt.createIf(testing_alloc, condition, expr_stmt, null);
    defer if_stmt.destroy(testing_alloc);
}

test "Stmt.createVariable" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const loop = Stmt.createVariable(
        testing_alloc,
        .{ .type = .identifier, .lexeme = "name", .line = 1, .column = 1 },
        .{ .type = .string, .lexeme = "string", .line = 1, .column = 1 },
        false,
        expr,
    );
    defer loop.destroy(testing_alloc);
}

test "Stmt.createFunction" {
    const expr = Expr.createLiteral(testing_alloc, .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr_stmt = Stmt.createExpr(testing_alloc, expr);
    const variable = Stmt.createVariable(
        testing_alloc,
        .{ .type = .identifier, .lexeme = "param", .line = 1, .column = 1 },
        .{ .type = .int, .lexeme = "int", .line = 1, .column = 1 },
        true,
        null,
    );

    const params = try testing_alloc.alloc(*Stmt, 1);
    params[0] = variable;
    const body = try testing_alloc.alloc(*Stmt, 1);
    body[0] = expr_stmt;

    const function = Stmt.createFunction(
        testing_alloc,
        .{ .type = .identifier, .lexeme = "name", .line = 1, .column = 1 },
        .{ .type = .int, .lexeme = "int", .line = 1, .column = 1 },
        params,
        body,
    );
    defer function.destroy(testing_alloc);
}

const testing = std.testing;
const testing_alloc = testing.allocator;

const Token = @import("Token.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
const definitions = @import("shared").definitions;
const oom = @import("shared").oom;
const Integer = definitions.Integer;
const Float = definitions.Float;
const FlowType = definitions.FlowType;
const FlowValue = definitions.FlowValue;
