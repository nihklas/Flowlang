pub const TypeHint = struct {
    type: FlowType,
    token: Token,
};

pub const Expr = union(enum) {
    pub const Literal = union(enum) {
        null: void,
        bool: bool,
        int: Integer,
        float: Float,
        string: []const u8,
        builtin_fn: void,
        function: void,
        array: []*Expr,
    };

    literal: struct { token: Token, value: Literal },
    grouping: struct { token: Token, expr: *Expr },
    unary: struct { op: Token, expr: *Expr },
    binary: struct { lhs: *Expr, op: Token, rhs: *Expr },
    logical: struct { lhs: *Expr, op: Token, rhs: *Expr },
    assignment: struct { variable: *Expr, value: *Expr },
    variable: struct { name: Token },
    call: struct { expr: *Expr, args: []*Expr },
    index: struct { expr: *Expr, bracket: Token, index: *Expr },

    pub fn createLiteral(alloc: Allocator, token: Token, value: Literal) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .literal = .{ .token = token, .value = value },
        };
        return new_expr;
    }

    pub fn createGrouping(alloc: Allocator, token: Token, expr: *Expr) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .grouping = .{ .token = token, .expr = expr },
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

    pub fn createAssignment(alloc: Allocator, variable: *Expr, expr: *Expr) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .assignment = .{ .variable = variable, .value = expr },
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

    pub fn createIndex(alloc: Allocator, expr: *Expr, bracket: Token, index: *Expr) *Expr {
        const new_expr = Expr.create(alloc);
        new_expr.* = .{
            .index = .{
                .expr = expr,
                .bracket = bracket,
                .index = index,
            },
        };
        return new_expr;
    }

    pub fn getToken(self: *const Expr) Token {
        return switch (self.*) {
            .literal => self.literal.token,
            .grouping => self.grouping.token,
            .unary => self.unary.op,
            .binary => self.binary.op,
            .logical => self.logical.op,
            .assignment => self.assignment.variable.getToken(),
            .variable => self.variable.name,
            .call => self.call.expr.getToken(),
            .index => self.index.bracket,
        };
    }

    fn create(alloc: Allocator) *Expr {
        return alloc.create(Expr) catch oom();
    }
};

pub const Stmt = union(enum) {
    expr: struct { expr: *Expr },

    block: struct { stmts: []*Stmt },

    loop: struct { condition: *Expr, body: []*Stmt, inc: ?*Expr = null },
    @"break": struct { token: Token },
    @"continue": struct { token: Token },

    @"if": struct { condition: *Expr, true_branch: *Stmt, false_branch: ?*Stmt },

    variable: struct {
        name: Token,
        constant: bool,
        value: ?*Expr,
        type_hint: ?TypeHint,
    },

    function: struct { name: Token, ret_type: TypeHint, params: []*Stmt, body: []*Stmt },
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

    pub fn createVariable(alloc: Allocator, name: Token, type_hint: ?TypeHint, constant: bool, value: ?*Expr) *Stmt {
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

    pub fn createFunction(alloc: Allocator, name: Token, ret_type: TypeHint, params: []*Stmt, body: []*Stmt) *Stmt {
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

    fn create(alloc: Allocator) *Stmt {
        return alloc.create(Stmt) catch oom();
    }
};

test "Expr.createLiteral" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    _ = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
}

test "Expr.createGrouping" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const literal = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 2 }, .{ .float = 12.34 });
    _ = Expr.createGrouping(testing_arena_state.allocator(), .{ .type = .@"(", .lexeme = "(", .line = 1, .column = 1 }, literal);
}

test "Expr.createUnary" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const literal = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    _ = Expr.createUnary(testing_arena_state.allocator(), .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, literal);
}

test "Expr.createBinary" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const left = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const right = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    _ = Expr.createBinary(testing_arena_state.allocator(), left, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, right);
}

test "Expr.createLogical" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const left = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const right = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    _ = Expr.createLogical(testing_arena_state.allocator(), left, .{ .type = .@"-", .lexeme = "-", .line = 1, .column = 1 }, right);
}

test "Expr.createAssignment" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const right = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    _ = Expr.createAssignment(testing_arena_state.allocator(), .{ .type = .identifier, .lexeme = "number", .line = 1, .column = 1 }, right);
}

test "Expr.createVariable" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    _ = Expr.createVariable(testing_arena_state.allocator(), .{ .type = .identifier, .lexeme = "number", .line = 1, .column = 1 });
}

test "Stmt.createExpr" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const expr = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    _ = Stmt.createExpr(testing_arena_state.allocator(), expr);
}

test "Stmt.createBlock" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const expr = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr_stmt = Stmt.createExpr(testing_arena_state.allocator(), expr);

    const stmts = try testing_arena_state.allocator().alloc(*Stmt, 1);
    stmts[0] = expr_stmt;

    _ = Stmt.createBlock(testing_arena_state.allocator(), stmts);
}

test "Stmt.createReturn" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const expr = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    _ = Stmt.createReturn(testing_arena_state.allocator(), .{ .type = .@"return", .lexeme = "return", .line = 1, .column = 1 }, expr);
}

test "Stmt.createLoop" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const condition = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr_stmt = Stmt.createExpr(testing_arena_state.allocator(), expr);
    const body = try testing_arena_state.allocator().alloc(*Stmt, 1);
    body[0] = expr_stmt;
    _ = Stmt.createLoop(testing_arena_state.allocator(), condition, body);
}

test "Stmt.createIf" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const condition = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr_stmt = Stmt.createExpr(testing_arena_state.allocator(), expr);
    _ = Stmt.createIf(testing_arena_state.allocator(), condition, expr_stmt, null);
}

test "Stmt.createVariable" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const expr = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    _ = Stmt.createVariable(
        testing_arena_state.allocator(),
        .{ .type = .identifier, .lexeme = "name", .line = 1, .column = 1 },
        null,
        false,
        expr,
    );
}

test "Stmt.createFunction" {
    var testing_arena_state: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer testing_arena_state.deinit();

    const expr = Expr.createLiteral(testing_arena_state.allocator(), .{ .type = .number, .lexeme = "12.34", .line = 1, .column = 1 }, .{ .float = 12.34 });
    const expr_stmt = Stmt.createExpr(testing_arena_state.allocator(), expr);
    const variable = Stmt.createVariable(
        testing_arena_state.allocator(),
        .{ .type = .identifier, .lexeme = "param", .line = 1, .column = 1 },
        null,
        true,
        null,
    );

    const params = try testing_arena_state.allocator().alloc(*Stmt, 1);
    params[0] = variable;
    const body = try testing_arena_state.allocator().alloc(*Stmt, 1);
    body[0] = expr_stmt;

    _ = Stmt.createFunction(
        testing_arena_state.allocator(),
        .{ .type = .identifier, .lexeme = "name", .line = 1, .column = 1 },
        .{ .type = .primitive(.int), .token = .{ .type = .int, .lexeme = "int", .line = 1, .column = 1 } },
        params,
        body,
    );
}

const Token = @import("Token.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
const definitions = @import("shared").definitions;
const oom = @import("shared").oom;
const Integer = definitions.Integer;
const Float = definitions.Float;
const FlowType = definitions.FlowType;
