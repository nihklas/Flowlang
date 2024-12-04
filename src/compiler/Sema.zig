alloc: Allocator,
program: []const *Stmt,
constants: std.ArrayList(FlowValue),
variables: Stack(Variable, MAX_LOCAL_SIZE, true),
globals: std.StringHashMap(Variable),
scope_depth: usize = 0,
has_error: bool = false,
last_expr_type: ?FlowType = null,
last_expr_sideeffect: bool = false,
loop_level: usize = 0,

pub fn init(alloc: Allocator, program: []const *Stmt) !Sema {
    return .{
        .alloc = alloc,
        .program = program,
        .constants = .init(alloc),
        .variables = try .init(alloc),
        .globals = .init(alloc),
    };
}

pub fn deinit(self: *Sema) void {
    self.constants.deinit();
    self.variables.deinit();
    self.globals.deinit();
}

pub fn analyse(self: *Sema) !void {
    for (self.program) |stmt| {
        try self.statement(stmt);
    }

    // TODO: maybe add 'constant_long' to be able to store more constant values
    // If we add that, should we sort the most used constant into the 'constant_short' area?
    if (self.constants.items.len > std.math.maxInt(u8)) {
        return error.TooManyConstants;
    }

    if (self.has_error) {
        return error.CompileError;
    }
}

fn statement(self: *Sema, stmt: *Stmt) !void {
    self.last_expr_type = null;
    switch (stmt.*) {
        .expr => self.expression(stmt.expr.expr),
        .print => self.expression(stmt.print.expr),
        .variable => try self.varDeclaration(stmt),
        .@"if" => |if_stmt| {
            self.expression(if_stmt.condition);
            try self.statement(if_stmt.true_branch);
            if (if_stmt.false_branch) |false_branch| {
                try self.statement(false_branch);
            }
        },
        .block => |block| {
            self.beginScope();
            for (block.stmts) |inner_stmt| {
                try self.statement(inner_stmt);
            }

            var locals_count: usize = 0;
            var stack_idx: usize = 0;
            while (stack_idx < self.variables.stack_top) : (stack_idx += 1) {
                const local = self.variables.at(stack_idx);
                if (local.scope_depth < self.scope_depth) {
                    break;
                }
                locals_count += 1;
            }
            stmt.block.local_count = locals_count;

            self.endScope();
        },
        .loop => |loop| {
            self.expression(loop.condition);
            self.loop_level += 1;
            try self.statement(loop.body);
            self.loop_level -= 1;
        },
        .@"break" => |break_stmt| {
            if (self.loop_level < 1) {
                error_reporter.reportError(break_stmt.token, "'break' is only allowed in loops", .{});
                self.has_error = true;
            }
        },
        .@"continue" => |continue_stmt| {
            if (self.loop_level < 1) {
                error_reporter.reportError(continue_stmt.token, "'continue' is only allowed in loops", .{});
                self.has_error = true;
            }
        },
        else => @panic("Illegal Instruction"),
    }
}

fn varDeclaration(self: *Sema, stmt: *Stmt) !void {
    const is_local = self.scope_depth > 0;
    stmt.variable.global = !is_local;

    if (!is_local) {
        self.constant(.{ .string = stmt.variable.name.lexeme });
    }

    if (stmt.variable.value) |value| {
        self.expression(value);
        const value_type = self.last_expr_type.?;
        if (stmt.variable.type_hint != null) {
            const correct_type = if (value_type == .null)
                true
            else switch (stmt.variable.type_hint.?.type) {
                .bool => value_type == .bool,
                .string => value_type == .string,
                .int => value_type == .int,
                .float => value_type == .float,
                else => @panic("Invalid Type Hint"),
            };

            if (!correct_type) {
                error_reporter.reportError(
                    stmt.variable.type_hint.?,
                    "Type Mismatch: Expected '{s}', got '{?}'",
                    .{
                        @tagName(stmt.variable.type_hint.?.type),
                        value_type,
                    },
                );
                self.has_error = true;
            }
        } else if (value_type != .null) {
            var expr_token = value.getToken();
            expr_token.type = switch (value_type) {
                .bool => .bool,
                .int => .int,
                .float => .float,
                .string => .string,
                .null => unreachable,
            };
            stmt.variable.type_hint = expr_token;
        }
    }

    if (stmt.variable.type_hint == null) {
        error_reporter.reportError(stmt.variable.name, "Cannot infer type of variable", .{});
        self.has_error = true;
    }

    const existent = blk: {
        var stack_idx: usize = 0;
        while (stack_idx < self.variables.stack_top) : (stack_idx += 1) {
            const local = self.variables.at(stack_idx);
            if (local.scope_depth == self.scope_depth and std.mem.eql(u8, stmt.variable.name.lexeme, local.token.lexeme)) {
                break :blk local;
            }
        }
        break :blk null;
    };

    if (existent) |found| {
        error_reporter.reportError(
            stmt.variable.name,
            "Variable '{s}' already defined at {d}:{d}, duplicated definition",
            .{ stmt.variable.name.lexeme, found.token.line, found.token.column },
        );
        self.has_error = true;
    } else if (stmt.variable.type_hint) |type_hint_token| {
        const type_hint = tokenToType(type_hint_token);
        const variable: Variable = .{
            .token = stmt.variable.name,
            .type = type_hint,
            .constant = stmt.variable.constant,
            .scope_depth = self.scope_depth,
        };

        if (self.scope_depth == 0) {
            try self.globals.put(stmt.variable.name.lexeme, variable);
        } else {
            self.variables.push(variable);
        }
    }
}

fn expression(self: *Sema, expr: *Expr) void {
    switch (expr.*) {
        .literal => {
            switch (expr.literal.value) {
                .int => |int| self.constant(.{ .int = int }),
                .float => |float| self.constant(.{ .float = float }),
                .string => |string| self.constant(.{ .string = string }),
                .null, .bool => {},
            }
            self.last_expr_type = std.meta.activeTag(expr.literal.value);
        },
        .unary => self.unary(expr),
        .grouping => self.expression(expr.grouping.expr),
        .assignment => self.assignment(expr),
        .binary => self.binary(expr),
        .logical => {
            self.expression(expr.logical.lhs);
            self.expression(expr.logical.rhs);
            self.last_expr_type = .bool;
        },
        .variable => self.variableExpr(expr),
    }
}

fn unary(self: *Sema, expr: *Expr) void {
    self.expression(expr.unary.expr);
    switch (expr.unary.op.type) {
        .@"!" => {
            self.last_expr_type = .bool;
        },
        .@"-" => {
            if (self.last_expr_type != .int and self.last_expr_type != .float) {
                error_reporter.reportError(
                    expr.unary.expr.getToken(),
                    "Expected expression following '-' to be int or float, got '{s}'",
                    .{@tagName(self.last_expr_type.?)},
                );
                self.has_error = true;
            }
        },
        else => unreachable,
    }
}

fn binary(self: *Sema, expr: *Expr) void {
    self.expression(expr.binary.lhs);
    const left_type = self.last_expr_type.?;

    self.expression(expr.binary.rhs);
    const right_type = self.last_expr_type.?;

    // TODO: What about null?
    if (left_type != right_type and expr.binary.op.type != .@".") {
        error_reporter.reportError(
            expr.binary.op,
            "Cannot compare value of type '{s}' to value of type '{s}'",
            .{ @tagName(left_type), @tagName(right_type) },
        );
        self.has_error = true;
    }

    switch (expr.binary.op.type) {
        .@"<", .@"<=", .@">=", .@">" => {
            self.checkNumericOperands(
                expr.binary.lhs.getToken(),
                left_type,
                expr.binary.rhs.getToken(),
                right_type,
                expr.binary.op.type,
            );
            self.last_expr_type = .bool;
        },
        .@"+", .@"-", .@"*", .@"/", .@"%" => {
            self.checkNumericOperands(
                expr.binary.lhs.getToken(),
                left_type,
                expr.binary.rhs.getToken(),
                right_type,
                expr.binary.op.type,
            );
        },
        .@"==", .@"!=" => {
            self.last_expr_type = .bool;
        },
        .@"." => {
            self.last_expr_type = .string;
        },
        else => unreachable,
    }
}

fn assignment(self: *Sema, expr: *Expr) void {
    self.expression(expr.assignment.value);
    const resulted_type = self.last_expr_type.?;
    const name = expr.assignment.name.lexeme;

    const maybe_variable, const local_idx = blk: {
        var stack_idx: usize = 0;
        while (stack_idx < self.variables.stack_top) : (stack_idx += 1) {
            const local = self.variables.at(stack_idx);
            if (local.scope_depth <= self.scope_depth and std.mem.eql(u8, name, local.token.lexeme)) {
                break :blk .{ local, stack_idx };
            }
        }
        break :blk .{ null, null };
    };

    const variable = maybe_variable orelse self.globals.get(name) orelse {
        error_reporter.reportError(expr.variable.name, "Undefined variable: '{s}'", .{name});
        self.has_error = true;
        return;
    };

    if (resulted_type != .null and variable.type != resulted_type) {
        error_reporter.reportError(
            expr.assignment.value.getToken(),
            "Type mismatch: Expected value of type '{s}', got '{s}'",
            .{ @tagName(variable.type), @tagName(resulted_type) },
        );
        self.has_error = true;
    }

    self.last_expr_type = variable.type;

    if (local_idx) |idx| {
        if (idx > std.math.maxInt(u8)) {
            // This kinda should not be really possible, hopefully
            @panic("Too many locals");
        }
        expr.assignment.local_idx = @intCast(idx);
    } else {
        expr.assignment.global = true;
    }
}

fn variableExpr(self: *Sema, expr: *Expr) void {
    const name = expr.variable.name.lexeme;

    const maybe_variable, const local_idx = blk: {
        var stack_idx: usize = 0;
        while (stack_idx < self.variables.stack_top) : (stack_idx += 1) {
            const local = self.variables.at(stack_idx);
            if (local.scope_depth <= self.scope_depth and std.mem.eql(u8, name, local.token.lexeme)) {
                break :blk .{ local, self.variables.stack_top - 1 - stack_idx };
            }
        }
        break :blk .{ null, null };
    };

    const variable = maybe_variable orelse self.globals.get(name) orelse {
        error_reporter.reportError(expr.variable.name, "Undefined variable: '{s}'", .{name});
        self.has_error = true;
        return;
    };
    self.last_expr_type = variable.type;

    if (local_idx) |idx| {
        if (idx > std.math.maxInt(u8)) {
            // This kinda should not be really possible, hopefully
            @panic("Too many locals");
        }
        expr.variable.local_idx = @intCast(idx);
    } else {
        expr.variable.global = true;
    }
}

fn beginScope(self: *Sema) void {
    self.scope_depth += 1;
}

fn endScope(self: *Sema) void {
    self.scope_depth -= 1;
    while (self.variables.stack_top > 0 and self.variables.at(0).scope_depth > self.scope_depth) {
        _ = self.variables.pop();
    }
}

fn constant(self: *Sema, value: FlowValue) void {
    for (self.constants.items) |c| {
        if (c.equals(value)) return;
    }
    self.constants.append(value) catch @panic("OOM");
}

fn checkNumericOperands(self: *Sema, left: Token, lhs: FlowType, right: Token, rhs: FlowType, op: Token.Type) void {
    if (!isNumeric(lhs)) {
        error_reporter.reportError(
            left,
            "Expected left operand of '{s}' to be int or float, got '{s}'",
            .{ @tagName(op), @tagName(lhs) },
        );
        self.has_error = true;
    }

    if (!isNumeric(rhs)) {
        error_reporter.reportError(
            right,
            "Expected right operand of '{s}' to be int or float, got '{s}'",
            .{ @tagName(op), @tagName(rhs) },
        );
        self.has_error = true;
    }
}

fn isNumeric(value_type: FlowType) bool {
    return value_type == .int or value_type == .float;
}

fn tokenToType(token: Token) FlowType {
    return switch (token.type) {
        .bool => .bool,
        .int => .int,
        .float => .float,
        .string => .string,
        else => unreachable,
    };
}

const Variable = struct {
    token: Token,
    constant: bool,
    type: FlowType,
    scope_depth: usize,
};

const Expr = @import("ast.zig").Expr;
const Stmt = @import("ast.zig").Stmt;

const Token = @import("Token.zig");

const Sema = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const FlowValue = @import("shared").definitions.FlowValue;
const FlowType = @import("shared").definitions.ValueType;
const Stack = @import("shared").Stack;

const error_reporter = @import("error_reporter.zig");

const MAX_LOCAL_SIZE = 1024;
