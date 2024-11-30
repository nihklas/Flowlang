alloc: Allocator,
program: []const *Stmt,
constants: std.ArrayList(FlowValue),
globals: std.StringHashMapUnmanaged(Global),
has_error: bool = false,
last_expr_type: ?FlowType = null,
last_expr_sideeffect: bool = false,

pub fn init(alloc: Allocator, program: []const *Stmt) Sema {
    return .{
        .alloc = alloc,
        .program = program,
        .constants = .init(alloc),
        .globals = .empty,
    };
}

pub fn deinit(self: *Sema) void {
    self.constants.deinit();
    self.globals.deinit(self.alloc);
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
            for (block.stmts) |inner_stmt| {
                try self.statement(inner_stmt);
            }
        },
        else => @panic("Illegal Instruction"),
    }
}

fn varDeclaration(self: *Sema, stmt: *Stmt) !void {
    // TODO: Track locals
    stmt.variable.global = true;
    self.constant(.{ .string = stmt.variable.name.lexeme });

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
    } else {
        if (self.globals.get(stmt.variable.name.lexeme)) |global| {
            error_reporter.reportError(
                stmt.variable.name,
                "Variable '{s}' already defined at {d}:{d}, duplicated definition",
                .{ stmt.variable.name.lexeme, global.token.line, global.token.column },
            );
            self.has_error = true;
        } else {
            const type_hint: FlowType = switch (stmt.variable.type_hint.?.type) {
                .bool => .bool,
                .int => .int,
                .float => .float,
                .string => .string,
                else => unreachable,
            };
            try self.globals.put(
                self.alloc,
                stmt.variable.name.lexeme,
                .{ .token = stmt.variable.name, .type = type_hint, .constant = stmt.variable.constant },
            );
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
        .variable => {
            const name = expr.variable.name.lexeme;

            // TODO: Check for locals

            if (self.globals.get(name)) |global| {
                self.last_expr_type = global.type;
                expr.variable.global = true;
            } else {
                error_reporter.reportError(expr.variable.name, "Undefined variable: '{s}'", .{name});
                self.has_error = true;
            }
        },
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
        .@"+", .@"-", .@"*", .@"/" => {
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

    if (self.globals.get(expr.assignment.name.lexeme)) |global| {
        if (global.constant) {
            error_reporter.reportError(expr.assignment.name, "Global is declared const", .{});
            self.has_error = true;
        }

        if (resulted_type != .null and global.type != resulted_type) {
            error_reporter.reportError(
                expr.assignment.value.getToken(),
                "Type mismatch: Expected value of type '{s}', got '{s}'",
                .{ @tagName(global.type), @tagName(resulted_type) },
            );
            self.has_error = true;
        }

        expr.assignment.global = true;
    } else {
        error_reporter.reportError(expr.assignment.name, "Undefined variable: '{s}'", .{expr.assignment.name.lexeme});
        self.has_error = true;
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

const Global = struct {
    token: Token,
    constant: bool,
    type: FlowType,
};

const Expr = @import("ast.zig").Expr;
const Stmt = @import("ast.zig").Stmt;

const Token = @import("Token.zig");

const Sema = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const FlowValue = @import("shared").definitions.FlowValue;
const FlowType = @import("shared").definitions.ValueType;
const error_reporter = @import("error_reporter.zig");
