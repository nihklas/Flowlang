alloc: Allocator,
program: []const *Stmt,
constants: std.ArrayList(FlowValue),
has_error: bool = false,
last_expr_type: ?FlowType = null,
last_expr_sideeffect: bool = false,

pub fn init(alloc: Allocator, program: []const *Stmt) Sema {
    return .{
        .alloc = alloc,
        .program = program,
        .constants = .init(alloc),
    };
}

pub fn deinit(self: *Sema) void {
    self.constants.deinit();
}

pub fn analyse(self: *Sema) !void {
    for (self.program) |stmt| {
        try self.visitStmt(stmt);
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

fn visitStmt(self: *Sema, stmt: *Stmt) !void {
    self.last_expr_type = null;
    switch (stmt.*) {
        .expr => try self.visitExpr(stmt.expr.expr),
        .print => try self.visitExpr(stmt.print.expr),
        .variable => try self.visitVarDecl(stmt),
        else => @panic("Illegal Instruction"),
    }
}

fn visitVarDecl(self: *Sema, stmt: *Stmt) !void {
    // TODO: track all variables globally, maybe just when we have assignments
    if (stmt.variable.value) |value| {
        try self.visitExpr(value);
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
}

fn visitExpr(self: *Sema, expr: *Expr) !void {
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
        .unary => {
            try self.visitExpr(expr.unary.expr);
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
        },
        .grouping => try self.visitExpr(expr.grouping.expr),
        .assignment => try self.visitExpr(expr.assignment.value),
        .binary => {
            try self.visitExpr(expr.binary.lhs);
            const left_type = self.last_expr_type.?;
            var had_sideeffect = self.last_expr_sideeffect;

            try self.visitExpr(expr.binary.rhs);
            const right_type = self.last_expr_type.?;
            had_sideeffect = had_sideeffect or self.last_expr_sideeffect;

            switch (expr.binary.op.type) {
                .@"<", .@"<=", .@">=", .@">", .@"-", .@"*", .@"/" => {
                    if (!isNumeric(left_type)) {
                        error_reporter.reportError(
                            expr.binary.lhs.getToken(),
                            "Expected left operand of '{s}' to be int or float, got '{s}'",
                            .{ @tagName(expr.binary.op.type), @tagName(left_type) },
                        );
                        self.has_error = true;
                    }

                    if (!isNumeric(right_type)) {
                        error_reporter.reportError(
                            expr.binary.rhs.getToken(),
                            "Expected right operand of '{s}' to be int or float, got '{s}'",
                            .{ @tagName(expr.binary.op.type), @tagName(right_type) },
                        );
                        self.has_error = true;
                    }
                },
                .@"+" => {
                    if (!isNumeric(left_type)) {
                        error_reporter.reportError(
                            expr.binary.lhs.getToken(),
                            "Expected left operand of '{s}' to be int, float or string, got '{s}'",
                            .{ @tagName(expr.binary.op.type), @tagName(left_type) },
                        );
                        self.has_error = true;
                    }

                    if (!isNumeric(right_type)) {
                        error_reporter.reportError(
                            expr.binary.rhs.getToken(),
                            "Expected right operand of '{s}' to be int, float or string, got '{s}'",
                            .{ @tagName(expr.binary.op.type), @tagName(right_type) },
                        );
                        self.has_error = true;
                    }
                },
                .@"==", .@"!=" => {
                    if (left_type != right_type and !had_sideeffect) {
                        // if the types are unequal, we already know the answer to this operation
                        const new_node = Expr.createLiteral(
                            self.alloc,
                            expr.binary.op,
                            .{ .bool = expr.binary.op.type == .@"!=" },
                        );
                        expr.* = new_node.*;
                    }
                },
                .@"." => {},
                else => unreachable,
            }
        },
        .logical => {
            try self.visitExpr(expr.logical.lhs);
            try self.visitExpr(expr.logical.rhs);
            self.last_expr_type = .bool;
        },
        .variable => {
            self.last_expr_type = .null;
        },
    }
}

fn constant(self: *Sema, value: FlowValue) void {
    for (self.constants.items) |c| {
        if (c.equals(value)) return;
    }
    self.constants.append(value) catch @panic("OOM");
}

fn isNumeric(value_type: FlowType) bool {
    return value_type == .int or value_type == .float;
}

const Expr = @import("ast.zig").Expr;
const Stmt = @import("ast.zig").Stmt;

const Sema = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const FlowValue = @import("shared").definitions.FlowValue;
const FlowType = @import("shared").definitions.ValueType;
const error_reporter = @import("error_reporter.zig");
