alloc: Allocator,
program: []const *Stmt,
constants: std.ArrayList(FlowValue),
has_error: bool = false,
last_expr_type: ?FlowType = null,

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

// TODO: Check types
pub fn analyse(self: *Sema) !void {
    for (self.program) |stmt| {
        try self.visitStmt(stmt);
    }

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
        else => @panic("Illegal Instruction"),
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
                            expr.unary.op,
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
            try self.visitExpr(expr.binary.rhs);
            const right_type = self.last_expr_type.?;

            switch (expr.binary.op.type) {
                .@"<", .@"<=", .@">=", .@">", .@"-", .@"*", .@"/" => {
                    if (!isNumeric(left_type)) {
                        error_reporter.reportError(
                            expr.binary.op,
                            "Expected left operand of '{s}' to be int or float, got '{s}'",
                            .{ @tagName(expr.binary.op.type), @tagName(left_type) },
                        );
                        self.has_error = true;
                    }

                    if (!isNumeric(right_type)) {
                        error_reporter.reportError(
                            expr.binary.op,
                            "Expected right operand of '{s}' to be int or float, got '{s}'",
                            .{ @tagName(expr.binary.op.type), @tagName(right_type) },
                        );
                        self.has_error = true;
                    }
                },
                .@"+" => {
                    // TODO: Does this need checking?
                    // Do we have another operator for string concats?
                },
                .@"==", .@"!=" => {},
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
