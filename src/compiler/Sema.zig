alloc: Allocator,
program: []const *Stmt,
constants: std.ArrayList(FlowValue),

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
}

fn visitStmt(self: *Sema, stmt: *Stmt) !void {
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
        },
        .unary => try self.visitExpr(expr.unary.expr),
        .grouping => try self.visitExpr(expr.grouping.expr),
        .assignment => try self.visitExpr(expr.assignment.value),
        .binary => {
            try self.visitExpr(expr.binary.lhs);
            try self.visitExpr(expr.binary.rhs);
        },
        .logical => {
            try self.visitExpr(expr.logical.lhs);
            try self.visitExpr(expr.logical.rhs);
        },
        .variable => {},
    }
}

fn constant(self: *Sema, value: FlowValue) void {
    for (self.constants.items) |c| {
        if (c.equals(value)) return;
    }
    self.constants.append(value) catch @panic("OOM");
}

const Expr = @import("ast.zig").Expr;
const Stmt = @import("ast.zig").Stmt;

const Sema = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const FlowValue = @import("shared").definitions.FlowValue;
