alloc: Allocator,
fir: *FIR,
errors: std.ArrayList(ErrorInfo),

pub fn init(alloc: Allocator, fir: *FIR) Sema {
    return .{
        .alloc = alloc,
        .fir = fir,
        .errors = .empty,
    };
}

pub fn deinit(self: *Sema) void {
    _ = self;
}

pub fn analyse(self: *Sema) !void {
    self.traverse();

    if (self.errors.items.len > 0) {
        self.printError();
        return error.SemaError;
    }
}

fn printError(self: *Sema) void {
    _ = self;
}

fn traverse(self: *Sema) void {
    for (0..self.fir.nodes.items.len) |idx| {
        self.traverseStmt(idx);
    }
}

fn traverseStmt(self: *Sema, node_idx: usize) void {
    const node = &self.fir.nodes.items[node_idx];
    switch (node.kind) {
        .pop => {},
        .@"break" => {},
        .@"continue" => {},
        .expr => {},
        .@"return" => {},
        .cond => {},
        .loop => {},
        .variable => {},
    }
}

fn traverseExpr(self: *Sema, expr_idx: usize) usize {
    const expr = self.fir.exprs.items[expr_idx];
    switch (expr.op) {
        .true, .false, .null, .literal, .variable => {},
        .add, .sub, .mul, .div, .mod => {},
        .assign => {},
        .assign_in_array => {},
        .not => {},
        .negate => {},
        .equal, .unequal => {},
        .less, .less_equal, .greater, .greater_equal => {},
        .concat => {},
        .@"and", .@"or" => {},
        .index => {},
        .append => {},
        .call => {},
        .builtin_fn => {},
        .array => {},
        .function => {},
    }

    return expr_idx;
}

const ErrorInfo = struct {
    err: anyerror,
    message: []const u8,
};

const Sema = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const FIR = @import("../ir/FIR.zig");
