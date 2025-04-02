alloc: Allocator,
program: []const *Stmt,

pub fn init(alloc: Allocator, program: []const *Stmt) Sema {
    return .{
        .program = program,
        .alloc = alloc,
    };
}

pub fn deinit(self: *Sema) void {
    _ = self;
}

pub fn analyse(self: *Sema) !void {
    _ = self;
}

const Sema = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Stmt = ast.Stmt;
const Expr = ast.Expr;
