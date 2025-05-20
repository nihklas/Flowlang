alloc: Allocator,
program: []const *Stmt,
errors: std.ArrayListUnmanaged(Error),
// type table

pub fn init(alloc: Allocator, program: []const *Stmt) Sema {
    return .{
        .program = program,
        .alloc = alloc,
        .errors = .empty,
    };
}

pub fn deinit(self: *Sema) void {
    self.errors.deinit(self.alloc);
}

/// Analyses the ast. Applies the following checks:
///
/// - Unknown variables
/// - Mutation of constants
/// - duplicate declarations (Functions and Variables in the same scope)
/// - type checking
/// - constant values
///
/// collects all errors in a list and prints them through `error_reporter.zig`.
pub fn analyse(self: *Sema) !void {
    // TODO: analysis

    // can i just iterate over everything once or do i need multiple passes?
    // -> I try for once, if it aint working i do more passes

    for (self.program) |stmt| {
        self.analyseStmt(stmt);
    }

    if (self.errors.items.len > 0) {
        self.printError();
        return error.SemaError;
    }
}

fn analyseStmt(self: *Sema, stmt: *const Stmt) void {
    switch (stmt.*) {
        .expr => |expr_stmt| self.analyseExpr(expr_stmt.expr),
        else => std.debug.panic("Unhandled Stmt type: {s}\n", .{@tagName(stmt.*)}),
    }
}

fn analyseExpr(self: *Sema, expr: *const Expr) void {
    _ = self;
    switch (expr.*) {
        .literal => {},
        .binary => |binary| {
            _ = binary;
            //
        },
        else => std.debug.panic("Unhandled Expr type: {s}\n", .{@tagName(expr.*)}),
    }
}

fn printError(self: *Sema) void {
    for (self.errors.items) |e| {
        switch (e.err_type) {}
    }
}

const Error = struct {
    token: Token,
    err_type: enum {},
};

// TODO: Add warnings?

const Sema = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Stmt = ast.Stmt;
const Expr = ast.Expr;

const Token = @import("Token.zig");
