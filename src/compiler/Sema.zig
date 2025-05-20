alloc: Allocator,
program: []const *Stmt,
errors: std.ArrayListUnmanaged(ErrorInfo),
types: std.AutoHashMapUnmanaged(*const Expr, FlowType),

pub fn init(alloc: Allocator, program: []const *Stmt) Sema {
    return .{
        .program = program,
        .alloc = alloc,
        .errors = .empty,
        .types = .empty,
    };
}

pub fn deinit(self: *Sema) void {
    self.errors.deinit(self.alloc);
    self.types.deinit(self.alloc);
}

/// Analyses the ast. Applies the following checks:
///
/// - type checking
/// - Unknown variables
/// - Mutation of constants
/// - duplicate declarations (Functions and Variables in the same scope)
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
    switch (expr.*) {
        .literal => self.putType(expr, expr.literal.value),
        .binary => |binary| {
            self.analyseExpr(binary.lhs);
            self.analyseExpr(binary.rhs);
            const left_type = self.getType(binary.lhs).?;
            const right_type = self.getType(binary.rhs).?;
            switch (binary.op.type) {
                .@".", .@".=" => self.putType(expr, .string),
                .@"==", .@"!=" => {
                    // if the two types are non-null and different, print error
                    if (left_type != .null and right_type != .null and left_type != right_type) {
                        self.pushError(.{ .token = binary.op, .err = TypeError.EqualityCheckOfUnequalTypes });
                    }
                    self.putType(expr, .bool);
                },
                .@"<", .@"<=", .@">=", .@">", .@"+", .@"+=", .@"-", .@"-=", .@"*", .@"*=", .@"/", .@"/=", .@"%", .@"%=" => {
                    if (left_type != right_type) {
                        self.pushError(.{ .token = binary.op, .err = TypeError.ArithmeticWithUnequalTypes });
                    }
                    if (left_type != .int and left_type != .float) {
                        self.pushError(.{ .token = binary.lhs.getToken(), .err = TypeError.ArithmeticWithNonNumeric });
                    }
                    if (right_type != .int and right_type != .float) {
                        self.pushError(.{ .token = binary.rhs.getToken(), .err = TypeError.ArithmeticWithNonNumeric });
                    }
                    // as they have to be of equal types, we can just use one of them to keep going.
                    // we go with the left one just because.
                    // In case of unequal types, the binary expression still needs a valid type to
                    // be further processed so we unconditionally use the left type
                    self.putType(expr, left_type);
                },
                else => unreachable,
            }
        },
        else => std.debug.panic("Unhandled Expr type: {s}\n", .{@tagName(expr.*)}),
    }
}

fn putType(self: *Sema, expr: *const Expr, t: FlowType) void {
    self.types.put(self.alloc, expr, t) catch oom();
}

fn getType(self: *Sema, expr: *const Expr) ?FlowType {
    return self.types.get(expr);
}

fn pushError(self: *Sema, err: ErrorInfo) void {
    self.errors.append(self.alloc, err) catch oom();
}

fn printError(self: *Sema) void {
    // TODO: Do we need to filter out duplicate errors? Are identical errors a bug in sema?

    for (self.errors.items) |e| {
        switch (e.err) {
            TypeError.EqualityCheckOfUnequalTypes => error_reporter.reportError(e.token, "Operands of Equality-Check (== and !=) have to be of the same type", .{}),
            TypeError.ArithmeticWithNonNumeric => error_reporter.reportError(e.token, "Operand of Arithmetic Operations has to be either int or float", .{}),
            TypeError.ArithmeticWithUnequalTypes => error_reporter.reportError(e.token, "Operands of Arithmetic Operations have to be of the same numeric type", .{}),
        }
    }
}

// -------------------------- UNIT TESTS --------------------------
// TODO:

const ErrorInfo = struct {
    token: Token,
    err: SemaError,
};

// combine all possible errors here
const SemaError = TypeError;

const TypeError = error{
    EqualityCheckOfUnequalTypes,
    ArithmeticWithNonNumeric,
    ArithmeticWithUnequalTypes,
};

// TODO: Add warnings?

const Sema = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Stmt = ast.Stmt;
const Expr = ast.Expr;

const oom = @import("shared").oom;
const FlowType = @import("shared").definitions.FlowType;
const error_reporter = @import("error_reporter.zig");

const Token = @import("Token.zig");
