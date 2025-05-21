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
        .block => |block_stmt| {
            // TODO: add scope level|
            for (block_stmt.stmts) |inner_stmt| {
                self.analyseStmt(inner_stmt);
            }
        },
        .loop => |loop_stmt| {
            self.analyseExpr(loop_stmt.condition);
            for (loop_stmt.body) |inner_stmt| {
                self.analyseStmt(inner_stmt);
            }
            if (loop_stmt.inc) |inc| {
                self.analyseStmt(inc);
            }
        },
        .@"break", .@"continue" => {},
        .@"if" => |if_stmt| {
            self.analyseExpr(if_stmt.condition);
            self.analyseStmt(if_stmt.true_branch);
            if (if_stmt.false_branch) |false_branch| {
                self.analyseStmt(false_branch);
            }
        },
        .channel, .channel_read, .channel_write => std.debug.panic("Channels are not yet supported\n", .{}),
        .variable => |var_stmt| {
            // TODO: Variable handling
            if (var_stmt.value) |value| {
                self.analyseExpr(value);
            }
            // TODO: Variables have to have either a type hint or initial value
        },
        .function => |function| {
            // TODO: assign callable to variable handling
            // TODO: open scope
            for (function.params) |param| {
                self.analyseStmt(param);
            }
            for (function.body) |inner_stmt| {
                self.analyseStmt(inner_stmt);
            }
        },
        .@"return" => |return_stmt| {
            if (return_stmt.value) |value| {
                self.analyseExpr(value);
            }
        },
    }
}

fn analyseExpr(self: *Sema, expr: *const Expr) void {
    switch (expr.*) {
        .literal => self.putType(expr, expr.literal.value),
        .grouping => {
            self.analyseExpr(expr.grouping.expr);
            self.putType(expr, self.getType(expr.grouping.expr).?);
        },
        .unary => |unary| {
            self.analyseExpr(unary.expr);
            switch (unary.op.type) {
                .@"-" => {
                    const value_type = self.getType(unary.expr).?;
                    if (value_type != .int and value_type != .float) {
                        self.pushError(.{ .token = unary.op, .err = TypeError.NegateWithNonNumeric });
                    }
                    self.putType(expr, value_type);
                },
                .@"!" => self.putType(expr, .bool),
                else => unreachable,
            }
        },
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
                    var both_numeric = true;
                    if (left_type != .int and left_type != .float) {
                        self.pushError(.{ .token = binary.lhs.getToken(), .err = TypeError.ArithmeticWithNonNumeric });
                        both_numeric = false;
                    }
                    if (right_type != .int and right_type != .float) {
                        self.pushError(.{ .token = binary.rhs.getToken(), .err = TypeError.ArithmeticWithNonNumeric });
                        both_numeric = false;
                    }
                    if (both_numeric and left_type != right_type) {
                        self.pushError(.{ .token = binary.op, .err = TypeError.ArithmeticWithUnequalTypes });
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
        .logical => |logical| {
            self.analyseExpr(logical.lhs);
            self.analyseExpr(logical.rhs);
            self.putType(expr, .bool);
        },
        .assignment => |assignment| {
            self.analyseExpr(assignment.value);
            // TODO: Check for existing variable and check if expected type is correct
            self.putType(expr, self.getType(assignment.value).?);
        },
        .variable => std.debug.panic("Variable Expressions are not yet supported bei Sema\n", .{}),
        .call => |call| {
            self.analyseExpr(call.expr);
            for (call.args) |arg_expr| {
                self.analyseExpr(arg_expr);
            }

            if (!isCallable(self.getType(call.expr).?)) {
                self.pushError(.{ .token = call.expr.getToken(), .err = TypeError.NotACallable });
            }
            // TODO: resolve correct return type
            self.putType(expr, .null);
        },
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

fn isCallable(t: FlowType) bool {
    return t == .function or t == .builtin_fn;
}

fn printError(self: *Sema) void {
    // TODO: Do we need to filter out duplicate errors? Are identical errors a bug in sema?

    for (self.errors.items) |e| {
        switch (e.err) {
            TypeError.EqualityCheckOfUnequalTypes => error_reporter.reportError(e.token, "Operands of Equality-Check (== and !=) have to be of the same type", .{}),
            TypeError.ArithmeticWithNonNumeric => error_reporter.reportError(e.token, "Operand of Arithmetic Operations has to be either int or float", .{}),
            TypeError.ArithmeticWithUnequalTypes => error_reporter.reportError(e.token, "Operands of Arithmetic Operations have to be of the same numeric type", .{}),
            TypeError.NegateWithNonNumeric => error_reporter.reportError(e.token, "Operand of Negate Operations has to be either int or float", .{}),
            TypeError.NotACallable => error_reporter.reportError(e.token, "Can only call callables", .{}),
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
    NegateWithNonNumeric,
    NotACallable,
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
