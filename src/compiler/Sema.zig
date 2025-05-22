alloc: Allocator,
program: []const *Stmt,
errors: ErrorList = .empty,
types: TypeTable = .empty,
variables: VariableList = .empty,
current_scope: u8 = 0,
loop_level: u8 = 0,

pub fn init(alloc: Allocator, program: []const *Stmt) Sema {
    return .{
        .program = program,
        .alloc = alloc,
    };
}

pub fn deinit(self: *Sema) void {
    self.errors.deinit(self.alloc);
    self.types.deinit(self.alloc);
    self.variables.deinit(self.alloc);
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
            self.scopeIncr();
            defer self.scopeDecr();

            for (block_stmt.stmts) |inner_stmt| {
                self.analyseStmt(inner_stmt);
            }
        },
        .loop => |loop_stmt| {
            self.scopeIncr();
            defer self.scopeDecr();

            self.loop_level += 1;
            defer self.loop_level -= 1;

            self.analyseExpr(loop_stmt.condition);
            for (loop_stmt.body) |inner_stmt| {
                self.analyseStmt(inner_stmt);
            }
            if (loop_stmt.inc) |inc| {
                self.analyseStmt(inc);
            }
        },
        // TODO: Check for loop context
        inline .@"break", .@"continue" => |keyword| {
            if (self.loop_level == 0) {
                self.pushError(.{
                    .token = keyword.token,
                    .err = ContextError.NotInALoop,
                    .extra_info1 = @tagName(stmt.*),
                });
            }
        },
        .@"if" => |if_stmt| {
            self.analyseExpr(if_stmt.condition);
            self.analyseStmt(if_stmt.true_branch);
            if (if_stmt.false_branch) |false_branch| {
                self.analyseStmt(false_branch);
            }
        },
        .channel, .channel_read, .channel_write => std.debug.panic("Channels are not yet supported\n", .{}),
        .variable => |var_stmt| {
            if (var_stmt.type_hint == null and var_stmt.value == null) {
                self.pushError(.{ .token = var_stmt.name, .err = VariableError.UnresolvableType });
                return;
            }

            if (var_stmt.value) |value| {
                self.analyseExpr(value);
            }

            const variable: Variable = .{
                .name = var_stmt.name,
                .constant = var_stmt.constant,
                .type = self.typeFromVariable(stmt),
                .scope = self.current_scope,
            };

            if (var_stmt.value) |value| {
                if (self.getType(value).? != variable.type) {
                    self.pushError(.{
                        .token = value.getToken(),
                        .err = TypeError.UnexpectedType,
                        .extra_info1 = @tagName(variable.type),
                        .extra_info2 = @tagName(self.getType(value).?),
                    });
                }
            }

            if (self.findVariable(variable.name)) |existing_variable| {
                if (existing_variable.scope == variable.scope) {
                    self.pushError(.{
                        .token = variable.name,
                        .err = VariableError.VariableAlreadyExists,
                        .extra_info1 = variable.name.lexeme,
                    });
                    return;
                }
            }

            self.putVariable(variable);
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
                        self.pushError(.{ .token = unary.op, .err = TypeError.NegateWithNonNumeric, .extra_info1 = @tagName(value_type) });
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
                        self.pushError(.{
                            .token = binary.op,
                            .err = TypeError.EqualityCheckOfUnequalTypes,
                            .extra_info1 = @tagName(left_type),
                            .extra_info2 = @tagName(right_type),
                        });
                    }
                    self.putType(expr, .bool);
                },
                .@"<", .@"<=", .@">=", .@">", .@"+", .@"+=", .@"-", .@"-=", .@"*", .@"*=", .@"/", .@"/=", .@"%", .@"%=" => {
                    var both_numeric = true;
                    if (left_type != .int and left_type != .float) {
                        self.pushError(.{ .token = binary.lhs.getToken(), .err = TypeError.ArithmeticWithNonNumeric, .extra_info1 = @tagName(left_type) });
                        both_numeric = false;
                    }
                    if (right_type != .int and right_type != .float) {
                        self.pushError(.{ .token = binary.rhs.getToken(), .err = TypeError.ArithmeticWithNonNumeric, .extra_info1 = @tagName(right_type) });
                        both_numeric = false;
                    }
                    if (both_numeric and left_type != right_type) {
                        self.pushError(.{
                            .token = binary.op,
                            .err = TypeError.ArithmeticWithUnequalTypes,
                            .extra_info1 = @tagName(left_type),
                            .extra_info2 = @tagName(right_type),
                        });
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
        .variable => |variable| {
            if (self.findVariable(variable.name)) |found_var| {
                self.putType(expr, found_var.type);
            } else {
                self.pushError(.{ .token = variable.name, .err = VariableError.UnknownVariable, .extra_info1 = variable.name.lexeme });
            }
        },
        .call => |call| {
            self.analyseExpr(call.expr);
            for (call.args) |arg_expr| {
                self.analyseExpr(arg_expr);
            }

            if (!isCallable(self.getType(call.expr).?)) {
                self.pushError(.{ .token = call.expr.getToken(), .err = TypeError.NotACallable, .extra_info1 = call.expr.getToken().lexeme });
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

fn putVariable(self: *Sema, variable: Variable) void {
    self.variables.append(self.alloc, variable) catch oom();
}

fn findVariable(self: *Sema, name: Token) ?Variable {
    var rev_iter = std.mem.reverseIterator(self.variables.items);
    while (rev_iter.next()) |variable| {
        if (std.mem.eql(u8, variable.name.lexeme, name.lexeme)) {
            return variable;
        }
    }
    return null;
}

fn scopeIncr(self: *Sema) void {
    self.current_scope += 1;
}

fn scopeDecr(self: *Sema) void {
    self.current_scope -= 1;

    var write_idx: usize = 0;
    for (self.variables.items, 0..) |variable, read_idx| {
        if (variable.scope <= self.current_scope) {
            if (write_idx != read_idx) {
                self.variables.items[write_idx] = variable;
            }
            write_idx += 1;
        }
    }
    self.variables.shrinkRetainingCapacity(write_idx);
}

fn isCallable(t: FlowType) bool {
    return t == .function or t == .builtin_fn;
}

fn typeFromVariable(self: *Sema, stmt: *const Stmt) FlowType {
    std.debug.assert(stmt.* == .variable);
    const variable = stmt.variable;

    const t = variable.type_hint orelse return self.getType(variable.value.?).?;
    return switch (t.type.type) {
        .bool => .bool,
        .string => .string,
        .int => .int,
        .float => .float,
        else => unreachable,
    };
}

fn printError(self: *Sema) void {
    // TODO: Do we need to filter out duplicate errors? Are identical errors a bug in sema?

    for (self.errors.items) |e| {
        switch (e.err) {
            TypeError.EqualityCheckOfUnequalTypes => error_reporter.reportError(e.token, "Operands of Equality-Check (== and !=) have to be of the same type, got '{s}' and '{s}'", .{ e.extra_info1, e.extra_info2 }),
            TypeError.ArithmeticWithNonNumeric => error_reporter.reportError(e.token, "Operand of Arithmetic Operations has to be either int or float, got '{s}'", .{e.extra_info1}),
            TypeError.ArithmeticWithUnequalTypes => error_reporter.reportError(e.token, "Operands of Arithmetic Operations have to be of the same numeric type, got '{s}' and '{s}'", .{ e.extra_info1, e.extra_info2 }),
            TypeError.NegateWithNonNumeric => error_reporter.reportError(e.token, "Operand of Negate Operations has to be either int or float, got '{s}'", .{e.extra_info1}),
            TypeError.NotACallable => error_reporter.reportError(e.token, "'{s}' is not a callable", .{e.extra_info1}),
            TypeError.UnexpectedType => error_reporter.reportError(e.token, "Unexpected type, expected '{s}', got '{s}'", .{ e.extra_info1, e.extra_info2 }),

            VariableError.UnresolvableType => error_reporter.reportError(e.token, "Type of Variable could not be resolved. Consider adding an explicit Typehint", .{}),
            VariableError.UnknownVariable => error_reporter.reportError(e.token, "Variable '{s}' is not defined", .{e.extra_info1}),
            VariableError.VariableAlreadyExists => error_reporter.reportError(e.token, "Variable '{s}' already exists", .{e.extra_info1}),

            ContextError.NotInALoop => error_reporter.reportError(e.token, "'{s}' is only allowed inside a loop", .{e.extra_info1}),
        }
    }
}

const VariableList = std.ArrayListUnmanaged(Variable);
const TypeTable = std.AutoHashMapUnmanaged(*const Expr, FlowType);
const ErrorList = std.ArrayListUnmanaged(ErrorInfo);

const ErrorInfo = struct {
    token: Token,
    err: SemaError,
    extra_info1: []const u8 = "",
    extra_info2: []const u8 = "",
};

// combine all possible errors here
const SemaError = TypeError || VariableError || ContextError;

const TypeError = error{
    EqualityCheckOfUnequalTypes,
    ArithmeticWithNonNumeric,
    ArithmeticWithUnequalTypes,
    NegateWithNonNumeric,
    NotACallable,
    UnexpectedType,
};
const VariableError = error{
    UnresolvableType,
    UnknownVariable,
    VariableAlreadyExists,
};
const ContextError = error{
    NotInALoop,
};

const Variable = struct {
    name: Token,
    constant: bool,
    type: FlowType,
    scope: u8,
};

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
