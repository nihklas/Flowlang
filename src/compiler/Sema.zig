alloc: Allocator,
arena_state: std.heap.ArenaAllocator,
program: []const *Stmt,
errors: std.ArrayListUnmanaged(ErrorInfo),
types: std.AutoHashMapUnmanaged(*const Expr, FlowType),
variables: std.ArrayListUnmanaged(Variable),
current_scope: u8,
loop_level: u8,
faulty_expr: bool,

pub fn init(alloc: Allocator, program: []const *Stmt) Sema {
    return .{
        .program = program,
        .alloc = alloc,
        .arena_state = .init(alloc),
        .errors = .empty,
        .types = .empty,
        .variables = .empty,
        .current_scope = 0,
        .loop_level = 0,
        .faulty_expr = false,
    };
}

pub fn deinit(self: *Sema) void {
    self.arena_state.deinit();
    self.errors.deinit(self.alloc);
    self.types.deinit(self.alloc);
    self.variables.deinit(self.alloc);
    self.* = undefined;
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
    self.registerTopLevelFunctions();

    for (self.program) |stmt| {
        self.analyseStmt(stmt);
    }

    if (self.errors.items.len > 0) {
        self.printError();
        return error.SemaError;
    }
}

fn registerTopLevelFunctions(self: *Sema) void {
    for (self.program) |stmt| {
        if (stmt.* != .function) continue;
        const function = stmt.function.expr.function;
        const arg_types = self.arena().alloc(FlowType, function.params.len) catch oom();
        for (function.params, 0..) |param, i| {
            assert(param.* == .variable);
            assert(param.variable.type_hint != null);

            arg_types[i] = param.variable.type_hint.?.type;
        }

        self.putFunction(function.token, function.ret_type.type, arg_types);
    }
}

fn analyseStmt(self: *Sema, stmt: *const Stmt) void {
    defer self.faulty_expr = false;

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
                self.analyseExpr(inc);
            }
        },
        inline .@"break", .@"continue" => |keyword| {
            if (self.loop_level == 0) {
                self.pushError(ContextError.NotInALoop, keyword.token, .{@tagName(stmt.*)});
            }
        },
        .@"if" => |if_stmt| {
            self.analyseExpr(if_stmt.condition);
            self.analyseStmt(if_stmt.true_branch);
            if (if_stmt.false_branch) |false_branch| {
                self.analyseStmt(false_branch);
            }
        },
        .variable => |var_stmt| {
            if (var_stmt.constant and var_stmt.value == null) {
                self.pushError(VariableError.ConstantWithoutValue, var_stmt.name, .{var_stmt.name.lexeme});
                return;
            }

            if (var_stmt.type_hint == null and var_stmt.value == null) {
                self.pushError(VariableError.UnresolvableType, var_stmt.name, .{});
                return;
            }

            if (var_stmt.value) |value| {
                self.analyseExpr(value);
                if (self.faulty_expr) return;
            }

            const variable: Variable = .{
                .name = var_stmt.name,
                .constant = var_stmt.constant,
                .type = self.typeFromVariable(stmt),
                .scope = self.current_scope,
            };

            if (variable.type.isNull()) {
                self.pushError(VariableError.UnresolvableType, var_stmt.name, .{});
                return;
            }

            if (var_stmt.value) |value| {
                if (!self.getType(value).?.equals(&variable.type)) {
                    self.pushError(TypeError.UnexpectedType, value.getToken(), .{ variable.type, self.getType(value).? });
                }
            }

            if (self.findVariable(variable.name)) |existing_variable| {
                if (existing_variable.scope == variable.scope) {
                    self.pushError(VariableError.VariableAlreadyExists, variable.name, .{variable.name.lexeme});
                    return;
                }
            }

            self.putVariable(variable);
        },
        .function => |function_decl| {
            const function = function_decl.expr.function;
            if (self.current_scope > 0) {
                const arg_types = self.arena().alloc(FlowType, function.params.len) catch oom();
                for (function.params, 0..) |param, i| {
                    assert(param.* == .variable);
                    arg_types[i] = self.typeFromVariable(param);
                }

                self.putFunction(function.token, function.ret_type.type, arg_types);
            }

            self.analyseExpr(function_decl.expr);
        },
        .@"return" => |return_stmt| {
            if (self.current_scope == 0) {
                self.pushError(ContextError.NotInAFunction, return_stmt.token, .{"return"});
            }
        },
    }
}

fn analyseExpr(self: *Sema, expr: *const Expr) void {
    if (self.faulty_expr) return;

    const prev_error_count = self.errors.items.len;
    defer {
        if (prev_error_count != self.errors.items.len) {
            self.faulty_expr = true;
        }
    }

    switch (expr.*) {
        .literal => switch (expr.literal.value) {
            .null => self.putType(expr, .null),
            .bool => self.putType(expr, .primitive(.bool)),
            .int => self.putType(expr, .primitive(.int)),
            .float => self.putType(expr, .primitive(.float)),
            .string => self.putType(expr, .primitive(.string)),
            .array => |arr| {
                for (arr) |value| {
                    self.analyseExpr(value);
                }

                if (arr.len == 0) {
                    self.putType(expr, .{ .order = 1, .type = .null });
                } else {
                    const first_entry = self.getType(arr[0]).?;
                    self.putType(expr, .{ .order = first_entry.order + 1, .type = first_entry.type });

                    for (1..arr.len) |idx| {
                        const value_type = self.getType(arr[idx]).?;
                        if (!first_entry.equals(&value_type)) {
                            self.pushError(TypeError.UnexpectedType, arr[idx].getToken(), .{ first_entry, value_type });
                        }
                    }
                }
            },
        },
        .grouping => {
            self.analyseExpr(expr.grouping.expr);
            self.putType(expr, self.getType(expr.grouping.expr).?);
        },
        .unary => |unary| {
            self.analyseExpr(unary.expr);
            switch (unary.op.type) {
                .@"-" => {
                    const value_type = self.getType(unary.expr).?;
                    if (!value_type.isPrimitive(.int) and !value_type.isPrimitive(.float)) {
                        self.pushError(TypeError.NegateWithNonNumeric, unary.op, .{value_type});
                    }
                    self.putType(expr, value_type);
                },
                .@"!" => self.putType(expr, .primitive(.bool)),
                else => unreachable,
            }
        },
        .binary => |binary| {
            self.analyseExpr(binary.lhs);
            self.analyseExpr(binary.rhs);
            const left_type = self.getType(binary.lhs).?;
            const right_type = self.getType(binary.rhs).?;
            switch (binary.op.type) {
                .@".", .@".=" => self.putType(expr, .primitive(.string)),
                .@"==", .@"!=" => {
                    // if the two types are non-null and different, print error
                    if (!left_type.isNull() and !right_type.isNull() and !left_type.equals(&right_type)) {
                        self.pushError(TypeError.EqualityCheckOfUnequalTypes, binary.op, .{ left_type, right_type });
                    }
                    self.putType(expr, .primitive(.bool));
                },
                .@"<", .@"<=", .@">=", .@">", .@"+", .@"+=", .@"-", .@"-=", .@"*", .@"*=", .@"/", .@"/=", .@"%", .@"%=" => {
                    var both_numeric = true;
                    if (!left_type.isPrimitive(.int) and !left_type.isPrimitive(.float)) {
                        self.pushError(TypeError.ArithmeticWithNonNumeric, binary.lhs.getToken(), .{left_type});
                        both_numeric = false;
                    }
                    if (!right_type.isPrimitive(.int) and !right_type.isPrimitive(.float)) {
                        self.pushError(TypeError.ArithmeticWithNonNumeric, binary.rhs.getToken(), .{right_type});
                        both_numeric = false;
                    }
                    if (both_numeric and !left_type.equals(&right_type)) {
                        self.pushError(TypeError.ArithmeticWithUnequalTypes, binary.op, .{ left_type, right_type });
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
            self.putType(expr, .primitive(.bool));
        },
        .assignment => |assignment| {
            self.analyseExpr(assignment.value);

            var order_diff: u8 = 0;
            outer: switch (assignment.variable.*) {
                .variable => |variable| {
                    if (self.findVariable(variable.name)) |existing_variable| {
                        const value_type = self.getType(assignment.value).?;
                        const expected_type: FlowType = .{ .type = existing_variable.type.type, .order = existing_variable.type.order - order_diff };
                        if (!value_type.equals(&expected_type)) {
                            self.pushError(
                                TypeError.UnexpectedType,
                                assignment.value.getToken(),
                                .{ expected_type, value_type },
                            );
                        } else if (existing_variable.constant) {
                            self.pushError(VariableError.ConstantMutation, variable.name, .{variable.name.lexeme});
                        }
                    } else {
                        self.pushError(VariableError.UnknownVariable, variable.name, .{variable.name.lexeme});
                    }
                },
                .index => |index| {
                    order_diff = 1;
                    self.analyseExpr(assignment.variable);
                    var current_index_expr = index.expr;
                    inner: switch (current_index_expr.*) {
                        .variable => continue :outer current_index_expr.*,
                        .index => {
                            order_diff += 1;
                            current_index_expr = index.expr.index.expr;
                            continue :inner current_index_expr.*;
                        },
                        // NOTE: this case will be checked by the call to analyseExpr(assignment.variable)
                        else => {},
                    }
                },
                else => self.pushError(
                    TypeError.NotAssignable,
                    expr.getToken(),
                    .{ assignment.variable.getToken().lexeme, @tagName(assignment.variable.*) },
                ),
            }

            self.putType(expr, self.getType(assignment.value).?);
        },
        .variable => |variable| {
            if (self.findVariable(variable.name)) |found_var| {
                self.putType(expr, found_var.type);
            } else {
                self.pushError(VariableError.UnknownVariable, variable.name, .{variable.name.lexeme});
            }
        },
        .call => |call| {
            self.analyseExpr(call.expr);
            for (call.args) |arg_expr| {
                self.analyseExpr(arg_expr);
            }

            if (self.getType(call.expr)) |callee_type| {
                switch (callee_type.type) {
                    .builtin_fn => {
                        // NOTE: builtin functions cannot be aliased
                        assert(builtins.get(call.expr.variable.name.lexeme) != null);

                        const builtin = builtins.get(call.expr.variable.name.lexeme).?;
                        self.validateFunction(builtin, call, expr);
                    },
                    .function => {
                        const function = callee_type.function_type;
                        self.validateFunction(function, call, expr);
                    },
                    else => self.pushError(TypeError.NotACallable, call.expr.getToken(), .{call.expr.getToken().lexeme}),
                }
            }
        },
        .index => |index| {
            self.analyseExpr(index.expr);
            self.analyseExpr(index.index);

            // NOTE: expr is a non-existent variable
            if (self.getType(index.expr) == null) return;

            const expr_type = self.getType(index.expr).?;

            if (expr_type.order == 0) {
                self.pushError(VariableError.IndexOnNonArray, expr.getToken(), .{expr_type});
                return;
            }

            // NOTE: index is a non-existent variable
            if (self.getType(index.index) == null) return;

            const index_type = self.getType(index.index).?;

            if (!index_type.isPrimitive(.int)) {
                self.pushError(VariableError.IndexNotAnInt, index.index.getToken(), .{index_type});
                return;
            }

            self.putType(expr, .{ .type = expr_type.type, .order = expr_type.order - 1 });
        },
        .append => |append| {
            self.analyseExpr(append.variable);
            self.analyseExpr(append.value);

            // NOTE: variable is a non-existent variable
            if (self.getType(append.variable) == null) return;

            const variable_type = self.getType(append.variable).?;
            if (variable_type.order == 0) {
                self.pushError(VariableError.AppendOnNonArray, append.variable.getToken(), .{variable_type});
                return;
            }

            s: switch (append.variable.*) {
                .variable => |variable| {
                    // NOTE: findVariable should never return null, because we check for existing
                    // variable already above
                    if (self.findVariable(variable.name).?.constant) {
                        self.pushError(VariableError.ConstantMutation, variable.name, .{variable.name.lexeme});
                    }
                },
                .index => |index| continue :s index.expr.*,
                else => {},
            }

            // NOTE: value is a non-existent variable
            if (self.getType(append.value) == null) return;

            const value_type = self.getType(append.value).?;
            const expected_type: FlowType = .{ .type = variable_type.type, .order = variable_type.order - 1 };
            if (!value_type.equals(&expected_type)) {
                self.pushError(TypeError.UnexpectedType, append.value.getToken(), .{ expected_type, value_type });
                return;
            }

            self.putType(expr, variable_type);
        },
        .function => |function| {
            self.scopeIncr();
            defer self.scopeDecr();

            const param_types = self.arena().alloc(FlowType, function.params.len) catch oom();
            for (function.params, param_types) |param, *param_type| {
                assert(param.* == .variable);
                assert(param.variable.type_hint != null);

                self.putVariable(.{
                    .constant = true,
                    .name = param.variable.name,
                    .type = param.variable.type_hint.?.type,
                    .scope = self.current_scope,
                });

                param_type.* = param.variable.type_hint.?.type;
            }

            for (function.body) |inner_stmt| {
                self.analyseStmt(inner_stmt);
            }

            self.checkReturnTypes(expr);

            self.putType(expr, .function(self.arena(), function.ret_type.type, param_types));
        },
    }
}

fn validateFunction(self: *Sema, function: anytype, call: anytype, expr: *const Expr) void {
    if (call.args.len != function.arg_types.len) {
        self.pushError(FunctionError.ArgumentCountMismatch, call.expr.getToken(), .{ call.args.len, function.arg_types.len });
        return;
    }

    for (call.args, function.arg_types) |arg, param| {
        // NOTE: This happens if an argument is a variable, that does not exist
        if (self.getType(arg) == null) continue;

        // NOTE: these get checked at runtime
        if (param.isNull()) continue;

        if (!self.getType(arg).?.equals(&param)) {
            self.pushError(FunctionError.ArgumentTypeMismatch, arg.getToken(), .{ param, self.getType(arg).? });
        }
    }

    self.putType(expr, function.ret_type);
}

fn checkReturnTypes(self: *Sema, function: *const Expr) void {
    assert(function.* == .function);
    const func = function.function;

    for (func.body) |stmt| {
        self.checkReturnType(stmt, func.ret_type.type);
    }
}

fn checkReturnType(self: *Sema, stmt: *const Stmt, expected: FlowType) void {
    switch (stmt.*) {
        .@"return" => |return_stmt| {
            if (return_stmt.value == null) {
                if (!expected.isNull()) {
                    self.pushError(TypeError.UnexpectedType, return_stmt.token, .{ expected, FlowType.null });
                }
                return;
            }

            const value = return_stmt.value.?;
            self.analyseExpr(value);
            const value_type = self.getType(value).?;
            if (!expected.equals(&value_type) or expected.isNull()) {
                self.pushError(TypeError.UnexpectedType, return_stmt.token, .{ expected, value_type });
            }
        },
        .block => |block| {
            for (block.stmts) |s| {
                self.checkReturnType(s, expected);
            }
        },
        .loop => |loop| {
            for (loop.body) |s| {
                self.checkReturnType(s, expected);
            }
        },
        .@"if" => |if_stmt| {
            self.checkReturnType(if_stmt.true_branch, expected);
            if (if_stmt.false_branch) |false_branch| {
                self.checkReturnType(false_branch, expected);
            }
        },
        .expr, .@"break", .@"continue", .function, .variable => {},
    }
}

fn putType(self: *Sema, expr: *const Expr, t: FlowType) void {
    self.types.put(self.alloc, expr, t) catch oom();
}

fn getType(self: *Sema, expr: *const Expr) ?FlowType {
    return self.types.get(expr);
}

fn pushError(self: *Sema, comptime err: SemaError, token: Token, args: anytype) void {
    const fmt = switch (err) {
        TypeError.EqualityCheckOfUnequalTypes => "Operands of Equality-Check (== and !=) have to be of the same type, got '{}' and '{}'",
        TypeError.ArithmeticWithNonNumeric => "Operand of Arithmetic Operations has to be either int or float, got '{}'",
        TypeError.ArithmeticWithUnequalTypes => "Operands of Arithmetic Operations have to be of the same numeric type, got '{}' and '{}'",
        TypeError.NegateWithNonNumeric => "Operand of Negate Operations has to be either int or float, got '{}'",
        TypeError.NotACallable => "'{s}' is not a callable",
        TypeError.NotAssignable => "'{s}' is not assignable. Can only assign to Variables, got '{s}'",
        TypeError.UnexpectedType => "Unexpected type, expected '{}', got '{}'",

        VariableError.UnresolvableType => "Type of Variable could not be resolved. Consider adding an explicit Typehint",
        VariableError.UnknownVariable => "Variable '{s}' is not defined",
        VariableError.VariableAlreadyExists => "Variable '{s}' already exists",
        VariableError.ConstantMutation => "Constant '{s}' cannot be modified",
        VariableError.ConstantWithoutValue => "Constant '{s}' must have an initial value",
        VariableError.IndexOnNonArray => "Can only index on arrays, got '{}'",
        VariableError.IndexNotAnInt => "Index has to be an integer, got '{}'",
        VariableError.AppendOnNonArray => "Can only append to arrays, got '{}'",

        ContextError.NotInALoop => "'{s}' is only allowed inside a loop",
        ContextError.NotInAFunction => "'{s}' is only allowed inside a function",

        FunctionError.ArgumentTypeMismatch => "Unexpected argument type, expected '{}', got '{}'",
        FunctionError.ArgumentCountMismatch => "Argument count mismatch, found {d} but expected {d}",
    };

    self.errors.append(self.alloc, .{
        .err = err,
        .token = token,
        .message = std.fmt.allocPrint(self.alloc, fmt, args) catch oom(),
    }) catch oom();
}

fn putVariable(self: *Sema, variable: Variable) void {
    self.variables.append(self.alloc, variable) catch oom();
}

fn putFunction(self: *Sema, name: Token, ret_type: FlowType, arg_types: []const FlowType) void {
    const function: FlowType = .function(self.arena(), ret_type, arg_types);
    self.putVariable(.{
        .name = name,
        .constant = true,
        .type = function,
        .scope = self.current_scope,
    });
}

fn findVariable(self: *Sema, name: Token) ?Variable {
    var rev_iter = std.mem.reverseIterator(self.variables.items);
    while (rev_iter.next()) |variable| {
        if (std.mem.eql(u8, variable.name.lexeme, name.lexeme)) {
            return variable;
        }
    }

    if (builtins.get(name.lexeme)) |_| {
        return .{
            .name = name,
            .constant = true,
            .type = .builtinFn(),
            .scope = 0,
        };
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

fn typeFromVariable(self: *Sema, stmt: *const Stmt) FlowType {
    assert(stmt.* == .variable);
    const variable = stmt.variable;

    const t = variable.type_hint orelse return self.getType(variable.value.?).?;
    return t.type;
}

fn formatNumber(self: *Sema, num: anytype) []const u8 {
    return std.fmt.allocPrint(self.alloc, "{d}", .{num}) catch oom();
}

fn printError(self: *Sema) void {
    for (self.errors.items) |e| {
        error_reporter.reportError(e.token, "{s}", .{e.message});
        self.alloc.free(e.message);
    }
}

fn arena(self: *Sema) Allocator {
    return self.arena_state.allocator();
}

const ErrorInfo = struct {
    token: Token,
    err: SemaError,
    message: []const u8,
};

// combine all possible errors here
const SemaError = TypeError || VariableError || ContextError || FunctionError;

const TypeError = error{
    EqualityCheckOfUnequalTypes,
    ArithmeticWithNonNumeric,
    ArithmeticWithUnequalTypes,
    NegateWithNonNumeric,
    NotACallable,
    NotAssignable,
    UnexpectedType,
};
const VariableError = error{
    UnresolvableType,
    UnknownVariable,
    VariableAlreadyExists,
    ConstantMutation,
    ConstantWithoutValue,
    IndexOnNonArray,
    IndexNotAnInt,
    AppendOnNonArray,
};
const ContextError = error{
    NotInALoop,
    NotInAFunction,
};
const FunctionError = error{
    ArgumentCountMismatch,
    ArgumentTypeMismatch,
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
const assert = std.debug.assert;

const ast = @import("ir/ast.zig");
const Stmt = ast.Stmt;
const Expr = ast.Expr;

const shared = @import("shared");
const oom = shared.oom;
const FlowType = shared.definitions.FlowType;
const builtins = shared.builtins;
const error_reporter = @import("util/error_reporter.zig");

const Token = @import("ir/Token.zig");
