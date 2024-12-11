alloc: Allocator,
program: []const *Stmt,
constants: std.ArrayList(FlowValue),
variables: Stack(Variable, MAX_LOCAL_SIZE),
current_function: Stack(Function, MAX_LOCAL_SIZE),
globals: std.StringHashMap(Variable),
functions: std.StringHashMap(Function),
scope_depth: usize = 0,
has_error: bool = false,
last_expr_type: ?FlowType = null,
last_expr_sideeffect: bool = false,
loop_level: usize = 0,

pub fn init(alloc: Allocator, program: []const *Stmt) Sema {
    return .{
        .alloc = alloc,
        .program = program,
        .constants = .init(alloc),
        .variables = .init(alloc),
        .current_function = .init(alloc),
        .globals = .init(alloc),
        .functions = .init(alloc),
    };
}

pub fn deinit(self: *Sema) void {
    self.constants.deinit();
    self.variables.deinit(self.alloc);
    self.current_function.deinit(self.alloc);
    self.globals.deinit();
    self.functions.deinit();
}

pub fn analyse(self: *Sema) !void {
    for (self.program) |stmt| {
        try self.scanGlobal(stmt);
    }

    for (self.program) |stmt| {
        try self.scanFunction(stmt);
    }

    for (self.program) |stmt| {
        try self.statement(stmt);
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

fn scanGlobal(self: *Sema, stmt: *Stmt) !void {
    switch (stmt.*) {
        .variable => try self.varDeclaration(stmt),
        else => {},
    }
}

fn scanFunction(self: *Sema, stmt: *Stmt) !void {
    switch (stmt.*) {
        .function => |func| {
            const func_name = func.name.lexeme;
            if (self.functions.get(func_name)) |_| {
                error_reporter.reportError(func.name, "Function '{s}' is already defined", .{func_name});
                self.has_error = true;
                return;
            }

            const param_types = self.alloc.alloc(FlowType, func.params.len) catch oom();

            for (func.params, param_types) |param, *param_type| {
                param_type.* = tokenToType(param.variable.type_hint.?);
            }

            self.constants.append(.{ .string = func_name }) catch oom();

            self.functions.put(func_name, .{
                .param_types = param_types,
                .ret_type = tokenToType(func.ret_type),
            }) catch oom();

            self.globals.put(func_name, .{
                .scope_depth = 0,
                .constant = true,
                .token = func.name,
                .type = .function,
            }) catch oom();
        },
        else => {},
    }
}

fn statement(self: *Sema, stmt: *Stmt) !void {
    self.last_expr_type = null;
    switch (stmt.*) {
        .expr => self.expression(stmt.expr.expr),
        .@"if" => |if_stmt| {
            self.expression(if_stmt.condition);
            try self.statement(if_stmt.true_branch);
            if (if_stmt.false_branch) |false_branch| {
                try self.statement(false_branch);
            }
        },
        .block => |block| {
            self.beginScope();
            for (block.stmts) |inner_stmt| {
                try self.statement(inner_stmt);
            }

            var locals_count: usize = 0;
            var stack_idx: usize = 0;
            while (stack_idx < self.variables.stack_top) : (stack_idx += 1) {
                const local = self.variables.at(stack_idx);
                if (local.scope_depth < self.scope_depth) {
                    break;
                }
                locals_count += 1;
            }
            stmt.block.local_count = locals_count;

            self.endScope();
        },
        .loop => |loop| {
            self.expression(loop.condition);
            self.loop_level += 1;
            try self.statement(loop.body);
            self.loop_level -= 1;
        },
        .@"break" => |break_stmt| {
            if (self.loop_level < 1) {
                error_reporter.reportError(break_stmt.token, "'break' is only allowed in loops", .{});
                self.has_error = true;
            }
        },
        .@"continue" => |continue_stmt| {
            if (self.loop_level < 1) {
                error_reporter.reportError(continue_stmt.token, "'continue' is only allowed in loops", .{});
                self.has_error = true;
            }
        },
        .function => |func| {
            // TODO: Create complete new scope or handle inner functions otherwise gracefully
            self.beginScope();
            self.current_function.push(self.functions.get(func.name.lexeme).?);
            for (func.params) |param| {
                self.variables.push(.{
                    .token = param.variable.name,
                    .constant = true,
                    .scope_depth = self.scope_depth,
                    .type = tokenToType(param.variable.type_hint.?),
                });
            }
            for (func.body) |line| {
                try self.statement(line);
            }
            _ = self.current_function.pop();
            self.endScope();
        },
        .@"return" => |return_stmt| {
            self.expression(return_stmt.value);
            const func = self.current_function.at(0);
            if (func.ret_type != self.last_expr_type.?) {
                error_reporter.reportError(return_stmt.value.getToken(), "Return type mismatch. Expected '{s}', got '{s}'", .{
                    @tagName(func.ret_type),
                    @tagName(self.last_expr_type.?),
                });
                self.has_error = true;
            }
        },
        .variable => try self.varDeclaration(stmt),
        else => panic("Illegal Instruction: {s}", .{@tagName(stmt.*)}),
    }
}

fn varDeclaration(self: *Sema, stmt: *Stmt) !void {
    const name = stmt.variable.name.lexeme;

    if (builtins.get(name)) |_| {
        error_reporter.reportError(stmt.variable.name, "'{s}' is a builtin and cannot be reassigned", .{name});
        self.has_error = true;
    }

    const is_local = self.scope_depth > 0;
    stmt.variable.global = !is_local;

    if (!is_local) {
        self.constant(.{ .string = name });
    }

    if (stmt.variable.value) |value| {
        self.expression(value);
        const value_type = self.last_expr_type.?;

        if (value_type == .builtin_fn) {
            error_reporter.reportError(value.getToken(), "Cannot reassign builtin '{s}'", .{value.getToken().lexeme});
            self.has_error = true;
            return;
        }

        if (stmt.variable.type_hint != null) {
            const correct_type = if (value_type == .null)
                true
            else switch (stmt.variable.type_hint.?.type) {
                .bool => value_type == .bool,
                .string => value_type == .string,
                .int => value_type == .int,
                .float => value_type == .float,
                else => |t| panic("Invalid Type Hint: {s}", .{@tagName(t)}),
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
                .null, .builtin_fn, .function => unreachable,
            };
            stmt.variable.type_hint = expr_token;
        }
    }

    if (stmt.variable.type_hint == null) {
        error_reporter.reportError(stmt.variable.name, "Cannot infer type of variable", .{});
        self.has_error = true;
    }

    const existent = blk: {
        var stack_idx: usize = 0;
        while (stack_idx < self.variables.stack_top) : (stack_idx += 1) {
            const local = self.variables.at(stack_idx);
            if (local.scope_depth == self.scope_depth and std.mem.eql(u8, stmt.variable.name.lexeme, local.token.lexeme)) {
                break :blk local;
            }
        }
        break :blk null;
    };

    if (existent) |found| {
        error_reporter.reportError(
            stmt.variable.name,
            "Variable '{s}' already defined at {d}:{d}, duplicated definition",
            .{ name, found.token.line, found.token.column },
        );
        self.has_error = true;
    } else if (stmt.variable.type_hint) |type_hint_token| {
        const type_hint = tokenToType(type_hint_token);
        const variable: Variable = .{
            .token = stmt.variable.name,
            .type = type_hint,
            .constant = stmt.variable.constant,
            .scope_depth = self.scope_depth,
        };

        if (self.scope_depth == 0) {
            try self.globals.put(stmt.variable.name.lexeme, variable);
        } else {
            self.variables.push(variable);
        }
    }
}

fn expression(self: *Sema, expr: *Expr) void {
    switch (expr.*) {
        .literal => {
            self.last_expr_type = blk: switch (expr.literal.value) {
                .int => |int| {
                    self.constant(.{ .int = int });
                    break :blk .int;
                },
                .float => |float| {
                    self.constant(.{ .float = float });
                    break :blk .float;
                },
                .string => |string| {
                    self.constant(.{ .string = string });
                    break :blk .string;
                },
                .null => .null,
                .bool => .bool,
            };
        },
        .unary => self.unary(expr),
        .grouping => self.expression(expr.grouping.expr),
        .assignment => self.assignment(expr),
        .append => @panic("TODO"),
        .binary => self.binary(expr),
        .logical => {
            self.expression(expr.logical.lhs);
            self.expression(expr.logical.rhs);
            self.last_expr_type = .bool;
        },
        .variable => self.variableExpr(expr),
        .call => |call| {
            self.expression(call.expr);
            const name = call.expr.getToken().lexeme;
            if (self.last_expr_type != .builtin_fn and self.last_expr_type != .function) {
                error_reporter.reportError(call.expr.getToken(), "'{s}' is not callable", .{name});
                self.has_error = true;
            }

            const maybe_builtin = builtins.get(name);

            if (maybe_builtin) |builtin| {
                if (builtin.arg_count != call.args.len) {
                    error_reporter.reportError(call.expr.getToken(), "'{s}' expected {d} arguments, got {d}", .{
                        name,
                        builtin.arg_count,
                        call.args.len,
                    });
                    self.has_error = true;
                }
            }

            for (expr.call.args, 0..) |arg, i| {
                self.expression(arg);

                if (maybe_builtin != null and maybe_builtin.?.arg_count == call.args.len and maybe_builtin.?.arg_types != null) {
                    const arg_type = maybe_builtin.?.arg_types.?[i];
                    if (self.last_expr_type != arg_type) {
                        error_reporter.reportError(arg.getToken(), "Expected argument of type '{s}', got '{s}'", .{
                            @tagName(arg_type),
                            @tagName(self.last_expr_type.?),
                        });
                        self.has_error = true;
                    }
                }
            }
        },
    }
}

fn unary(self: *Sema, expr: *Expr) void {
    self.expression(expr.unary.expr);
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
}

fn binary(self: *Sema, expr: *Expr) void {
    self.expression(expr.binary.lhs);
    const left_type = self.last_expr_type.?;

    self.expression(expr.binary.rhs);
    const right_type = self.last_expr_type.?;

    if (left_type != right_type and expr.binary.op.type != .@"." and expr.binary.op.type != .@"==" and expr.binary.op.type != .@"!=") {
        error_reporter.reportError(
            expr.binary.op,
            "Cannot compare value of type '{s}' to value of type '{s}'",
            .{ @tagName(left_type), @tagName(right_type) },
        );
        self.has_error = true;
    }

    switch (expr.binary.op.type) {
        .@"<", .@"<=", .@">=", .@">" => {
            self.checkNumericOperands(
                expr.binary.lhs.getToken(),
                left_type,
                expr.binary.rhs.getToken(),
                right_type,
                expr.binary.op.type,
            );
            self.last_expr_type = .bool;
        },
        .@"+", .@"+=", .@"-", .@"-=", .@"*", .@"*=", .@"/", .@"/=", .@"%", .@"%=" => {
            self.checkNumericOperands(
                expr.binary.lhs.getToken(),
                left_type,
                expr.binary.rhs.getToken(),
                right_type,
                expr.binary.op.type,
            );
        },
        .@"==", .@"!=" => {
            self.last_expr_type = .bool;
        },
        .@".", .@".=" => {
            self.last_expr_type = .string;
        },
        else => unreachable,
    }
}

fn assignment(self: *Sema, expr: *Expr) void {
    self.expression(expr.assignment.value);
    const resulted_type = self.last_expr_type.?;
    const name = expr.assignment.name.lexeme;

    const maybe_variable, const local_idx = blk: {
        var stack_idx: usize = 0;
        while (stack_idx < self.variables.stack_top) : (stack_idx += 1) {
            const local = self.variables.at(stack_idx);
            if (local.scope_depth <= self.scope_depth and std.mem.eql(u8, name, local.token.lexeme)) {
                break :blk .{ local, stack_idx };
            }
        }
        break :blk .{ null, null };
    };

    const variable = maybe_variable orelse self.globals.get(name) orelse {
        error_reporter.reportError(expr.variable.name, "Undefined variable: '{s}'", .{name});
        self.has_error = true;
        return;
    };

    if (variable.constant) {
        error_reporter.reportError(
            expr.assignment.value.getToken(),
            "Cannot assign to a constant",
            .{},
        );
        self.has_error = true;
    }

    if (resulted_type != .null and variable.type != resulted_type) {
        error_reporter.reportError(
            expr.assignment.value.getToken(),
            "Type mismatch: Expected value of type '{s}', got '{s}'",
            .{ @tagName(variable.type), @tagName(resulted_type) },
        );
        self.has_error = true;
    }

    self.last_expr_type = variable.type;

    if (local_idx) |idx| {
        if (idx > std.math.maxInt(u8)) {
            // This kinda should not be really possible, hopefully
            panic("Too many locals: {d}", .{idx});
        }
        expr.assignment.local_idx = @intCast(idx);
    } else {
        expr.assignment.global = true;
    }
}

fn variableExpr(self: *Sema, expr: *Expr) void {
    const name = expr.variable.name.lexeme;

    if (builtins.get(name)) |_| {
        self.constant(.{ .string = name });
        expr.variable.global = true;
        self.last_expr_type = .builtin_fn;
        return;
    }

    const maybe_variable, const local_idx = blk: {
        var stack_idx: usize = 0;
        while (stack_idx < self.variables.stack_top) : (stack_idx += 1) {
            const local = self.variables.at(stack_idx);
            if (local.scope_depth <= self.scope_depth and std.mem.eql(u8, name, local.token.lexeme)) {
                break :blk .{ local, self.variables.stack_top - 1 - stack_idx };
            }
        }
        break :blk .{ null, null };
    };

    const variable = maybe_variable orelse self.globals.get(name) orelse {
        error_reporter.reportError(expr.variable.name, "Undefined variable: '{s}'", .{name});
        self.has_error = true;
        return;
    };
    self.last_expr_type = variable.type;

    if (local_idx) |idx| {
        if (idx > std.math.maxInt(u8)) {
            // This kinda should not be really possible, hopefully
            panic("Too many locals: {d}", .{idx});
        }
        expr.variable.local_idx = @intCast(idx);
    } else {
        expr.variable.global = true;
    }
}

fn beginScope(self: *Sema) void {
    self.scope_depth += 1;
}

fn endScope(self: *Sema) void {
    self.scope_depth -= 1;
    while (self.variables.stack_top > 0 and self.variables.at(0).scope_depth > self.scope_depth) {
        _ = self.variables.pop();
    }
}

fn constant(self: *Sema, value: FlowValue) void {
    for (self.constants.items) |c| {
        if (c.equals(value)) return;
    }
    self.constants.append(value) catch oom();
}

fn checkNumericOperands(self: *Sema, left: Token, lhs: FlowType, right: Token, rhs: FlowType, op: Token.Type) void {
    if (!isNumeric(lhs)) {
        error_reporter.reportError(
            left,
            "Expected left operand of '{s}' to be int or float, got '{s}'",
            .{ @tagName(op), @tagName(lhs) },
        );
        self.has_error = true;
    }

    if (!isNumeric(rhs)) {
        error_reporter.reportError(
            right,
            "Expected right operand of '{s}' to be int or float, got '{s}'",
            .{ @tagName(op), @tagName(rhs) },
        );
        self.has_error = true;
    }
}

fn isNumeric(value_type: FlowType) bool {
    return value_type == .int or value_type == .float;
}

fn tokenToType(token: Token) FlowType {
    return switch (token.type) {
        .bool => .bool,
        .int => .int,
        .float => .float,
        .string => .string,
        .null => .null,
        else => unreachable,
    };
}

const Variable = struct {
    token: Token,
    constant: bool,
    type: FlowType,
    scope_depth: usize,
};

const Function = struct {
    param_types: []FlowType,
    ret_type: FlowType,
};

const Expr = @import("ast.zig").Expr;
const Stmt = @import("ast.zig").Stmt;

const Token = @import("Token.zig");

const Sema = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const FlowValue = @import("shared").definitions.FlowValue;
const FlowType = @import("shared").definitions.FlowType;
const Stack = @import("shared").Stack;
const builtins = @import("shared").builtins;
const oom = @import("shared").oom;
const panic = std.debug.panic;

const error_reporter = @import("error_reporter.zig");

const MAX_LOCAL_SIZE = 1024;
