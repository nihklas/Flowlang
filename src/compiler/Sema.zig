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
last_function_ret_type: ?FlowType = null,
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
        self.scanGlobal(stmt);
    }

    for (self.program) |stmt| {
        self.scanFunction(stmt);
    }

    for (self.program) |stmt| {
        self.statement(stmt);
    }

    if (self.constants.items.len > std.math.maxInt(u8)) {
        return error.TooManyConstants;
    }

    if (self.has_error) {
        return error.CompileError;
    }
}

fn scanGlobal(self: *Sema, stmt: *Stmt) void {
    switch (stmt.*) {
        .variable => self.varDeclaration(stmt),
        else => {},
    }
}

fn scanFunction(self: *Sema, stmt: *Stmt) void {
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

            self.constant(.{ .string = func_name });

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

fn statement(self: *Sema, stmt: *Stmt) void {
    self.last_expr_type = null;
    switch (stmt.*) {
        .expr => self.expression(stmt.expr.expr),
        .@"if" => self.ifStatement(stmt),
        .block => self.blockStatement(stmt),
        .loop => self.loopStatement(stmt),
        .@"break" => self.breakStatement(stmt),
        .@"continue" => self.continueStatement(stmt),
        .function => self.functionDeclaration(stmt),
        .@"return" => self.returnStatement(stmt),
        .variable => self.varDeclaration(stmt),
        else => panic("Illegal Instruction: {s}", .{@tagName(stmt.*)}),
    }
}

fn expression(self: *Sema, expr: *Expr) void {
    switch (expr.*) {
        .literal => self.literalExpression(expr),
        .unary => self.unaryExpression(expr),
        .grouping => self.expression(expr.grouping.expr),
        .assignment => self.assignmentExpression(expr),
        .binary => self.binaryExpression(expr),
        .logical => self.logicalExpression(expr),
        .variable => self.variableExpression(expr),
        .call => self.callExpression(expr),
        .append => @panic("TODO"),
    }
}

fn ifStatement(self: *Sema, stmt: *Stmt) void {
    const if_stmt = stmt.@"if";
    self.expression(if_stmt.condition);
    self.statement(if_stmt.true_branch);
    if (if_stmt.false_branch) |false_branch| {
        self.statement(false_branch);
    }
}

fn blockStatement(self: *Sema, stmt: *Stmt) void {
    const block = stmt.block;
    self.beginScope();
    for (block.stmts) |inner_stmt| {
        self.statement(inner_stmt);
    }

    stmt.block.local_count = self.localCount();

    self.endScope();
}

fn localCount(self: *Sema) usize {
    var locals_count: usize = 0;
    var stack_idx: usize = 0;
    while (stack_idx < self.variables.stack_top) : (stack_idx += 1) {
        const local = self.variables.at(stack_idx);
        if (local.scope_depth < self.scope_depth) {
            break;
        }
        locals_count += 1;
    }
    return locals_count;
}

fn loopStatement(self: *Sema, stmt: *Stmt) void {
    const loop = stmt.loop;
    self.beginScope();
    self.expression(loop.condition);
    self.loop_level += 1;
    for (loop.body) |body_stmt| {
        self.statement(body_stmt);
    }
    self.loop_level -= 1;
    self.endScope();
}

fn breakStatement(self: *Sema, stmt: *Stmt) void {
    const break_stmt = stmt.@"break";
    if (self.loop_level < 1) {
        error_reporter.reportError(break_stmt.token, "'break' is only allowed in loops", .{});
        self.has_error = true;
    }
}

fn continueStatement(self: *Sema, stmt: *Stmt) void {
    const continue_stmt = stmt.@"continue";
    if (self.loop_level < 1) {
        error_reporter.reportError(continue_stmt.token, "'continue' is only allowed in loops", .{});
        self.has_error = true;
    }
}

fn functionDeclaration(self: *Sema, stmt: *Stmt) void {
    const func = stmt.function;

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
        self.statement(line);
    }
    _ = self.current_function.pop();

    self.endScope();
}

fn returnStatement(self: *Sema, stmt: *Stmt) void {
    const return_stmt = stmt.@"return";
    const func = self.current_function.at(0);

    if (return_stmt.value) |value| {
        self.expression(value);

        if (func.ret_type != self.last_expr_type.?) {
            error_reporter.reportError(value.getToken(), "Return type mismatch. Expected '{s}', got '{s}'", .{
                @tagName(func.ret_type),
                @tagName(self.last_expr_type.?),
            });
            self.has_error = true;
        }
    } else if (func.ret_type != .null) {
        error_reporter.reportError(return_stmt.token, "Missing return value", .{});
        self.has_error = true;
    }
}

fn varDeclaration(self: *Sema, stmt: *Stmt) void {
    const name = stmt.variable.name.lexeme;

    if (builtins.get(name)) |_| {
        error_reporter.reportError(stmt.variable.name, "'{s}' is a builtin and cannot be reassigned", .{name});
        self.has_error = true;
    }

    stmt.variable.global = self.scope_depth == 0;
    if (stmt.variable.global) {
        self.constant(.{ .string = name });
    }

    self.resolveValue(stmt);

    const existent, _ = self.findLocal(name);
    if (existent != null and existent.?.scope_depth == self.scope_depth) {
        error_reporter.reportError(
            stmt.variable.name,
            "Variable '{s}' already defined at {d}:{d}, duplicated definition",
            .{ name, existent.?.token.line, existent.?.token.column },
        );
        self.has_error = true;
    }

    if (stmt.variable.type_hint == null) {
        error_reporter.reportError(stmt.variable.name, "Cannot infer type of variable", .{});
        self.has_error = true;
        return;
    }

    const type_hint = tokenToType(stmt.variable.type_hint.?);

    const variable: Variable = .{
        .token = stmt.variable.name,
        .type = type_hint,
        .constant = stmt.variable.constant,
        .scope_depth = self.scope_depth,
    };

    if (self.scope_depth == 0) {
        self.globals.put(stmt.variable.name.lexeme, variable) catch oom();
    } else {
        self.variables.push(variable);
    }
}

fn resolveValue(self: *Sema, stmt: *Stmt) void {
    if (stmt.variable.value == null) return;

    const value = stmt.variable.value.?;
    self.expression(value);
    const value_type = self.last_expr_type.?;

    if (value_type == .null and stmt.variable.type_hint == null) return;

    if (stmt.variable.type_hint == null) {
        var expr_token = value.getToken();
        expr_token.type = switch (value_type) {
            .bool => .bool,
            .int => .int,
            .float => .float,
            .string => .string,
            .null, .builtin_fn, .function => unreachable,
        };
        stmt.variable.type_hint = expr_token;
        return;
    }

    if (value_type == .null) {
        return;
    }

    const correct_type = switch (stmt.variable.type_hint.?.type) {
        .bool => value_type == .bool,
        .string => value_type == .string,
        .int => value_type == .int,
        .float => value_type == .float,
        else => unreachable,
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
}

fn literalExpression(self: *Sema, expr: *Expr) void {
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
}

fn unaryExpression(self: *Sema, expr: *Expr) void {
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

fn binaryExpression(self: *Sema, expr: *Expr) void {
    self.expression(expr.binary.lhs);
    const left_type = self.last_expr_type.?;

    self.expression(expr.binary.rhs);
    const right_type = self.last_expr_type.?;

    // set last_expr_type
    self.last_expr_type = switch (expr.binary.op.type) {
        .@"==", .@"!=", .@"<", .@"<=", .@">=", .@">" => .bool,
        .@"+", .@"+=", .@"-", .@"-=", .@"*", .@"*=", .@"/", .@"/=", .@"%", .@"%=" => left_type,
        .@".", .@".=" => .string,
        else => unreachable,
    };

    // Check types
    switch (expr.binary.op.type) {
        // String concats always work
        .@".", .@".=" => {},
        // Equality works with same type and with null always
        .@"==", .@"!=" => {
            if (left_type != .null and right_type != .null) {
                if (left_type != right_type) {
                    error_reporter.reportError(
                        expr.binary.op,
                        "Cannot compare value of type '{s}' to value of type '{s}'",
                        .{ @tagName(left_type), @tagName(right_type) },
                    );
                    self.has_error = true;
                }
            }
        },
        .@"<", .@"<=", .@">=", .@">", .@"+", .@"+=", .@"-", .@"-=", .@"*", .@"*=", .@"/", .@"/=", .@"%", .@"%=" => {
            self.checkNumericOperands(
                expr.binary.lhs.getToken(),
                left_type,
                expr.binary.rhs.getToken(),
                right_type,
                expr.binary.op.type,
            );
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
            @panic("Too many locals");
        }
        expr.assignment.local_idx = @intCast(idx);
    } else {
        expr.assignment.global = true;
    }
}

fn variableExpr(self: *Sema, expr: *Expr) void {
    const name = expr.variable.name.lexeme;

    if (builtins.get(name)) |builtin| {
        self.constant(.{ .string = name });
        expr.variable.global = true;
        self.last_expr_type = .builtin_fn;
        self.last_function_ret_type = builtin.ret_type;
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

    if (variable.type == .function) {
        const func = self.functions.get(variable.token.lexeme).?;
        self.last_function_ret_type = func.ret_type;
    }

    if (local_idx) |idx| {
        if (idx > std.math.maxInt(u8)) {
            // This kinda should not be really possible, hopefully
            @panic("Too many locals");
        }
        expr.variable.local_idx = @intCast(idx);
    } else {
        expr.variable.global = true;
    }
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

fn logicalExpression(self: *Sema, expr: *Expr) void {
    self.expression(expr.logical.lhs);
    self.expression(expr.logical.rhs);
    self.last_expr_type = .bool;
}

fn assignmentExpression(self: *Sema, expr: *Expr) void {
    self.expression(expr.assignment.value);
    const resulted_type = self.last_expr_type.?;
    const name = expr.assignment.name.lexeme;

    const maybe_variable, const local_idx = self.findLocal(name);

    const variable = maybe_variable orelse self.globals.get(name) orelse {
        error_reporter.reportError(expr.variable.name, "Undefined variable: '{s}'", .{name});
        self.has_error = true;
        return;
    };

    if (variable.constant) {
        error_reporter.reportError(expr.assignment.value.getToken(), "Cannot assign to a constant", .{});
        self.has_error = true;
        return;
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

fn variableExpression(self: *Sema, expr: *Expr) void {
    const name = expr.variable.name.lexeme;

    if (builtins.get(name)) |_| {
        self.constant(.{ .string = name });
        expr.variable.global = true;
        self.last_expr_type = .builtin_fn;
        return;
    }

    const maybe_variable, const local_idx = self.findLocal(name);

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

fn findLocal(self: *Sema, name: []const u8) struct { ?Variable, ?usize } {
    var stack_idx: usize = 0;
    while (stack_idx < self.variables.stack_top) : (stack_idx += 1) {
        const local = self.variables.at(stack_idx);
        if (local.scope_depth <= self.scope_depth and std.mem.eql(u8, name, local.token.lexeme)) {
            return .{ local, self.variables.stack_top - 1 - stack_idx };
        }
    }

    return .{ null, null };
}

fn callExpression(self: *Sema, expr: *Expr) void {
    const call = expr.call;

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

        if (maybe_builtin) |builtin| {
            if (builtin.arg_count == call.args.len and builtin.arg_types != null) {
                const arg_type = builtin.arg_types.?[i];
                if (self.last_expr_type != arg_type) {
                    error_reporter.reportError(arg.getToken(), "Expected argument of type '{s}', got '{s}'", .{
                        @tagName(arg_type),
                        @tagName(self.last_expr_type.?),
                    });
                    self.has_error = true;
                }
            }
        }
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
