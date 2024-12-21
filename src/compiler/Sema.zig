alloc: Allocator,
program: []const *Stmt,
constants: ArrayList(FlowValue),
globals: HashMap(Global),
functions: HashMap(Function),
variables: Stack(Variable, MAX_LOCAL_SIZE),
current_function: Stack(Function, MAX_LOCAL_SIZE),
scope_depth: usize = 0,
loop_level: usize = 0,
last_expr_type: FlowType = .null,
has_error: bool = false,

pub fn init(alloc: Allocator, program: []const *Stmt) Sema {
    return .{
        .program = program,
        .alloc = alloc,
        .constants = .empty,
        .globals = .empty,
        .functions = .empty,
        .variables = .init(alloc),
        .current_function = .init(alloc),
    };
}

pub fn deinit(self: *Sema) void {
    var functions = self.functions.iterator();
    while (functions.next()) |func| {
        self.alloc.free(func.value_ptr.param_types);
    }

    self.constants.deinit(self.alloc);
    self.globals.deinit(self.alloc);
    self.functions.deinit(self.alloc);
    self.variables.deinit(self.alloc);
    self.current_function.deinit(self.alloc);
}

pub fn analyse(self: *Sema) !void {
    self.resolveFunctions();
    self.resolveGlobals();

    for (self.program) |stmt| {
        self.statement(stmt);
    }

    if (self.has_error) {
        return error.CompileError;
    }

    if (self.constants.items.len > std.math.maxInt(u8)) {
        return error.TooManyConstants;
    }
}

fn resolveFunctions(self: *Sema) void {
    for (self.program) |stmt| switch (stmt.*) {
        .function => |func| {
            const func_name = func.name.lexeme;
            if (self.functions.get(func_name)) |_| {
                self.err(func.name, "Function '{s}' is already defined", .{func_name});
                return;
            }

            const param_types = self.alloc.alloc(FlowType, func.params.len) catch oom();

            for (func.params, param_types) |param, *param_type| {
                param_type.* = tokenToType(param.variable.type_hint.?);
            }

            self.constant(.{ .string = func_name });

            self.functions.put(self.alloc, func_name, .{
                .param_types = param_types,
                .ret_type = tokenToType(func.ret_type),
            }) catch oom();

            self.globals.put(self.alloc, func_name, .{
                .constant = true,
                .token = func.name,
                .type = .function,
            }) catch oom();
        },
        else => {},
    };
}

fn resolveGlobals(self: *Sema) void {
    for (self.program) |stmt| switch (stmt.*) {
        .variable => |variable| {
            const gop = self.globals.getOrPut(self.alloc, variable.name.lexeme) catch oom();
            if (gop.found_existing) {
                self.err(
                    variable.name,
                    "Variable '{s}' already defined at {d}:{d}, duplicated definition",
                    .{ variable.name, gop.value_ptr.token.line, gop.value_ptr.token.column },
                );
                continue;
            }

            var global: Global = .{ .token = variable.name, .constant = variable.constant };
            if (variable.type_hint) |type_token| {
                global.type = tokenToType(type_token);
            }
            gop.value_ptr.* = global;
        },
        else => {},
    };
}

fn statement(self: *Sema, stmt: *Stmt) void {
    self.last_expr_type = .null;
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

// ---------- Statements

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
        self.err(break_stmt.token, "'break' is only allowed in loops", .{});
    }
}

fn continueStatement(self: *Sema, stmt: *Stmt) void {
    const continue_stmt = stmt.@"continue";
    if (self.loop_level < 1) {
        self.err(continue_stmt.token, "'continue' is only allowed in loops", .{});
    }
}

fn returnStatement(self: *Sema, stmt: *Stmt) void {
    const return_stmt = stmt.@"return";
    const func = self.current_function.at(0);

    if (return_stmt.value) |value| {
        self.expression(value);

        if (func.ret_type != self.last_expr_type) {
            self.err(value.getToken(), "Return type mismatch. Expected '{s}', got '{s}'", .{
                @tagName(func.ret_type),
                @tagName(self.last_expr_type),
            });
        }
    } else if (func.ret_type != .null) {
        self.err(return_stmt.token, "Missing return value", .{});
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

fn varDeclaration(self: *Sema, stmt: *Stmt) void {
    const variable = stmt.variable;
    const name = variable.name.lexeme;

    if (builtins.get(name)) |_| {
        self.err(stmt.variable.name, "'{s}' is a builtin and cannot be reassigned", .{name});
        return;
    }

    self.resolveValue(stmt);

    const existent, _ = self.findLocal(name);
    if (existent != null and existent.?.scope_depth == self.scope_depth) {
        self.err(
            stmt.variable.name,
            "Variable '{s}' already defined at {d}:{d}, duplicated definition",
            .{ name, existent.?.token.line, existent.?.token.column },
        );
        return;
    }

    if (stmt.variable.type_hint == null) {
        self.err(stmt.variable.name, "Cannot infer type of variable", .{});
        return;
    }

    const type_hint = tokenToType(stmt.variable.type_hint.?);

    if (self.scope_depth == 0) {
        self.globals.getPtr(name).?.type = type_hint;
        self.constant(.{ .string = name });
        stmt.variable.global = true;
    } else {
        self.variables.push(.{
            .token = stmt.variable.name,
            .type = type_hint,
            .constant = stmt.variable.constant,
            .scope_depth = self.scope_depth,
        });
    }
}

// ---------- Expressions

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
                self.err(
                    expr.unary.expr.getToken(),
                    "Expected expression following '-' to be int or float, got '{s}'",
                    .{@tagName(self.last_expr_type)},
                );
            }
        },
        else => unreachable,
    }
}

fn binaryExpression(self: *Sema, expr: *Expr) void {
    self.expression(expr.binary.lhs);
    const left_type = self.last_expr_type;

    self.expression(expr.binary.rhs);
    const right_type = self.last_expr_type;

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
                    self.err(
                        expr.binary.op,
                        "Cannot compare value of type '{s}' to value of type '{s}'",
                        .{ @tagName(left_type), @tagName(right_type) },
                    );
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
            expr.binary.type = left_type;
        },
        else => unreachable,
    }
}

fn logicalExpression(self: *Sema, expr: *Expr) void {
    self.expression(expr.logical.lhs);
    self.expression(expr.logical.rhs);
    self.last_expr_type = .bool;
}

fn assignmentExpression(self: *Sema, expr: *Expr) void {
    const assignment = expr.assignment;
    self.expression(assignment.value);
    const resulted_type = self.last_expr_type;
    const name = assignment.name.lexeme;

    const maybe_variable, const local_idx = self.findLocal(name);

    if (maybe_variable) |variable| {
        if (variable.constant) {
            self.err(expr.assignment.value.getToken(), "Cannot assign to a constant", .{});
            return;
        }

        if (resulted_type != .null and variable.type != resulted_type) {
            self.err(
                expr.assignment.value.getToken(),
                "Type mismatch: Expected value of type '{s}', got '{s}'",
                .{ @tagName(variable.type), @tagName(resulted_type) },
            );
        }

        self.last_expr_type = variable.type;

        const idx = local_idx.?;
        if (idx > std.math.maxInt(u8)) {
            // This kinda should not be really possible, hopefully
            panic("Too many locals: {d}", .{idx});
        }
        expr.assignment.local_idx = @intCast(idx);
        return;
    }

    const global = self.globals.get(name) orelse {
        self.err(expr.variable.name, "Undefined variable: '{s}'", .{name});
        return;
    };

    if (global.constant) {
        self.err(assignment.value.getToken(), "Cannot assign to a constant", .{});
        return;
    }

    if (resulted_type != .null and global.type != resulted_type) {
        error_reporter.reportError(
            assignment.value.getToken(),
            "Type mismatch: Expected value of type '{s}', got '{s}'",
            .{ @tagName(global.type.?), @tagName(resulted_type) },
        );
        self.has_error = true;
    }

    self.last_expr_type = global.type.?;

    expr.assignment.global = true;
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

    if (maybe_variable) |variable| {
        self.last_expr_type = variable.type;
        const idx = local_idx.?;

        if (idx > std.math.maxInt(u8)) {
            // This kinda should not be really possible, hopefully
            panic("Too many locals: {d}", .{idx});
        }
        expr.variable.local_idx = @intCast(idx);
        return;
    }

    const global = self.globals.get(name) orelse {
        self.err(expr.variable.name, "Undefined variable: '{s}'", .{name});
        return;
    };

    if (global.type == null) {
        self.err(expr.variable.name, "Could not infer type of variable '{s}'", .{name});
        return;
    }

    self.last_expr_type = global.type.?;
    expr.variable.global = true;
}

fn callExpression(self: *Sema, expr: *Expr) void {
    const call = expr.call;

    self.expression(call.expr);
    const name = call.expr.getToken().lexeme;
    if (self.last_expr_type != .builtin_fn and self.last_expr_type != .function) {
        self.err(call.expr.getToken(), "'{s}' is not callable", .{name});
        return;
    }

    const expected_args, const ret_type = self.getFunctionSignature(name) orelse unreachable;

    if (expected_args.len != call.args.len) {
        self.err(call.expr.getToken(), "'{s}' expected {d} arguments, got {d}", .{
            name,
            expected_args.len,
            call.args.len,
        });
    } else {
        self.checkArgs(call.args, expected_args);
    }

    self.last_expr_type = ret_type;
}

// ---------- utils

fn resolveValue(self: *Sema, stmt: *Stmt) void {
    if (stmt.variable.value == null) return;

    const value = stmt.variable.value.?;
    self.expression(value);
    const value_type = self.last_expr_type;

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

fn checkNumericOperands(self: *Sema, left: Token, lhs: FlowType, right: Token, rhs: FlowType, op: Token.Type) void {
    if (!isNumeric(lhs)) {
        self.err(
            left,
            "Expected left operand of '{s}' to be int or float, got '{s}'",
            .{ @tagName(op), @tagName(lhs) },
        );
    }

    if (!isNumeric(rhs)) {
        self.err(
            right,
            "Expected right operand of '{s}' to be int or float, got '{s}'",
            .{ @tagName(op), @tagName(rhs) },
        );
    }

    if (lhs != rhs) {
        self.err(
            right,
            "Expected operands of '{s}' to be the same. Got left: '{s}', right: '{s}'",
            .{ @tagName(op), @tagName(lhs), @tagName(rhs) },
        );
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

fn getFunctionSignature(self: *Sema, name: []const u8) ?struct { []const FlowType, FlowType } {
    if (builtins.get(name)) |builtin| {
        return .{ builtin.arg_types, builtin.ret_type };
    }

    if (self.functions.get(name)) |func| {
        return .{ func.param_types, func.ret_type };
    }

    return null;
}

fn checkArgs(self: *Sema, args: []*Expr, expected: []const FlowType) void {
    for (args, expected) |arg, arg_type| {
        self.expression(arg);

        if (arg_type != .null and self.last_expr_type != arg_type) {
            self.err(arg.getToken(), "Expected argument of type '{s}', got '{s}'", .{
                @tagName(arg_type),
                @tagName(self.last_expr_type),
            });
        }
    }
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

fn isNumeric(value_type: FlowType) bool {
    return value_type == .int or value_type == .float;
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

fn constant(self: *Sema, value: FlowValue) void {
    for (self.constants.items) |c| {
        if (c.equals(value)) return;
    }
    self.constants.append(self.alloc, value) catch oom();
}

fn err(self: *Sema, token: Token, comptime fmt: []const u8, args: anytype) void {
    error_reporter.reportError(token, fmt, args);
    self.has_error = true;
}

// ---------- Types

const Global = struct {
    token: Token,
    constant: bool,
    type: ?FlowType = null,
};

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

// ----------- Imports and aliases

const Sema = @This();

const error_reporter = @import("error_reporter.zig");

const ast = @import("ast.zig");
const Stmt = ast.Stmt;
const Expr = ast.Expr;
const Token = @import("Token.zig");

const Stack = @import("shared").Stack;
const oom = @import("shared").oom;
const definitions = @import("shared").definitions;
const FlowType = definitions.FlowType;
const FlowValue = definitions.FlowValue;
const builtins = @import("shared").builtins;

const std = @import("std");
const Allocator = std.mem.Allocator;
const HashMap = std.StringHashMapUnmanaged;
const ArrayList = std.ArrayListUnmanaged;
const panic = std.debug.panic;

const MAX_LOCAL_SIZE = 1024;
