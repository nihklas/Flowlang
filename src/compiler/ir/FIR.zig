//! FIR - Flow Intermediate Representation
//!
//! - Complete type information
//! - Simpler Information content
//! - Data-Oriented-Design (hopefully)
//! - allows passes for constant folding and dead code elimination
//!

/// general allocator that is passed into from init
alloc: Allocator,
/// Arena state for all intermediate/small slices like the operands in an expression
/// backed by the general allocator
arena_state: std.heap.ArenaAllocator,
/// Collection of constant values found in the source code
constants: std.ArrayListUnmanaged(FlowValue),
nodes: std.ArrayListUnmanaged(Node),
exprs: std.ArrayListUnmanaged(Node.Expr),
conds: std.ArrayListUnmanaged(Node.Cond),
loops: std.ArrayListUnmanaged(Node.Loop),
globals: std.ArrayListUnmanaged(Node.Variable),
functions: std.ArrayListUnmanaged(Node.Function),
locals: std.ArrayListUnmanaged(Node.Variable),
locals_stack: std.ArrayListUnmanaged(usize),
scope: usize,
/// The entrypoint of the FIR graph, gets initialized to `uninitialized_entry`.
entry: usize,

/// Generally, all Nodes are Statements. That means, no Node produces a value.
/// The actual type of Statement is encoded in the `kind` field. Depending on the value there, the
/// `index` field points into a different collection. `before` and `after` contain the index of the
/// previous and next node in the list of instructions.
pub const Node = struct {
    /// The actual node kind
    /// Every Kind has its own collection in which `index` points to the actual information.
    ///
    /// `pop`, `break` and `continue` are special cases in that the index field is not used. It is a
    /// literal instruction for the compiler
    kind: enum { expr, cond, loop, global, local, pop, @"break", @"continue" },
    /// The index into the concrete node kind collection
    index: usize,
    /// index into the node collection to the node directly in front of the current one. If this
    /// field is null, it means the current node is at the start of a block
    before: ?usize = null,
    /// index into the node collection to the node directly after the current one. If this field is
    /// null, it means the current node is at the end of a block
    after: ?usize = null,

    pub const Expr = struct {
        /// the type of expression
        op: Operator,
        /// The resulting type of the expression
        type: FlowType,
        /// The used operands. Depending on the `op`, this can have a different size and the
        /// elements point into a different collection
        operands: []const usize = &.{},

        const Operator = enum {
            /// Operands: none
            true,
            /// Operands: none
            false,
            /// Operands: none
            null,
            /// Operands: 1 -> index
            global,
            /// Operands: 1 -> index
            local,
            /// Operands: 1 -> index
            assign_global,
            /// Operands: 1 -> index
            assign_local,
            /// Operands: 1 -> constants
            literal,
            /// Operands: 1 -> expression
            not,
            /// Operands: 1 -> expression
            negate,
            /// Operands: 2 -> expressions
            equal,
            /// Operands: 2 -> expressions
            unequal,
            /// Operands: 2 -> expressions
            less,
            /// Operands: 2 -> expressions
            less_equal,
            /// Operands: 2 -> expressions
            greater,
            /// Operands: 2 -> expressions
            greater_equal,
            /// Operands: 2 -> expressions
            add,
            /// Operands: 2 -> expressions
            sub,
            /// Operands: 2 -> expressions
            div,
            /// Operands: 2 -> expressions
            mul,
            /// Operands: 2 -> expressions
            mod,
            /// Operands: 2 -> expressions
            concat,
            /// Operands: 2 -> expressions
            @"and",
            /// Operands: 2 -> expressions
            @"or",
            /// Operands: n >= 1 -> 0 = callee, 1..n = arguments
            call,
            /// Operands: 1 -> constant string
            builtin_fn,
        };
    };

    pub const Cond = struct {
        /// points to an `expr`
        condition: usize,
        /// node to be executed on true
        true: usize,
        /// node to be executed on false
        false: ?usize,
    };

    pub const Loop = struct {
        /// points to an expr
        condition: usize,
        /// may point to an expr
        inc: ?usize,
        /// points to the starting node of the block
        body: usize,
    };

    pub const Variable = struct {
        /// name of the variable used to resolve its position
        name: []const u8,
        /// type of the variable (not sure if we need it yet)
        type: FlowType,
        /// optional expression to initialize the variable
        expr: ?usize,
        /// scope level in which it got declared in
        scope: usize,
        /// stack index, with which it will get fetched in VM. Set for both local and global but
        /// only used for local
        stack_idx: usize,
        /// index for additional purpose
        /// - index into the functions collection in case `.type == .function`
        extra_idx: usize,
    };

    pub const Function = struct {
        name: []const u8,
        ret_type: FlowType,
        param_count: usize,
        body: usize,
    };

    pub fn format(self: Node, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll("Node{ ");
        try writer.print("kind: {s}, ", .{@tagName(self.kind)});
        try writer.print("index: {d}, ", .{self.index});
        try writer.print("before: {?d}, ", .{self.before});
        try writer.print("after: {?d}", .{self.after});
        try writer.writeAll(" }");
    }
};

pub fn init(alloc: Allocator) FIR {
    return .{
        .alloc = alloc,
        .arena_state = .init(alloc),
        .constants = .empty,
        .nodes = .empty,
        .exprs = .empty,
        .conds = .empty,
        .loops = .empty,
        .globals = .empty,
        .functions = .empty,
        .locals = .empty,
        .locals_stack = .empty,
        .entry = uninitialized_entry,
        .scope = 0,
    };
}

pub fn deinit(self: *FIR) void {
    self.arena_state.deinit();
    self.constants.deinit(self.alloc);
    self.nodes.deinit(self.alloc);
    self.exprs.deinit(self.alloc);
    self.conds.deinit(self.alloc);
    self.loops.deinit(self.alloc);
    self.globals.deinit(self.alloc);
    self.functions.deinit(self.alloc);
    self.locals.deinit(self.alloc);
    self.locals_stack.deinit(self.alloc);
}

pub fn fromAST(alloc: Allocator, program: []const *ast.Stmt) FIR {
    var fir: FIR = .init(alloc);
    fir.getTopLevelFunctions(program);
    fir.traverseToplevel(program);
    return fir;
}

fn getTopLevelFunctions(self: *FIR, program: []const *ast.Stmt) void {
    for (program) |stmt| {
        if (stmt.* != .function) continue;
        const function = stmt.function;
        if (self.traverseBlock(function.body)) |body| {
            self.putFunction(.{
                .name = function.name.lexeme,
                .body = self.startOfBlock(body),
                .ret_type = typeFromToken(function.ret_type.type).?,
                .param_count = function.params.len,
            });
        }
    }
}

fn traverseToplevel(self: *FIR, stmts: []const *ast.Stmt) void {
    var prev_node: ?usize = null;
    for (stmts) |stmt| {
        if (self.traverseStmt(stmt)) |current_node| {
            if (prev_node) |prev| {
                self.nodes.items[prev].after = current_node;
            }

            self.nodes.items[current_node].before = prev_node;
            prev_node = self.nodes.items.len - 1;

            if (self.entry == uninitialized_entry) {
                self.entry = current_node;
            }
        }
    }
}

fn traverseBlock(self: *FIR, stmts: []const *ast.Stmt) ?usize {
    self.scopeIncr();
    defer self.scopeDecr();

    var prev_node: ?usize = null;
    for (stmts) |stmt| {
        if (self.traverseStmt(stmt)) |current_node| {
            if (prev_node) |prev| {
                self.nodes.items[prev].after = current_node;
            }

            self.nodes.items[current_node].before = prev_node;
            prev_node = self.nodes.items.len - 1;
        }
    }
    return prev_node;
}

fn traverseStmt(self: *FIR, stmt: *ast.Stmt) ?usize {
    switch (stmt.*) {
        .expr => {
            const expr_idx = self.traverseExpr(stmt.expr.expr);
            self.nodes.append(self.alloc, .{ .kind = .expr, .index = expr_idx }) catch oom();
        },
        .block => |block| {
            return self.startOfBlock(self.traverseBlock(block.stmts) orelse return null);
        },
        .@"if" => |if_stmt| {
            const condition = self.traverseExpr(if_stmt.condition);
            const true_branch = blk: {
                if (self.traverseStmt(if_stmt.true_branch)) |true_branch| {
                    break :blk self.startOfBlock(true_branch);
                }
                return null;
            };
            const false_branch = blk: {
                if (if_stmt.false_branch) |false_branch_stmt| {
                    if (self.traverseStmt(false_branch_stmt)) |false_branch| {
                        break :blk self.startOfBlock(false_branch);
                    }
                }
                break :blk null;
            };

            self.conds.append(self.alloc, .{ .condition = condition, .true = true_branch, .false = false_branch }) catch oom();
            self.nodes.append(self.alloc, .{ .kind = .cond, .index = self.conds.items.len - 1 }) catch oom();
        },
        .loop => |loop| {
            const condition = self.traverseExpr(loop.condition);
            const body = self.traverseBlock(loop.body) orelse return null;
            const inc = if (loop.inc) |inc| self.traverseExpr(inc) else null;

            self.loops.append(self.alloc, .{ .condition = condition, .body = self.startOfBlock(body), .inc = inc }) catch oom();
            self.nodes.append(self.alloc, .{ .kind = .loop, .index = self.loops.items.len - 1 }) catch oom();
        },
        .variable => |variable| {
            const initializer = if (variable.value) |value| self.traverseExpr(value) else null;
            const typehint: FlowType = blk: {
                if (variable.type_hint) |typehint| {
                    break :blk switch (typehint.type.type) {
                        .bool => .bool,
                        .string => .string,
                        .int => .int,
                        .float => .float,
                        else => unreachable,
                    };
                }
                break :blk self.exprs.getLast().type;
            };

            self.putVariable(variable.name.lexeme, initializer, typehint);
        },
        .@"break" => self.nodes.append(self.alloc, .{ .kind = .@"break", .index = 0 }) catch oom(),
        .@"continue" => self.nodes.append(self.alloc, .{ .kind = .@"continue", .index = 0 }) catch oom(),
        .function => |function| {
            if (self.scope == 0) return null;

            const body = self.startOfBlock(self.traverseBlock(function.body).?);

            self.putFunction(.{
                .name = function.name.lexeme,
                .body = body,
                .ret_type = typeFromToken(function.ret_type.type).?,
                .param_count = function.params.len,
            });
        },
        .@"return" => @panic("Not yet supported"),
    }

    return self.nodes.items.len - 1;
}

fn traverseExpr(self: *FIR, expr: *const ast.Expr) usize {
    switch (expr.*) {
        .grouping => return self.traverseExpr(expr.grouping.expr),
        .literal => {
            const value = resolveFlowValue(expr);
            const node_expr: Node.Expr = blk: switch (value) {
                .bool => |boolean| .{ .op = if (boolean) .true else .false, .type = .bool },
                .null => .{ .op = .null, .type = .null },
                .string, .int, .float => {
                    const operands = self.arena().alloc(usize, 1) catch oom();
                    operands[0] = self.resolveConstant(value);
                    break :blk .{ .op = .literal, .type = self.constants.items[operands[0]], .operands = operands };
                },
                else => unreachable,
            };
            self.exprs.append(self.alloc, node_expr) catch oom();
        },
        .binary => |binary| {
            const operands = self.arena().alloc(usize, 2) catch oom();

            operands[0] = self.traverseExpr(binary.lhs);
            operands[1] = self.traverseExpr(binary.rhs);

            const op: Node.Expr.Operator, const flow_type: FlowType = switch (binary.op.type) {
                .@"==" => .{ .equal, .bool },
                .@"!=" => .{ .unequal, .bool },
                .@"<" => .{ .less, .bool },
                .@"<=" => .{ .less_equal, .bool },
                .@">=" => .{ .greater_equal, .bool },
                .@">" => .{ .greater, .bool },
                .@".", .@".=" => .{ .concat, .string },
                .@"+", .@"+=" => .{ .add, self.exprs.items[operands[0]].type },
                .@"-", .@"-=" => .{ .sub, self.exprs.items[operands[0]].type },
                .@"*", .@"*=" => .{ .mul, self.exprs.items[operands[0]].type },
                .@"/", .@"/=" => .{ .div, self.exprs.items[operands[0]].type },
                .@"%", .@"%=" => .{ .mod, self.exprs.items[operands[0]].type },
                else => unreachable,
            };

            self.exprs.append(self.alloc, .{ .op = op, .type = flow_type, .operands = operands }) catch oom();
        },
        .logical => |logical| {
            const operands = self.arena().alloc(usize, 2) catch oom();

            operands[0] = self.traverseExpr(logical.lhs);
            operands[1] = self.traverseExpr(logical.rhs);

            self.exprs.append(self.alloc, .{ .op = if (logical.op.type == .@"and") .@"and" else .@"or", .type = .bool, .operands = operands }) catch oom();
        },
        .unary => |unary| {
            const operands = self.arena().alloc(usize, 1) catch oom();
            operands[0] = self.traverseExpr(unary.expr);

            const op: Node.Expr.Operator, const flow_type: FlowType = switch (unary.op.type) {
                .@"!" => .{ .not, .bool },
                .@"-" => .{ .negate, self.exprs.items[operands[0]].type },
                else => unreachable,
            };

            self.exprs.append(self.alloc, .{ .op = op, .type = flow_type, .operands = operands }) catch oom();
        },
        .variable => |variable| {
            const var_idx, const variable_node = self.resolveVariable(variable.name.lexeme);
            const operands = self.arena().alloc(usize, 1) catch oom();

            if (variable_node.type == .builtin_fn) {
                operands[0] = self.resolveConstant(.{ .string = variable_node.name });
                self.exprs.append(self.alloc, .{ .op = .builtin_fn, .type = .builtin_fn, .operands = operands }) catch oom();
            } else {
                operands[0] = var_idx;
                self.exprs.append(self.alloc, .{ .op = if (variable_node.scope == 0) .global else .local, .type = variable_node.type, .operands = operands }) catch oom();
            }
        },
        .assignment => |assignment| {
            const var_idx, const variable_node = self.resolveVariable(assignment.name.lexeme);
            const operands = self.arena().alloc(usize, 2) catch oom();
            operands[0] = self.traverseExpr(assignment.value);
            operands[1] = var_idx;
            self.exprs.append(self.alloc, .{ .op = if (variable_node.scope == 0) .assign_global else .assign_local, .type = variable_node.type, .operands = operands }) catch oom();
        },
        .call => |call| {
            const operands = self.arena().alloc(usize, call.args.len + 1) catch oom();
            operands[0] = self.traverseExpr(call.expr);
            for (call.args, 1..) |arg, idx| {
                operands[idx] = self.traverseExpr(arg);
            }

            self.exprs.append(self.alloc, .{ .op = .call, .operands = operands, .type = self.resolveFunctionReturnType(operands[0], call.expr) }) catch oom();
        },
    }
    return self.exprs.items.len - 1;
}

fn startOfBlock(self: *FIR, idx: usize) usize {
    std.debug.assert(idx < self.nodes.items.len);

    var node = self.nodes.items[idx];
    var node_idx = idx;
    while (node.before) |before| {
        std.debug.assert(before < self.nodes.items.len);

        node = self.nodes.items[before];
        node_idx = before;
    }
    return node_idx;
}

fn resolveFlowValue(expr: *const ast.Expr) FlowValue {
    std.debug.assert(expr.* == .literal);
    return switch (expr.literal.value) {
        .int => |int| .{ .int = int },
        .float => |float| .{ .float = float },
        .bool => |boolean| .{ .bool = boolean },
        .string => |string| .{ .string = string },
        .null => .null,
        .function, .builtin_fn => unreachable,
    };
}

fn resolveConstant(self: *FIR, value: FlowValue) usize {
    return for (self.constants.items, 0..) |c, i| {
        if (c.equals(value)) break i;
    } else {
        self.constants.append(self.alloc, value) catch oom();
        return self.constants.items.len - 1;
    };
}

fn resolveVariable(self: *FIR, name: []const u8) struct { usize, Node.Variable } {
    if (builtins.get(name)) |_| {
        return .{ 0, .{
            .name = name,
            .type = .builtin_fn,
            .scope = 0,
            .expr = null,
            .stack_idx = 0,
            .extra_idx = 0,
        } };
    }

    var l_idx = self.locals_stack.items.len;
    while (l_idx > 0) {
        l_idx -= 1;

        const local = self.locals.items[self.locals_stack.items[l_idx]];
        if (std.mem.eql(u8, local.name, name)) {
            return .{ l_idx, local };
        }
    }

    return for (self.globals.items, 0..) |global, g_idx| {
        if (std.mem.eql(u8, global.name, name)) {
            return .{ g_idx, global };
        }
    } else @panic("No Variable found");
}

fn resolveFunctionReturnType(self: *FIR, callee: usize, original_callee_expr: *const ast.Expr) FlowType {
    const callee_expr = self.exprs.items[callee];
    std.debug.assert(callee_expr.type == .function or callee_expr.type == .builtin_fn);
    std.debug.assert(original_callee_expr.* == .variable);

    switch (callee_expr.type) {
        .builtin_fn => {
            std.debug.assert(builtins.get(original_callee_expr.variable.name.lexeme) != null);

            const builtin = builtins.get(original_callee_expr.variable.name.lexeme).?;
            return builtin.ret_type;
        },
        .function => {
            _, const variable = self.resolveVariable(original_callee_expr.variable.name.lexeme);
            std.debug.assert(variable.type == .function);

            return self.functions.items[variable.extra_idx].ret_type;
        },
        else => unreachable,
    }
}

fn scopeIncr(self: *FIR) void {
    self.scope += 1;
}

fn scopeDecr(self: *FIR) void {
    // NOTE: We cannot go lower than global scope
    std.debug.assert(self.scope > 0);

    self.scope -= 1;

    const prev_len = self.locals_stack.items.len;
    const new_len = for (self.locals_stack.items, 0..) |local_idx, idx| {
        const local = self.locals.items[local_idx];
        if (local.scope > self.scope) break idx;
    } else self.locals_stack.items.len;

    const pop_count = prev_len - new_len;
    // NOTE: In order to pop something of the stack, there need to be AT LEAST as many nodes before
    // that to create the stack in the first place
    std.debug.assert(self.nodes.items.len >= pop_count);
    for (0..pop_count) |_| {
        const before = self.nodes.items.len - 1;
        self.nodes.append(self.alloc, .{ .kind = .pop, .index = 0, .before = before }) catch oom();
        self.nodes.items[before].after = self.nodes.items.len - 1;
    }

    self.locals_stack.shrinkRetainingCapacity(new_len);
}

fn putVariable(self: *FIR, name: []const u8, expr: ?usize, var_type: FlowType) void {
    const variable: Node.Variable = .{
        .name = name,
        .expr = expr,
        .type = var_type,
        .scope = self.scope,
        .stack_idx = self.locals.items.len,
        .extra_idx = if (var_type == .function) self.functions.items.len - 1 else 0,
    };

    if (self.scope == 0) {
        self.globals.append(self.alloc, variable) catch oom();
        self.nodes.append(self.alloc, .{ .kind = .global, .index = self.globals.items.len - 1 }) catch oom();
    } else {
        self.locals.append(self.alloc, variable) catch oom();
        self.locals_stack.append(self.alloc, self.locals.items.len - 1) catch oom();
        self.nodes.append(self.alloc, .{ .kind = .local, .index = self.locals.items.len - 1 }) catch oom();
    }
}

fn putFunction(self: *FIR, function: Node.Function) void {
    self.functions.append(self.alloc, function) catch oom();
    self.putVariable(function.name, null, .function);
}

fn typeFromToken(token: Token) ?FlowType {
    return switch (token.type) {
        .bool => .bool,
        .string => .string,
        .int => .int,
        .float => .float,
        .null => .null,
        else => null,
    };
}

fn arena(self: *FIR) Allocator {
    return self.arena_state.allocator();
}

pub const uninitialized_entry = std.math.maxInt(usize);

const FIR = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");

const shared = @import("shared");
const oom = shared.oom;
const FlowValue = shared.definitions.FlowValue;
const FlowType = shared.definitions.FlowType;
const builtins = shared.builtins;
const Token = @import("Token.zig");
