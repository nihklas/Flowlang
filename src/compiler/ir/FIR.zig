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
variables: std.ArrayListUnmanaged(Node.Variable),
locals_stack: *std.ArrayListUnmanaged(usize),
stack_list: std.ArrayListUnmanaged(std.ArrayListUnmanaged(usize)),
scope: usize,
globals_count: usize,
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
    kind: enum { expr, cond, loop, variable, pop, @"break", @"continue", @"return" },
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
        operands: []usize = &.{},

        const Operator = enum {
            /// Operands: none
            true,
            /// Operands: none
            false,
            /// Operands: none
            null,
            /// Operands: 1 -> index
            variable,
            /// Operands: 2 -> value, index
            assign,
            /// Operands: n -> value, var_index, ... remaining indices on lhs
            assign_in_array,
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
            /// Operands: 2 -> expressions (first array, second index)
            index,
            /// Operands: 2 -> array, value
            append,
            /// Operands: n >= 1 -> 0 = callee, 1..n = arguments
            call,
            /// Operands: 1 -> constant string
            builtin_fn,
            /// Operands: n
            array,
            /// Operands: n -> 0 = count of params, 1 = start of body, 2..n = closed_values expr indexes
            function,
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
        /// Whether this variable should be hoisted up to the beginning of the scope
        /// For Example, this is applied for function declarations in global scope
        hoist: bool,
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
        .variables = .empty,
        .locals_stack = undefined,
        .stack_list = .empty,
        .entry = uninitialized_entry,
        .scope = 0,
        .globals_count = 0,
    };
}

pub fn deinit(self: *FIR) void {
    assert(self.stack_list.items.len == 1);

    self.arena_state.deinit();
    self.constants.deinit(self.alloc);
    self.nodes.deinit(self.alloc);
    self.exprs.deinit(self.alloc);
    self.conds.deinit(self.alloc);
    self.loops.deinit(self.alloc);
    self.variables.deinit(self.alloc);
    self.locals_stack.deinit(self.alloc);
    self.stack_list.deinit(self.alloc);
    self.* = undefined;
}

pub fn fromAST(alloc: Allocator, program: []const *ast.Stmt) FIR {
    var fir: FIR = .init(alloc);
    fir.stack_list.append(fir.alloc, .empty) catch oom();
    fir.updateLocalStack();
    fir.traverseToplevel(program);
    return fir;
}

fn traverseToplevel(self: *FIR, stmts: []const *ast.Stmt) void {
    var prev_node: ?usize = self.hoistSymbols(stmts);
    if (prev_node) |hoisted| {
        self.entry = self.startOfBlock(hoisted);
    }
    for (stmts) |stmt| {
        if (self.traverseStmt(stmt)) |current_node| {
            self.patchNodesTogether(&prev_node, current_node);

            if (self.entry == uninitialized_entry) {
                self.entry = current_node;
            }
        }
    }
}

fn traverseBlock(self: *FIR, stmts: []const *ast.Stmt) ?usize {
    self.scopeIncr();
    defer self.scopeDecr();

    var prev_node: ?usize = self.hoistSymbols(stmts);
    for (stmts) |stmt| {
        if (self.traverseStmt(stmt)) |current_node| {
            self.patchNodesTogether(&prev_node, current_node);
        }
    }
    return prev_node;
}

fn hoistSymbols(self: *FIR, stmts: []const *ast.Stmt) ?usize {
    var prev_node: ?usize = null;
    for (stmts) |stmt| {
        if (self.scope != 0) continue;
        if (stmt.* != .function) continue;

        if (self.traverseHoistedStmt(stmt)) |current_node| {
            self.patchNodesTogether(&prev_node, current_node);
        }
    }
    return prev_node;
}

fn patchNodesTogether(self: *FIR, prev_node: *?usize, current_node: usize) void {
    if (prev_node.*) |prev| {
        self.nodes.items[prev].after = current_node;
    }

    self.nodes.items[current_node].before = prev_node.*;
    prev_node.* = self.nodes.items.len - 1;
}

fn traverseHoistedStmt(self: *FIR, stmt: *ast.Stmt) ?usize {
    switch (stmt.*) {
        .expr,
        .block,
        .@"if",
        .loop,
        .@"break",
        .@"continue",
        .@"return",
        .variable,
        => return null,
        .function => |function_decl| {
            const function = function_decl.expr.function;

            const param_types = self.alloc.alloc(FlowType, function.params.len) catch oom();
            defer self.alloc.free(param_types);

            for (function.params, param_types) |param, *param_type| {
                assert(param.* == .variable);
                assert(param.variable.type_hint != null);
                param_type.* = param.variable.type_hint.?.type;
            }

            self.putVariableHoisted(
                function.token.lexeme,
                null,
                .function(self.arena(), function.ret_type.type, param_types),
            );
            return self.nodes.items.len - 1;
        },
    }
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
            const typehint = if (variable.type_hint) |typehint| typehint.type else self.exprs.getLast().type;

            self.putVariable(variable.name.lexeme, initializer, typehint.clone(self.arena()));
        },
        .@"break" => self.nodes.append(self.alloc, .{ .kind = .@"break", .index = 0 }) catch oom(),
        .@"continue" => self.nodes.append(self.alloc, .{ .kind = .@"continue", .index = 0 }) catch oom(),
        .function => |function_decl| {
            const function = function_decl.expr.function;
            const var_ref, const hoisted = blk: {
                if (self.resolveVariableOptional(function.token.lexeme)) |result| {
                    if (result[1].scope == self.scope) {
                        break :blk .{ result[0], true };
                    }
                }

                const param_types = self.alloc.alloc(FlowType, function.params.len) catch oom();
                for (function.params, param_types) |param, *param_type| {
                    assert(param.* == .variable);
                    assert(param.variable.type_hint != null);
                    param_type.* = param.variable.type_hint.?.type.clone(self.alloc);
                }

                self.putVariable(
                    function.token.lexeme,
                    null,
                    .function(self.alloc, function.ret_type.type, param_types),
                );

                break :blk .{ self.variables.items.len - 1, false };
            };

            const expr_idx = self.traverseExpr(function_decl.expr);

            self.variables.items[var_ref].expr = expr_idx;

            if (hoisted) {
                return null;
            }
        },
        .@"return" => |return_stmt| {
            const expr = blk: {
                if (return_stmt.value) |value| {
                    break :blk self.traverseExpr(value);
                }
                return self.addExpr(.{ .op = .null, .type = .null });
            };

            self.nodes.append(self.alloc, .{ .kind = .@"return", .index = expr }) catch oom();
        },
    }

    return self.nodes.items.len - 1;
}

fn traverseExpr(self: *FIR, expr: *const ast.Expr) usize {
    switch (expr.*) {
        .grouping => return self.traverseExpr(expr.grouping.expr),
        .literal => {
            if (expr.literal.value != .array) {
                const value = resolveFlowValue(expr);
                const node_expr: Node.Expr = switch (value) {
                    .bool => |boolean| .{ .op = if (boolean) .true else .false, .type = .primitive(.bool) },
                    .null => .{ .op = .null, .type = .null },
                    .string, .int, .float => {
                        return self.addConstantExpr(value);
                    },
                    else => unreachable,
                };
                return self.addExpr(node_expr);
            }

            if (expr.literal.value.array.len == 0) {
                return self.addExpr(.{ .op = .array, .operands = &.{}, .type = .{ .type = .null, .order = 1 } });
            }

            const operands = self.arena().alloc(usize, expr.literal.value.array.len) catch oom();
            for (expr.literal.value.array, 0..) |item, i| {
                operands[i] = self.traverseExpr(item);
            }
            const item_type = self.exprs.items[operands[0]].type;
            return self.addExpr(.{ .op = .array, .operands = operands, .type = .{ .type = item_type.type, .order = item_type.order + 1 } });
        },
        .binary => |binary| {
            const operands = self.arena().alloc(usize, 2) catch oom();

            operands[0] = self.traverseExpr(binary.lhs);
            operands[1] = self.traverseExpr(binary.rhs);

            const op: Node.Expr.Operator, const flow_type: FlowType = switch (binary.op.type) {
                .@"==" => .{ .equal, .primitive(.bool) },
                .@"!=" => .{ .unequal, .primitive(.bool) },
                .@"<" => .{ .less, .primitive(.bool) },
                .@"<=" => .{ .less_equal, .primitive(.bool) },
                .@">=" => .{ .greater_equal, .primitive(.bool) },
                .@">" => .{ .greater, .primitive(.bool) },
                .@".", .@".=" => .{ .concat, .primitive(.string) },
                .@"+", .@"+=" => .{ .add, self.exprs.items[operands[0]].type },
                .@"-", .@"-=" => .{ .sub, self.exprs.items[operands[0]].type },
                .@"*", .@"*=" => .{ .mul, self.exprs.items[operands[0]].type },
                .@"/", .@"/=" => .{ .div, self.exprs.items[operands[0]].type },
                .@"%", .@"%=" => .{ .mod, self.exprs.items[operands[0]].type },
                else => unreachable,
            };

            return self.addExpr(.{ .op = op, .type = flow_type, .operands = operands });
        },
        .logical => |logical| {
            const operands = self.arena().alloc(usize, 2) catch oom();

            operands[0] = self.traverseExpr(logical.lhs);
            operands[1] = self.traverseExpr(logical.rhs);

            return self.addExpr(.{ .op = if (logical.op.type == .@"and") .@"and" else .@"or", .type = .primitive(.bool), .operands = operands });
        },
        .unary => |unary| {
            const operands = self.arena().alloc(usize, 1) catch oom();
            operands[0] = self.traverseExpr(unary.expr);

            const op: Node.Expr.Operator, const flow_type: FlowType = switch (unary.op.type) {
                .@"!" => .{ .not, .primitive(.bool) },
                .@"-" => .{ .negate, self.exprs.items[operands[0]].type },
                else => unreachable,
            };

            return self.addExpr(.{ .op = op, .type = flow_type, .operands = operands });
        },
        .variable => |variable| {
            const var_idx, const variable_node = self.resolveVariable(variable.name.lexeme);
            const operands = self.arena().alloc(usize, 1) catch oom();

            if (variable_node.type.isBuiltinFn()) {
                operands[0] = self.resolveConstant(.{ .string = variable_node.name });
                return self.addExpr(.{ .op = .builtin_fn, .type = .builtinFn(), .operands = operands });
            } else {
                operands[0] = var_idx;
                return self.addExpr(.{ .op = .variable, .type = variable_node.type, .operands = operands });
            }
        },
        .assignment => |assignment| {
            if (assignment.variable.* == .variable) {
                const variable = assignment.variable.variable;
                const var_idx, const variable_node = self.resolveVariable(variable.name.lexeme);
                const operands = self.arena().alloc(usize, 2) catch oom();
                operands[0] = self.traverseExpr(assignment.value);
                operands[1] = var_idx;
                return self.addExpr(.{
                    .op = .assign,
                    .type = variable_node.type,
                    .operands = operands,
                });
            } else {
                assert(assignment.variable.* == .index);
                const index = assignment.variable.index;
                // Gesamtes operands array besser nutzen:
                // - 0 = value
                // - 1 = var_idx
                // - 2.. = all remaining indices of the lhs

                const index_amount = blk: {
                    var order: u8 = 1;
                    s: switch (index.expr.*) {
                        .variable => break :blk order,
                        .index => |index_expr| {
                            order += 1;
                            continue :s index_expr.expr.*;
                        },
                        else => unreachable,
                    }
                };

                const operands = self.arena().alloc(usize, 2 + index_amount) catch oom();
                operands[0] = self.traverseExpr(assignment.value);
                operands[1] = variable: switch (index.expr.*) {
                    .index => |index_expr| continue :variable index_expr.expr.*,
                    .variable => |variable| {
                        const var_idx, _ = self.resolveVariable(variable.name.lexeme);
                        break :variable var_idx;
                    },
                    else => unreachable,
                };

                var op_idx = operands.len - 1;
                s: switch (assignment.variable.*) {
                    .index => |index_expr| {
                        // NOTE: Index Operands start at index 2, so that check ensures that the
                        // other operands aren't overwritten
                        assert(op_idx >= 2);
                        operands[op_idx] = self.traverseExpr(index_expr.index);
                        op_idx -= 1;
                        continue :s index_expr.expr.*;
                    },
                    .variable => {},
                    else => unreachable,
                }

                return self.addExpr(.{
                    .op = .assign_in_array,
                    .type = self.exprs.items[operands[0]].type,
                    .operands = operands,
                });
            }
        },
        .call => |call| {
            const operands = self.arena().alloc(usize, call.args.len + 1) catch oom();
            operands[0] = self.traverseExpr(call.expr);
            for (call.args, 1..) |arg, idx| {
                operands[idx] = self.traverseExpr(arg);
            }

            return self.addExpr(.{ .op = .call, .operands = operands, .type = self.resolveFunctionReturnType(operands[0], call.expr) });
        },
        .index => |index| {
            const operands = self.arena().alloc(usize, 2) catch oom();
            operands[0] = self.traverseExpr(index.expr);
            operands[1] = self.traverseExpr(index.index);
            const item_type = self.exprs.items[operands[0]].type;
            assert(item_type.order > 0);
            var new_type = item_type.clone(self.alloc);
            new_type.order -= 1;
            return self.addExpr(.{ .op = .index, .operands = operands, .type = new_type });
        },
        .append => |append| {
            const operands = self.arena().alloc(usize, 2) catch oom();
            operands[0] = self.traverseExpr(append.variable);
            operands[1] = self.traverseExpr(append.value);
            return self.addExpr(.{ .op = .append, .operands = operands, .type = self.exprs.items[operands[0]].type });
        },
        .function => |function| {
            const closed_values = self.arena().alloc(usize, function.closed_values.len) catch oom();
            for (function.closed_values, 0..) |closed_value, idx| {
                assert(closed_value.* == .variable);
                closed_values[idx] = self.traverseExpr(closed_value);
            }

            self.openFunction();
            defer self.closeFunction();

            const param_types = self.arena().alloc(FlowType, function.params.len) catch oom();
            for (function.params, param_types) |param, *param_type| {
                param_type.* = param.variable.type_hint.?.type;
                self.putVariable(param.variable.name.lexeme, null, param_type.*);
            }

            for (function.closed_values, closed_values) |value, idx| {
                self.putVariable(
                    value.variable.name.lexeme,
                    null,
                    self.exprs.items[idx].type,
                );
            }

            var maybe_body = self.traverseBlock(function.body);
            if (maybe_body) |body| {
                if (self.nodes.items[body].kind != .@"return") {
                    _ = self.addExpr(.{ .op = .null, .type = .null });
                    self.nodes.append(self.alloc, .{ .kind = .@"return", .index = self.exprs.items.len - 1 }) catch oom();
                    self.patchNodesTogether(&maybe_body, self.nodes.items.len - 1);
                }
            }

            const operands = self.arena().alloc(usize, 2 + closed_values.len) catch oom();
            operands[0] = function.params.len;
            operands[1] = if (maybe_body) |body| self.startOfBlock(body) else uninitialized_entry;

            if (closed_values.len > 0) {
                for (operands[2..], closed_values) |*operand, closed_value| {
                    operand.* = closed_value;
                }
            }

            const func_type: FlowType = .function(self.arena(), function.ret_type.type, param_types);
            return self.addExpr(.{ .op = .function, .operands = operands, .type = func_type });
        },
    }
}

pub fn addConstantExpr(self: *FIR, value: FlowValue) usize {
    const operands = self.arena().alloc(usize, 1) catch oom();
    operands[0] = self.resolveConstant(value);
    return self.addExpr(.{ .op = .literal, .type = self.constants.items[operands[0]].getType(), .operands = operands });
}

pub fn addExpr(self: *FIR, expr: Node.Expr) usize {
    self.exprs.append(self.alloc, expr) catch oom();
    return self.exprs.items.len - 1;
}

fn startOfBlock(self: *FIR, idx: usize) usize {
    assert(idx < self.nodes.items.len);

    var node = self.nodes.items[idx];
    var node_idx = idx;
    while (node.before) |before| {
        assert(before < self.nodes.items.len);

        node = self.nodes.items[before];
        node_idx = before;
    }
    return node_idx;
}

fn resolveFlowValue(expr: *const ast.Expr) FlowValue {
    assert(expr.* == .literal);
    return switch (expr.literal.value) {
        .int => |int| .{ .int = int },
        .float => |float| .{ .float = float },
        .bool => |boolean| .{ .bool = boolean },
        .string => |string| .{ .string = string },
        .null => .null,
        .array => unreachable,
    };
}

pub fn resolveConstant(self: *FIR, value: FlowValue) usize {
    return for (self.constants.items, 0..) |c, i| {
        if (c.equals(&value)) break i;
    } else {
        self.constants.append(self.alloc, value) catch oom();
        return self.constants.items.len - 1;
    };
}

fn resolveVariable(self: *FIR, name: []const u8) struct { usize, Node.Variable } {
    return self.resolveVariableOptional(name) orelse @panic("No Variable found");
}
fn resolveVariableOptional(self: *FIR, name: []const u8) ?struct { usize, Node.Variable } {
    if (builtins.get(name)) |_| {
        return .{ 0, .{
            .name = name,
            .type = .builtinFn(),
            .scope = 0,
            .expr = null,
            .stack_idx = 0,
            .hoist = false,
        } };
    }

    // Locals in Scope
    var l_idx = self.locals_stack.items.len;
    while (l_idx > 0) {
        l_idx -= 1;

        const idx = self.locals_stack.items[l_idx];
        const local = self.variables.items[idx];
        if (std.mem.eql(u8, local.name, name)) {
            return .{ idx, local };
        }
    }

    // Globals
    return for (self.variables.items, 0..) |variable, idx| {
        if (variable.scope != 0) continue;
        if (std.mem.eql(u8, variable.name, name)) {
            return .{ idx, variable };
        }
    } else null;
}

fn resolveFunctionReturnType(self: *FIR, callee: usize, original_callee_expr: *const ast.Expr) FlowType {
    const callee_expr = self.exprs.items[callee];

    assert(callee_expr.type.isFunction() or callee_expr.type.isBuiltinFn());

    switch (callee_expr.type.type) {
        .builtin_fn => {
            assert(builtins.get(original_callee_expr.variable.name.lexeme) != null);

            const builtin = builtins.get(original_callee_expr.variable.name.lexeme).?;
            return builtin.ret_type;
        },
        .function => {
            return callee_expr.type.function_type.ret_type;
        },
        else => unreachable,
    }
}

fn scopeIncr(self: *FIR) void {
    self.scope += 1;
}

fn openFunction(self: *FIR) void {
    self.scopeIncr();
    self.stack_list.append(self.alloc, .empty) catch oom();
    self.updateLocalStack();
}

fn scopeDecr(self: *FIR) void {
    // NOTE: We cannot go lower than global scope
    assert(self.scope > 0);

    self.scope -= 1;

    const prev_len = self.locals_stack.items.len;
    const new_len = for (self.locals_stack.items, 0..) |local_idx, idx| {
        const local = self.variables.items[local_idx];
        if (local.scope > self.scope) break idx;
    } else self.locals_stack.items.len;

    const pop_count = prev_len - new_len;
    // NOTE: In order to pop something of the stack, there need to be AT LEAST as many nodes before
    // that to create the stack in the first place
    assert(self.nodes.items.len >= pop_count);
    for (0..pop_count) |_| {
        const before = self.nodes.items.len - 1;
        self.nodes.append(self.alloc, .{ .kind = .pop, .index = 0, .before = before }) catch oom();
        self.nodes.items[before].after = self.nodes.items.len - 1;
    }

    self.locals_stack.shrinkRetainingCapacity(new_len);
}

fn closeFunction(self: *FIR) void {
    self.scopeDecr();
    var last_stack = self.stack_list.pop();
    last_stack.?.deinit(self.alloc);
    self.updateLocalStack();
}

fn updateLocalStack(self: *FIR) void {
    self.locals_stack = &self.stack_list.items[self.stack_list.items.len - 1];
}

fn putVariableHoisted(self: *FIR, name: []const u8, expr: ?usize, var_type: FlowType) void {
    self.putVariableRaw(name, expr, var_type, true);
}

fn putVariable(self: *FIR, name: []const u8, expr: ?usize, var_type: FlowType) void {
    self.putVariableRaw(name, expr, var_type, false);
}

fn putVariableRaw(self: *FIR, name: []const u8, expr: ?usize, var_type: FlowType, hoist: bool) void {
    const stack_idx = blk: {
        if (self.scope == 0) {
            defer self.globals_count += 1;
            break :blk self.globals_count;
        }
        defer self.locals_stack.append(self.alloc, self.variables.items.len) catch oom();
        break :blk self.locals_stack.items.len;
    };
    self.variables.append(self.alloc, .{
        .name = name,
        .expr = expr,
        .type = var_type,
        .scope = self.scope,
        .stack_idx = stack_idx,
        .hoist = hoist,
    }) catch oom();
    self.nodes.append(self.alloc, .{ .kind = .variable, .index = self.variables.items.len - 1 }) catch oom();
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

pub const uninitialized_entry = std.math.maxInt(@FieldType(FIR, "entry"));

const FIR = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const ast = @import("ast.zig");

const shared = @import("shared");
const oom = shared.oom;
const FlowValue = shared.definitions.FlowValue;
const FlowType = shared.definitions.FlowType;
const builtins = shared.builtins;
const Token = @import("Token.zig");
