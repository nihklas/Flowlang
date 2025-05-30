//! FIR - Flow Intermediate Representation
//!
//! - Complete type information
//! - Simpler Information content
//! - Data-Oriented-Design (hopefully)
//! - allows passes for constant folding and dead code elimination
//!

alloc: Allocator,
arena_state: std.heap.ArenaAllocator,
constants: std.ArrayListUnmanaged(FlowValue),
nodes: std.ArrayListUnmanaged(Node),
exprs: std.ArrayListUnmanaged(Node.Expr),
conds: std.ArrayListUnmanaged(Node.Cond),
loops: std.ArrayListUnmanaged(Node.Loop),
entry: usize,

// TODO: how do they look?
// functions: std.ArrayListUnmanaged(),

/// Generally, all Nodes are Statements. That means, no Node produces a value.
/// The actual type of Statement is encoded in the `kind` field. Depending on the value there, the
/// `index` field points into a different collection. `before` and `after` contain the index of the
/// previous and next node in the list of instructions.
pub const Node = struct {
    kind: enum { expr, cond, loop },
    index: usize,
    // TODO: should this be a slice?
    before: ?usize = null,
    after: ?usize = null,

    pub const Expr = struct {
        op: Operator,
        type: FlowType,
        operands: []const usize = &.{},

        const Operator = enum {
            true,
            false,
            null,
            literal,
            equal,
            unequal,
            less,
            less_equal,
            greater,
            greater_equal,
            add,
            sub,
            div,
            mul,
            mod,
            concat,
        };
    };

    pub const Cond = struct {
        /// points to an `expr`
        condition: usize,
        /// node to be executed on true
        true: usize,
        /// node to be executed on false
        false: ?usize = null,
    };

    pub const Loop = struct {
        condition: usize,
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
        .entry = uninitialized_entry,
    };
}

pub fn deinit(self: *FIR) void {
    self.arena_state.deinit();
    self.constants.deinit(self.alloc);
    self.nodes.deinit(self.alloc);
    self.exprs.deinit(self.alloc);
    self.conds.deinit(self.alloc);
    self.loops.deinit(self.alloc);
}

pub fn fromAST(alloc: Allocator, program: []const *ast.Stmt) FIR {
    var fir: FIR = .init(alloc);
    fir.traverseToplevel(program);
    return fir;
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
            if (loop.inc) |inc| {
                // there should be no way, that the inc_stmt in a loop is an empty block or produces
                // an otherwise "empty" Statement
                const inc_idx = self.traverseStmt(inc).?;
                self.nodes.items[body].after = inc_idx;
                self.nodes.items[inc_idx].before = body;
            }

            self.loops.append(self.alloc, .{ .condition = condition, .body = self.startOfBlock(body) }) catch oom();
            self.nodes.append(self.alloc, .{ .kind = .loop, .index = self.loops.items.len - 1 }) catch oom();
        },
        else => std.debug.panic("Statement '{s}' is not yet supported", .{@tagName(stmt.*)}),
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
        else => std.debug.panic("Expression '{s}' is not yet supported", .{@tagName(expr.*)}),
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
