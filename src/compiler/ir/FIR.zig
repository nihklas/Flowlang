//! FIR - Flow Intermediate Representation
//!
//! - Complete type information
//! - Simpler Information content
//! - Data-Oriented-Design (hopefully)
//! - allows passes for constant folding and dead code elimination
//!

// NOTE:
// - tree-like structure
// - struct of arrays
// - Node has a kind and an index
// - arrays for each kind
// - childnode is an index into the node list
// - list of constants
// - entrypoint
alloc: Allocator,
arena_state: std.heap.ArenaAllocator,
constants: std.ArrayListUnmanaged(FlowValue) = .empty,
nodes: std.ArrayListUnmanaged(Node) = .empty,
exprs: std.ArrayListUnmanaged(Node.Expr) = .empty,
// conds: std.ArrayListUnmanaged(Node.Cond) = .empty,

// TODO: how do they look?
// functions: std.ArrayListUnmanaged(),

/// Generally, all Nodes are Statements. That means, no Node produces a value.
/// The actual type of Statement is encoded in the `kind` field. Depending on the value there, the
/// `index` field points into a different collection. `before` and `after` contain the index of the
/// previous and next node in the list of instructions.
pub const Node = struct {
    kind: enum { expr },
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

    // pub const Cond = struct {
    //     /// points to a node of kind `expr`
    //     condition: usize,
    //     true: []usize,
    //     false: []usize,
    // };
};

pub fn init(alloc: Allocator) FIR {
    return .{
        .alloc = alloc,
        .arena_state = .init(alloc),
    };
}

pub fn deinit(self: *FIR) void {
    self.arena_state.deinit();
    self.constants.deinit(self.alloc);
    self.nodes.deinit(self.alloc);
    self.exprs.deinit(self.alloc);
    // self.conds.deinit(self.alloc);
}

pub fn fromAST(alloc: Allocator, program: []const *ast.Stmt) FIR {
    var fir: FIR = .init(alloc);
    fir.traverseStmts(program);
    return fir;
}

fn traverseStmts(self: *FIR, stmts: []const *ast.Stmt) void {
    var prev_node: ?usize = null;
    for (stmts) |stmt| {
        const current_node = self.traverseStmt(stmt);

        if (prev_node) |prev| {
            self.nodes.items[prev].after = current_node;
        }

        self.nodes.items[current_node].before = prev_node;
        prev_node = current_node;
    }
}

fn traverseStmt(self: *FIR, stmt: *ast.Stmt) usize {
    switch (stmt.*) {
        .expr => {
            const expr_idx = self.traverseExpr(stmt.expr.expr);
            self.nodes.append(self.alloc, .{ .kind = .expr, .index = expr_idx }) catch oom();
        },
        .block => |block| self.traverseStmts(block.stmts),
        else => std.debug.panic("Statement '{s}' is not yet supported", .{@tagName(stmt.*)}),
    }

    return self.nodes.items.len - 1;
}

fn traverseExpr(self: *FIR, expr: *const ast.Expr) usize {
    switch (expr.*) {
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

inline fn arena(self: *FIR) Allocator {
    return self.arena_state.allocator();
}

const FIR = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");

const shared = @import("shared");
const oom = shared.oom;
const FlowValue = shared.definitions.FlowValue;
const FlowType = shared.definitions.FlowType;
