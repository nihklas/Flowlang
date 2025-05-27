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
constants: std.ArrayListUnmanaged(FlowValue) = .empty,
nodes: std.ArrayListUnmanaged(Node) = .empty,
exprs: std.ArrayListUnmanaged(Node.Expr) = .empty,
conds: std.ArrayListUnmanaged(Node.Cond) = .empty,

// TODO: how do they look?
// functions: std.ArrayListUnmanaged(),

const Node = struct {
    kind: enum { expr, cond, pop },
    index: usize,
    // TODO: should this be a slice?
    before: ?usize = null,
    after: ?usize = null,

    const Expr = struct {
        op: enum { literal, add, sub, div, mod },
        operands: []const usize,
    };

    const Cond = struct {
        /// must point to a node of kind `expr`
        condition: usize,
        true: []usize,
        false: []usize,
    };
};

pub fn init(alloc: Allocator) FIR {
    return .{ .alloc = alloc };
}

pub fn deinit(self: *FIR) void {
    self.constants.deinit(self.alloc);
    self.nodes.deinit(self.alloc);
    self.exprs.deinit(self.alloc);
    self.conds.deinit(self.alloc);
}

pub fn fromAST(alloc: Allocator, program: []const *ast.Stmt) FIR {
    var fir: FIR = .init(alloc);
    fir.traverseStmts(program);
    return fir;
}

fn traverseStmts(self: *FIR, stmts: []const *ast.Stmt) void {
    for (stmts) |stmt| {
        self.traverseStmt(stmt);
    }
}

fn traverseStmt(self: *FIR, stmt: *ast.Stmt) void {
    switch (stmt.*) {
        .expr => {
            const expr_idx = self.traverseExpr(stmt.expr.expr);
            self.nodes.append(self.alloc, .{ .kind = .expr, .index = expr_idx }) catch oom();
        },
        else => std.debug.panic("Statement '{s}' is not yet supported", .{@tagName(stmt.*)}),
    }
}

fn traverseExpr(self: *FIR, expr: *const ast.Expr) usize {
    switch (expr.*) {
        .literal => {
            const flowvalue: FlowValue = switch (expr.literal.value) {
                .int => |int| .{ .int = int },
                .float => |float| .{ .float = float },
                .bool => |boolean| .{ .bool = boolean },
                .null => .null,
                .string => |string| .{ .string = string },
                .function, .builtin_fn => unreachable,
            };
            const constant_idx = self.resolveConstant(flowvalue);
            self.exprs.append(self.alloc, .{ .op = .literal, .operands = &.{constant_idx} }) catch oom();
            return self.exprs.items.len - 1;
        },
        else => std.debug.panic("Expression '{s}' is not yet supported", .{@tagName(expr.*)}),
    }
}

fn resolveConstant(self: *FIR, value: FlowValue) usize {
    return for (self.constants.items, 0..) |c, i| {
        if (c.equals(value)) break i;
    } else {
        self.constants.append(self.alloc, value) catch oom();
        return self.constants.items.len - 1;
    };
}

const FIR = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");

const shared = @import("shared");
const oom = shared.oom;
const FlowValue = shared.definitions.FlowValue;
