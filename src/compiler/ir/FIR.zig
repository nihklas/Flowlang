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
stmts: std.ArrayListUnmanaged(Node.Stmt) = .empty,
exprs: std.ArrayListUnmanaged(Node.Expr) = .empty,
conds: std.ArrayListUnmanaged(Node.Cond) = .empty,

// TODO: how do they look?
// functions: std.ArrayListUnmanaged(),

const Node = struct {
    kind: enum { stmt, expr, cond },
    index: usize,
    // TODO: should this be a slice?
    before: ?usize,
    after: ?usize,

    const Stmt = struct {
        //
    };

    const Expr = struct {
        op: enum { literal, add, sub, div, mod },
        operands: []usize,
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
    self.stmts.deinit(self.alloc);
    self.exprs.deinit(self.alloc);
    self.conds.deinit(self.alloc);
}

const FIR = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");

const shared = @import("shared");
const FlowValue = shared.definitions.FlowValue;
