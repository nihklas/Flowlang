pub fn foldConstants(fir: *FIR) void {
    var folder: Folder = .{ .fir = fir };
    folder.traverse();
}

const Folder = struct {
    fir: *FIR,

    fn traverse(self: *Folder) void {
        for (0..self.fir.nodes.items.len) |idx| {
            self.traverseStmt(idx);
        }
    }

    fn traverseStmt(self: *Folder, node_idx: usize) void {
        const node = &self.fir.nodes.items[node_idx];
        switch (node.kind) {
            .pop => {},
            .@"break" => {},
            .@"continue" => {},
            .expr, .@"return" => {
                node.index = self.traverseExpr(node.index);
            },
            .cond => {
                const cond = &self.fir.conds.items[node.index];
                cond.condition = self.traverseExpr(cond.condition);
            },
            .loop => {
                const loop = &self.fir.loops.items[node.index];
                loop.condition = self.traverseExpr(loop.condition);
                if (loop.inc) |inc| {
                    loop.inc = self.traverseExpr(inc);
                }
            },
            .variable => {
                const variable = &self.fir.variables.items[node.index];
                if (variable.expr) |expr| {
                    variable.expr = self.traverseExpr(expr);
                }
            },
        }
    }

    fn traverseExpr(self: *Folder, expr_idx: usize) usize {
        const expr = self.fir.exprs.items[expr_idx];
        switch (expr.op) {
            .true,
            .false,
            .null,
            .literal,
            .variable,
            => {},
            .add, .sub, .mul, .div, .mod => |tag| {
                expr.operands[0] = self.traverseExpr(expr.operands[0]);
                expr.operands[1] = self.traverseExpr(expr.operands[1]);
                const lhs_result = self.fir.exprs.items[expr.operands[0]];
                const rhs_result = self.fir.exprs.items[expr.operands[1]];
                if (lhs_result.op == .literal and rhs_result.op == .literal) {
                    const lhs = self.fir.constants.items[lhs_result.operands[0]];
                    const rhs = self.fir.constants.items[rhs_result.operands[0]];

                    const new_constant: FlowValue = switch (tag) {
                        .add => switch (expr.type.type) {
                            .int => .{ .int = lhs.int + rhs.int },
                            .float => .{ .float = lhs.float + rhs.float },
                            else => unreachable,
                        },
                        .sub => switch (expr.type.type) {
                            .int => .{ .int = lhs.int - rhs.int },
                            .float => .{ .float = lhs.float - rhs.float },
                            else => unreachable,
                        },
                        .mul => switch (expr.type.type) {
                            .int => .{ .int = lhs.int * rhs.int },
                            .float => .{ .float = lhs.float * rhs.float },
                            else => unreachable,
                        },
                        .div => switch (expr.type.type) {
                            .int => .{ .int = @divTrunc(lhs.int, rhs.int) },
                            .float => .{ .float = lhs.float / rhs.float },
                            else => unreachable,
                        },
                        .mod => switch (expr.type.type) {
                            .int => .{ .int = @mod(lhs.int, rhs.int) },
                            .float => .{ .float = @mod(lhs.float, rhs.float) },
                            else => unreachable,
                        },
                        else => unreachable,
                    };

                    return self.fir.addConstantExpr(new_constant);
                }
            },
            .assign => {
                expr.operands[0] = self.traverseExpr(expr.operands[0]);
            },
            .assign_in_array => {
                for (2..expr.operands.len) |idx| {
                    expr.operands[idx] = self.traverseExpr(expr.operands[idx]);
                }
                expr.operands[0] = self.traverseExpr(expr.operands[0]);
            },
            .not => {
                expr.operands[0] = self.traverseExpr(expr.operands[0]);
                const value = self.fir.exprs.items[expr.operands[0]];
                switch (value.op) {
                    .literal => {
                        const truey = self.fir.constants.items[value.operands[0]].isTrue();
                        return self.fir.addExpr(.{ .op = if (truey) .false else .true, .operands = &.{}, .type = .primitive(.bool) });
                    },
                    .false => {
                        return self.fir.addExpr(.{ .op = .true, .operands = &.{}, .type = .primitive(.bool) });
                    },
                    .true => {
                        return self.fir.addExpr(.{ .op = .false, .operands = &.{}, .type = .primitive(.bool) });
                    },
                    else => unreachable,
                }
            },
            .negate => {
                expr.operands[0] = self.traverseExpr(expr.operands[0]);
                const value = self.fir.exprs.items[expr.operands[0]];
                switch (value.op) {
                    .literal => {
                        const prev_value = self.fir.constants.items[value.operands[0]];
                        switch (prev_value) {
                            .int => return self.fir.addConstantExpr(.{ .int = -prev_value.int }),
                            .float => return self.fir.addConstantExpr(.{ .float = -prev_value.float }),
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            .equal, .unequal => {
                expr.operands[0] = self.traverseExpr(expr.operands[0]);
                expr.operands[1] = self.traverseExpr(expr.operands[1]);
                const lhs_result = self.fir.exprs.items[expr.operands[0]];
                const rhs_result = self.fir.exprs.items[expr.operands[1]];
                if (lhs_result.op == .literal and rhs_result.op == .literal) {
                    const lhs = self.fir.constants.items[lhs_result.operands[0]];
                    const rhs = self.fir.constants.items[rhs_result.operands[0]];

                    const result = blk: {
                        const res = lhs.equals(&rhs);
                        break :blk if (expr.op == .unequal) !res else res;
                    };

                    return self.fir.addExpr(.{ .op = if (result) .true else .false, .operands = &.{}, .type = .primitive(.bool) });
                }
            },
            .less => {},
            .less_equal => {},
            .greater => {},
            .greater_equal => {},
            .concat => {},
            .@"and" => {},
            .@"or" => {},
            .index => {},
            .append => {},
            .call => {},
            .builtin_fn => {},
            .array => {},
            .function => {},
        }

        return expr_idx;
    }
};

const std = @import("std");
const assert = std.debug.assert;

const shared = @import("shared");
const oom = shared.oom;
const FlowValue = shared.definitions.FlowValue;

const FIR = @import("../ir/FIR.zig");
