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
            .true, .false, .null, .literal => {},
            .add, .sub, .mul, .div, .mod => |tag| {
                assert(expr.type.isPrimitive(.int) or expr.type.isPrimitive(.float));

                expr.operands[0] = self.traverseExpr(expr.operands[0]);
                expr.operands[1] = self.traverseExpr(expr.operands[1]);
                const lhs_result = self.fir.exprs.items[expr.operands[0]];
                const rhs_result = self.fir.exprs.items[expr.operands[1]];
                if (lhs_result.op == .literal and rhs_result.op == .literal) {
                    const lhs = self.fir.constants.items[lhs_result.operands[0]];
                    const rhs = self.fir.constants.items[rhs_result.operands[0]];

                    const new_constant = switch (tag) {
                        .add => switch (expr.type.type) {
                            .int => self.fir.resolveConstant(.{ .int = lhs.int + rhs.int }),
                            .float => self.fir.resolveConstant(.{ .float = lhs.float + rhs.float }),
                            else => unreachable,
                        },
                        .sub => switch (expr.type.type) {
                            .int => self.fir.resolveConstant(.{ .int = lhs.int - rhs.int }),
                            .float => self.fir.resolveConstant(.{ .float = lhs.float - rhs.float }),
                            else => unreachable,
                        },
                        .mul => switch (expr.type.type) {
                            .int => self.fir.resolveConstant(.{ .int = lhs.int * rhs.int }),
                            .float => self.fir.resolveConstant(.{ .float = lhs.float * rhs.float }),
                            else => unreachable,
                        },
                        .div => switch (expr.type.type) {
                            .int => self.fir.resolveConstant(.{ .int = @divTrunc(lhs.int, rhs.int) }),
                            .float => self.fir.resolveConstant(.{ .float = lhs.float / rhs.float }),
                            else => unreachable,
                        },
                        .mod => switch (expr.type.type) {
                            .int => self.fir.resolveConstant(.{ .int = @mod(lhs.int, rhs.int) }),
                            .float => self.fir.resolveConstant(.{ .float = @mod(lhs.float, rhs.float) }),
                            else => unreachable,
                        },
                        else => unreachable,
                    };

                    const operands = self.fir.arena().alloc(usize, 1) catch oom();
                    operands[0] = new_constant;
                    self.fir.exprs.append(self.fir.alloc, .{ .op = .literal, .type = expr.type, .operands = operands }) catch oom();
                    return self.fir.exprs.items.len - 1;
                }
            },
            .variable => {},
            .assign => {},
            .assign_in_array => {},
            .not => {},
            .negate => {},
            .equal => {},
            .unequal => {},
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

const FIR = @import("../ir/FIR.zig");
