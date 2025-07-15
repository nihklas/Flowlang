pub fn foldConstants(fir: *FIR) void {
    var folder: Folder = .{ .fir = fir };
    folder.traverseBlock(fir.entry);
}

const Folder = struct {
    fir: *FIR,

    fn traverseBlock(self: *Folder, start_idx: usize) void {
        if (start_idx == FIR.uninitialized_entry) return;

        var maybe_node_idx: ?usize = start_idx;
        while (maybe_node_idx) |node_idx| {
            self.traverseStmt(node_idx);
            maybe_node_idx = self.fir.nodes.items[node_idx].after;
        }
    }

    fn traverseStmt(self: *Folder, node_idx: usize) void {
        const node = &self.fir.nodes.items[node_idx];
        switch (node.kind) {
            .pop => {},
            .@"break" => {},
            .@"continue" => {},
            .expr => {
                node.index = self.traverseExpr(node.index);
            },
            .@"return" => {
                // TODO: traverse Expr
            },
            .cond => {
                // TODO: Traverse conditional
            },
            .loop => {
                // TODO: Traverse loop
            },
            .variable => {
                // TODO: travers variable
            },
        }
    }

    fn traverseExpr(self: *Folder, expr_idx: usize) usize {
        const expr = self.fir.exprs.items[expr_idx];
        switch (expr.op) {
            .true, .false, .null, .literal => {},
            .add => {
                assert(expr.type.isPrimitive(.int) or expr.type.isPrimitive(.float));

                expr.operands[0] = self.traverseExpr(expr.operands[0]);
                expr.operands[1] = self.traverseExpr(expr.operands[1]);
                const lhs_result = self.fir.exprs.items[expr.operands[0]];
                const rhs_result = self.fir.exprs.items[expr.operands[1]];
                if (lhs_result.op == .literal and rhs_result.op == .literal) {
                    const lhs = self.fir.constants.items[lhs_result.operands[0]];
                    const rhs = self.fir.constants.items[rhs_result.operands[0]];
                    const new_constant: usize = switch (expr.type.type) {
                        .int => self.fir.resolveConstant(.{ .int = lhs.int + rhs.int }),
                        .float => self.fir.resolveConstant(.{ .float = lhs.float + rhs.float }),
                        else => unreachable,
                    };

                    const operands = self.fir.arena().alloc(usize, 1) catch oom();
                    operands[0] = new_constant;
                    self.fir.exprs.append(self.fir.alloc, .{ .op = .literal, .type = expr.type, .operands = operands }) catch oom();
                    return self.fir.exprs.items.len - 1;
                }
            },
            .sub => {},
            .div => {},
            .mul => {},
            .mod => {},
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
