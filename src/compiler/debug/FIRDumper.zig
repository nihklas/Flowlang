pub fn dump(writer: anytype, fir: *const FIR) !void {
    if (fir.nodes.items.len == 0) return;

    for (fir.constants.items, 0..) |constant, idx| {
        try writer.print("Constant: <{d}> = {}\n", .{ idx, constant });
    }
    try writer.writeAll("\n");

    for (fir.nodes.items) |node| {
        switch (node.kind) {
            .expr => {
                try dumpExpr(writer, fir, node.index);
            },
            .cond => @panic("'cond' is not yet supported in FIRDumper"),
        }
        try writer.writeAll(";\n");
    }
}

fn dumpExpr(writer: anytype, fir: *const FIR, expr_idx: usize) !void {
    const expr = fir.exprs.items[expr_idx];
    switch (expr.op) {
        .literal => try writer.print("<{d}>", .{expr.operands[0]}),
        .equal, .unequal, .less, .less_equal, .greater, .greater_equal, .add, .sub, .div, .mul, .mod, .concat => {
            try dumpExpr(writer, fir, expr.operands[0]);
            try writer.print(" {s} ", .{@tagName(expr.op)});
            try dumpExpr(writer, fir, expr.operands[1]);
        },
        // else => std.debug.panic("'{s}' is not yet supported in FIRDumper", .{@tagName(expr.op)}),
    }
}

const FIR = @import("../ir/FIR.zig");
const std = @import("std");
