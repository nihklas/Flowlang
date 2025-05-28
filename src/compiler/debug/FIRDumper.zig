pub fn dump(writer: anytype, fir: *const FIR) !void {
    if (fir.nodes.items.len == 0) return;

    for (fir.constants.items, 0..) |constant, idx| {
        try writer.print("Constant: <{d}> | {s} = {}\n", .{ idx, @tagName(constant), constant });
    }
    try writer.writeAll("\n");

    var maybe_node_idx: ?usize = 0;
    while (maybe_node_idx) |node_idx| {
        const node = fir.nodes.items[node_idx];
        defer maybe_node_idx = node.after;

        switch (node.kind) {
            .expr => {
                try dumpExpr(writer, fir, node.index);
            },
        }
        try writer.writeAll(";\n");
    }
}

fn dumpExpr(writer: anytype, fir: *const FIR, expr_idx: usize) !void {
    const expr = fir.exprs.items[expr_idx];
    try writer.writeAll("(");
    switch (expr.op) {
        .literal => try writer.print("<{d}>", .{expr.operands[0]}),
        .true => try writer.writeAll("true"),
        .false => try writer.writeAll("false"),
        .null => try writer.writeAll("null"),
        .equal, .unequal, .less, .less_equal, .greater, .greater_equal, .add, .sub, .div, .mul, .mod, .concat => {
            try dumpExpr(writer, fir, expr.operands[0]);
            try writer.print(" {s} ", .{@tagName(expr.op)});
            try dumpExpr(writer, fir, expr.operands[1]);
        },
        // else => std.debug.panic("'{s}' is not yet supported in FIRDumper", .{@tagName(expr.op)}),
    }
    try writer.print("|{s})", .{@tagName(expr.type)});
}

const FIR = @import("../ir/FIR.zig");
const std = @import("std");
