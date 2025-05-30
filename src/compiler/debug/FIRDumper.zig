pub fn dump(writer: anytype, fir: *const FIR) WriterError!void {
    if (fir.nodes.items.len == 0) return;

    for (fir.constants.items, 0..) |constant, idx| {
        try writer.print("Constant: <{d}> | {s} = {}\n", .{ idx, @tagName(constant), constant });
    }
    try writer.writeAll("\n");

    try dumpBlock(writer, fir, fir.entry, 0);
}

fn dumpBlock(writer: anytype, fir: *const FIR, start_idx: usize, depth: usize) WriterError!void {
    var maybe_node_idx: ?usize = start_idx;
    while (maybe_node_idx) |node_idx| {
        try dumpStmt(writer, fir, node_idx, depth);
        maybe_node_idx = fir.nodes.items[node_idx].after;
    }
}

fn dumpStmt(writer: anytype, fir: *const FIR, node_idx: usize, depth: usize) WriterError!void {
    for (0..depth) |_| {
        try writer.writeAll("  ");
    }

    const node = fir.nodes.items[node_idx];
    switch (node.kind) {
        .expr => {
            try dumpExpr(writer, fir, node.index);
            try writer.writeAll(";");
        },
        .cond => try dumpCond(writer, fir, node.index, depth),
        .loop => try dumpLoop(writer, fir, node.index, depth),
    }

    try writer.writeAll("\n");
}

fn dumpExpr(writer: anytype, fir: *const FIR, expr_idx: usize) WriterError!void {
    const expr = fir.exprs.items[expr_idx];
    switch (expr.op) {
        .literal => try writer.print("<{d}>", .{expr.operands[0]}),
        .true => try writer.writeAll("true"),
        .false => try writer.writeAll("false"),
        .null => try writer.writeAll("null"),
        .equal, .unequal, .less, .less_equal, .greater, .greater_equal, .add, .sub, .div, .mul, .mod, .concat => {
            try dumpExpr(writer, fir, expr.operands[0]);
            try writer.writeAll(" ");
            try dumpExpr(writer, fir, expr.operands[1]);
            try writer.print(" {s}", .{@tagName(expr.op)});
        },
    }
}

fn dumpCond(writer: anytype, fir: *const FIR, cond_idx: usize, depth: usize) WriterError!void {
    const cond = fir.conds.items[cond_idx];

    try writer.writeAll("if ");
    try dumpExpr(writer, fir, cond.condition);
    try writer.writeAll(" {\n");
    try dumpBlock(writer, fir, cond.true, depth + 1);
    try printDepth(writer, depth);
    try writer.writeAll("}");

    if (cond.false) |false_idx| {
        try writer.writeAll(" else {\n");
        try dumpBlock(writer, fir, false_idx, depth + 1);
        try printDepth(writer, depth);
        try writer.writeAll("}");
    }
}

fn dumpLoop(writer: anytype, fir: *const FIR, loop_idx: usize, depth: usize) WriterError!void {
    const loop = fir.loops.items[loop_idx];

    try writer.writeAll("loop ");
    try dumpExpr(writer, fir, loop.condition);
    try writer.writeAll(" {\n");
    try dumpBlock(writer, fir, loop.body, depth + 1);
    try printDepth(writer, depth);
    try writer.writeAll("}");
}

fn printDepth(writer: anytype, depth: usize) WriterError!void {
    for (0..depth) |_| {
        try writer.writeAll("  ");
    }
}

const FIR = @import("../ir/FIR.zig");
const std = @import("std");
const WriterError = std.io.AnyWriter.Error;
