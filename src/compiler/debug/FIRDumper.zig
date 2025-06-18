pub fn dump(writer: anytype, fir: *const FIR) WriterError!void {
    if (fir.nodes.items.len == 0) return;

    for (fir.constants.items, 0..) |constant, idx| {
        try writer.print("Constant: <{d}> | {s} = {}\n", .{ idx, @tagName(constant), constant });
    }
    try writer.writeAll("\n");

    var global_counter: usize = 0;
    for (fir.globals.items) |global| {
        if (!global.type.isPrimitive(.function)) continue;

        const func = fir.functions.items[global.extra_idx];
        try writer.print("${d} = func ({d}) {{\n", .{ global_counter, func.param_count });
        if (func.body != FIR.uninitialized_entry) {
            try dumpBlock(writer, fir, func.body, 1);
        }
        try writer.writeAll("}");
        try writer.writeAll("\n");

        global_counter += 1;
    }

    if (global_counter > 0) {
        try writer.writeAll("\n");
    }

    if (fir.entry == FIR.uninitialized_entry) return;

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
        .@"return" => {
            try writer.writeAll("return ");
            try dumpExpr(writer, fir, node.index);
            try writer.writeAll(";");
        },
        .pop => try writer.writeAll("();"),
        .@"break" => try writer.writeAll("break;"),
        .@"continue" => try writer.writeAll("continue;"),
        .cond => try dumpCond(writer, fir, node.index, depth),
        .loop => try dumpLoop(writer, fir, node.index, depth),
        .global => try dumpGlobal(writer, fir, node.index),
        .local => try dumpLocal(writer, fir, node.index, depth),
    }

    try writer.writeAll("\n");
}

fn dumpExpr(writer: anytype, fir: *const FIR, expr_idx: usize) WriterError!void {
    const expr = fir.exprs.items[expr_idx];
    switch (expr.op) {
        .builtin_fn => try writer.print("{}", .{fir.constants.items[expr.operands[0]]}),
        .literal => try writer.print("<{d}>", .{expr.operands[0]}),
        .true => try writer.writeAll("true"),
        .false => try writer.writeAll("false"),
        .null => try writer.writeAll("null"),
        .equal, .unequal, .less, .less_equal, .greater, .greater_equal, .add, .sub, .div, .mul, .mod, .concat, .@"and", .@"or" => {
            try dumpExpr(writer, fir, expr.operands[0]);
            try writer.writeAll(" ");
            try dumpExpr(writer, fir, expr.operands[1]);
            try writer.print(" {s}", .{@tagName(expr.op)});
        },
        .not, .negate => {
            try dumpExpr(writer, fir, expr.operands[0]);
            try writer.print(" {s}", .{@tagName(expr.op)});
        },
        .global => try writer.print("${d}", .{expr.operands[0]}),
        .local => try writer.print("%{d}", .{expr.operands[0]}),
        .assign_global, .assign_local => {
            if (expr.op == .assign_global) {
                try writer.print("${d} = ", .{expr.operands[1]});
            } else {
                try writer.print("%{d} = ", .{expr.operands[1]});
            }

            try dumpExpr(writer, fir, expr.operands[0]);
        },
        .assign_in_array_global, .assign_in_array_local => {
            if (expr.op == .assign_in_array_global) {
                try writer.print("${d}", .{expr.operands[1]});
            } else {
                try writer.print("%{d}", .{expr.operands[1]});
            }

            for (2..expr.operands.len) |idx| {
                try writer.writeAll("[");
                try dumpExpr(writer, fir, expr.operands[idx]);
                try writer.writeAll("]");
            }
            try writer.writeAll(" = ");
            try dumpExpr(writer, fir, expr.operands[0]);
        },
        .call => {
            try writer.writeAll("call ");
            try dumpExpr(writer, fir, expr.operands[0]);
            if (expr.operands.len > 1) {
                try writer.writeAll(" with (");
                for (1..expr.operands.len) |idx| {
                    if (idx > 1) {
                        try writer.writeAll(", ");
                    }
                    try dumpExpr(writer, fir, expr.operands[idx]);
                }
                try writer.writeAll(")");
            }
        },
        .array => {
            try writer.writeAll("[");
            for (expr.operands, 0..) |operand, i| {
                try dumpExpr(writer, fir, operand);
                if (i < expr.operands.len - 1) {
                    try writer.writeAll(", ");
                }
            }
            try writer.writeAll("]");
        },
        .index => {
            try dumpExpr(writer, fir, expr.operands[0]);
            try writer.writeAll("[");
            try dumpExpr(writer, fir, expr.operands[1]);
            try writer.writeAll("]");
        },
        .append => {
            try dumpExpr(writer, fir, expr.operands[0]);
            try writer.writeAll("[] = ");
            try dumpExpr(writer, fir, expr.operands[1]);
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
    if (loop.inc) |inc| {
        try writer.writeAll(" -> (");
        try dumpExpr(writer, fir, inc);
        try writer.writeAll(")");
    }
    try writer.writeAll(" {\n");
    try dumpBlock(writer, fir, loop.body, depth + 1);
    try printDepth(writer, depth);
    try writer.writeAll("}");
}

fn dumpGlobal(writer: anytype, fir: *const FIR, var_idx: usize) WriterError!void {
    const variable = fir.globals.items[var_idx];

    if (variable.type.isPrimitive(.function)) return;

    try writer.print("${d} = ", .{var_idx});
    if (variable.expr) |expr_idx| {
        try dumpExpr(writer, fir, expr_idx);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(";");
}

fn dumpLocal(writer: anytype, fir: *const FIR, var_idx: usize, depth: usize) WriterError!void {
    const variable = fir.locals.items[var_idx];

    try writer.print("%{d} = ", .{variable.stack_idx});

    if (variable.type.isPrimitive(.function)) {
        const func = fir.functions.items[variable.extra_idx];
        try writer.print("func ({d}) {{\n", .{func.param_count});
        try dumpBlock(writer, fir, func.body, depth + 1);
        try printDepth(writer, depth);
        try writer.writeAll("}");
    } else if (variable.expr) |expr_idx| {
        try dumpExpr(writer, fir, expr_idx);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(";");
}

fn printDepth(writer: anytype, depth: usize) WriterError!void {
    for (0..depth) |_| {
        try writer.writeAll("  ");
    }
}

const FIR = @import("../ir/FIR.zig");
const std = @import("std");
const WriterError = std.io.AnyWriter.Error;
