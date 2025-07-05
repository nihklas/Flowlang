pub var source: []const u8 = "";
pub var colored_output: bool = true;

pub fn reportError(token: Token, comptime fmt: []const u8, args: anytype) void {
    reportErrorFailing(token, fmt, args) catch {};
}

fn reportErrorFailing(token: Token, comptime fmt: []const u8, args: anytype) !void {
    if (colored_output) {
        return reportErrorFailingColored(token, fmt, args);
    } else {
        return reportErrorFailingBasic(token, fmt, args);
    }
}

fn reportErrorFailingColored(token: Token, comptime fmt: []const u8, args: anytype) !void {
    try stderr.writeAll("\x1b[1;31merror:\x1b[0m ");
    try stderr.print(fmt, args);
    try stderr.print(" at {d}:{d}\n", .{ token.line, token.column });

    const line_text = getLineAt(token.line) orelse "";
    try stderr.print("\x1b[34m{d: >4} |\x1b[0m {s}\n", .{ token.line, line_text });
    try stderr.writeAll("       ");

    assert(token.column > 0);
    for (0..token.column - 1) |_| {
        try stderr.writeByte(' ');
    }
    try stderr.writeAll("\x1b[36m^~~~~\x1b[0m Here\n");
    try stderr.writeByte('\n');
}

fn reportErrorFailingBasic(token: Token, comptime fmt: []const u8, args: anytype) !void {
    try stderr.writeAll("error: ");
    try stderr.print(fmt, args);
    try stderr.print(" at {d}:{d}\n", .{ token.line, token.column });

    const line_text = getLineAt(token.line) orelse "";
    try stderr.print("{d: >4} | {s}\n", .{ token.line, line_text });
    try stderr.writeAll("       ");

    assert(token.column > 0);
    for (0..token.column - 1) |_| {
        try stderr.writeByte(' ');
    }
    try stderr.writeAll("^~~~~ Here\n");
    try stderr.writeByte('\n');
}

fn getLineAt(line_num: usize) ?[]const u8 {
    var line_iter = std.mem.splitScalar(u8, source, '\n');
    for (0..line_num - 1) |_| {
        _ = line_iter.next();
    }
    return line_iter.next();
}

const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");
const testing_options = @import("testing_options");
const stderr = if (builtin.is_test and !testing_options.use_stderr) std.io.null_writer else std.io.getStdErr().writer();
const Token = @import("../ir/Token.zig");
