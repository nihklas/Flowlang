pub var source: []const u8 = "";

pub fn reportError(token: Token, comptime fmt: []const u8, args: anytype) void {
    reportErrorFailing(token, fmt, args) catch {};
}

fn reportErrorFailing(token: Token, comptime fmt: []const u8, args: anytype) !void {
    try stderr.writeAll("\x1b[1;31merror:\x1b[0m ");
    try stderr.print(fmt, args);
    try stderr.print(" at {d}:{d}\n", .{ token.line, token.column });
    try stderr.print("\x1b[34m{d: >4} |\x1b[0m {s}\n", .{ token.line, getLineAt(token.line) orelse "" });
    try stderr.writeAll("       ");
    for (0..token.column - 1) |_| {
        try stderr.writeByte(' ');
    }
    try stderr.writeAll("\x1b[36m^~~~~\x1b[0m Here\n\n");
}

fn getLineAt(line_num: usize) ?[]const u8 {
    var line_iter = std.mem.splitScalar(u8, source, '\n');
    for (0..line_num - 1) |_| {
        _ = line_iter.next();
    }
    return line_iter.next();
}

const std = @import("std");
const builtin = @import("builtin");
const testing_options = @import("testing_options");
const stderr = if (builtin.is_test and !testing_options.use_stderr) std.io.null_writer else std.io.getStdErr().writer();
const Token = @import("Token.zig");
