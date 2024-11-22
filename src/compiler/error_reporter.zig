pub var source: []const u8 = "";

pub fn reportError(token: Token, comptime fmt: []const u8, args: anytype) void {
    stderr.print(fmt, args) catch {};
    stderr.print(" at {d}:{d}\n", .{ token.line, token.column }) catch {};
    stderr.print("{d: >4} | {s}\n", .{ token.line, getLineAt(token.line) orelse "" }) catch {};
    stderr.writeAll("       ") catch {};
    for (0..token.column - 1) |_| {
        stderr.writeByte(' ') catch {};
    }
    stderr.writeAll("^~~~~ Here\n") catch {};
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
