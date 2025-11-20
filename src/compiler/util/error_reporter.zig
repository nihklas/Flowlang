pub var source: []const u8 = "";
pub var colored_output: bool = true;

pub fn reportError(token: Token, comptime fmt: []const u8, args: anytype) void {
    reportErrorFailing(token, fmt, args) catch {};

    var stderr = &stderr_container.interface;
    stderr.flush() catch unreachable;
}

fn reportErrorFailing(token: Token, comptime fmt: []const u8, args: anytype) !void {
    if (colored_output) {
        return reportErrorFailingColored(token, fmt, args);
    } else {
        return reportErrorFailingBasic(token, fmt, args);
    }
}

fn reportErrorFailingColored(token: Token, comptime fmt: []const u8, args: anytype) !void {
    writeError("\x1b[1;31merror:\x1b[0m ", .{});
    writeError(fmt, args);
    writeError(" at {d}:{d}\n", .{ token.line, token.column });

    const line_text = getLineAt(token.line) orelse "";
    writeError("\x1b[34m{d: >4} |\x1b[0m {s}\n", .{ token.line, line_text });
    writeError("       ", .{});

    assert(token.column > 0);
    for (0..token.column - 1) |_| {
        writeError(" ", .{});
    }
    writeError("\x1b[36m^~~~~\x1b[0m Here\n", .{});
    writeError("\n", .{});
}

fn reportErrorFailingBasic(token: Token, comptime fmt: []const u8, args: anytype) !void {
    writeError("error: ", .{});
    writeError(fmt, args);
    writeError(" at {d}:{d}\n", .{ token.line, token.column });

    const line_text = getLineAt(token.line) orelse "";
    writeError("{d: >4} | {s}\n", .{ token.line, line_text });
    writeError("       ", .{});

    assert(token.column > 0);
    for (0..token.column - 1) |_| {
        writeError(" ", .{});
    }
    writeError("^~~~~ Here\n", .{});
    writeError("\n", .{});
}

fn getLineAt(line_num: usize) ?[]const u8 {
    var line_iter = std.mem.splitScalar(u8, source, '\n');
    for (0..line_num - 1) |_| {
        _ = line_iter.next();
    }
    return line_iter.next();
}

fn writeError(comptime fmt: []const u8, args: anytype) void {
    if (builtin.is_test and !testing_options.use_stderr) {
        return;
    }

    var stderr = &stderr_container.interface;
    stderr.print(fmt, args) catch unreachable;
}

var stderr_buf: [1024]u8 = undefined;
var stderr_container = std.fs.File.stderr().writer(&stderr_buf);
const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");
const testing_options = @import("testing_options");
const Token = @import("../ir/Token.zig");
