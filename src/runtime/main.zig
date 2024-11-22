const std = @import("std");
const code = @embedFile("input");

pub fn main() !void {
    std.debug.print("Hello From VM\n", .{});
    std.debug.print("\nBytecode:\n\n{any}\n", .{code});
}
