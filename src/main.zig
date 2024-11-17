const std = @import("std");

pub fn main() !void {
    std.debug.print("Hello World\n", .{});
}

test "for" {
    const str1: []const u8 = "Hallo";
    const str2: []const u8 = "Welt!";

    for (str1, str2) |char1, char2| {
        std.debug.print("Left: {c}, Right: {c}\n", .{ char1, char2 });
    }
}
