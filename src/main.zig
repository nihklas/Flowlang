const std = @import("std");
const Token = @import("Token.zig");

pub fn main() !void {
    for (Token.ReservedKeywords.keys(), Token.ReservedKeywords.values()) |key, value| {
        std.debug.print("Reserved Keyword: '{s}' maps to {}\n", .{ key, value });
    }
}

test "Token" {
    _ = @import("Token.zig");
}
