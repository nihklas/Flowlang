pub const Value = union(enum) {
    null: void,
    bool: bool,
    int: definitions.Integer,
    float: definitions.Float,

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .null => try writer.writeAll("null"),
            .bool => try writer.print("{}", .{self.bool}),
            .int => try writer.print("{d}", .{self.int}),
            .float => try writer.print("{d}", .{self.float}),
        }
    }

    pub fn equals(self: Value, other: Value) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;

        return switch (self) {
            .int => self.int == other.int,
            .float => self.float == other.float,
            .bool => self.bool == other.bool,
            .null => true,
        };
    }

    pub fn isTrue(self: Value) bool {
        return switch (self) {
            .null => false,
            .bool => self.bool,
            .int => self.int != 0,
            .float => self.float != 0,
        };
    }
};

const definitions = @import("shared").definitions;

const std = @import("std");
