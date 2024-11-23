pub const Value = union(enum) {
    null: void,
    boolean: bool,
    integer: definitions.Integer,
    float: definitions.Float,

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .null => try writer.writeAll("null"),
            .boolean => try writer.print("{}", .{self.boolean}),
            .integer => try writer.print("{d}", .{self.integer}),
            .float => try writer.print("{d}", .{self.float}),
        }
    }

    pub fn isTrue(self: Value) bool {
        return switch (self) {
            .null => false,
            .boolean => self.boolean,
            .integer => self.integer != 0,
            .float => self.float != 0,
        };
    }
};

const definitions = @import("shared").definitions;

const std = @import("std");
