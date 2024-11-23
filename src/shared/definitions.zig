pub const Integer = i64;
pub const Float = f64;

pub const ValueType = enum { null, bool, int, float, string };

pub const FlowValue = union(ValueType) {
    null: void,
    bool: bool,
    int: Integer,
    float: Float,
    string: []const u8,

    pub fn equals(self: FlowValue, other: FlowValue) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;

        return switch (self) {
            .int => self.int == other.int,
            .float => self.float == other.float,
            .bool => self.bool == other.bool,
            .null => true,
            .string => @panic("not supported yet"),
        };
    }
};

const std = @import("std");
