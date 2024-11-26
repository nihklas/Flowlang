pub const Integer = i64;
pub const Float = f64;

pub const ValueType = enum { null, bool, int, float, string };

// TODO: Change Runtime definition to be a packed union/struct
// As we have a statically type language, this is possible and allows us a few performance
// optimisations
// We have to be able to differentiate between garbage collected values and non-gc'ed values
// GC'ed Values:
// - strings
// - arrays (but not necessarily the elements of it)
// - functions/closures
// Non GC'ed Values:
// - null
// - bool
// - int
// - float
pub const FlowValue = union(ValueType) {
    null: void,
    bool: bool,
    int: Integer,
    float: Float,
    string: []const u8,

    pub fn format(self: FlowValue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .null => try writer.writeAll("null"),
            .bool => try writer.print("{}", .{self.bool}),
            .int => try writer.print("{d}", .{self.int}),
            .float => try writer.print("{d}", .{self.float}),
            .string => try writer.print("{s}", .{self.string}),
        }
    }

    pub fn equals(self: FlowValue, other: FlowValue) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;

        return switch (self) {
            .int => self.int == other.int,
            .float => self.float == other.float,
            .bool => self.bool == other.bool,
            .null => true,
            .string => {
                const a = self.string;
                const b = other.string;
                if (a.ptr == b.ptr) return true;
                if (a.len != b.len) return false;

                for (a, b) |el1, el2| {
                    if (el1 != el2) return false;
                }
                return true;
            },
        };
    }

    pub fn isTrue(self: FlowValue) bool {
        return switch (self) {
            .null => false,
            .bool => self.bool,
            .int => self.int != 0,
            .float => self.float != 0,
            .string => self.string.len > 0,
        };
    }
};

const std = @import("std");
