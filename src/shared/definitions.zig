pub const Integer = i64;
pub const Float = f64;

pub const BuiltinFunction = struct {
    arg_count: u8,
    /// If this is set to null, Semantic analysing won't perform compile time type checking
    arg_types: ?[]const FlowType,
    function: *const fn ([]FlowValue) FlowValue,
};

pub const FlowFunction = struct {
    name: []const u8,
    arg_count: u8,
    start_ip: usize,
};

pub const FlowType = enum {
    null,
    bool,
    int,
    float,
    string,
    builtin_fn,
    function,
};

pub const FlowValue = union(FlowType) {
    null: void,
    bool: bool,
    int: Integer,
    float: Float,
    string: []const u8,
    builtin_fn: BuiltinFunction,
    function: FlowFunction,

    pub fn format(self: FlowValue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .null => try writer.writeAll("null"),
            .bool => try writer.print("{}", .{self.bool}),
            .int => try writer.print("{d}", .{self.int}),
            .float => try writer.print("{d}", .{self.float}),
            .string => try writer.print("{s}", .{self.string}),
            .function => try writer.print("<fn {s}>", .{self.function.name}),
            .builtin_fn => try writer.print("<builtin fn>", .{}),
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
            .function => self.function.start_ip == other.function.start_ip,
            .builtin_fn => self.builtin_fn.function == other.builtin_fn.function,
        };
    }

    pub fn isTrue(self: FlowValue) bool {
        return switch (self) {
            .null => false,
            .bool => self.bool,
            .int => self.int != 0,
            .float => self.float != 0,
            .string => self.string.len > 0,
            .builtin_fn, .function => true,
        };
    }
};

const std = @import("std");
