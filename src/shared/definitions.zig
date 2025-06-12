pub const Integer = i64;
pub const Float = f64;

pub const BuiltinFunction = struct {
    /// The position with type of .null will not be checked and has to be checked at runtime
    /// The length of this slice defines how many arguments are expected. This is comptime checked
    arg_types: []const FlowType,
    ret_type: FlowType,
    function: *const fn (std.mem.Allocator, []FlowValue) FlowValue,
};

pub const FlowFunction = struct {
    arg_count: u8,
    start_ip: usize,
};

pub const FlowArray = struct {
    type: FlowPrimitive,
    order: u8,
    items: []FlowValue,
};

pub const FlowPrimitive = enum {
    null,
    bool,
    int,
    float,
    string,
    builtin_fn,
    function,
};

pub const FlowType = enum {
    null,
    bool,
    int,
    float,
    string,
    builtin_fn,
    function,
    array,
};

pub const FlowValue = union(FlowType) {
    null: void,
    bool: bool,
    int: Integer,
    float: Float,
    string: []const u8,
    builtin_fn: BuiltinFunction,
    function: FlowFunction,
    array: FlowArray,

    pub fn format(self: FlowValue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .null => try writer.writeAll("null"),
            .bool => try writer.print("{}", .{self.bool}),
            .int => try writer.print("{d}", .{self.int}),
            .float => try writer.print("{d}", .{self.float}),
            .string => try writer.print("{s}", .{self.string}),
            .function => try writer.writeAll("<fn>"),
            .builtin_fn => try writer.writeAll("<builtin fn>"),
            .array => try writer.writeAll("<array>"),
        }
    }

    pub fn equals(self: FlowValue, other: FlowValue) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;

        return switch (self) {
            .null => true,
            .int => self.int == other.int,
            .float => self.float == other.float,
            .bool => self.bool == other.bool,
            .function => self.function.start_ip == other.function.start_ip,
            .builtin_fn => self.builtin_fn.function == other.builtin_fn.function,
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
            .array => {
                const a = self.array;
                const b = other.array;

                if (a.type != b.type) return false;
                if (a.order != b.order) return false;
                if (a.items.len != b.items.len) return false;

                for (a.items, b.items) |ia, ib| {
                    if (!ia.equals(ib)) return false;
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
            .array => self.array.items.len > 0,
            .builtin_fn, .function => true,
        };
    }
};

const std = @import("std");
