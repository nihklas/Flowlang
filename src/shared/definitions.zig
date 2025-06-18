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
    items: [*]FlowValue,
    cap: usize,
    len: usize,
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

pub const FlowType = struct {
    order: u8,
    type: FlowPrimitive,

    pub const @"null": FlowType = .{ .type = .null, .order = 0 };

    pub fn primitive(primitive_type: FlowPrimitive) FlowType {
        return .{ .type = primitive_type, .order = 0 };
    }

    pub fn isNull(self: *const FlowType) bool {
        return self.type == .null;
    }

    pub fn isPrimitive(self: *const FlowType, primitive_type: FlowPrimitive) bool {
        return self.type == primitive_type and self.order == 0;
    }

    pub fn equals(self: *const FlowType, other: *const FlowType) bool {
        // a null primitive is "equal" to any other type in the sense of type checking
        if (self.isPrimitive(.null)) return true;
        if (other.isPrimitive(.null)) return true;

        if (self.order != other.order) return false;
        return self.type == .null or other.type == .null or self.type == other.type;
    }

    pub fn format(self: FlowType, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (0..self.order) |_| {
            try writer.writeAll("[]");
        }
        try writer.print("{s}", .{@tagName(self.type)});
    }
};

pub const FlowValue = union(enum) {
    null: void,
    bool: bool,
    int: Integer,
    float: Float,
    string: []const u8,
    builtin_fn: BuiltinFunction,
    function: FlowFunction,
    array: FlowArray,

    pub fn getType(self: *const FlowValue) FlowType {
        return switch (self.*) {
            .null => .null,
            .bool => .primitive(.bool),
            .int => .primitive(.int),
            .float => .primitive(.float),
            .string => .primitive(.string),
            .builtin_fn => .primitive(.builtin_fn),
            .function => .primitive(.function),
            .array => {
                if (self.array.len == 0) return .{ .type = .null, .order = 1 };
                const item_type = self.array.items[0].getType();
                return .{ .type = item_type.type, .order = item_type.order + 1 };
            },
        };
    }

    pub fn format(self: FlowValue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .null => try writer.writeAll("null"),
            .bool => try writer.print("{}", .{self.bool}),
            .int => try writer.print("{d}", .{self.int}),
            .float => try writer.print("{d}", .{self.float}),
            .string => try writer.print("{s}", .{self.string}),
            .function => try writer.writeAll("<fn>"),
            .builtin_fn => try writer.writeAll("<builtin fn>"),
            .array => {
                try writer.writeAll("[");
                for (self.array.items[0..self.array.len], 0..) |item, i| {
                    try writer.print("{}", .{item});
                    if (i < self.array.len - 1) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll("]");
            },
        }
    }

    pub fn equals(self: *const FlowValue, other: *const FlowValue) bool {
        if (std.meta.activeTag(self.*) != std.meta.activeTag(other.*)) return false;

        return switch (self.*) {
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

                if (a.len != b.len) return false;

                for (a.items[0..a.len], b.items[0..b.len]) |ia, ib| {
                    if (!ia.equals(&ib)) return false;
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
            .array => self.array.len > 0,
            .builtin_fn, .function => true,
        };
    }
};

const std = @import("std");
