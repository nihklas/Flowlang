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
pub const FlowFunctionType = struct {
    arg_types: []const FlowType,
    ret_type: FlowType,
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
    function_type: ?*FlowFunctionType = null,

    pub const @"null": FlowType = .{ .type = .null, .order = 0 };

    /// Only available for actual primitive values, asserts that the parameter is not a function
    /// type
    /// To create a Function Type, use `.function()`
    pub fn primitive(primitive_type: FlowPrimitive) FlowType {
        std.debug.assert(primitive_type != .function and primitive_type != .builtin_fn);

        return .{ .type = primitive_type, .order = 0 };
    }

    pub fn function(alloc: std.mem.Allocator, ret_type: FlowType, arg_types: []const FlowType) !FlowType {
        const func = try alloc.create(FlowFunctionType);
        func.ret_type = ret_type;
        func.arg_types = arg_types;
        return .{ .order = 0, .type = .function, .function_type = func };
    }

    pub fn isNull(self: *const FlowType) bool {
        return self.type == .null;
    }

    /// Asserts primitive_type is not a function type
    pub fn isPrimitive(self: *const FlowType, primitive_type: FlowPrimitive) bool {
        // Functions are not primitives
        std.debug.assert(primitive_type != .function and primitive_type != .builtin_fn);
        if (self.type == .function or self.type == .builtin_fn) return false;

        return self.type == primitive_type and self.order == 0;
    }

    pub fn equals(self: *const FlowType, other: *const FlowType) bool {
        // NOTE: null is "equal" to any other type in the sense of type checking
        if (self.isPrimitive(.null)) return true;
        if (other.isPrimitive(.null)) return true;

        if (self.order != other.order) return false;
        if (self.type != other.type) return false;

        if (self.type == .function) {
            std.debug.assert(self.function_type != null);
            std.debug.assert(other.function_type != null);

            const self_func = self.function_type.?;
            const other_func = other.function_type.?;

            if (!self_func.ret_type.equals(&other_func.ret_type)) return false;
            if (self_func.arg_types.len != other_func.arg_types.len) return false;

            for (self_func.arg_types, other_func.arg_types) |a, b| {
                if (!a.equals(&b)) return false;
            }
        }

        return true;
    }

    pub fn format(self: FlowType, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (0..self.order) |_| {
            try writer.writeAll("[]");
        }
        if (self.type == .function) {
            std.debug.assert(self.function_type != null);

            try writer.writeAll("func(");
            for (self.function_type.?.arg_types, 0..) |arg, i| {
                if (i > 0) {
                    try writer.writeAll(", ");
                }
                try writer.print("{}", .{arg});
            }
            try writer.writeAll(")");

            if (!self.function_type.?.ret_type.isNull()) {
                try writer.print(" {}", .{self.function_type.?.ret_type});
            }

            return;
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
    array: *FlowArray,

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

    pub fn clone(self: *const FlowValue, gc: std.mem.Allocator) FlowValue {
        return switch (self.*) {
            .null, .bool, .int, .float, .builtin_fn, .function => self.*,
            .string => .{ .string = gc.dupe(u8, self.string) catch oom() },
            .array => {
                const new_arr = gc.alloc(FlowValue, self.array.cap) catch oom();
                @memcpy(new_arr[0..self.array.len], self.array.items[0..self.array.len]);
                const array = gc.create(FlowArray) catch oom();
                array.cap = self.array.cap;
                array.len = self.array.len;
                array.items = new_arr.ptr;
                return .{ .array = array };
            },
        };
    }

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
};

const std = @import("std");
const oom = @import("root.zig").oom;
