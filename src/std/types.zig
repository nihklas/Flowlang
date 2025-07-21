pub const typeOf: BuiltinFunction = .{
    .arg_types = &.{.null},
    .ret_type = .{ .type = .string, .order = 0 },
    .function = &_typeOf,
};

fn _typeOf(_: Allocator, args: []FlowValue) FlowValue {
    assert(args.len == 1);
    return .{ .string = @tagName(args[0]) };
}

pub const @"bool": BuiltinFunction = .{
    .arg_types = &.{.null},
    .ret_type = .{ .type = .bool, .order = 0 },
    .function = &_bool,
};

fn _bool(_: Allocator, args: []FlowValue) FlowValue {
    assert(args.len == 1);
    return .{ .bool = args[0].isTrue() };
}

pub const int: BuiltinFunction = .{
    .arg_types = &.{.null},
    .ret_type = .{ .type = .int, .order = 0 },
    .function = &_int,
};

fn _int(_: Allocator, args: []FlowValue) FlowValue {
    assert(args.len == 1);
    const result: Integer = switch (args[0]) {
        .null => 0,
        .bool => |b| @intFromBool(b),
        .int => |i| i,
        .float => |f| @intFromFloat(f),
        .string => |s| std.fmt.parseInt(Integer, s, 10) catch return .null,
        .builtin_fn, .function, .array => panic("Could not convert {s} to int", .{@tagName(args[0])}),
    };

    return .{ .int = result };
}

pub const float: BuiltinFunction = .{
    .arg_types = &.{.null},
    .ret_type = .{ .type = .float, .order = 0 },
    .function = &_float,
};

fn _float(_: Allocator, args: []FlowValue) FlowValue {
    assert(args.len == 1);
    const result: Float = switch (args[0]) {
        .null => 0.0,
        .bool => |b| if (b) 1.0 else 0.0,
        .int => |i| @floatFromInt(i),
        .float => |f| f,
        .string => |s| std.fmt.parseFloat(Float, s) catch return .null,
        .builtin_fn, .function, .array => panic("Could not convert {s} to float", .{@tagName(args[0])}),
    };

    return .{ .float = result };
}

pub const string: BuiltinFunction = .{
    .arg_types = &.{.null},
    .ret_type = .{ .type = .string, .order = 0 },
    .function = &_string,
};

fn _string(gc: Allocator, args: []FlowValue) FlowValue {
    assert(args.len == 1);
    const str = std.fmt.allocPrint(gc, "{}", .{args[0]}) catch oom();
    return .{ .string = str };
}

const BuiltinFunction = @import("shared").definitions.BuiltinFunction;
const FlowValue = @import("shared").definitions.FlowValue;
const Integer = @import("shared").definitions.Integer;
const Float = @import("shared").definitions.Float;
const oom = @import("shared").oom;

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
