pub const time: BuiltinFunction = .{
    .arg_types = &.{},
    .ret_type = .{ .type = .int, .order = 0 },
    .function = &_time,
};

fn _time(_: Allocator, _: []FlowValue) FlowValue {
    return .{ .int = std.time.milliTimestamp() };
}

pub const len: BuiltinFunction = .{
    .arg_types = &.{.null},
    .ret_type = .{ .type = .int, .order = 0 },
    .function = &_len,
};

fn _len(_: Allocator, args: []FlowValue) FlowValue {
    assert(args.len == 1);
    return switch (args[0]) {
        .array => |array| .{ .int = @intCast(array.len) },
        .string => |string| .{ .int = @intCast(string.len) },
        else => panic("Invalid argument type passed to len(), expected string or array, got '{s}'", .{@tagName(args[0])}),
    };
}

pub const exit: BuiltinFunction = .{
    .arg_types = &.{.primitive(.int)},
    .ret_type = .null,
    .function = &_exit,
};

fn _exit(_: Allocator, args: []FlowValue) FlowValue {
    assert(args.len == 1);
    assert(args[0] == .int);
    assert(args[0].int < std.math.maxInt(u8));
    assert(args[0].int >= 0);

    std.process.exit(@intCast(args[0].int));

    return .null;
}

const BuiltinFunction = @import("shared").definitions.BuiltinFunction;
const FlowValue = @import("shared").definitions.FlowValue;
const oom = @import("shared").oom;

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
