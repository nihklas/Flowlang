pub const betterAdd: BuiltinFunction = .{
    .arg_types = &.{ .int, .int },
    .ret_type = .int,
    .function = &_add,
};

fn _add(_: Allocator, args: []FlowValue) FlowValue {
    return .{ .int = args[0].int + args[1].int };
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const BuiltinFunction = @import("shared").definitions.BuiltinFunction;
const FlowValue = @import("shared").definitions.FlowValue;
