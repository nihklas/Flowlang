pub const builtins: std.StaticStringMap(Function) = .initComptime(.{
    .{ "test", testFunc },
});

const impls = struct {
    fn testFunc(_: []FlowValue) FlowValue {
        std.debug.print("Test\n", .{});

        return .null;
    }
};

const testFunc: Function = .{
    .arg_count = 0,
    .arg_types = &.{},
    .function = &impls.testFunc,
};

const Function = @import("definitions.zig").BuiltinFunction;
const FlowValue = @import("definitions.zig").FlowValue;

const std = @import("std");
