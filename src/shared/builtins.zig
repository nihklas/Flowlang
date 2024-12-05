pub const builtins: std.StaticStringMap(Function) = .initComptime(.{
    .{ "print", print },
});

const impls = struct {
    fn print(args: []FlowValue) FlowValue {
        stdout.print("{}\n", .{args[0]}) catch @panic("IO Error");
        return .null;
    }
};

const print: Function = .{
    .arg_count = 1,
    .arg_types = null,
    .function = &impls.print,
};

const Function = @import("definitions.zig").BuiltinFunction;
const FlowValue = @import("definitions.zig").FlowValue;

const stdout = std.io.getStdOut().writer();

const std = @import("std");
