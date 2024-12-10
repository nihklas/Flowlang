pub const builtins: std.StaticStringMap(Function) = .initComptime(.{
    .{ "print", print },
    .{ "readline", readline },
});

const impls = struct {
    fn print(_: Allocator, args: []FlowValue) FlowValue {
        stdout.print("{}\n", .{args[0]}) catch panic("IO Error", .{});
        return .null;
    }

    fn readline(gc_alloc: Allocator, args: []FlowValue) FlowValue {
        const prompt = args[0].string;
        stdout.writeAll(prompt) catch panic("IO Error", .{});
        stdout.writeByte('\n') catch panic("IO Error", .{});
        var buf: [1024]u8 = undefined;
        const read = stdin.readUntilDelimiter(&buf, '\n') catch panic("IO Error", .{});

        return .{ .string = gc_alloc.dupe(u8, read) catch oom() };
    }
};

const print: Function = .{
    .arg_count = 1,
    .arg_types = null,
    .ret_type = .null,
    .function = &impls.print,
};

const readline: Function = .{
    .arg_count = 1,
    .arg_types = &.{.string},
    .ret_type = .string,
    .function = &impls.readline,
};

const Function = @import("definitions.zig").BuiltinFunction;
const FlowValue = @import("definitions.zig").FlowValue;
const oom = @import("root.zig").oom;

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const std = @import("std");
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
