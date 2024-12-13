pub const builtins: std.StaticStringMap(Function) = .initComptime(.{
    .{ "print", print },
    .{ "readline", readline },
    .{ "readfile", readfile },
    .{ "writefile", writefile },
    .{ "time", time },
});

const impls = struct {
    fn print(_: Allocator, args: []FlowValue) FlowValue {
        stdout.print("{}\n", .{args[0]}) catch |err| panic("IO Error: {s}", .{@errorName(err)});
        return .null;
    }

    fn readline(gc_alloc: Allocator, args: []FlowValue) FlowValue {
        const prompt = args[0].string;
        stdout.writeAll(prompt) catch |err| panic("IO Error: {s}", .{@errorName(err)});
        stdout.writeByte(' ') catch |err| panic("IO Error: {s}", .{@errorName(err)});
        var buf: [1024]u8 = undefined;
        const read = stdin.readUntilDelimiter(&buf, '\n') catch |err| panic("IO Error: {s}", .{@errorName(err)});

        return .{ .string = gc_alloc.dupe(u8, read) catch oom() };
    }

    fn readfile(gc_alloc: Allocator, args: []FlowValue) FlowValue {
        const path = args[0].string;

        const cwd = std.fs.cwd();

        var file = cwd.openFile(path, .{}) catch |err| panic("FileSystem Error: {s}", .{@errorName(err)});
        defer file.close();

        // 1 MB
        const content = file.readToEndAlloc(gc_alloc, 1024 * 1024 * 1024) catch |err| panic("Error on File Read: {s}", .{@errorName(err)});
        return .{ .string = content };
    }

    fn writefile(_: Allocator, args: []FlowValue) FlowValue {
        const path = args[0].string;
        const content = args[1];

        const cwd = std.fs.cwd();

        var file = cwd.createFile(path, .{}) catch |err| panic("Error on File Write: {s}", .{@errorName(err)});
        defer file.close();

        file.writer().print("{}", .{content}) catch |err| panic("Error on File Write: {s}", .{@errorName(err)});

        return .null;
    }

    fn time(_: Allocator, _: []FlowValue) FlowValue {
        return .{ .int = std.time.milliTimestamp() };
    }
};

const print: Function = .{
    .arg_types = &.{.null},
    .ret_type = .null,
    .function = &impls.print,
};

const readline: Function = .{
    .arg_types = &.{.string},
    .ret_type = .string,
    .function = &impls.readline,
};

const readfile: Function = .{
    .arg_types = &.{.string},
    .ret_type = .string,
    .function = &impls.readfile,
};

const writefile: Function = .{
    .arg_types = &.{ .string, .string },
    .ret_type = .null,
    .function = &impls.writefile,
};

const time: Function = .{
    .arg_types = &.{},
    .ret_type = .int,
    .function = &impls.time,
};

const Function = @import("definitions.zig").BuiltinFunction;
const FlowValue = @import("definitions.zig").FlowValue;
const oom = @import("root.zig").oom;

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const std = @import("std");
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
