pub const print: BuiltinFunction = .{
    .arg_types = &.{.null},
    .ret_type = .null,
    .function = &_print,
};

fn _print(_: Allocator, args: []FlowValue) FlowValue {
    stdout.print("{}\n", .{args[0]}) catch |err| panic("IO Error: {s}", .{@errorName(err)});
    return .null;
}

pub const readline: BuiltinFunction = .{
    .arg_types = &.{.primitive(.string)},
    .ret_type = .{ .type = .string, .order = 0 },
    .function = &_readline,
};

fn _readline(gc_alloc: Allocator, args: []FlowValue) FlowValue {
    const prompt = args[0].string;
    stdout.writeAll(prompt) catch |err| panic("IO Error: {s}", .{@errorName(err)});
    stdout.writeByte(' ') catch |err| panic("IO Error: {s}", .{@errorName(err)});
    var buf: [1024]u8 = undefined;
    const read = stdin.readUntilDelimiter(&buf, '\n') catch |err| panic("IO Error: {s}", .{@errorName(err)});

    return .{ .string = gc_alloc.dupe(u8, read) catch oom() };
}

pub const readfile: BuiltinFunction = .{
    .arg_types = &.{.primitive(.string)},
    .ret_type = .{ .type = .string, .order = 0 },
    .function = &_readfile,
};

fn _readfile(gc_alloc: Allocator, args: []FlowValue) FlowValue {
    const path = args[0].string;

    const cwd = std.fs.cwd();

    var file = cwd.openFile(path, .{}) catch |err| panic("FileSystem Error: {s}", .{@errorName(err)});
    defer file.close();

    // 1 MB
    const content = file.readToEndAlloc(gc_alloc, 1024 * 1024 * 1024) catch |err| panic("Error on File Read: {s}", .{@errorName(err)});
    return .{ .string = content };
}

pub const writefile: BuiltinFunction = .{
    .arg_types = &.{ .primitive(.string), .primitive(.string) },
    .ret_type = .null,
    .function = &_writefile,
};

fn _writefile(_: Allocator, args: []FlowValue) FlowValue {
    const path = args[0].string;
    const content = args[1];

    const cwd = std.fs.cwd();

    var file = cwd.createFile(path, .{}) catch |err| panic("Error on File Write: {s}", .{@errorName(err)});
    defer file.close();

    file.writer().print("{}", .{content}) catch |err| panic("Error on File Write: {s}", .{@errorName(err)});

    return .null;
}

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
    std.debug.assert(args.len == 1);
    return switch (args[0]) {
        .array => |array| .{ .int = @intCast(array.len) },
        .string => |string| .{ .int = @intCast(string.len) },
        else => panic("Invalid argument type passed to len(), expected string or array, got '{s}'", .{@tagName(args[0])}),
    };
}

const BuiltinFunction = @import("shared").definitions.BuiltinFunction;
const FlowValue = @import("shared").definitions.FlowValue;
const oom = @import("shared").oom;

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const std = @import("std");
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
