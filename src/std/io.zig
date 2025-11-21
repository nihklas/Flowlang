pub const print: BuiltinFunction = .{
    .arg_types = &.{.null},
    .ret_type = .null,
    .function = &_print,
};

fn _print(_: Allocator, args: []FlowValue) FlowValue {
    var buffer: [1024]u8 = undefined;
    var stdout_writer = stdout.writer(&buffer);
    var writer = &stdout_writer.interface;
    writer.print("{f}\n", .{args[0]}) catch |err| panic("IO Error: {s}", .{@errorName(err)});
    writer.flush() catch |err| panic("IO Error: {s}", .{@errorName(err)});
    return .null;
}

pub const readline: BuiltinFunction = .{
    .arg_types = &.{.primitive(.string)},
    .ret_type = .{ .type = .string, .order = 0 },
    .function = &_readline,
};

fn _readline(gc_alloc: Allocator, args: []FlowValue) FlowValue {
    const prompt = args[0].string;
    var buf: [1024]u8 = undefined;
    var stdout_writer = stdout.writer(&buf);
    var writer = &stdout_writer.interface;

    writer.writeAll(prompt) catch |err| panic("IO Error: {s}", .{@errorName(err)});
    writer.writeByte(' ') catch |err| panic("IO Error: {s}", .{@errorName(err)});
    writer.flush() catch |err| panic("IO Error: {s}", .{@errorName(err)});

    // we wrote and flushed everything, so we can reuse the buffer
    buf = undefined;
    var stdin_reader = stdin.reader(&buf);
    var reader = &stdin_reader.interface;

    var input = std.Io.Writer.Allocating.initCapacity(gc_alloc, 1024) catch oom();
    defer input.deinit();

    _ = reader.streamDelimiter(&input.writer, '\n') catch |err| panic("IO Error: {s}", .{@errorName(err)});

    return .{ .string = gc_alloc.dupe(u8, input.written()) catch oom() };
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
    var buffer: [4096]u8 = undefined;

    const cwd = std.fs.cwd();

    var file = cwd.createFile(path, .{}) catch |err| panic("Error on File Write: {s}", .{@errorName(err)});
    defer file.close();

    var writer = file.writer(&buffer).interface;
    writer.print("{f}", .{content}) catch |err| panic("Error on File Write: {s}", .{@errorName(err)});
    writer.flush() catch |err| panic("Error on File Write: {s}", .{@errorName(err)});

    return .null;
}

const BuiltinFunction = @import("shared").definitions.BuiltinFunction;
const FlowValue = @import("shared").definitions.FlowValue;
const oom = @import("shared").oom;

const stdout = std.fs.File.stdout();
const stdin = std.fs.File.stdin();

const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
