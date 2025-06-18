pub fn main() !void {
    const alloc = std.heap.smp_allocator;

    var arg_iter = std.process.args();
    _ = arg_iter.skip();

    const run_file = arg_iter.next() orelse return error.MissingInput;

    var exe = std.process.Child.init(&.{run_file}, alloc);
    _ = try exe.spawnAndWait();
}

const std = @import("std");
