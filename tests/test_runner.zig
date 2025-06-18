pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    const alloc = debug_allocator.allocator();
    defer _ = debug_allocator.deinit();

    var arg_iter = std.process.args();
    _ = arg_iter.skip();

    const run_file = arg_iter.next() orelse return error.MissingInput;

    var exe = std.process.Child.init(&.{run_file}, alloc);
    _ = try exe.spawnAndWait();
}

const std = @import("std");
