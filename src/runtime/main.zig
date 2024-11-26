pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer std.debug.assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    if (comptime debug_options.dump) {
        Dumper.dump(code);
        return;
    }

    var vm: VM = try .init(alloc, code);
    defer vm.deinit();

    try vm.run();
}

const std = @import("std");
const code = @embedFile("input");
const VM = @import("VM.zig");
const Dumper = @import("shared").BytecodeDumper;
const debug_options = @import("debug_options");
