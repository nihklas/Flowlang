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
const VM = @import("VM.zig");
const Dumper = @import("shared").BytecodeDumper;
const debug_options = @import("debug_options");

const code = @embedFile("input");
// TODO: Add another @embedFile or @import for source code locations for error messages
// Should be a kind of map, mapping the bytecode index to source code locations
