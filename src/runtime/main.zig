pub fn main() !void {
    if (comptime debug_options.dump) {
        DumpTruck.dump(code);
        return;
    }

    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer std.debug.assert(gpa.deinit() == .ok);

    var gc: GC = .init(gpa.allocator());
    defer gc.deinit();

    var vm: VM = try .init(gpa.allocator(), gc.allocator(), code);
    defer vm.deinit();

    try vm.run();
}

const std = @import("std");
const VM = @import("VM.zig");
const GC = @import("GC.zig");
const DumpTruck = @import("shared").BytecodeDumper;
const debug_options = @import("debug_options");

const code = @embedFile("input");
// TODO: Add another @embedFile or @import for source code locations for error messages
// Should be a kind of map, mapping the bytecode index to source code locations
