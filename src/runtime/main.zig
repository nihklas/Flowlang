const std = @import("std");
const code = @embedFile("input");
const VM = @import("VM.zig");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer std.debug.assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    var vm: VM = try .init(alloc, code);
    defer vm.deinit();

    try vm.run();
}
