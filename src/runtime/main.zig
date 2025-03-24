pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer std.debug.assert(gpa.deinit() == .ok);

    const code = try readByteCode(gpa.allocator());
    defer gpa.allocator().free(code);

    if (comptime debug_options.dump_bc) {
        DumpTruck.dump(code);
        return;
    }

    var gc: GC = .init(gpa.allocator());
    defer gc.deinit();

    var vm: VM = .init(gpa.allocator(), gc.allocator(), code);
    defer vm.deinit();

    vm.run();
}

fn readByteCode(alloc: std.mem.Allocator) ![]const u8 {
    const self_path = try std.fs.selfExePathAlloc(alloc);
    defer alloc.free(self_path);

    var exe_file = try std.fs.openFileAbsolute(self_path, .{});
    defer exe_file.close();

    try exe_file.seekFromEnd(-8);

    var length_buffer: [8]u8 = undefined;
    _ = try exe_file.read(&length_buffer);

    const bytecode_len: u64 = @bitCast(length_buffer);
    if (bytecode_len > std.math.maxInt(i64)) {
        std.debug.print("Bytecode is too long, damn\n", .{});
        return error.BytecodeTooLong;
    }

    try exe_file.seekFromEnd(-8 - @as(i64, @intCast(bytecode_len)));
    const bytecode = try alloc.alloc(u8, bytecode_len);
    errdefer alloc.free(bytecode);

    const bytes_read = try exe_file.read(bytecode);
    if (bytes_read != bytecode_len) {
        return error.InvalidByteCode;
    }

    return bytecode;
}

const std = @import("std");
const VM = @import("VM.zig");
const GC = @import("GC.zig");
const DumpTruck = @import("debug/BytecodeDumper.zig");
const debug_options = @import("debug_options");
