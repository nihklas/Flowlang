pub fn run(gpa: Allocator, code: []const u8) !void {
    var gc: GC = .pre_init;
    defer gc.deinit();

    var vm: VM = .init(gpa, gc.allocator(), code);
    defer vm.deinit();

    gc.init(gpa, &vm);

    vm.run();
}

pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    const gpa, const is_debug = switch (@import("builtin").mode) {
        .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
        .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
    };
    defer if (is_debug) {
        _ = debug_allocator.deinit();
    };

    const code = try readByteCode(gpa);
    defer gpa.free(code);

    try run(gpa, code);
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
const GC = @import("gc/Simple.zig");
const Allocator = std.mem.Allocator;
