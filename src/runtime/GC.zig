child_alloc: Allocator,
vtable: std.mem.Allocator.VTable,
managed_objects: std.AutoHashMapUnmanaged(usize, ManagedObject),

pub fn init(child_alloc: Allocator) GC {
    return .{
        .child_alloc = child_alloc,
        .managed_objects = .empty,
        .vtable = .{
            .alloc = alloc,
            .resize = resize,
            .remap = remap,
            .free = free,
        },
    };
}

/// Frees all still managed objects
pub fn deinit(self: *GC) void {
    if (comptime trace) {
        std.debug.print("[DEBUG] Deinit GC, freeing all managed objects\n", .{});
    }

    defer self.managed_objects.deinit(self.child_alloc);
    var iter = self.managed_objects.iterator();
    while (iter.next()) |entry| {
        const buf = @as([*]u8, @ptrFromInt(entry.key_ptr.*));
        const obj = entry.value_ptr.*;

        self.child_alloc.rawFree(buf[0..obj.len], obj.alignment, @returnAddress());

        if (comptime trace) {
            std.debug.print("[DEBUG] freed {d} bytes at {*}\n", .{ obj.len, buf });
        }
    }
}

pub fn allocator(self: *GC) Allocator {
    return .{
        .ptr = self,
        .vtable = &self.vtable,
    };
}

pub fn alloc(ctx: *anyopaque, len: usize, ptr_align: Alignment, ret_addr: usize) ?[*]u8 {
    const self: *GC = @ptrCast(@alignCast(ctx));

    const bytes = self.child_alloc.rawAlloc(len, ptr_align, ret_addr);

    if (bytes) |alloced_bytes| {
        self.managed_objects.put(self.child_alloc, @intFromPtr(alloced_bytes), .{ .alignment = ptr_align, .len = len }) catch {
            self.child_alloc.free(alloced_bytes[0..len]);
            return null;
        };

        if (comptime trace) {
            std.debug.print("[DEBUG] alloc {d} bytes at {*}\n", .{ len, alloced_bytes });
        }
    }

    return bytes;
}

pub fn resize(ctx: *anyopaque, buf: []u8, buf_align: Alignment, new_len: usize, ret_addr: usize) bool {
    const self: *GC = @ptrCast(@alignCast(ctx));

    const old_len = buf.len;

    const success = self.child_alloc.rawResize(buf, buf_align, new_len, ret_addr);

    if (success) {
        const key: usize = @intFromPtr(buf.ptr);
        if (self.managed_objects.getPtr(key)) |entry| {
            entry.len = new_len;
        } else {
            @panic("What just happened? You resized, but I don't know that object yet");
        }

        if (comptime trace) {
            std.debug.print("[DEBUG] resized from {d} to {d} at {*}\n", .{ old_len, new_len, buf.ptr });
        }
    }

    return success;
}

pub fn remap(ctx: *anyopaque, buf: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    const self: *GC = @ptrCast(@alignCast(ctx));

    const old_len = buf.len;

    const maybe_new_mem = self.child_alloc.rawRemap(buf, alignment, new_len, ret_addr);

    if (maybe_new_mem) |new_mem| {
        const old_key: usize = @intFromPtr(buf.ptr);
        if (self.managed_objects.getPtr(old_key) == null) {
            @panic("What just happened? You remapped, but I don't know that object yet");
        }

        _ = self.managed_objects.remove(old_key);
        self.managed_objects.put(self.child_alloc, @intFromPtr(new_mem), .{ .alignment = alignment, .len = new_len }) catch {
            self.child_alloc.free(new_mem[0..new_len]);
            return null;
        };

        if (comptime trace) {
            std.debug.print("[DEBUG] resized from {d} to {d} at {*}\n", .{ old_len, new_len, buf.ptr });
        }
    }

    return maybe_new_mem;
}

pub fn free(ctx: *anyopaque, buf: []u8, buf_align: Alignment, ret_addr: usize) void {
    const self: *GC = @ptrCast(@alignCast(ctx));

    if (comptime trace) {
        std.debug.print("[DEBUG] free {d} bytes at {*}\n", .{ buf.len, buf.ptr });
    }

    self.child_alloc.rawFree(buf, buf_align, ret_addr);
    _ = self.managed_objects.remove(@intFromPtr(buf.ptr));
}

const ManagedObject = struct {
    len: usize,
    alignment: Alignment,
};

const GC = @This();
const std = @import("std");
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;
const trace = @import("debug_options").memory;
