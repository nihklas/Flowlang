child_alloc: Allocator,
vtable: std.mem.Allocator.VTable,
managed_objects: std.AutoHashMapUnmanaged(usize, usize),

pub fn init(child_alloc: Allocator) GC {
    return .{
        .child_alloc = child_alloc,
        .managed_objects = .empty,
        .vtable = .{
            .alloc = alloc,
            .free = free,
            .resize = resize,
        },
    };
}

/// Frees all still managed objects
pub fn deinit(self: *GC) void {
    defer self.managed_objects.deinit(self.child_alloc);
    var iter = self.managed_objects.iterator();
    while (iter.next()) |entry| {
        const buf = @as([*]u8, @ptrFromInt(entry.key_ptr.*));
        self.child_alloc.free(buf[0..entry.value_ptr.*]);
    }
}

pub fn allocator(self: *GC) Allocator {
    return .{
        .ptr = self,
        .vtable = &self.vtable,
    };
}

pub fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
    const self: *GC = @ptrCast(@alignCast(ctx));

    const bytes = self.child_alloc.rawAlloc(len, ptr_align, ret_addr);

    if (bytes) |alloced_bytes| {
        self.managed_objects.put(self.child_alloc, @intFromPtr(alloced_bytes), len) catch {
            self.child_alloc.free(alloced_bytes[0..len]);
            return null;
        };
    }

    return bytes;
}

pub fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
    const self: *GC = @ptrCast(@alignCast(ctx));

    const success = self.child_alloc.rawResize(buf, buf_align, new_len, ret_addr);

    if (success) {
        const key: usize = @intFromPtr(buf.ptr);
        if (self.managed_objects.get(key)) |_| {
            self.managed_objects.putAssumeCapacity(key, new_len);
        } else {
            @panic("What just happened? You resized, but I don't know of that object");
        }
    }

    return success;
}

pub fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
    const self: *GC = @ptrCast(@alignCast(ctx));

    self.child_alloc.rawFree(buf, buf_align, ret_addr);
    _ = self.managed_objects.remove(@intFromPtr(buf.ptr));
}

const GC = @This();
const std = @import("std");
const Allocator = std.mem.Allocator;
