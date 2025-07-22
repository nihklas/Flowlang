child_alloc: Allocator,
vtable: std.mem.Allocator.VTable,
managed_objects: std.AutoHashMapUnmanaged(usize, ManagedObject),
vm: *VM,
next_gc: usize,
bytes_allocated: usize,
enabled: bool,

pub const pre_init: GC = .{
    .managed_objects = .empty,
    .vm = undefined,
    .child_alloc = undefined,
    .bytes_allocated = 0,
    .next_gc = initial_gc_theshold,
    .enabled = true,
    .vtable = .{
        .alloc = alloc,
        .resize = resize,
        .remap = remap,
        .free = free,
    },
};

pub fn init(self: *GC, child_alloc: Allocator, vm: *VM) void {
    self.child_alloc = child_alloc;
    self.vm = vm;
    // TODO: add compiler option to start managed_objects with initialCapacity
}

/// Frees all still managed objects
pub fn deinit(self: *GC) void {
    if (comptime trace) {
        std.debug.print("[DEBUG] Deinit GC, freeing all managed objects\n", .{});
    }

    var iter = self.managed_objects.iterator();
    while (iter.next()) |entry| {
        const buf = @as([*]u8, @ptrFromInt(entry.key_ptr.*));
        const obj = entry.value_ptr.*;

        self.child_alloc.rawFree(buf[0..obj.len], obj.alignment, @returnAddress());

        if (comptime trace) {
            std.debug.print("[DEBUG] freed {d} bytes at {*}\n", .{ obj.len, buf });
        }
    }

    self.managed_objects.deinit(self.child_alloc);
    self.* = undefined;
}

/// Make sure to call gc.init() before any allocations
pub fn allocator(self: *GC) Allocator {
    return .{
        .ptr = self,
        .vtable = &self.vtable,
    };
}

pub fn alloc(ctx: *anyopaque, len: usize, ptr_align: Alignment, ret_addr: usize) ?[*]u8 {
    const self: *GC = @ptrCast(@alignCast(ctx));

    self.gc();

    const bytes = self.child_alloc.rawAlloc(len, ptr_align, ret_addr);

    if (bytes) |alloced_bytes| {
        if (len == 0) {
            if (comptime trace) {
                std.debug.print("[DEBUG] allocation of 0 bytes will not be tracked, address: {*}\n", .{alloced_bytes});
            }
            return bytes;
        }

        self.managed_objects.put(self.child_alloc, @intFromPtr(alloced_bytes), .{ .alignment = ptr_align, .len = len, .alive = true }) catch {
            self.child_alloc.free(alloced_bytes[0..len]);
            return null;
        };
        self.bytes_allocated += len;

        if (comptime trace) {
            std.debug.print("[DEBUG] alloc {d} bytes at {*}\n", .{ len, alloced_bytes });
        }
    }

    return bytes;
}

pub fn resize(ctx: *anyopaque, buf: []u8, buf_align: Alignment, new_len: usize, ret_addr: usize) bool {
    const self: *GC = @ptrCast(@alignCast(ctx));

    const old_len = buf.len;

    if (new_len > old_len) {
        // NOTE: only gc on allocation
        self.gc();
    }

    const success = self.child_alloc.rawResize(buf, buf_align, new_len, ret_addr);

    if (success) {
        const key: usize = @intFromPtr(buf.ptr);
        if (self.managed_objects.getPtr(key)) |entry| {
            entry.len = new_len;
            self.bytes_allocated += new_len - old_len;
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

    if (new_len > old_len) {
        // NOTE: only gc on allocation
        self.gc();
    }

    const maybe_new_mem = self.child_alloc.rawRemap(buf, alignment, new_len, ret_addr);

    if (maybe_new_mem) |new_mem| {
        const old_key: usize = @intFromPtr(buf.ptr);
        if (self.managed_objects.getPtr(old_key) == null) {
            @panic("What just happened? You remapped, but I don't know that object yet");
        }

        _ = self.managed_objects.remove(old_key);
        self.managed_objects.put(self.child_alloc, @intFromPtr(new_mem), .{ .alignment = alignment, .len = new_len, .alive = true }) catch {
            self.child_alloc.free(new_mem[0..new_len]);
            return null;
        };
        self.bytes_allocated += new_len - old_len;

        if (comptime trace) {
            std.debug.print("[DEBUG] remapped from {d} to {d} at {*}\n", .{ old_len, new_len, buf.ptr });
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
    self.bytes_allocated -= buf.len;
}

pub fn disable(self: *GC) void {
    self.enabled = false;
}

pub fn enable(self: *GC) void {
    self.enabled = true;
}

fn gc(self: *GC) void {
    if (!self.enabled) return;

    if (stress_gc or self.bytes_allocated >= self.next_gc) {
        if (comptime trace) {
            std.debug.print("[DEBUG] -- gc start\n", .{});
        }
        const before = self.bytes_allocated;

        self.mark();
        self.sweep();

        self.next_gc = self.bytes_allocated * gc_growth_factor;
        if (self.bytes_allocated == 0) {
            self.next_gc = initial_gc_theshold;
        }

        if (comptime trace) {
            std.debug.print("[DEBUG] collected {d} bytes\n", .{before - self.bytes_allocated});
            std.debug.print("[DEBUG] next collection is at {d} bytes\n", .{self.next_gc});
            std.debug.print("[DEBUG] -- gc end\n", .{});
        }
    }
}

fn mark(self: *GC) void {
    for (self.vm.globals[0..self.vm.globals_count]) |global| self.markFlowValue(global);
    for (self.vm.value_stack.stack[0..self.vm.value_stack.stack_top]) |value| self.markFlowValue(value.deref());
}

fn markFlowValue(self: *GC, value: FlowValue) void {
    switch (value) {
        .null, .bool, .int, .float => return,
        .function => return,
        .builtin_fn => return,
        .string => |str| {
            if (self.managed_objects.getPtr(@intFromPtr(str.ptr))) |obj| {
                obj.alive = true;
            }
        },
        .array => |arr| {
            if (self.managed_objects.getPtr(@intFromPtr(arr))) |obj| {
                obj.alive = true;
            }
            if (self.managed_objects.getPtr(@intFromPtr(arr.items))) |mem| {
                mem.alive = true;
            }
        },
    }
}

fn sweep(self: *GC) void {
    var iter = self.managed_objects.iterator();
    while (iter.next()) |entry| {
        const obj = entry.value_ptr;
        if (obj.alive) {
            obj.alive = false;
            continue;
        }

        const buf = @as([*]u8, @ptrFromInt(entry.key_ptr.*));
        self.allocator().rawFree(buf[0..obj.len], obj.alignment, @returnAddress());
    }
}

const ManagedObject = struct {
    len: usize,
    alignment: Alignment,
    alive: bool,
};

const GC = @This();
const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;
const VM = @import("../VM.zig");
const FlowValue = @import("shared").definitions.FlowValue;

const trace = @import("debug_options").memory;
const stress_gc = @import("debug_options").stress_gc;
const initial_gc_theshold = @import("vm_options").initial_gc_threshold;
const gc_growth_factor = @import("vm_options").gc_growth_factor;

comptime {
    if (gc_growth_factor == 0) {
        @compileError("gc_growth_factor must be greater than 0");
    }
}
