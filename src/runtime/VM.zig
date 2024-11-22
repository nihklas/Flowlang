alloc: Allocator,
ip: usize = 0,

pub fn init(alloc: Allocator) VM {
    return .{
        .alloc = alloc,
    };
}

pub fn run(self: *VM) !void {
    _ = self;
}

const VM = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
