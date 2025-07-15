alloc: std.mem.Allocator,
fir: *FIR,

pub fn init(alloc: std.mem.Allocator, fir: *FIR) Optimizer {
    return .{
        .alloc = alloc,
        .fir = fir,
    };
}

pub fn deinit(self: *Optimizer) void {
    self.* = undefined;
}

pub fn optimize(self: *Optimizer) void {
    _ = self;
}

const Optimizer = @This();
const std = @import("std");
const FIR = @import("ir/FIR.zig");
