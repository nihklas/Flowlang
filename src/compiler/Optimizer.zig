gpa: std.mem.Allocator,
fir: *FIR,

pub fn init(gpa: std.mem.Allocator, fir: *FIR) Optimizer {
    return .{
        .gpa = gpa,
        .fir = fir,
    };
}

pub fn deinit(self: *Optimizer) void {
    self.* = undefined;
}

pub fn foldConstants(self: *Optimizer) void {
    @import("optimizer/constant_folding.zig").foldConstants(self.fir);
}

const Optimizer = @This();

const std = @import("std");

const FIR = @import("ir/FIR.zig");
