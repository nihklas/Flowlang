alloc: Allocator,
ip: usize = 0,

pub fn init(alloc: Allocator) VM {
    return .{
        .alloc = alloc,
    };
}

// TODO: Write two main run functions:
//    1. while(true) with switch
//    2. labeled switch continue
// Compare those in benchmarks
pub fn run(self: *VM) !void {
    self.runWhileSwitch();
    // self.runSwitchContinue();
}

fn runWhileSwitch(self: *VM) !void {
    _ = self;
}

fn runSwitchContinue(self: *VM) !void {
    _ = self;
}

const VM = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
