const std = @import("std");
const flow = @import("flow");

pub fn build(b: *std.Build) void {
    flow.compile(b);
}
