const std = @import("std");
const flow = @import("flow");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const flow_out = flow.compile(b, .{
        .name = "flow-example",
        .source = b.path("src/main.flow"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(flow_out);
}
