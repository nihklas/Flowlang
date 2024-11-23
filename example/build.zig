const std = @import("std");
const flow = @import("flow");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const flow_out = flow.compile(b, .{
        .name = "flow-example",
        .source = b.path("src/main.flow"),
        .target = target,
    });

    b.installArtifact(flow_out);

    const run_flow = b.addRunArtifact(flow_out);
    const run_step = b.step("run", "Run the code directly");
    run_step.dependOn(&run_flow.step);
}
