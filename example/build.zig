const std = @import("std");
const flow = @import("flow");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const example_module = b.addModule("betterAdd", .{
        .root_source_file = b.path("example_module/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const flow_out = flow.compile(b, .{
        .name = "flow-example",
        .source = b.path("src/test.flow"),
        .target = target,
        .optimize = optimize,
        .extension = .{
            .modules = &.{.{ .name = "betterAdd", .module = example_module }},
            .export_file = b.path("exports.zig"),
        },
    });

    b.installArtifact(flow_out);

    const run_flow = b.addRunArtifact(flow_out);
    const run_step = b.step("run", "Run the code directly");
    run_step.dependOn(&run_flow.step);
}
