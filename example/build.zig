const std = @import("std");
const flow = @import("flow");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const src_file = b.option([]const u8, "main", "Flow file to execute") orelse "src/main.flow";

    const betterAdd = b.dependency("betterAdd", .{});

    const flow_out = flow.compile(b, .{
        .name = "flow-example",
        .source = b.path(src_file),
        .target = target,
        .optimize = optimize,
        .extension = .{
            .modules = &.{.{ .name = "betterAdd", .module = betterAdd.module("betterAdd") }},
            .export_file = b.path("exports.zig"),
        },
    });

    b.installArtifact(flow_out);

    const run_flow = b.addRunArtifact(flow_out);
    const run_step = b.step("run", "Run the code directly");
    run_step.dependOn(&run_flow.step);
}
