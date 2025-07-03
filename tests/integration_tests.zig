const cases_dir = "tests/cases";

const IntegrationConfig = struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    compiler: *std.Build.Step.Compile,
};

pub fn addIntegrationTests(b: *std.Build, config: IntegrationConfig) void {
    const case = b.option([]const u8, "case", "Target a specific Test case for integration testing");
    const integration_step = b.step("integration", "Run the integration tests");

    const runner_mod = b.addModule("integration_test", .{
        .root_source_file = b.path("tests/test_runner.zig"),
        .target = config.target,
        .optimize = config.optimize,
    });

    const runner_exe = b.addExecutable(.{
        .name = "integration-test-runner",
        .root_module = runner_mod,
    });

    const runner = b.addRunArtifact(runner_exe);
    runner.addFileArg(config.compiler.getEmittedBin());
    runner.addDirectoryArg(b.path(cases_dir));
    if (case) |c| runner.addArg(c);
    runner.addCheck(.{ .expect_term = .{ .Exited = 0 } });

    // NOTE: this ensures that the tests are actually re-run
    runner.stdio = .inherit;

    integration_step.dependOn(&runner.step);
}

const std = @import("std");
