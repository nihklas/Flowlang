const cases_dir = "bench/benches";

pub fn addBenchmark(b: *std.Build) void {
    const benchmark_step = b.step("benchmark", "Run the benchmarks");
    const bench_setting = b.option(enum {
        Debug,
        Safe,
        Fast,
        Small,
        All,
    }, "bench-setting", "Which Version of the Compiler to benchmark") orelse .Debug;

    const debug_compiler = build.buildCompiler(b, .{ .target = b.resolveTargetQuery(.{}), .optimize = .Debug });
    const safe_compiler = build.buildCompiler(b, .{ .target = b.resolveTargetQuery(.{}), .optimize = .ReleaseSafe });
    const fast_compiler = build.buildCompiler(b, .{ .target = b.resolveTargetQuery(.{}), .optimize = .ReleaseFast });
    const small_compiler = build.buildCompiler(b, .{ .target = b.resolveTargetQuery(.{}), .optimize = .ReleaseSmall });

    const runner_mod = b.addModule("benchmark", .{
        .root_source_file = b.path("bench/bench_runner.zig"),
        .target = b.resolveTargetQuery(.{}),
        .optimize = .ReleaseSafe,
    });
    const runner_exe = b.addExecutable(.{
        .name = "benchmark-runner",
        .root_module = runner_mod,
    });

    if (bench_setting == .Debug or bench_setting == .All) {
        buildRunner(b, debug_compiler, runner_exe, benchmark_step);
    }
    if (bench_setting == .Safe or bench_setting == .All) {
        buildRunner(b, safe_compiler, runner_exe, benchmark_step);
    }
    if (bench_setting == .Fast or bench_setting == .All) {
        buildRunner(b, fast_compiler, runner_exe, benchmark_step);
    }
    if (bench_setting == .Small or bench_setting == .All) {
        buildRunner(b, small_compiler, runner_exe, benchmark_step);
    }
}

fn buildRunner(b: *std.Build, compiler: *std.Build.Step.Compile, runner_exe: *std.Build.Step.Compile, benchmark_step: *std.Build.Step) void {
    const runner = b.addRunArtifact(runner_exe);
    runner.addFileArg(compiler.getEmittedBin());
    runner.addDirectoryArg(b.path(cases_dir));
    runner.addArg(b.fmt("{?}\n", .{compiler.root_module.optimize}));
    runner.addCheck(.{ .expect_term = .{ .Exited = 0 } });

    // NOTE: this ensures that the tests are actually re-run
    runner.stdio = .inherit;

    benchmark_step.dependOn(&runner.step);
}

const std = @import("std");
const build = @import("../build.zig");
