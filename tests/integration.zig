const cases_dir = "tests/cases";
const split_marker = "=====";

pub fn addIntegrationTest(b: *std.Build) !void {
    // const case = b.option([]const u8, "case", "Specific integration test case to run");
    const integration_tests = b.step("integration-test", "Run integration tests");

    // if (case) |case_name| {
    //     try runSingle(b, integration_tests, case_name);
    // } else {
    try runAll(b, integration_tests);
    // }
}

fn runSingle(b: *std.Build, integration_tests: *Step, case_name: []const u8) !void {
    const file_name = b.fmt("{s}/{s}.flow", .{ cases_dir, case_name });
    var test_file = try b.build_root.handle.openFile(file_name, .{});
    defer test_file.close();
    try buildTest(b, case_name, test_file, integration_tests);
}

fn runAll(b: *std.Build, integration_tests: *Step) !void {
    var test_cases = try b.build_root.handle.openDir(cases_dir, .{ .iterate = true });
    defer test_cases.close();

    var cases = test_cases.iterate();

    while (try cases.next()) |case| {
        if (case.kind != .file) continue;

        var test_file = try test_cases.openFile(case.name, .{});
        defer test_file.close();
        try buildTest(b, case.name, test_file, integration_tests);
    }
}

fn buildTest(
    b: *std.Build,
    case_name: []const u8,
    test_file: File,
    integration_tests: *Step,
) !void {
    const test_content = try test_file.readToEndAlloc(b.allocator, 1024 * 1024); // 1 MB

    var iterator = std.mem.splitSequence(u8, test_content, split_marker);
    const flow_src = iterator.next().?;
    const expected = blk: {
        if (iterator.next()) |next| {
            break :blk std.mem.trimLeft(u8, next, " \n\t\r");
        }
        return error.MalformedTestCase;
    };

    const write_file = b.addWriteFiles();
    const src_path = write_file.add("code.flow", flow_src);

    makeTest(b, case_name, src_path, expected, integration_tests);
}

fn makeTest(
    b: *std.Build,
    case_name: []const u8,
    src_path: std.Build.LazyPath,
    expected: []const u8,
    integration_tests: *Step,
) void {
    const compiler = build.buildCompiler(b, .{
        .target = b.resolveTargetQuery(.{}),
        .optimize = .Debug,
        .debug = .{ .stress_gc = true },
    });
    const compile_step = b.addRunArtifact(compiler);
    compile_step.addFileArg(src_path);
    const final_exe = compile_step.addOutputFileArg(case_name);

    const runner = b.addExecutable(.{
        .name = "test_runner",
        .root_source_file = b.path("tests/test_runner.zig"),
        .target = b.resolveTargetQuery(.{}),
        .optimize = .ReleaseFast,
    });

    const run_test = b.addRunArtifact(runner);
    run_test.addFileArg(final_exe);
    const output = run_test.captureStdOut();

    const check_file = b.addCheckFile(output, .{ .expected_exact = expected });
    check_file.setName(b.dupe(case_name));

    integration_tests.dependOn(&check_file.step);
}

const builtin = @import("builtin");
const std = @import("std");
const File = std.fs.File;
const Step = std.Build.Step;
const Module = std.Build.Module;
const Compile = Step.Compile;
const build = @import("../build.zig");
