const std = @import("std");
const File = std.fs.File;
const Step = std.Build.Step;
const Module = std.Build.Module;
const Compile = Step.Compile;

const cases_dir = "tests/cases";
const split_marker = "=====";

pub fn addIntegrationTest(b: *std.Build, compiler: *Compile, specific_case: ?[]const u8) !void {
    const integration_tests = b.step("integration-test", "Run integration tests");

    if (specific_case) |case_name| {
        try runSingle(b, compiler, integration_tests, case_name);
    } else {
        try runAll(b, compiler, integration_tests);
    }
}

fn runSingle(b: *std.Build, compiler: *Compile, integration_tests: *Step, case_name: []const u8) !void {
    const file_name = b.fmt("{s}/{s}.flow", .{ cases_dir, case_name });
    var test_file = try b.build_root.handle.openFile(file_name, .{});
    defer test_file.close();
    try buildTest(b, compiler, case_name, test_file, integration_tests);
}

fn runAll(b: *std.Build, compiler: *Compile, integration_tests: *Step) !void {
    var test_cases = try b.build_root.handle.openDir(cases_dir, .{ .iterate = true });
    defer test_cases.close();

    var cases = test_cases.iterate();

    while (try cases.next()) |case| {
        if (case.kind != .file) continue;

        var test_file = try test_cases.openFile(case.name, .{});
        defer test_file.close();
        try buildTest(b, compiler, case.name, test_file, integration_tests);
    }
}

fn buildTest(
    b: *std.Build,
    compiler: *Compile,
    case_name: []const u8,
    test_file: File,
    integration_tests: *Step,
) !void {
    const test_content = try test_file.readToEndAlloc(b.allocator, 1024 * 1024); // 1 MB
    const split_mark = std.mem.indexOf(u8, test_content, split_marker) orelse return error.MalformedTestCase;

    const flow_src = test_content[0..split_mark];
    const expected = std.mem.trimLeft(u8, test_content[split_mark + split_marker.len ..], " \n");

    const write_file = b.addWriteFiles();
    const src_path = write_file.add("code.flow", flow_src);

    makeTest(b, compiler, case_name, src_path, expected, integration_tests);
}

fn makeTest(
    b: *std.Build,
    compiler: *Compile,
    case_name: []const u8,
    src_path: std.Build.LazyPath,
    expected: []const u8,
    integration_tests: *Step,
) void {
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
