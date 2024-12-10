const std = @import("std");
const File = std.fs.File;
const Step = std.Build.Step;
const Module = std.Build.Module;
const Compile = Step.Compile;

const cases_dir = "tests/cases";
const split_marker = "=====";

pub fn addIntegrationTest(b: *std.Build, shared: *Module, debug_options: *Step.Options, specific_case: ?[]const u8) !void {
    const integration_tests = b.step("integration-test", "Run integration tests");

    if (specific_case) |case_name| {
        try runSingle(b, shared, debug_options, integration_tests, case_name);
    } else {
        try runAll(b, shared, debug_options, integration_tests);
    }
}

fn runSingle(b: *std.Build, shared: *Module, debug_options: *Step.Options, integration_tests: *Step, case_name: []const u8) !void {
    const file_name = b.fmt("{s}/{s}.flow", .{ cases_dir, case_name });
    var test_file = try b.build_root.handle.openFile(file_name, .{});
    defer test_file.close();
    try buildTest(b, case_name, test_file, shared, debug_options, integration_tests);
}

fn runAll(b: *std.Build, shared: *Module, debug_options: *Step.Options, integration_tests: *Step) !void {
    var test_cases = try b.build_root.handle.openDir(cases_dir, .{ .iterate = true });
    defer test_cases.close();

    var cases = test_cases.iterate();

    while (try cases.next()) |case| {
        if (case.kind != .file) continue;

        var test_file = try test_cases.openFile(case.name, .{});
        defer test_file.close();
        try buildTest(b, case.name, test_file, shared, debug_options, integration_tests);
    }
}

fn buildTest(
    b: *std.Build,
    case_name: []const u8,
    test_file: File,
    shared: *Module,
    debug_options: *Step.Options,
    integration_tests: *Step,
) !void {
    const test_content = try test_file.readToEndAlloc(b.allocator, 1024 * 1024); // 1 MB
    const split_mark = std.mem.indexOf(u8, test_content, split_marker) orelse return error.MalformedTestCase;

    const flow_src = test_content[0..split_mark];
    const expected = std.mem.trimLeft(u8, test_content[split_mark + split_marker.len ..], " \n");

    const write_file = b.addWriteFiles();
    const src_path = write_file.add("code.flow", flow_src);

    makeTest(b, case_name, src_path, expected, shared, debug_options, integration_tests);
}

fn makeTest(
    b: *std.Build,
    case_name: []const u8,
    src_path: std.Build.LazyPath,
    expected: []const u8,
    shared: *Module,
    debug_options: *Step.Options,
    integration_tests: *Step,
) void {
    const exes = makeExes(b, shared, debug_options);
    const compiler = exes.compiler;
    const runtime = exes.runtime;

    const compile_step = b.addRunArtifact(compiler);
    compile_step.addFileArg(src_path);
    const bytecode = compile_step.addOutputFileArg(case_name);

    runtime.root_module.addAnonymousImport("input", .{
        .root_source_file = bytecode,
    });

    const run_test = b.addRunArtifact(runtime);
    const output = run_test.captureStdOut();

    const check_file = b.addCheckFile(output, .{ .expected_exact = expected });
    check_file.setName(b.dupe(case_name));

    integration_tests.dependOn(&check_file.step);
}

fn makeExes(b: *std.Build, shared: *Module, debug_options: *Step.Options) struct { compiler: *Compile, runtime: *Compile } {
    const compiler = b.addExecutable(.{
        .name = "compiler",
        .root_source_file = b.path("src/compiler/main.zig"),
        .target = b.host,
        .optimize = .Debug,
    });
    compiler.root_module.addOptions("debug_options", debug_options);
    compiler.root_module.addImport("shared", shared);

    const runtime = b.addExecutable(.{
        .name = "runtime",
        .root_source_file = b.path("src/runtime/main.zig"),
        .target = b.host,
        .optimize = .Debug,
    });
    runtime.root_module.addOptions("debug_options", debug_options);
    runtime.root_module.addImport("shared", shared);

    return .{
        .compiler = compiler,
        .runtime = runtime,
    };
}
