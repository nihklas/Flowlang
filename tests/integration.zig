const std = @import("std");
const File = std.fs.File;
const Step = std.Build.Step;
const Module = std.Build.Module;
const Compile = Step.Compile;

const cases_dir = "tests/cases";

pub fn addIntegrationTest(b: *std.Build, shared: *Module, debug_options: *Step.Options) !void {
    const integration_tests = b.step("integration-test", "Run integration tests");

    var test_cases = try b.build_root.handle.openDir(cases_dir, .{ .iterate = true });
    defer test_cases.close();

    var cases = test_cases.iterate();

    while (try cases.next()) |case| {
        if (case.kind != .directory) continue;

        var case_dir = try test_cases.openDir(case.name, .{});
        defer case_dir.close();

        var expected_file = try case_dir.openFile("expected.txt", .{});
        defer expected_file.close();

        try buildTest(b, case.name, expected_file, shared, debug_options, integration_tests);
    }
}

fn buildTest(b: *std.Build, case_dir: []const u8, expected_file: File, shared: *Module, debug_options: *Step.Options, integration_tests: *Step) !void {
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

    const src_path = b.pathResolve(&.{ cases_dir, case_dir, "code.flow" });

    const compile_step = b.addRunArtifact(compiler);
    compile_step.addFileArg(b.path(src_path));
    const bytecode = compile_step.addOutputFileArg(case_dir);

    runtime.root_module.addAnonymousImport("input", .{
        .root_source_file = bytecode,
    });

    const run_test = b.addRunArtifact(runtime);
    const output = run_test.captureStdOut();

    const expected = try expected_file.readToEndAlloc(b.allocator, 1024 * 1024); // 1 MB

    const check_file = b.addCheckFile(output, .{ .expected_exact = expected });

    integration_tests.dependOn(&check_file.step);
}
