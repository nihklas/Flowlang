const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const use_stderr = b.option(bool, "stderr", "Output custom errors to StdErr instead of NullWriter (Only used in tests)") orelse false;

    const compiler = b.addExecutable(.{
        .name = "flowlang",
        .root_source_file = b.path("src/compiler/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(compiler);

    const run_cmd = b.addRunArtifact(compiler);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/testing.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_options = b.addOptions();
    test_options.addOption(bool, "use_stderr", use_stderr);

    exe_unit_tests.root_module.addOptions("testing_options", test_options);
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const check_compiler = b.addExecutable(.{
        .name = "flowlang",
        .root_source_file = b.path("src/compiler/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const check_step = b.step("check", "Check Step for LSP");
    check_step.dependOn(test_step);
    check_step.dependOn(&check_compiler.step);
}

pub fn compile(b: *std.Build) void {
    _ = b;
    // TODO: Compile and Execute Compiler
    // TODO: Embed Bytecode file into runtime and compile
    std.debug.print("Hello from flow\n", .{});
}
