const std = @import("std");
const Build = std.Build;
const Step = Build.Step;
const Compile = Step.Compile;
const Module = Build.Module;
const LazyPath = Build.LazyPath;

pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_stderr = b.option(bool, "stderr", "Output custom errors to StdErr instead of NullWriter (Only used in tests)") orelse false;
    const run_with_debug = b.option(bool, "debug", "Enable all trace and debugging options for the Runtime") orelse false;
    const dump_bytecode = b.option(bool, "dump-bc", "Dump the Bytecode instead of running the VM") orelse false;
    const dump_ast = b.option(bool, "dump-ast", "Dump the AST and exit") orelse false;
    const dump_fir = b.option(bool, "dump-fir", "Dump the FIR and exit") orelse false;
    const trace_stack = b.option(bool, "trace-stack", "Trace the Stack on running") orelse run_with_debug;
    const trace_bytecode = b.option(bool, "trace-bytecode", "Trace the Bytecode on running") orelse run_with_debug;
    const trace_memory = b.option(bool, "trace-memory", "Trace the Memory allocations and frees") orelse run_with_debug;
    const integration_test_case = b.option([]const u8, "case", "Specific integration test case to run");

    const debug_options = b.addOptions();
    debug_options.addOption(bool, "dump_bc", dump_bytecode);
    debug_options.addOption(bool, "dump_ast", dump_ast);
    debug_options.addOption(bool, "dump_fir", dump_fir);
    debug_options.addOption(bool, "stack", trace_stack);
    debug_options.addOption(bool, "bytecode", trace_bytecode);
    debug_options.addOption(bool, "memory", trace_memory);

    const extension_options = b.addOptions();
    extension_options.addOption(bool, "enabled", false);

    // Option to output compiler errors in tests
    const test_options = b.addOptions();
    test_options.addOption(bool, "use_stderr", use_stderr);

    // Shared code between compiler and runtime
    // such as value types and representations
    const shared = b.addModule("shared", .{
        .root_source_file = b.path("src/shared/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    shared.addOptions("module_debug_options", debug_options);
    shared.addOptions("extensions", extension_options);

    const flow_std = b.addModule("flow_std", .{
        .root_source_file = b.path("src/std/stdlib.zig"),
        .target = target,
        .optimize = optimize,
    });
    flow_std.addImport("shared", shared);
    shared.addImport("flow_std", flow_std);

    // Runtime
    const runtime_mod = b.addModule("runtime", .{
        .root_source_file = b.path("src/runtime/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "debug_options", .module = debug_options.createModule() },
            .{ .name = "shared", .module = shared },
        },
    });
    const runtime = b.addExecutable(.{
        .name = "runtime",
        .root_module = runtime_mod,
    });
    b.installArtifact(runtime);

    // Compiler
    const compiler_mod = b.addModule("compiler", .{
        .root_source_file = b.path("src/compiler/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "debug_options", .module = debug_options.createModule() },
            .{ .name = "shared", .module = shared },
        },
    });
    compiler_mod.addAnonymousImport("runtime", .{ .root_source_file = runtime.getEmittedBin() });
    const compiler = b.addExecutable(.{
        .name = "compiler",
        .root_module = compiler_mod,
    });
    b.installArtifact(compiler);
    compiler.step.dependOn(&runtime.step);

    // Unit tests in compiler and runtime
    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/testing.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_unit_tests.root_module.addImport("shared", shared);
    exe_unit_tests.root_module.addOptions("testing_options", test_options);
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Step to run tests
    const test_step = b.step("unit-test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    // Integration tests
    try @import("tests/integration.zig").addIntegrationTest(b, compiler, integration_test_case);

    // Check step for lsp compile errors
    const check_step = b.step("check", "Check Step for LSP");
    check_step.dependOn(&compiler.step);
    check_step.dependOn(&runtime.step);

    const run_compiler = b.addRunArtifact(compiler);
    const compile_step = b.step("compile", "Run the compiler, pass --help to get more information");
    compile_step.dependOn(&run_compiler.step);

    if (b.args) |args| {
        run_compiler.addArgs(args);
    }
}
