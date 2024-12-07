const std = @import("std");
const Step = std.Build.Step;
const Compile = Step.Compile;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const example = b.option([]const u8, "example", "The path to a .flow file to be executed. Useful for compiler development");
    const use_stderr = b.option(bool, "stderr", "Output custom errors to StdErr instead of NullWriter (Only used in tests)") orelse false;
    const run_with_debug = b.option(bool, "debug", "Enable all trace and debugging options for the Runtime") orelse false;
    const dump_bytecode = b.option(bool, "dump", "Dump the Bytecode instead of running the VM") orelse false;
    const trace_stack = b.option(bool, "trace-stack", "Trace the Stack on running") orelse run_with_debug;
    const trace_bytecode = b.option(bool, "trace-bytecode", "Trace the Bytecode on running") orelse run_with_debug;
    const trace_memory = b.option(bool, "trace-memory", "Trace the Memory allocations and frees") orelse run_with_debug;

    const debug_options = b.addOptions();
    debug_options.addOption(bool, "dump", dump_bytecode);
    debug_options.addOption(bool, "stack", trace_stack);
    debug_options.addOption(bool, "bytecode", trace_bytecode);
    debug_options.addOption(bool, "memory", trace_memory);

    // Shared code between compiler and runtime
    // such as value types and representations
    const shared = b.addModule("shared", .{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/shared/root.zig"),
    });
    shared.addOptions("module_debug_options", debug_options);

    // Compiler executable
    const compiler = b.addExecutable(.{
        .name = "compiler",
        .root_source_file = b.path("src/compiler/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    compiler.root_module.addOptions("debug_options", debug_options);
    compiler.root_module.addImport("shared", shared);
    b.installArtifact(compiler);

    // Runtime executable
    const runtime = b.addExecutable(.{
        .name = "runtime",
        .root_source_file = b.path("src/runtime/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    runtime.root_module.addOptions("debug_options", debug_options);
    runtime.root_module.addImport("shared", shared);
    // Add empty file as byte-code input, to allow compilation in lsp check step
    runtime.root_module.addAnonymousImport("input", .{
        .root_source_file = b.addWriteFiles().add("dummy", ""),
    });
    b.installArtifact(runtime);

    // Unit tests in compiler and runtime
    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/testing.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_unit_tests.root_module.addImport("shared", shared);
    // Option to output compiler errors
    const test_options = b.addOptions();
    test_options.addOption(bool, "use_stderr", use_stderr);
    exe_unit_tests.root_module.addOptions("testing_options", test_options);
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Step to run tests
    const test_step = b.step("unit", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    // Integration tests
    const exe_integration_tests = b.addExecutable(.{
        .name = "integration_test",
        .root_source_file = b.path("tests/main.zig"),
        .optimize = .Debug,
        .target = target,
    });
    const integration_tests = b.step("test", "Run integration tests");
    integration_tests.dependOn(&exe_integration_tests.step);

    // Check step for lsp compile errors
    const check_step = b.step("check", "Check Step for LSP");
    check_step.dependOn(test_step);
    check_step.dependOn(&compiler.step);
    check_step.dependOn(&runtime.step);

    // Example compilation
    if (example) |path| {
        const flow_out = compileImpl(b, .{
            .name = "flow_out",
            .source = b.path(path),
            .target = target,
            .optimize = optimize,
        }, compiler, runtime);

        const run_flow = b.addRunArtifact(flow_out);
        b.getInstallStep().dependOn(&run_flow.step);
    }
}

const CompileOptions = struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode = .ReleaseSafe,
    name: []const u8,
    source: std.Build.LazyPath,
};

pub fn compile(
    b: *std.Build,
    options: CompileOptions,
) *Compile {
    const flow = b.dependency("flow", .{
        .target = options.target,
        .optimize = options.optimize,
    });
    const compiler = flow.artifact("compiler");
    const runtime = flow.artifact("runtime");
    runtime.name = options.name;
    runtime.out_filename = options.name;

    return compileImpl(b, options, compiler, runtime);
}

fn compileImpl(b: *std.Build, options: CompileOptions, compiler: *Compile, runtime: *Compile) *Compile {
    const compile_step = b.addRunArtifact(compiler);
    compile_step.addFileArg(options.source);
    const bytecode = compile_step.addOutputFileArg("out.f");

    runtime.root_module.addAnonymousImport("input", .{
        .root_source_file = bytecode,
    });

    return runtime;
}
