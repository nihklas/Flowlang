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
    const trace_stack = b.option(bool, "trace-stack", "Trace the Stack on running") orelse run_with_debug;
    const trace_bytecode = b.option(bool, "trace-bytecode", "Trace the Bytecode on running") orelse run_with_debug;
    const trace_memory = b.option(bool, "trace-memory", "Trace the Memory allocations and frees") orelse run_with_debug;
    const stress_gc = b.option(bool, "gc-stress", "Enable Garbage Collection on every Allocation") orelse false;
    const initial_gc_threshold = b.option(usize, "gc-thresh", "Initial Threshold on which the GC kicks in (bytes)") orelse 1024 * 1024;
    const gc_growth_factor = b.option(usize, "gc-growth", "Factor by which the threshold is determined") orelse 2;
    const integration_test_case = b.option([]const u8, "case", "Specific integration test case to run");

    const extension_options = b.addOptions();
    extension_options.addOption(bool, "enabled", false);

    // Option to output compiler errors in tests
    const test_options = b.addOptions();
    test_options.addOption(bool, "use_stderr", use_stderr);

    const compiler = buildCompiler(b, b, .{
        .target = target,
        .optimize = optimize,
        .debug = .{
            .trace_stack = trace_stack,
            .trace_bytecode = trace_bytecode,
            .trace_memory = trace_memory,
            .stress_gc = stress_gc,
        },
        .vm = .{
            .initial_gc_threshold = initial_gc_threshold,
            .gc_growth_factor = gc_growth_factor,
        },
    });

    // Unit tests in compiler and runtime
    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/testing.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_unit_tests.root_module.addImport("shared", buildShared(b, .{ .target = target, .optimize = optimize }));
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

    const run_compiler = b.addRunArtifact(compiler);
    const compile_step = b.step("compile", "Run the compiler, pass --help to get more information");
    compile_step.dependOn(&run_compiler.step);

    if (b.args) |args| {
        run_compiler.addArgs(args);
    }
}

const ExtensionOptions = struct {
    modules: []const struct { name: []const u8, module: *Module },
    exports_file: LazyPath,
};

const CompilerOptions = struct {
    extensions: ?ExtensionOptions = null,
    target: Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    debug: struct {
        trace_stack: bool = false,
        trace_bytecode: bool = false,
        trace_memory: bool = false,
        stress_gc: bool = false,
    } = .{},
    vm: struct {
        initial_gc_threshold: usize = 1024 * 1024,
        gc_growth_factor: u8 = 2,
    } = .{},
};

pub fn buildCompiler(b: *Build, flow_builder: *Build, compile_options: CompilerOptions) *Compile {
    const shared = buildShared(flow_builder, compile_options);

    const debug_options = flow_builder.addOptions();
    debug_options.addOption(bool, "stack", compile_options.debug.trace_stack);
    debug_options.addOption(bool, "bytecode", compile_options.debug.trace_bytecode);
    debug_options.addOption(bool, "memory", compile_options.debug.trace_memory);
    debug_options.addOption(bool, "stress_gc", compile_options.debug.stress_gc);

    const flow_std = flow_builder.addModule("flow_std", .{
        .root_source_file = flow_builder.path("src/std/stdlib.zig"),
        .target = compile_options.target,
        .optimize = compile_options.optimize,
    });

    // Runtime
    const runtime_mod = flow_builder.addModule("runtime", .{
        .root_source_file = flow_builder.path("src/runtime/main.zig"),
        .target = compile_options.target,
        .optimize = compile_options.optimize,
    });
    runtime_mod.addImport("debug_options", debug_options.createModule());

    const runtime = flow_builder.addExecutable(.{
        .name = "runtime",
        .root_module = runtime_mod,
    });

    // Compiler
    const compiler_mod = flow_builder.addModule("compiler", .{
        .root_source_file = flow_builder.path("src/compiler/main.zig"),
        .target = compile_options.target,
        .optimize = compile_options.optimize,
    });
    compiler_mod.addAnonymousImport("runtime_bin", .{ .root_source_file = runtime.getEmittedBin() });
    compiler_mod.addImport("debug_options", debug_options.createModule());
    compiler_mod.addImport("runtime", runtime_mod);

    const compiler = flow_builder.addExecutable(.{
        .name = "compiler",
        .root_module = compiler_mod,
    });

    b.installArtifact(compiler);
    compiler.step.dependOn(&runtime.step);

    flow_std.addImport("shared", shared);
    shared.addImport("flow_std", flow_std);
    compiler_mod.addImport("shared", shared);
    runtime_mod.addImport("shared", shared);

    return compiler;
}

fn buildShared(b: *Build, compile_options: CompilerOptions) *Module {
    const shared = b.addModule("shared", .{
        .root_source_file = b.path("src/shared/root.zig"),
        .target = compile_options.target,
        .optimize = compile_options.optimize,
    });

    const ext_opts = b.addOptions();
    ext_opts.addOption(bool, "enabled", compile_options.extensions != null);

    if (compile_options.extensions) |extension_options| {
        const extension = b.addModule("flow_ext", .{
            .target = compile_options.target,
            .optimize = compile_options.optimize,
            .root_source_file = extension_options.exports_file,
        });

        for (extension_options.modules) |mod| {
            mod.module.addImport("shared", shared);
            extension.addImport(mod.name, mod.module);
        }

        shared.addImport("flow_ext", extension);
    }

    shared.addOptions("extensions", ext_opts);
    return shared;
}
