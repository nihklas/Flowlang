const std = @import("std");
const Step = std.Build.Step;
const Compile = Step.Compile;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const use_stderr = b.option(bool, "stderr", "Output custom errors to StdErr instead of NullWriter (Only used in tests)") orelse false;
    const dump_bytecode = b.option(bool, "dump", "Dump the Bytecode instead of running the VM") orelse false;
    const trace_stack = b.option(bool, "trace-stack", "Trace the Stack on running") orelse false;
    const trace_bytecode = b.option(bool, "trace-bytecode", "Trace the Bytecode on running") orelse false;

    const debug_options = b.addOptions();
    debug_options.addOption(bool, "dump", dump_bytecode);
    debug_options.addOption(bool, "stack", trace_stack);
    debug_options.addOption(bool, "bytecode", trace_bytecode);

    const shared = b.addModule("shared", .{
        .target = target,
        .optimize = optimize,
        .root_source_file = b.path("src/shared/root.zig"),
    });
    shared.addOptions("module_debug_options", debug_options);

    const compiler = b.addExecutable(.{
        .name = "compiler",
        .root_source_file = b.path("src/compiler/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    compiler.root_module.addOptions("debug_options", debug_options);
    compiler.root_module.addImport("shared", shared);

    b.installArtifact(compiler);

    const runtime = b.addExecutable(.{
        .name = "runtime",
        .root_source_file = b.path("src/runtime/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    runtime.root_module.addOptions("debug_options", debug_options);
    runtime.root_module.addImport("shared", shared);
    runtime.root_module.addAnonymousImport("input", .{
        .root_source_file = b.addWriteFiles().add("dummy", ""),
    });

    b.installArtifact(runtime);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/testing.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe_unit_tests.root_module.addImport("shared", shared);

    const test_options = b.addOptions();
    test_options.addOption(bool, "use_stderr", use_stderr);

    exe_unit_tests.root_module.addOptions("testing_options", test_options);
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const check_step = b.step("check", "Check Step for LSP");
    check_step.dependOn(test_step);
    check_step.dependOn(&compiler.step);
    check_step.dependOn(&runtime.step);

    const flow_out = compileImpl(b, .{
        .name = "flow_out",
        .source = b.path("example/src/main.flow"),
        .target = target,
        .optimize = optimize,
    }, compiler, runtime);
    const run_flow = b.addRunArtifact(flow_out);
    const run_step = b.step("run", "Run the complete compiler pipeline on an example .flow file and execute the resulting binary");
    run_step.dependOn(&run_flow.step);
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
