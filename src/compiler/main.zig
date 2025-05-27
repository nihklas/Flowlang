pub fn main() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    const gpa, const is_debug = switch (@import("builtin").mode) {
        .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
        .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
    };
    defer if (is_debug) {
        _ = debug_allocator.deinit();
    };

    var arena_state: std.heap.ArenaAllocator = .init(gpa);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const cli_opts = cli.parse();

    const flow_source = try readFile(gpa, cli_opts.source);
    defer gpa.free(flow_source);

    const tokens = try Scanner.scan(gpa, flow_source);
    defer gpa.free(tokens);

    const ast = try Parser.createAST(arena, tokens);
    if (cli_opts.dump_ast) {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(gpa);

        const writer = buf.writer(gpa);
        try ASTDumper.dump(writer, ast);
        try writer.writeByte('\n');

        try writeOutput(buf.items, cli_opts);
        return;
    }

    var sema: Sema = .init(gpa, ast);
    defer sema.deinit();
    try sema.analyse();

    // TODO: Optimisatios
    // - Dead Code Elimination
    // - Constants evaluation

    var fir: FIR = .fromAST(gpa, ast);
    defer fir.deinit();

    if (cli_opts.dump_fir) {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(gpa);

        const writer = buf.writer(gpa);
        try FIRDumper.dump(writer, &fir);
        try writer.writeByte('\n');

        try writeOutput(buf.items, cli_opts);
        return;
    }

    // var compiler: Compiler = .init(gpa, ast);
    // const bytecode = compiler.compile();
    // defer gpa.free(bytecode);
    //
    // if (cli_opts.dump_bc) {
    //     var buf: std.ArrayListUnmanaged(u8) = .empty;
    //     defer buf.deinit(gpa);
    //
    //     const writer = buf.writer(gpa);
    //     BytecodeDumper.dump(writer.any(), bytecode);
    //
    //     try writeOutput(buf.items, cli_opts);
    //     return;
    // }
    //
    // const file = try std.fs.cwd().createFile(cli_opts.output, .{});
    // defer file.close();
    //
    // try file.chmod(0o755);
    //
    // try file.writeAll(vm);
    // try file.writeAll(bytecode);
    //
    // const bytecode_len: [8]u8 = @bitCast(bytecode.len);
    // try file.writeAll(&bytecode_len);
}

fn writeOutput(output: []const u8, cli_opts: cli.Options) !void {
    if (cli_opts.dump_stdout) {
        try std.io.getStdOut().writeAll(output);
        return;
    }

    const file = try std.fs.cwd().createFile(cli_opts.output, .{});
    defer file.close();

    try file.writeAll(output);
}

fn readFile(alloc: Allocator, path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    return try file.readToEndAlloc(alloc, 1 * 1024 * 1024); // 1 GB
}

const debug_options = @import("debug_options");
const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("ir/Token.zig");
const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const Compiler = @import("Compiler.zig");
const Sema = @import("Sema.zig");
const FIR = @import("ir/FIR.zig");

const cli = @import("util/cli.zig");

const vm = @embedFile("runtime");

const ASTDumper = @import("debug/ASTDumper.zig");
const FIRDumper = @import("debug/FIRDumper.zig");
const BytecodeDumper = @import("shared").debug.BytecodeDumper;
