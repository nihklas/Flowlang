pub fn main() u8 {
    // TODO: more meaningful return codes?
    run() catch return 1;
    return 0;
}

fn run() !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    const gpa, const is_debug = switch (@import("builtin").mode) {
        .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
        .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
    };
    defer if (is_debug) {
        _ = debug_allocator.deinit();
    };

    const cli_opts = cli.parse();

    const flow_source = try readFile(gpa, cli_opts.source);
    defer gpa.free(flow_source);

    const code = try compile(gpa, flow_source, cli_opts) orelse return;
    defer gpa.free(code);

    if (cli_opts.run) {
        return runtime.run(gpa, code);
    }

    const file = try std.fs.cwd().createFile(cli_opts.output, .{});
    defer file.close();

    try file.chmod(0o755);

    try file.writeAll(vm);
    try file.writeAll(code);

    const bytecode_len: [8]u8 = @bitCast(code.len);
    try file.writeAll(&bytecode_len);
}

pub fn compile(gpa: Allocator, flow_source: []const u8, cli_opts: cli.Options) !?[]const u8 {
    var arena_state: std.heap.ArenaAllocator = .init(gpa);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const tokens = try Scanner.scan(gpa, flow_source);
    defer gpa.free(tokens);

    const ast = try Parser.createAST(arena, tokens);
    if (cli_opts.dump_ast) {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(gpa);

        const writer = buf.writer(gpa);
        try ASTDumper.dump(writer, ast);
        try writer.writeByte('\n');

        try dump(buf.items);
        return null;
    }

    var sema: Sema = .init(gpa, ast);
    defer sema.deinit();
    try sema.analyse();

    if (cli_opts.check) {
        return null;
    }

    var fir: FIR = .fromAST(gpa, ast);
    defer fir.deinit();

    var optimizer: Optimizer = .init(gpa, &fir);
    defer optimizer.deinit();

    if (cli_opts.constant_folding) {
        optimizer.constantFolding();
    }

    if (cli_opts.dump_fir) {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(gpa);

        const writer = buf.writer(gpa);
        try FIRDumper.dump(writer, &fir);
        try writer.writeByte('\n');

        try dump(buf.items);
        return null;
    }

    var compiler: Compiler = .init(gpa, &fir);
    defer compiler.deinit();

    const bytecode = compiler.compile();

    if (cli_opts.dump_bc) {
        defer gpa.free(bytecode);

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(gpa);

        const writer = buf.writer(gpa);
        BytecodeDumper.dump(writer.any(), bytecode);

        try dump(buf.items);
        return null;
    }

    return bytecode;
}

fn dump(output: []const u8) !void {
    try std.io.getStdOut().writeAll(output);
}

fn readFile(alloc: Allocator, path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    return try file.readToEndAlloc(alloc, 1 * 1024 * 1024); // 1 MB
}

pub const runtime = @import("runtime");

const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("ir/Token.zig");
const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const Compiler = @import("Compiler.zig");
const Sema = @import("Sema.zig");
const FIR = @import("ir/FIR.zig");
const Optimizer = @import("Optimizer.zig");

const cli = @import("util/cli.zig");

const vm = @embedFile("runtime_bin");

const ASTDumper = @import("debug/ASTDumper.zig");
const FIRDumper = @import("debug/FIRDumper.zig");
const BytecodeDumper = @import("shared").debug.BytecodeDumper;
