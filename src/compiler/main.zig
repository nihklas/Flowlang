pub fn main() !void {
    var gpa_state: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    var arena_state: std.heap.ArenaAllocator = .init(gpa_state.allocator());
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

    // const bytecode = Compiler.compile(gpa, ast, &sema);
    // defer gpa.free(bytecode);
    //
    // if (dump_bc) {
    //     BytecodeDumper.dump(output_writer.any(), bytecode);
    //     return;
    // }
    //
    // try output_file.chmod(0o755);
    //
    // try output_file.writeAll(vm);
    // try output_file.writeAll(bytecode);
    //
    // const bytecode_len: [8]u8 = @bitCast(bytecode.len);
    // try output_file.writeAll(&bytecode_len);
    //
    // try output_file.writeAll(bytecode);
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
const Token = @import("Token.zig");
const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const Compiler = @import("Compiler.zig");
const Sema = @import("Sema.zig");

const cli = @import("cli.zig");

const vm = @embedFile("runtime");

const ASTDumper = @import("debug/ASTDumper.zig");
const BytecodeDumper = @import("shared").debug.BytecodeDumper;
