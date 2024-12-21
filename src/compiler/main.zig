pub fn main() !void {
    var gpa_state: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    var arena_state: std.heap.ArenaAllocator = .init(gpa_state.allocator());
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var args = std.process.args();
    _ = args.next();

    const input = args.next() orelse return error.MissingArgument;

    const output = args.next() orelse return error.MissingArgument;

    const flow_source = try readFile(gpa, input);
    defer gpa.free(flow_source);

    const tokens = try Scanner.scan(gpa, flow_source);
    defer gpa.free(tokens);

    const ast = try Parser.createAST(arena, tokens);

    var sema: Sema = .init(gpa, ast);
    defer sema.deinit();
    try sema.analyse();

    const bytecode = Compiler.compile(gpa, ast, &sema);
    defer gpa.free(bytecode);

    const output_file = try std.fs.cwd().createFile(output, .{});
    defer output_file.close();

    try output_file.writeAll(bytecode);
}

fn readFile(alloc: Allocator, path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    return try file.readToEndAlloc(alloc, 1 * 1024 * 1024);
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("Token.zig");
const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const Compiler = @import("Compiler.zig");
const Sema = @import("Sema.zig");
