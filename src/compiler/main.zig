pub fn main() !void {
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var args = std.process.args();
    _ = args.next();

    const input = args.next() orelse return error.MissingArgument;

    const output = args.next() orelse return error.MissingArgument;

    const flow_source = try readFile(alloc, input);

    const tokens = try Scanner.scan(alloc, flow_source);

    const ast = try Parser.createAST(alloc, tokens);

    var sema: Sema = .init(alloc, ast);
    try sema.analyse();

    const bytecode = Compiler.compile(alloc, ast, &sema);

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
