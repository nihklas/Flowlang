// TODO: Use correct stdout and stderr
pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer std.debug.assert(gpa.deinit() == .ok);
    const alloc = gpa.allocator();

    var args = std.process.args();
    _ = args.next();

    const input = args.next() orelse return error.MissingArgument;

    const output = args.next() orelse return error.MissingArgument;

    const flow_source = try readFile(alloc, input);
    defer alloc.free(flow_source);

    const tokens = try Scanner.scan(alloc, flow_source);
    defer alloc.free(tokens);

    const ast = try Parser.createAST(alloc, tokens);
    defer alloc.free(ast);
    defer for (ast) |node| node.destroy(alloc);

    const bytecode = try Compiler.compile(alloc, ast);
    defer alloc.free(bytecode);

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
