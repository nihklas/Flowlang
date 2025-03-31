pub fn main() !void {
    var gpa_state: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    var arena_state: std.heap.ArenaAllocator = .init(gpa_state.allocator());
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var args = std.process.args();
    _ = args.next(); // skip program name

    var argument_counter: usize = 0;
    var input_arg: ?[]const u8 = null;
    var output_arg: ?[]const u8 = null;
    var dump_bc = debug_options.dump_bc;
    var dump_ast = debug_options.dump_ast;
    var dump_to_stdout = false;
    while (args.next()) |argument| {
        if (!std.mem.startsWith(u8, argument, "--")) {
            switch (argument_counter) {
                0 => input_arg = argument,
                1 => output_arg = argument,
                else => {},
            }

            argument_counter += 1;
            continue;
        }

        if (std.mem.eql(u8, argument, "--dump-bc")) {
            dump_bc = true;
        } else if (std.mem.eql(u8, argument, "--dump-ast")) {
            dump_ast = true;
        } else if (std.mem.eql(u8, argument, "--help")) {
            printHelpAndQuit(0);
        } else if (std.mem.eql(u8, argument, "--stdout")) {
            dump_to_stdout = true;
        }
    }

    const input = input_arg orelse printHelpAndQuit(1);
    const output = output_arg orelse out: {
        // Use only the base filename of the input file
        const basename = std.fs.path.basename(input);
        break :out basename[0 .. basename.len - std.fs.path.extension(input).len];
    };

    const output_file, const output_writer = blk: {
        if (dump_to_stdout) {
            break :blk .{ null, std.io.getStdOut().writer() };
        }
        const file = try std.fs.cwd().createFile(output, .{});
        break :blk .{ file, file.writer() };
    };
    defer if (output_file) |file| file.close();

    const flow_source = try readFile(gpa, input);
    defer gpa.free(flow_source);

    const tokens = try Scanner.scan(gpa, flow_source);
    defer gpa.free(tokens);

    const ast = try Parser.createAST(arena, tokens);
    if (dump_ast) {
        try ASTDumper.dump(output_writer, ast);
        try output_writer.writeByte('\n');
        return;
    }

    // var sema: Sema = .init(gpa, ast);
    // defer sema.deinit();
    // try sema.analyse();
    //
    // const bytecode = Compiler.compile(gpa, ast, &sema);
    // defer gpa.free(bytecode);
    //
    // if (dump_bc) {
    //     BytecodeDumper.dump(output_file.writer().any(), bytecode);
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

fn printHelpAndQuit(exit_code: u8) noreturn {
    const help =
        \\flow_compiler [SOURCE] [OUTPUT] (options)
        \\
        \\    Options
        \\        --dump-bc           Dump the resulting Bytecode into the output File 
        \\                            instead of building an executable
        \\        --dump-ast          Dump the resulting Abstract Syntax Tree to the output File
        \\                            instead of building an executable
        \\        --stdout            Dump output of the other dump options are printed to stdout 
        \\                            instead of the output file
        \\        --help              Print this message
        \\
        \\    Arguments
        \\        SOURCE              Path to the .flow Source file
        \\        OUTPUT              Path to the target output file, 
        \\                            default is filename of the SOURCE argument without extension
        \\
    ;

    std.io.getStdErr().writeAll(help) catch {};

    std.posix.exit(exit_code);
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

const vm = @embedFile("runtime");

const ASTDumper = @import("debug/ASTDumper.zig");
const BytecodeDumper = @import("shared").debug.BytecodeDumper;
