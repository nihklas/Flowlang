pub const Options = struct {
    source: []const u8,
    output: []const u8,
    dump_bc: bool = debug_options.dump_bc,
    dump_ast: bool = debug_options.dump_ast,
    dump_stdout: bool = false,
};

pub fn parse() Options {
    var args = std.process.args();
    _ = args.next(); // skip program name

    var argument_counter: usize = 0;
    var input_arg: ?[]const u8 = null;
    var output_arg: ?[]const u8 = null;

    var options: Options = .{
        .source = undefined,
        .output = undefined,
    };

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
            options.dump_bc = true;
        } else if (std.mem.eql(u8, argument, "--dump-ast")) {
            options.dump_ast = true;
        } else if (std.mem.eql(u8, argument, "--help")) {
            printHelpAndQuit(0);
        } else if (std.mem.eql(u8, argument, "--stdout")) {
            options.dump_stdout = true;
        }
    }
    options.dump_stdout = options.dump_stdout and (options.dump_bc or options.dump_ast);

    options.source = input_arg orelse printHelpAndQuit(1);
    options.output = output_arg orelse out: {
        // Use only the base filename of the input file
        const basename = std.fs.path.basename(options.source);
        break :out basename[0 .. basename.len - std.fs.path.extension(options.source).len];
    };

    return options;
}

pub fn printHelpAndQuit(exit_code: u8) noreturn {
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

const std = @import("std");
const debug_options = @import("debug_options");
