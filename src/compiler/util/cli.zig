pub const Options = struct {
    source: []const u8,
    output: []const u8,
    check: bool = false,
    dump_bc: bool = false,
    dump_ast: bool = false,
    dump_fir: bool = false,
    constant_folding: bool = true,
    run: bool = false,
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
                else => {
                    std.debug.print("Error: Unexpected argument: {s}\n", .{argument});
                    printHelpAndQuit(1);
                },
            }

            argument_counter += 1;
            continue;
        }

        if (std.mem.eql(u8, argument, "--dump-bc")) {
            options.dump_bc = true;
        } else if (std.mem.eql(u8, argument, "--dump-ast")) {
            options.dump_ast = true;
        } else if (std.mem.eql(u8, argument, "--dump-fir")) {
            options.dump_fir = true;
        } else if (std.mem.eql(u8, argument, "--check")) {
            options.check = true;
        } else if (std.mem.eql(u8, argument, "--run")) {
            options.run = true;
        } else if (std.mem.eql(u8, argument, "--no-constant-folding")) {
            options.constant_folding = false;
        } else if (std.mem.eql(u8, argument, "--no-color")) {
            @import("error_reporter.zig").colored_output = false;
        } else if (std.mem.eql(u8, argument, "--help")) {
            printHelpAndQuit(0);
        } else {
            std.io.getStdErr().writer().print("Unrecognized option: {s}\n\n", .{argument}) catch {};
            printHelpAndQuit(1);
        }
    }

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
        \\        --dump-bc               Dump the resulting Bytecode into the output File 
        \\                                instead of building an executable
        \\        --dump-ast              Dump the resulting Abstract Syntax Tree to the output File
        \\                                instead of building an executable
        \\        --dump-fir              Dump the resulting FIR (Flowlang Intermediate Representation)
        \\                                to the output File instead of building an executable
        \\        --check                 Go through Semantic Analysis and quit
        \\        --run                   Run the compiled code directly
        \\        --no-color              Turns off colored error messages, useful for testing or 
        \\                                running in terminals that display the error messages wrongly
        \\
        \\        --no-constant-folding   Turn off Constant folding optimzations
        \\
        \\        --help                  Print this message
        \\
        \\    Arguments
        \\        SOURCE                  Path to the .flow Source file
        \\        OUTPUT                  Path to the target output file, 
        \\                                default is filename of the SOURCE argument without extension
        \\
    ;

    std.io.getStdErr().writeAll(help) catch {};

    std.posix.exit(exit_code);
}

const std = @import("std");
