const MAX_TEST_CASE_SIZE = 1024 * 1024 * 1024; // 1 MB
const MAX_OUTPUT_BYTES = 1024 * 1024 * 1024; // 1 MB
const SPLIT_MARKER_LEN = 7;
const SPLIT_MARKER_STDOUT = "\n=====\n";
const SPLIT_MARKER_STDERR = "\n+++++\n";

comptime {
    if (SPLIT_MARKER_STDOUT.len != SPLIT_MARKER_LEN) @compileError("Split Marker for STDOUT has a wrong length");
    if (SPLIT_MARKER_STDERR.len != SPLIT_MARKER_LEN) @compileError("Split Marker for STDERR has a wrong length");
}

pub fn main() u8 {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    const gpa = debug_allocator.allocator();
    defer _ = debug_allocator.deinit();

    var args = std.process.args();
    _ = args.skip(); // skip own program name

    const compiler = args.next() orelse {
        std.debug.print("Missing Argument: compiler\n", .{});
        return 1;
    };

    const cases_dir = args.next() orelse {
        std.debug.print("Missing Argument: cases directory\n", .{});
        return 1;
    };

    if (args.next()) |case| {
        const case_path = std.fmt.allocPrint(gpa, "{s}/{s}.flow", .{ cases_dir, case }) catch return 1;
        defer gpa.free(case_path);

        const case_file = std.fs.openFileAbsolute(case_path, .{}) catch |err| switch (err) {
            error.FileNotFound => {
                std.debug.print("Test Case not found: {s}\n", .{case_path});
                return 1;
            },
            else => {
                std.debug.print("Error on file opening: {}\n", .{err});
                return 1;
            },
        };
        defer case_file.close();

        var test_case = buildTestCase(gpa, case_file, case) catch return 1;
        defer test_case.free(gpa);

        runTestCase(gpa, compiler, test_case) catch return 1;
    } else {
        const root_node = std.Progress.start(.{});
        _ = root_node;
    }

    return 0;
}

const OutputPipe = enum {
    stdout,
    stderr,
};
const TestCase = struct {
    tmp_file_path: []const u8,
    name: []const u8,
    assertion: []const u8,
    output: OutputPipe,

    fn free(self: *TestCase, gpa: std.mem.Allocator) void {
        gpa.free(self.name);
        gpa.free(self.assertion);
        gpa.free(self.tmp_file_path);
    }
};

fn buildTestCase(gpa: std.mem.Allocator, case: std.fs.File, case_name: []const u8) !TestCase {
    const content = try case.readToEndAlloc(gpa, MAX_TEST_CASE_SIZE);
    defer gpa.free(content);

    const maybe_stdout = std.mem.indexOf(u8, content, SPLIT_MARKER_STDOUT);
    const maybe_stderr = std.mem.indexOf(u8, content, SPLIT_MARKER_STDERR);

    if (maybe_stdout != null and maybe_stderr != null) {
        std.debug.print("In test case {s} there are both stdout and stderr assertions. Only one at a time can be applied.\n", .{case_name});
        return error.WrongAssertions;
    }

    const assertions_start, const output: OutputPipe = blk: {
        if (maybe_stdout) |stdout| {
            break :blk .{ stdout, .stdout };
        }
        if (maybe_stderr) |stderr| {
            break :blk .{ stderr, .stderr };
        }

        std.debug.print("In test case {s} are no assertions.\n", .{case_name});
        return error.MissingAssertions;
    };

    const source = std.mem.trim(u8, content[0..assertions_start], " \n");
    const assertion = content[assertions_start + SPLIT_MARKER_LEN ..];

    const tmp_file_path = try std.fmt.allocPrint(gpa, "/tmp/{s}.flow", .{case_name});

    {
        const source_file = try std.fs.createFileAbsolute(tmp_file_path, .{});
        defer source_file.close();
        try source_file.writeAll(source);
    }

    return .{
        .name = try gpa.dupe(u8, case_name),
        .assertion = try gpa.dupe(u8, assertion),
        .output = output,
        .tmp_file_path = tmp_file_path,
    };
}

fn runTestCase(gpa: std.mem.Allocator, compiler: []const u8, test_case: TestCase) !void {
    var child = std.process.Child.init(&.{ compiler, "--run", test_case.tmp_file_path }, gpa);
    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Pipe;

    var stdout: std.ArrayListUnmanaged(u8) = .empty;
    defer stdout.deinit(gpa);

    var stderr: std.ArrayListUnmanaged(u8) = .empty;
    defer stderr.deinit(gpa);

    try child.spawn();
    try child.collectOutput(gpa, &stdout, &stderr, MAX_OUTPUT_BYTES);
    const term = try child.wait();

    if (term != .Exited) {
        std.debug.print("Test Case got interrupted\n", .{});
        return error.ExecutionInterrupted;
    }

    if (term.Exited != 0) {
        if (test_case.output != .stderr) {
            std.debug.print("Expected the test program to succeed, but it failed. TestCase: {s}\n", .{test_case.name});
            std.debug.print("Error Output:\n{s}\n", .{stderr.items});
            std.debug.print("======================\n", .{});
            return error.TermMismatch;
        }
        try std.testing.expectEqualStrings(test_case.assertion, stderr.items);
    } else {
        if (test_case.output != .stdout) {
            std.debug.print("Expected the test program to fail, but it succeeded. TestCase: {s}\n", .{test_case.name});
            std.debug.print("Succesfull Output:\n{s}\n", .{stdout.items});
            std.debug.print("======================\n", .{});
            return error.TermMismatch;
        }
        try std.testing.expectEqualStrings(test_case.assertion, stdout.items);
    }
}

const std = @import("std");
