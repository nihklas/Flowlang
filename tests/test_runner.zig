const MAX_TEST_CASE_SIZE = 1024 * 1024 * 1024; // 1 MB
const MAX_OUTPUT_BYTES = 1024 * 1024 * 1024; // 1 MB
const SPLIT_MARKER_LEN = 7;
const SPLIT_MARKER_STDOUT = "\n=====\n";
const SPLIT_MARKER_STDERR = "\n+++++\n";

comptime {
    if (SPLIT_MARKER_STDOUT.len != SPLIT_MARKER_LEN) @compileError("Split Marker for STDOUT has a wrong length");
    if (SPLIT_MARKER_STDERR.len != SPLIT_MARKER_LEN) @compileError("Split Marker for STDERR has a wrong length");
}

const OutputPipe = enum {
    stdout,
    stderr,
};

const Result = enum {
    success,
    failure,
    crash,
};

const SharedState = struct {
    mutex: std.Thread.Mutex,
    alloc: std.mem.Allocator,
    results: std.StringHashMapUnmanaged(Result),

    fn setResult(self: *SharedState, case: []const u8, result: Result) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        self.results.put(self.alloc, self.alloc.dupe(u8, case) catch unreachable, result) catch unreachable;
    }
};

pub fn main() u8 {
    var debug_allocator: std.heap.DebugAllocator(.{ .thread_safe = true }) = .init;
    const gpa = debug_allocator.allocator();
    defer _ = debug_allocator.deinit();

    var args = std.process.args();
    _ = args.skip(); // skip own program name

    const compiler = args.next() orelse {
        printStdErr("Missing Argument: compiler\n", .{});
        return 1;
    };

    const cases_dir = args.next() orelse {
        printStdErr("Missing Argument: cases directory\n", .{});
        return 1;
    };

    const file_filter = args.next() orelse "";

    var state: SharedState = .{
        .mutex = .{},
        .alloc = gpa,
        .results = .empty,
    };
    defer {
        var key_iter = state.results.keyIterator();
        while (key_iter.next()) |key| {
            state.alloc.free(key.*);
        }
        state.results.deinit(gpa);
    }

    runTests(gpa, cases_dir, file_filter, compiler, &state) catch return 1;

    printStdErr("\n======================================\n\n", .{});

    var stats: std.AutoHashMapUnmanaged(Result, u16) = .empty;
    defer stats.deinit(gpa);
    stats.ensureTotalCapacity(gpa, 3) catch return 1;

    stats.putAssumeCapacity(.success, 0);
    stats.putAssumeCapacity(.failure, 0);
    stats.putAssumeCapacity(.crash, 0);

    var iter = state.results.iterator();
    while (iter.next()) |entry| {
        const count = stats.getPtr(entry.value_ptr.*).?;
        count.* += 1;

        const icon = switch (entry.value_ptr.*) {
            .success => "✅",
            .failure => "❌",
            .crash => "💀",
        };

        printStdOut("{s} Test Case: {s}\n", .{ icon, entry.key_ptr.* });
    }

    printStdOut("\n{d} Tests Succeeded, {d} Tests Failed", .{ stats.get(.success).?, stats.get(.failure).? });
    if (stats.get(.crash).? > 0) {
        printStdOut(", {d} Tests crashed", .{stats.get(.crash).?});
    }
    printStdOut("\n", .{});

    if (stats.get(.failure).? > 0 or stats.get(.crash).? > 0) {
        return 1;
    }

    return 0;
}

fn runTests(gpa: std.mem.Allocator, cases_dir: []const u8, file_filter: []const u8, compiler: []const u8, state: *SharedState) !void {
    var test_dir = try std.fs.openDirAbsolute(cases_dir, .{ .iterate = true });
    defer test_dir.close();

    var pool: std.Thread.Pool = undefined;
    try pool.init(.{ .allocator = gpa });
    defer pool.deinit();

    var dir_iter = test_dir.iterate();
    while (try dir_iter.next()) |entry| {
        if (entry.kind != .file) continue;

        if (std.mem.indexOf(u8, entry.name, file_filter) == null) {
            continue;
        }

        try pool.spawn(workerFn, .{ test_dir, entry.name, compiler, state });
    }
}

fn workerFn(dir: std.fs.Dir, sub_path: []const u8, compiler: []const u8, state: *SharedState) void {
    var error_buf: std.ArrayListUnmanaged(u8) = .empty;
    defer error_buf.deinit(state.alloc);
    const error_writer = error_buf.writer(state.alloc);

    doTest(error_writer, dir, sub_path, compiler, state) catch |err| {
        const result: Result = switch (err) {
            TestError.TestFailed => .failure,
            else => .crash,
        };

        printStdErr("\n======================================\n", .{});
        printStdErr("Test Case {s} {s}:", .{
            sub_path,
            if (result == .failure) "failed" else "crashed",
        });
        printStdErr("\n======================================\n", .{});
        printStdErr("{s}\n", .{error_buf.items});

        state.setResult(sub_path, result);
    };
}

const TestError = error{
    TestFailed,
    WrongAssertion,
    MissingAssertion,
    ExecutionInterrupted,
};

fn doTest(error_writer: anytype, dir: std.fs.Dir, sub_path: []const u8, compiler: []const u8, state: *SharedState) !void {
    var file = try dir.openFile(sub_path, .{});
    defer file.close();

    const content = try file.readToEndAlloc(state.alloc, MAX_TEST_CASE_SIZE);
    defer state.alloc.free(content);

    const maybe_stdout = std.mem.indexOf(u8, content, SPLIT_MARKER_STDOUT);
    const maybe_stderr = std.mem.indexOf(u8, content, SPLIT_MARKER_STDERR);

    if (maybe_stdout != null and maybe_stderr != null) {
        printTo(error_writer, "In test case {s} there are both stdout and stderr assertions. Only one at a time can be applied.\n", .{sub_path});
        return TestError.WrongAssertion;
    }

    const assertions_start, const output: OutputPipe = blk: {
        if (maybe_stdout) |stdout| {
            break :blk .{ stdout, .stdout };
        }
        if (maybe_stderr) |stderr| {
            break :blk .{ stderr, .stderr };
        }

        printTo(error_writer, "In test case {s} are no assertions.\n", .{sub_path});
        return TestError.MissingAssertion;
    };

    const source = std.mem.trim(u8, content[0..assertions_start], " \n");
    const assertion = content[assertions_start + SPLIT_MARKER_LEN ..];

    const tmp_file_path = try std.fmt.allocPrint(state.alloc, "/tmp/{s}", .{sub_path});
    defer state.alloc.free(tmp_file_path);

    {
        const source_file = try std.fs.createFileAbsolute(tmp_file_path, .{});
        defer source_file.close();
        try source_file.writeAll(source);
    }

    var child = std.process.Child.init(&.{ compiler, "--run", tmp_file_path }, state.alloc);
    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Pipe;

    var stdout: std.ArrayListUnmanaged(u8) = .empty;
    defer stdout.deinit(state.alloc);

    var stderr: std.ArrayListUnmanaged(u8) = .empty;
    defer stderr.deinit(state.alloc);

    try child.spawn();
    try child.collectOutput(state.alloc, &stdout, &stderr, MAX_OUTPUT_BYTES);
    const term = try child.wait();

    if (term != .Exited) {
        printTo(error_writer, "Test Case {s} got interrupted\n", .{sub_path});
        return TestError.ExecutionInterrupted;
    }

    if (term.Exited != 0) {
        if (output != .stderr) {
            printTo(error_writer, "Expected the test program to succeed, but it failed. TestCase: {s}\n", .{sub_path});
            printTo(error_writer, "Error Output:\n{s}\n\n", .{stderr.items});
            return TestError.TestFailed;
        }
        try expectEqualStrings(error_writer, assertion, stderr.items);
    } else {
        if (output != .stdout) {
            printTo(error_writer, "Expected the test program to fail, but it succeeded. TestCase: {s}\n", .{sub_path});
            printTo(error_writer, "Succesfull Output:\n{s}\n\n", .{stdout.items});
            return TestError.TestFailed;
        }
        try expectEqualStrings(error_writer, assertion, stdout.items);
    }

    state.setResult(sub_path, .success);
}

fn printStdOut(comptime fmt: []const u8, args: anytype) void {
    stdout_writer.print(fmt, args) catch unreachable;
}

fn printStdErr(comptime fmt: []const u8, args: anytype) void {
    stderr_writer.print(fmt, args) catch unreachable;
}

fn printTo(writer: anytype, comptime fmt: []const u8, args: anytype) void {
    writer.print(fmt, args) catch unreachable;
}

fn expectEqualStrings(writer: anytype, expected: []const u8, actual: []const u8) TestError!void {
    if (std.mem.indexOfDiff(u8, actual, expected)) |diff_index| {
        printTo(writer, "\n------ expected this output: ---------\n", .{});
        printWithVisibleNewlines(writer, expected);
        printTo(writer, "\n-------- instead found this: ---------\n", .{});
        printWithVisibleNewlines(writer, actual);
        printTo(writer, "\n--------------------------------------\n", .{});

        var diff_line_number: usize = 1;
        for (expected[0..diff_index]) |value| {
            if (value == '\n') diff_line_number += 1;
        }
        printTo(writer, "First difference occurs on line {d}:\n", .{diff_line_number});

        printTo(writer, "expected:\n", .{});
        printIndicatorLine(writer, expected, diff_index);

        printTo(writer, "found:\n", .{});
        printIndicatorLine(writer, actual, diff_index);

        return TestError.TestFailed;
    }
}

fn printIndicatorLine(writer: anytype, source: []const u8, indicator_index: usize) void {
    const line_begin_index = if (std.mem.lastIndexOfScalar(u8, source[0..indicator_index], '\n')) |line_begin|
        line_begin + 1
    else
        0;
    const line_end_index = if (std.mem.indexOfScalar(u8, source[indicator_index..], '\n')) |line_end|
        (indicator_index + line_end)
    else
        source.len;

    printLine(writer, source[line_begin_index..line_end_index]);
    for (line_begin_index..indicator_index) |_|
        printTo(writer, " ", .{});
    if (indicator_index >= source.len)
        printTo(writer, "^ (end of string)\n", .{})
    else
        printTo(writer, "^ ('\\x{x:0>2}')\n", .{source[indicator_index]});
}

fn printWithVisibleNewlines(writer: anytype, source: []const u8) void {
    var i: usize = 0;
    while (std.mem.indexOfScalar(u8, source[i..], '\n')) |nl| : (i += nl + 1) {
        printLine(writer, source[i..][0..nl]);
    }
    printTo(writer, "{s}␃\n", .{source[i..]}); // End of Text symbol (ETX)
}

fn printLine(writer: anytype, line: []const u8) void {
    if (line.len != 0) switch (line[line.len - 1]) {
        ' ', '\t' => return printTo(writer, "{s}⏎\n", .{line}), // Return symbol
        else => {},
    };
    printTo(writer, "{s}\n", .{line});
}

var print_mutex: std.Thread.Mutex = .{};
const stdout_writer = std.io.getStdOut().writer();
const stderr_writer = std.io.getStdErr().writer();

const std = @import("std");
