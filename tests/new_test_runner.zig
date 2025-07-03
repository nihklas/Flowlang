const MAX_TEST_CASE_SIZE = 1024 * 1024 * 1024; // 1 MB
const MAX_OUTPUT_BYTES = 1024 * 1024 * 1024; // 1 MB
const SPLIT_MARKER_LEN = 7;
const SPLIT_MARKER_STDOUT = "\n=====\n";
const SPLIT_MARKER_STDERR = "\n+++++\n";

comptime {
    if (SPLIT_MARKER_STDOUT.len != SPLIT_MARKER_LEN) @compileError("Split Marker for STDOUT has a wrong length");
    if (SPLIT_MARKER_STDERR.len != SPLIT_MARKER_LEN) @compileError("Split Marker for STDERR has a wrong length");
}

const Result = enum {
    success,
    failure,
    crash,
};

const SharedState = struct {
    mutex: std.Thread.Mutex,
    alloc: std.mem.Allocator,
    results: std.StringHashMapUnmanaged(Result),
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

    if (args.next()) |case| {
        const case_path = std.fmt.allocPrint(gpa, "{s}/{s}.flow", .{ cases_dir, case }) catch return 1;
        defer gpa.free(case_path);

        const case_file = std.fs.openFileAbsolute(case_path, .{}) catch |err| switch (err) {
            error.FileNotFound => {
                printStdErr("Test Case not found: {s}\n", .{case_path});
                return 1;
            },
            else => {
                printStdErr("Error on file opening: {}\n", .{err});
                return 1;
            },
        };
        defer case_file.close();

        var test_case = buildTestCase(gpa, case_file, case) catch return 1;
        defer test_case.free(gpa);

        runTestCase(gpa, compiler, test_case) catch return 1;
    } else {
        var state: SharedState = .{
            .mutex = .{},
            .alloc = gpa,
            .results = .empty,
        };
        defer state.results.deinit(gpa);

        // Pipeline should look like this:
        // - Iterate over all test cases
        // - Hand work for each file directly over to thread pool
        // - for each test case:
        //      - open and read the file
        //      - look for the split marks and split source code and assertions
        //      - run source code
        //      - perform assertions
        //      - acquire results_mutex and save result in the shared result state
        //      - on failure, aquire print_mutex and print out the failed assertions
        // - Print out statistics

        {
            var test_dir = std.fs.openDirAbsolute(cases_dir, .{ .iterate = true }) catch return 1;
            defer test_dir.close();

            var pool: std.Thread.Pool = undefined;
            pool.init(.{
                .allocator = gpa,
            }) catch return 1;
            defer pool.deinit();

            var dir_iter = test_dir.iterate();
            while (dir_iter.next() catch return 1) |entry| {
                if (entry.kind != .file) continue;

                pool.spawn(workerFn, .{ test_dir, entry.name, compiler, &state }) catch return 1;
            }
        }

        printStdOut("\n\n------------------------------\n\n", .{});

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

            // stdout.print("{s} ({s}){s} Test Case: {s}\n", .{
            //     icon,
            //     @tagName(entry.value_ptr.*),
            //     if (entry.value_ptr.* == .crash) "  " else "",
            //     entry.key_ptr.*,
            // }) catch return 1;
        }

        printStdOut("\n-----------------------------------\n{d} Tests Succeeded, {d} Tests Failed", .{ stats.get(.success).?, stats.get(.failure).? });
        if (stats.get(.crash).? > 0) {
            printStdOut(", {d} Tests crashed", .{stats.get(.crash).?});
        }
        printStdOut("\n", .{});

        if (stats.get(.failure).? > 0 or stats.get(.crash).? > 0) {
            return 1;
        }
    }

    return 0;
}

// Note: gets file and *state
fn workerFn(dir: std.fs.Dir, sub_path: []const u8, compiler: []const u8, state: *SharedState) void {
    doTest(dir, sub_path, compiler, state) catch |err| {
        const result: Result = switch (err) {
            error.TestFailed => .failure,
            else => .crash,
        };

        {
            state.mutex.lock();
            defer state.mutex.unlock();

            state.results.put(state.alloc, sub_path, result) catch return;
        }
    };
}

fn doTest(dir: std.fs.Dir, sub_path: []const u8, compiler: []const u8, state: *SharedState) !void {
    var file = try dir.openFile(sub_path, .{});
    defer file.close();

    const content = try file.readToEndAlloc(state.alloc, MAX_TEST_CASE_SIZE);
    defer state.alloc.free(content);

    const maybe_stdout = std.mem.indexOf(u8, content, SPLIT_MARKER_STDOUT);
    const maybe_stderr = std.mem.indexOf(u8, content, SPLIT_MARKER_STDERR);

    if (maybe_stdout != null and maybe_stderr != null) {
        printStdErr("In test case {s} there are both stdout and stderr assertions. Only one at a time can be applied.\n", .{sub_path});
        return error.WrongAssertions;
    }

    const assertions_start, const output: OutputPipe = blk: {
        if (maybe_stdout) |stdout| {
            break :blk .{ stdout, .stdout };
        }
        if (maybe_stderr) |stderr| {
            break :blk .{ stderr, .stderr };
        }

        printStdErr("In test case {s} are no assertions.\n", .{sub_path});
        return error.MissingAssertions;
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
        printStdErr("Test Case got interrupted\n", .{});
        return error.ExecutionInterrupted;
    }

    if (term.Exited != 0) {
        if (output != .stderr) {
            printStdErr("Expected the test program to succeed, but it failed. TestCase: {s}\n", .{sub_path});
            printStdErr("Error Output:\n{s}\n", .{stderr.items});
            printStdErr("======================\n", .{});
            return error.TestFailed;
        }
        try expectEqualStrings(assertion, stderr.items);
    } else {
        if (output != .stdout) {
            printStdErr("Expected the test program to fail, but it succeeded. TestCase: {s}\n", .{sub_path});
            printStdErr("Succesfull Output:\n{s}\n", .{stdout.items});
            printStdErr("======================\n", .{});
            return error.TestFailed;
        }
        try expectEqualStrings(assertion, stdout.items);
    }

    {
        state.mutex.lock();
        defer state.mutex.unlock();

        state.results.put(state.alloc, sub_path, .success) catch return;
    }
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
        printStdErr("In test case {s} there are both stdout and stderr assertions. Only one at a time can be applied.\n", .{case_name});
        return error.WrongAssertions;
    }

    const assertions_start, const output: OutputPipe = blk: {
        if (maybe_stdout) |stdout| {
            break :blk .{ stdout, .stdout };
        }
        if (maybe_stderr) |stderr| {
            break :blk .{ stderr, .stderr };
        }

        printStdErr("In test case {s} are no assertions.\n", .{case_name});
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
        printStdErr("Test Case got interrupted\n", .{});
        return error.ExecutionInterrupted;
    }

    if (term.Exited != 0) {
        if (test_case.output != .stderr) {
            printStdErr("Expected the test program to succeed, but it failed. TestCase: {s}\n", .{test_case.name});
            printStdErr("Error Output:\n{s}\n", .{stderr.items});
            printStdErr("======================\n", .{});
            return error.TermMismatch;
        }
        try expectEqualStrings(test_case.assertion, stderr.items);
    } else {
        if (test_case.output != .stdout) {
            printStdErr("Expected the test program to fail, but it succeeded. TestCase: {s}\n", .{test_case.name});
            printStdErr("Succesfull Output:\n{s}\n", .{stdout.items});
            printStdErr("======================\n", .{});
            return error.TermMismatch;
        }
        try expectEqualStrings(test_case.assertion, stdout.items);
    }
}

fn printStdOut(comptime fmt: []const u8, args: anytype) void {
    print_mutex.lock();
    defer print_mutex.unlock();

    stdout_writer.print(fmt, args) catch unreachable;
}

fn printStdErr(comptime fmt: []const u8, args: anytype) void {
    print_mutex.lock();
    defer print_mutex.unlock();

    stderr_writer.print(fmt, args) catch unreachable;
}

fn expectEqualStrings(expected: []const u8, actual: []const u8) !void {
    if (std.mem.indexOfDiff(u8, actual, expected)) |diff_index| {
        printStdErr("\n====== expected this output: =========\n", .{});
        printWithVisibleNewlines(expected);
        printStdErr("\n======== instead found this: =========\n", .{});
        printWithVisibleNewlines(actual);
        printStdErr("\n======================================\n", .{});

        var diff_line_number: usize = 1;
        for (expected[0..diff_index]) |value| {
            if (value == '\n') diff_line_number += 1;
        }
        printStdErr("First difference occurs on line {d}:\n", .{diff_line_number});

        printStdErr("expected:\n", .{});
        printIndicatorLine(expected, diff_index);

        printStdErr("found:\n", .{});
        printIndicatorLine(actual, diff_index);

        return error.TestFailed;
    }
}

fn printIndicatorLine(source: []const u8, indicator_index: usize) void {
    const line_begin_index = if (std.mem.lastIndexOfScalar(u8, source[0..indicator_index], '\n')) |line_begin|
        line_begin + 1
    else
        0;
    const line_end_index = if (std.mem.indexOfScalar(u8, source[indicator_index..], '\n')) |line_end|
        (indicator_index + line_end)
    else
        source.len;

    printLine(source[line_begin_index..line_end_index]);
    for (line_begin_index..indicator_index) |_|
        printStdErr(" ", .{});
    if (indicator_index >= source.len)
        printStdErr("^ (end of string)\n", .{})
    else
        printStdErr("^ ('\\x{x:0>2}')\n", .{source[indicator_index]});
}

fn printWithVisibleNewlines(source: []const u8) void {
    var i: usize = 0;
    while (std.mem.indexOfScalar(u8, source[i..], '\n')) |nl| : (i += nl + 1) {
        printLine(source[i..][0..nl]);
    }
    printStdErr("{s}␃\n", .{source[i..]}); // End of Text symbol (ETX)
}

fn printLine(line: []const u8) void {
    if (line.len != 0) switch (line[line.len - 1]) {
        ' ', '\t' => return printStdErr("{s}⏎\n", .{line}), // Return symbol
        else => {},
    };
    printStdErr("{s}\n", .{line});
}

var print_mutex: std.Thread.Mutex = .{};
const stdout_writer = std.io.getStdOut().writer();
const stderr_writer = std.io.getStdErr().writer();

const std = @import("std");
