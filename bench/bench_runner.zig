const MAX_BENCH_SIZE = 1024 * 1024 * 1024; // 1 MB
/// How many times should a single benchmark be ran
const RUN_COUNT = 8;

const Options = struct {};

pub fn main() u8 {
    var debug_allocator: std.heap.DebugAllocator(.{ .thread_safe = true }) = .init;
    const gpa = debug_allocator.allocator();
    defer _ = debug_allocator.deinit();

    // var options: Options = .{};

    var args = std.process.args();
    _ = args.skip(); // skip own program name

    const compiler = args.next() orelse {
        printStdErr("Missing Argument: compiler\n", .{});
        return 1;
    };

    const benches_dir = args.next() orelse {
        printStdErr("Missing Argument: directory of benchmarks\n", .{});
        return 1;
    };

    const compiler_mode = args.next() orelse {
        printStdErr("Missing Argument: compiler mode\n", .{});
        return 1;
    };

    printStdOut("Running Benchmarks for Compiler mode: {s}\n", .{compiler_mode});

    runBenches(gpa, benches_dir, compiler) catch return 1;

    // while (args.next()) |arg| {
    //     if (std.mem.startsWith(u8, arg, "--filter=")) {
    //         options.filter = arg["--filter=".len..];
    //     } else if (std.mem.eql(u8, arg, "--verbose")) {
    //         options.verbose = true;
    //     } else {
    //         printStdErr("Unrecognized Option: {s}\n", .{arg});
    //         return 1;
    //     }
    // }
    return 0;
}

fn runBenches(alloc: std.mem.Allocator, bench_dir: []const u8, compiler: []const u8) !void {
    var root_dir = try std.fs.openDirAbsolute(bench_dir, .{ .iterate = true });
    defer root_dir.close();

    var iter = root_dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".flow")) continue;

        const full_path = try std.fs.path.join(alloc, &.{ bench_dir, entry.name });
        defer alloc.free(full_path);

        try runSingleBenchmark(alloc, full_path, compiler);
    }
}

fn runSingleBenchmark(alloc: std.mem.Allocator, bench_path: []const u8, compiler: []const u8) !void {
    const basename = std.fs.path.basename(bench_path);

    const tmp_file_path = try std.fmt.allocPrint(alloc, "/tmp/flowlang/benchmarks/{s}", .{basename});
    defer alloc.free(tmp_file_path);

    if (std.fs.path.dirname(tmp_file_path)) |dir_name| {
        if (!std.mem.eql(u8, dir_name, "/tmp")) {
            var tmp_dir = std.fs.openDirAbsolute("/tmp", .{}) catch unreachable;
            defer tmp_dir.close();
            try tmp_dir.makePath(dir_name);
        }
    }

    var compile_step = std.process.Child.init(&.{ compiler, bench_path, tmp_file_path }, alloc);
    const term = try compile_step.spawnAndWait();
    if (term != .Exited or term.Exited != 0) {
        printStdErr("Benchmark '{s}' could not be compiled\n", .{basename});
        return error.CompileError;
    }

    var run_step = std.process.Child.init(&.{tmp_file_path}, alloc);
    var total_time: u64 = 0;
    var times: [RUN_COUNT]u64 = undefined;
    var timer: std.time.Timer = try .start();
    for (0..RUN_COUNT) |idx| {
        timer.reset();
        defer {
            times[idx] = timer.read();
            total_time += times[idx];
        }

        const run_term = try run_step.spawnAndWait();
        if (run_term != .Exited or run_term.Exited != 0) {
            printStdErr("Running '{s}' did not return a success status code\n", .{basename});
            return error.RunError;
        }
    }

    std.mem.sort(u64, &times, .{}, lessThan);

    const average = (total_time / times.len) / std.time.ns_per_ms;
    const median = times[times.len / 2] / std.time.ns_per_ms;
    const fastest = times[0] / std.time.ns_per_ms;
    const slowest = times[times.len - 1] / std.time.ns_per_ms;

    printStdOut("======================\n", .{});
    printStdOut("Benchmark '{s}' ran {d} times\n\n", .{ basename, times.len });
    printStdOut("average: {d}ms\n", .{average});
    printStdOut("median : {d}ms\n", .{median});
    printStdOut("fastest: {d}ms\n", .{fastest});
    printStdOut("slowest: {d}ms\n", .{slowest});
    printStdOut("======================\n\n", .{});
}

fn lessThan(_: @TypeOf(.{}), lhs: u64, rhs: u64) bool {
    return lhs < rhs;
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

const stdout_writer = std.io.getStdOut().writer();
const stderr_writer = std.io.getStdErr().writer();

const std = @import("std");
