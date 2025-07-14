const MAX_BENCH_SIZE = 1024 * 1024 * 1024; // 1 MB
/// How many times should a single benchmark be ran
const RUN_COUNT = 10;

const Options = struct {};

pub fn main() u8 {
    var timer = std.time.Timer.start() catch return 1;
    defer {
        const total_time = timer.read();
        const factor, const notation = getFactorAndNotation(total_time);

        printStdOut("total runtime: {d:.5}{s}\n", .{ calcTiming(total_time, factor), notation });
    }

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

    printStdOut("======================\n", .{});
    printStdOut("Running Benchmarks for Compiler mode: {s}\n", .{compiler_mode});
    printStdOut("Running each benchmark {d} times\n", .{RUN_COUNT});

    runBenches(gpa, benches_dir, compiler) catch return 1;

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

    printStdOut("----------------------\n", .{});
    printStdOut("Running Benchmark '{s}'\n", .{basename});

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

    const factor: u64, const time_notation = getFactorAndNotation(times[0]);

    const average = calcTiming(total_time / times.len, factor);
    const median = calcTiming(times[times.len / 2], factor);
    const fastest = calcTiming(times[0], factor);
    const slowest = calcTiming(times[times.len - 1], factor);

    printStdOut("{d} runs\n\n", .{times.len});
    printStdOut("average: {d:.5}{s}\n", .{ average, time_notation });
    printStdOut("median : {d:.5}{s}\n", .{ median, time_notation });
    printStdOut("fastest: {d:.5}{s}\n", .{ fastest, time_notation });
    printStdOut("slowest: {d:.5}{s}\n", .{ slowest, time_notation });
    printStdOut("----------------------\n", .{});
}

fn calcTiming(time: u64, factor: u64) f64 {
    const converted: f64 = @floatFromInt(time);
    const converted_factor: f64 = @floatFromInt(factor);
    return converted / converted_factor;
}

fn getFactorAndNotation(time: u64) struct { u64, []const u8 } {
    return blk: {
        if (time > std.time.ns_per_hour) {
            break :blk .{ std.time.ns_per_hour, "h" };
        }
        if (time > std.time.ns_per_min) {
            break :blk .{ std.time.ns_per_min, "min" };
        }
        if (time > std.time.ns_per_s) {
            break :blk .{ std.time.ns_per_s, "s" };
        }
        if (time > std.time.ns_per_ms) {
            break :blk .{ std.time.ns_per_ms, "ms" };
        }
        break :blk .{ 1, "ns" };
    };
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
