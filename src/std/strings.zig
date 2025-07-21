pub const substring: BuiltinFunction = .{
    .arg_types = &.{ .primitive(.string), .primitive(.int), .primitive(.int) },
    .ret_type = .{ .type = .string, .order = 0 },
    .function = &_substring,
};

fn _substring(gc: Allocator, args: []FlowValue) FlowValue {
    assert(args.len == 3);
    assert(args[0] == .string);
    assert(args[1] == .int);
    assert(args[2] == .int);

    assert(args[1].int >= 0 and args[1].int < std.math.maxInt(usize));
    assert(args[2].int >= 0 and args[2].int < std.math.maxInt(usize));

    const start_idx: usize = @intCast(args[1].int);
    const end_idx: usize = @intCast(args[2].int);

    const substr = args[0].string[start_idx..end_idx];
    return .{ .string = gc.dupe(u8, substr) catch oom() };
}

pub const split: BuiltinFunction = .{
    .arg_types = &.{ .primitive(.string), .primitive(.string) },
    .ret_type = .{ .type = .string, .order = 1 },
    .function = &_split,
};

fn _split(gc: Allocator, args: []FlowValue) FlowValue {
    assert(args.len == 2);
    assert(args[0] == .string);
    assert(args[1] == .string);

    var arr = gc.alloc(FlowValue, args[0].string.len / 2) catch oom();

    var iter = std.mem.splitSequence(u8, args[0].string, args[1].string);

    var idx: usize = 0;
    while (iter.next()) |entry| : (idx += 1) {
        if (arr.len == idx) {
            arr = gc.realloc(arr, arr.len * 2) catch oom();
        }
        arr[idx] = .{ .string = gc.dupe(u8, entry) catch oom() };
    }

    const array = gc.create(FlowArray) catch oom();
    array.* = .{
        .items = arr.ptr,
        .cap = arr.len,
        .len = idx,
    };
    return .{ .array = array };
}

pub const charAt: BuiltinFunction = .{
    .arg_types = &.{ .primitive(.string), .primitive(.int) },
    .ret_type = .primitive(.string),
    .function = &_charAt,
};

fn _charAt(gc: Allocator, args: []FlowValue) FlowValue {
    assert(args.len == 2);
    assert(args[0] == .string);
    assert(args[1] == .int);
    std.debug.print("{}\n", .{args[1].int});
    assert(args[1].int >= 0 and args[1].int < std.math.maxInt(usize));

    const str = args[0].string;
    const idx: usize = @intCast(args[1].int);

    if (idx < 0) {
        @branchHint(.unlikely);
        std.debug.panic("IndexUnderflow: {d}\n", .{idx});
    } else if (idx > str.len) {
        @branchHint(.unlikely);
        std.debug.panic("IndexOverflow: {d}, string len: {d}\n", .{ idx, str.len });
    }

    // TODO: Is this possible without allocation?
    const single_char = gc.alloc(u8, 1) catch oom();
    single_char[0] = str[idx];
    return .{ .string = single_char };
}

const BuiltinFunction = @import("shared").definitions.BuiltinFunction;
const FlowValue = @import("shared").definitions.FlowValue;
const FlowArray = @import("shared").definitions.FlowArray;
const Integer = @import("shared").definitions.Integer;
const Float = @import("shared").definitions.Float;
const oom = @import("shared").oom;

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
