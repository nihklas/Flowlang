const STACK_SIZE = 1024;

alloc: Allocator,
code: []const u8,
ip: usize = 0,
value_stack: Stack(Value, STACK_SIZE, true),

pub fn init(alloc: Allocator, code: []const u8) !VM {
    return .{
        .alloc = alloc,
        .code = code,
        .value_stack = try .init(alloc),
    };
}

pub fn deinit(self: *VM) void {
    self.value_stack.deinit();
    self.* = undefined;
}

// TODO: Write two main run functions:
//    1. while(true) with switch
//    2. labeled switch continue
// Compare those in benchmarks
pub fn run(self: *VM) !void {
    try self.runWhileSwitch();
    // try self.runSwitchContinue();
}

fn runWhileSwitch(self: *VM) !void {
    while (self.ip < self.code.len) {
        const op = self.instruction();
        switch (op) {
            .integer => {
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const int = std.mem.bytesToValue(Integer, bytes);
                self.value_stack.push(.{ .integer = int });
            },
            .float => {
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const float = std.mem.bytesToValue(Float, bytes);
                self.value_stack.push(.{ .float = float });
            },
            .true => self.value_stack.push(.{ .boolean = true }),
            .false => self.value_stack.push(.{ .boolean = false }),
            .null => self.value_stack.push(.null),
            .print => {
                const value = self.value_stack.pop();
                try stdout.print("{}\n", .{value});
            },
            .pop => _ = self.value_stack.pop(),
            else => {
                std.debug.print("Illegal Instruction: {}\n", .{op});
                return error.IllegalInstruction;
            },
        }
    }
}

fn runSwitchContinue(self: *VM) !void {
    _ = self;
    // TODO:
}

fn instruction(self: *VM) OpCode {
    defer self.ip += 1;
    return @enumFromInt(self.code[self.ip]);
}

fn byte(self: *VM) u8 {
    defer self.ip += 1;
    return self.code[self.ip];
}

const VM = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;

const stdout = std.io.getStdOut().writer();

const OpCode = @import("shared").OpCode;
const Integer = @import("shared").definitions.Integer;
const Float = @import("shared").definitions.Float;
const Stack = @import("shared").Stack;
