const STACK_SIZE = 1024;

alloc: Allocator,
code: []const u8,
ip: usize = 0,
value_stack: Stack(Value, STACK_SIZE, true),
constants: [256]Value = undefined,

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
    try self.loadConstants();
    try self.runWhileSwitch();
    // try self.runSwitchContinue();
}

fn loadConstants(self: *VM) !void {
    var constants_counter: usize = 0;
    while (self.ip < self.code.len) {
        const op = self.instruction();
        switch (op) {
            .integer => {
                defer constants_counter += 1;
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const int = std.mem.bytesToValue(Integer, bytes);
                self.constants[constants_counter] = .{ .integer = int };
            },
            .float => {
                defer constants_counter += 1;
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const float = std.mem.bytesToValue(Float, bytes);
                self.constants[constants_counter] = .{ .float = float };
            },
            .constants_done => break,
            else => {
                std.debug.print("Illegal Instruction: {}\n", .{op});
                return error.IllegalInstruction;
            },
        }
    }
}

fn runWhileSwitch(self: *VM) !void {
    while (self.ip < self.code.len) {
        const op = self.instruction();
        switch (op) {
            .true => self.value_stack.push(.{ .boolean = true }),
            .false => self.value_stack.push(.{ .boolean = false }),
            .null => self.value_stack.push(.null),
            .print => {
                const value = self.value_stack.pop();
                try stdout.print("{}\n", .{value});
            },
            .pop => _ = self.value_stack.pop(),
            .constant => {
                const constant = self.constants[self.byte()];
                self.value_stack.push(constant);
            },
            .negate => {
                const value = self.value_stack.pop();
                const negated: Value = switch (value) {
                    .null, .boolean => return error.CanOnlyNegateNumbers,
                    .float => .{ .float = -value.float },
                    .integer => .{ .integer = -value.integer },
                };
                self.value_stack.push(negated);
            },
            .not => {
                const value = self.value_stack.pop();
                self.value_stack.push(.{ .boolean = !value.isTrue() });
            },
            .add, .sub, .mul, .div => self.arithmatic(op),
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

fn arithmatic(self: *VM, op: OpCode) void {
    const rhs = self.value_stack.pop();
    const lhs = self.value_stack.pop();

    // If one of the operands is a float, result is also a float
    // a division always results in a float
    const is_float = rhs == .float or lhs == .float;
    if (is_float or op == .div) {
        const right: Float = if (rhs == .float) rhs.float else @floatFromInt(rhs.integer);
        const left: Float = if (lhs == .float) lhs.float else @floatFromInt(lhs.integer);

        const result: Value = switch (op) {
            .add => .{ .float = left + right },
            .sub => .{ .float = left - right },
            .mul => .{ .float = left * right },
            .div => .{ .float = left / right },
            else => @panic("Unsupported Operation"),
        };

        self.value_stack.push(result);
        return;
    }

    const right = rhs.integer;
    const left = lhs.integer;

    const result: Value = switch (op) {
        .add => .{ .integer = left + right },
        .sub => .{ .integer = left - right },
        .mul => .{ .integer = left * right },
        else => @panic("Unsupported Operation"),
    };

    self.value_stack.push(result);
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
