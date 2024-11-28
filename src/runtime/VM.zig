const STACK_SIZE = 1024;

gpa: Allocator,
gc: Allocator,
code: []const u8,
ip: usize = 0,
value_stack: Stack(Value, STACK_SIZE, true),
constants: [256]Value = undefined,

pub fn init(gpa: Allocator, gc: Allocator, code: []const u8) !VM {
    return .{
        .gpa = gpa,
        .gc = gc,
        .code = code,
        .value_stack = try .init(gpa),
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
        if (comptime debug_options.bytecode) {
            std.debug.print("{}\n", .{op});
        }
        switch (op) {
            .integer => {
                defer constants_counter += 1;
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const int = std.mem.bytesToValue(Integer, bytes);
                self.constants[constants_counter] = .{ .int = int };
            },
            .float => {
                defer constants_counter += 1;
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const float = std.mem.bytesToValue(Float, bytes);
                self.constants[constants_counter] = .{ .float = float };
            },
            .string => {
                defer constants_counter += 1;
                const len = self.code[self.ip];
                self.ip += 1;
                self.constants[constants_counter] = .{ .string = self.code[self.ip .. self.ip + len + 1] };
                self.ip += len;
            },
            .string_long => {
                defer constants_counter += 1;
                const bytes = self.code[self.ip .. self.ip + 4];
                self.ip += 4;
                const len = std.mem.bytesToValue(u32, bytes);
                self.constants[constants_counter] = .{ .string = self.code[self.ip .. self.ip + len + 1] };
                self.ip += len;
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
        if (comptime debug_options.bytecode) {
            std.debug.print("{}\n", .{op});
        }
        if (comptime debug_options.stack) {
            self.value_stack.dump();
        }
        switch (op) {
            .true => self.value_stack.push(.{ .bool = true }),
            .false => self.value_stack.push(.{ .bool = false }),
            .null => self.value_stack.push(.null),
            .pop => _ = self.value_stack.pop(),
            .add, .sub, .mul, .div => self.arithmetic(op),
            .concat => self.concat(),
            .lower, .lower_equal, .greater, .greater_equal => self.comparison(op),
            .print => {
                const value = self.value_stack.pop();
                try stdout.print("{}\n", .{value});
            },
            .constant => {
                const constant = self.constants[self.byte()];
                self.value_stack.push(constant);
            },
            .negate => {
                const value = self.value_stack.pop();
                const negated: Value = switch (value) {
                    .null, .bool, .string => return error.CanOnlyNegateNumbers,
                    .float => .{ .float = -value.float },
                    .int => .{ .int = -value.int },
                };
                self.value_stack.push(negated);
            },
            .not => {
                const value = self.value_stack.pop();
                self.value_stack.push(.{ .bool = !value.isTrue() });
            },
            .equal, .unequal => {
                const rhs = self.value_stack.pop();
                const lhs = self.value_stack.pop();
                const equal = lhs.equals(rhs);
                if (op == .equal) {
                    self.value_stack.push(.{ .bool = equal });
                } else {
                    self.value_stack.push(.{ .bool = !equal });
                }
            },
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

fn concat(self: *VM) void {
    const rhs = self.value_stack.pop();
    const lhs = self.value_stack.pop();

    const result = std.fmt.allocPrint(self.gc, "{}{}", .{ lhs, rhs }) catch @panic("OOM");
    self.value_stack.push(.{ .string = result });
}

fn arithmetic(self: *VM, op: OpCode) void {
    const rhs = self.value_stack.pop();
    const lhs = self.value_stack.pop();

    // If one of the operands is a float, result is also a float
    // a division always results in a float
    const is_float = rhs == .float or lhs == .float;
    if (is_float or op == .div) {
        const right: Float = if (rhs == .float) rhs.float else @floatFromInt(rhs.int);
        const left: Float = if (lhs == .float) lhs.float else @floatFromInt(lhs.int);

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

    const right = rhs.int;
    const left = lhs.int;

    const result: Value = switch (op) {
        .add => .{ .int = left + right },
        .sub => .{ .int = left - right },
        .mul => .{ .int = left * right },
        else => @panic("Unsupported Operation"),
    };

    self.value_stack.push(result);
}

fn comparison(self: *VM, op: OpCode) void {
    const rhs = self.value_stack.pop();
    const lhs = self.value_stack.pop();

    const is_float = rhs == .float or lhs == .float;
    if (is_float) {
        const right: Float = if (rhs == .float) rhs.float else @floatFromInt(rhs.int);
        const left: Float = if (lhs == .float) lhs.float else @floatFromInt(lhs.int);

        switch (op) {
            .lower => self.value_stack.push(.{ .bool = left < right }),
            .lower_equal => self.value_stack.push(.{ .bool = left <= right }),
            .greater => self.value_stack.push(.{ .bool = left > right }),
            .greater_equal => self.value_stack.push(.{ .bool = left >= right }),
            else => @panic("Unsupported Operation"),
        }
        return;
    }

    const left = lhs.int;
    const right = rhs.int;
    switch (op) {
        .lower => self.value_stack.push(.{ .bool = left < right }),
        .lower_equal => self.value_stack.push(.{ .bool = left <= right }),
        .greater => self.value_stack.push(.{ .bool = left > right }),
        .greater_equal => self.value_stack.push(.{ .bool = left >= right }),
        else => @panic("Unsupported Operation"),
    }
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

const stdout = std.io.getStdOut().writer();

const OpCode = @import("shared").OpCode;
const Integer = @import("shared").definitions.Integer;
const Float = @import("shared").definitions.Float;
const Value = @import("shared").definitions.FlowValue;
const Stack = @import("shared").Stack;
const debug_options = @import("debug_options");
