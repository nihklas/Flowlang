const STACK_SIZE = 1024;

gpa: Allocator,
gc: Allocator,
code: []const u8,
ip: usize = 0,
value_stack: Stack(FlowValue, STACK_SIZE, true),
call_stack: Stack(CallFrame, STACK_SIZE, true),
constants: [256]FlowValue = undefined,
globals: std.StringHashMapUnmanaged(FlowValue),

pub fn init(gpa: Allocator, gc: Allocator, code: []const u8) VM {
    return .{
        .gpa = gpa,
        .gc = gc,
        .code = code,
        .value_stack = Stack(FlowValue, STACK_SIZE, true).init(gpa) catch oom(),
        .call_stack = Stack(CallFrame, STACK_SIZE, true).init(gpa) catch oom(),
        .globals = .empty,
    };
}

pub fn deinit(self: *VM) void {
    self.value_stack.deinit();
    self.call_stack.deinit();
    self.globals.deinit(self.gpa);
    self.* = undefined;
}

pub fn run(self: *VM) void {
    self.loadConstants();
    self.loadFunctions();
    self.call_stack.push(.{
        .stack_bottom = 0,
        .ret_addr = self.ip,
    });
    self.runWhileSwitch();
    // try self.runSwitchContinue();
}

fn loadConstants(self: *VM) void {
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
                self.constants[constants_counter] = .{ .string = self.code[self.ip .. self.ip + len] };
                self.ip += len;
            },
            .string_long => {
                defer constants_counter += 1;
                const bytes = self.code[self.ip .. self.ip + 4];
                self.ip += 4;
                const len = std.mem.bytesToValue(u32, bytes);
                self.constants[constants_counter] = .{ .string = self.code[self.ip .. self.ip + len] };
                self.ip += len;
            },
            .constants_done => break,
            else => panic("Illegal Instruction at {x:0>4}: {}\n", .{ self.ip, op }),
        }
    }
}

fn loadFunctions(self: *VM) void {
    while (self.ip < self.code.len) {
        const op = self.instruction();
        if (comptime debug_options.bytecode) {
            std.debug.print("{}\n", .{op});
        }
        switch (op) {
            .function => {
                const name_idx = self.byte();
                const argc = self.byte();
                const end = self.short();

                const name = self.constants[name_idx];

                self.globals.put(self.gpa, name.string, .{
                    .function = .{
                        .name = name.string,
                        .arg_count = argc,
                        .start_ip = self.ip,
                    },
                }) catch oom();

                self.ip += end - 1;
            },
            .functions_done => break,
            else => panic("Illegal Instruction at {x:0>4}: {}\n", .{ self.ip, op }),
        }
    }
}

fn runWhileSwitch(self: *VM) void {
    while (self.ip < self.code.len) {
        const op = self.instruction();
        if (comptime debug_options.stack) {
            self.value_stack.dump();
        }
        if (comptime debug_options.bytecode) {
            std.debug.print("{x:0>4} {}\n", .{ self.ip, op });
        }
        switch (op) {
            .true => self.value_stack.push(.{ .bool = true }),
            .false => self.value_stack.push(.{ .bool = false }),
            .null => self.value_stack.push(.null),
            .pop => _ = self.value_stack.pop(),
            .add, .sub, .mul, .div, .mod => self.arithmetic(op),
            .concat => self.concat(),
            .lower, .lower_equal, .greater, .greater_equal => self.comparison(op),
            .constant => {
                const constant = self.constants[self.byte()];
                self.value_stack.push(constant);
            },
            .negate => {
                const value = self.value_stack.pop();
                const negated: FlowValue = switch (value) {
                    .float => .{ .float = -value.float },
                    .int => .{ .int = -value.int },

                    .null, .bool, .string, .builtin_fn, .function => unreachable,
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
            .create_global => {
                const name = self.value_stack.pop();
                const value = self.value_stack.pop();

                self.globals.put(self.gpa, name.string, value) catch oom();
            },
            .get_global => {
                const name = self.value_stack.pop();
                const value: FlowValue = self.globals.get(name.string) orelse .{ .builtin_fn = builtins.get(name.string).? };

                self.value_stack.push(value);
            },
            .set_global => {
                const name = self.value_stack.pop();
                const value = self.value_stack.at(0);

                // We can safely assume capacity, as this set only works if the global already exists
                self.globals.putAssumeCapacity(name.string, value);
            },
            .get_local => {
                const idx = self.byte();
                const value = self.getLocal(idx);
                self.value_stack.push(value);
            },
            .set_local => {
                const idx = self.byte();
                const value = self.value_stack.at(0);
                self.setLocal(idx, value);
            },
            .jump => {
                // For some reason we cannot inline this
                const distance = self.short();
                self.ip += distance;
            },
            .jump_back => {
                // For some reason we cannot inline this
                const distance = self.short();
                self.ip -= distance;
            },
            .jump_if_false => {
                const distance = self.short();
                const value = self.value_stack.at(0);
                if (!value.isTrue()) self.ip += distance;
            },
            .call => {
                self.call();
            },
            .@"return" => {
                const ret_value = self.value_stack.pop();
                const frame = self.call_stack.pop();
                self.ip = frame.ret_addr;
                self.value_stack.stack_top = frame.stack_bottom;
                self.value_stack.push(ret_value);
            },

            .string,
            .string_long,
            .integer,
            .float,
            .constants_done,
            .functions_done,
            .function,
            => panic("Illegal Instruction at {x:0>4}: {}\n", .{ self.ip, op }),
        }
    }
}

fn concat(self: *VM) void {
    const rhs = self.value_stack.pop();
    const lhs = self.value_stack.pop();

    const result = std.fmt.allocPrint(self.gc, "{}{}", .{ lhs, rhs }) catch oom();
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

        const result: FlowValue = switch (op) {
            .add => .{ .float = left + right },
            .sub => .{ .float = left - right },
            .mul => .{ .float = left * right },
            .div => .{ .float = left / right },
            .mod => .{ .float = @mod(left, right) },
            else => unreachable,
        };

        self.value_stack.push(result);
        return;
    }

    const right = rhs.int;
    const left = lhs.int;

    const result: FlowValue = switch (op) {
        .add => .{ .int = left + right },
        .sub => .{ .int = left - right },
        .mul => .{ .int = left * right },
        .mod => .{ .int = @mod(left, right) },
        else => unreachable,
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
            else => unreachable,
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
        else => unreachable,
    }
}

fn call(self: *VM) void {
    const value = self.value_stack.pop();
    switch (value) {
        .builtin_fn => self.callBuiltin(value),
        .function => self.callFlowFunction(value),
        else => unreachable,
    }
}

fn callBuiltin(self: *VM, value: FlowValue) void {
    const arg_count = value.builtin_fn.arg_count;
    const args = self.value_stack.stack[self.value_stack.stack_top - arg_count .. self.value_stack.stack_top];
    const result = value.builtin_fn.function(args);
    for (0..arg_count) |_| {
        _ = self.value_stack.pop();
    }

    self.value_stack.push(result);
}

fn callFlowFunction(self: *VM, value: FlowValue) void {
    const arg_count = value.function.arg_count;
    const stack_bottom = self.value_stack.stack_top - arg_count;
    self.call_stack.push(.{
        .ret_addr = self.ip,
        .stack_bottom = stack_bottom,
    });
    self.ip = value.function.start_ip;
}

fn getLocal(self: *VM, local_idx: usize) FlowValue {
    const frame = self.call_stack.at(0);
    return self.value_stack.stack[frame.stack_bottom + local_idx];
}

fn setLocal(self: *VM, local_idx: usize, value: FlowValue) void {
    const frame = self.call_stack.at(0);
    self.value_stack.stack[frame.stack_bottom + local_idx] = value;
}

fn instruction(self: *VM) OpCode {
    defer self.ip += 1;
    return @enumFromInt(self.code[self.ip]);
}

fn short(self: *VM) u16 {
    return std.mem.bytesToValue(u16, &.{ self.byte(), self.byte() });
}

fn byte(self: *VM) u8 {
    defer self.ip += 1;
    return self.code[self.ip];
}

const CallFrame = struct {
    ret_addr: usize,
    stack_bottom: usize,
};

const VM = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;

const stdout = std.io.getStdOut().writer();

const OpCode = @import("shared").OpCode;
const Integer = @import("shared").definitions.Integer;
const Float = @import("shared").definitions.Float;
const FlowValue = @import("shared").definitions.FlowValue;
const builtins = @import("shared").builtins;
const Stack = @import("shared").Stack;
const oom = @import("shared").oom;
const debug_options = @import("debug_options");

const panic = std.debug.panic;
