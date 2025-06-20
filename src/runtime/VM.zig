const STACK_SIZE = 1024;

gpa: Allocator,
gc: Allocator,
code: []const u8,
ip: usize,
value_stack: Stack(FlowValue),
call_stack: Stack(CallFrame),
constants: [256]FlowValue,
globals: [256]FlowValue,
globals_count: u8,

pub fn init(gpa: Allocator, gc: Allocator, code: []const u8) VM {
    return .{
        .gpa = gpa,
        .gc = gc,
        .code = code,
        .ip = 0,
        .value_stack = .init(gpa.alloc(FlowValue, STACK_SIZE) catch oom()),
        .call_stack = .init(gpa.alloc(CallFrame, STACK_SIZE) catch oom()),
        .constants = undefined,
        .globals = undefined,
        .globals_count = 0,
    };
}

pub fn deinit(self: *VM) void {
    self.gpa.free(self.value_stack.stack);
    self.gpa.free(self.call_stack.stack);
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
    var global_counter: usize = 0;
    while (self.ip < self.code.len) {
        const op = self.instruction();
        if (comptime debug_options.bytecode) {
            std.debug.print("{}\n", .{op});
        }
        switch (op) {
            .function => {
                defer global_counter += 1;

                const argc = self.byte();
                const end = self.short();

                self.globals[global_counter] = .{ .function = .{
                    .arg_count = argc,
                    .start_ip = self.ip,
                } };

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
        defer if (comptime debug_options.stack) {
            self.value_stack.dump();
        };
        if (comptime debug_options.bytecode) {
            std.debug.print("{x:0>4} {}\n", .{ self.ip, op });
        }
        switch (op) {
            .true => self.push(.{ .bool = true }),
            .false => self.push(.{ .bool = false }),
            .null => self.push(.null),
            .pop => _ = self.pop(),
            .add_i, .sub_i, .mul_i, .div_i, .mod_i => self.arithmeticInt(op),
            .add_f, .sub_f, .mul_f, .div_f, .mod_f => self.arithmeticFloat(op),
            .concat => self.concat(),
            .lower, .lower_equal, .greater, .greater_equal => self.comparison(op),
            .array => {
                const len = self.byte();
                const cap = std.math.ceilPowerOfTwoAssert(usize, if (len == 0) 1 else len);
                const items = self.gc.alloc(FlowValue, cap) catch oom();
                const arr = self.gc.create(definitions.FlowArray) catch oom();
                for (0..len) |i| {
                    items[len - 1 - i] = self.pop();
                }
                arr.items = items.ptr;
                arr.cap = cap;
                arr.len = len;
                self.push(.{ .array = arr });
            },
            .clone => {
                const value = self.pop();
                self.push(value.clone(self.gc));
            },
            .index => {
                const index = self.pop().int;
                const array = self.pop().array;
                if (index < 0) {
                    std.debug.panic("IndexUnderflow: {d}\n", .{index});
                } else if (index > array.len) {
                    std.debug.panic("IndexOverflow: {d}, array len: {d}\n", .{ index, array.len });
                }
                self.push(array.items[@intCast(index)]);
            },
            .append => {
                const value = self.pop();
                var array = &self.value_stack.stack[self.value_stack.stack_top - 1];

                if (array.array.cap >= array.array.len + 1) {
                    array.array.items[array.array.len] = value;
                    array.array.len += 1;
                } else {
                    if (array.array.cap == 0) array.array.cap = 1;
                    const new_values = self.gc.alloc(FlowValue, array.array.cap * 2) catch oom();
                    @memcpy(new_values[0..array.array.len], array.array.items[0..array.array.len]);
                    new_values[array.array.len] = value;
                    array.array.len += 1;
                    array.array.cap *= 2;
                    array.array.items = new_values.ptr;
                }
            },
            .constant => {
                const constant = self.constants[self.byte()];
                self.push(constant);
            },
            .negate_i => {
                const value = self.pop();
                const negated: FlowValue = .{ .int = -value.int };
                self.push(negated);
            },
            .negate_f => {
                const value = self.pop();
                const negated: FlowValue = .{ .float = -value.float };
                self.push(negated);
            },
            .not => {
                const value = self.pop();
                self.push(.{ .bool = !value.isTrue() });
            },
            .equal, .unequal => {
                const rhs = self.pop();
                const lhs = self.pop();
                const equal = lhs.equals(&rhs);
                if (op == .equal) {
                    self.push(.{ .bool = equal });
                } else {
                    self.push(.{ .bool = !equal });
                }
            },
            .get_builtin => {
                const name = self.pop();
                self.push(.{ .builtin_fn = builtins.get(name.string).? });
            },
            .get_global => {
                const idx = self.byte();
                const value = self.globals[idx];

                self.push(value);
            },
            .set_global => {
                const idx = self.byte();
                const value = self.value_stack.at(0);
                self.globals[idx] = value;

                if (self.globals_count <= idx) {
                    self.globals_count = idx + 1;
                }
            },
            .set_global_array => {
                const global_idx = self.byte();
                const index_amount = self.byte();
                std.debug.assert(index_amount > 0);

                var array = self.globals[global_idx];

                for (0..index_amount - 1) |_| {
                    const idx = self.pop().int;
                    if (idx < 0) {
                        std.debug.panic("IndexUnderflow: {d}\n", .{idx});
                    } else if (idx > array.array.len) {
                        std.debug.panic("IndexOverflow: {d}, array len: {d}\n", .{ idx, array.array.len });
                    }
                    array = array.array.items[@intCast(idx)];
                }

                const last_idx = self.pop().int;
                array.array.items[@intCast(last_idx)] = self.value_stack.at(0);
            },
            .get_local => {
                const idx = self.byte();
                const value = self.getLocal(idx);
                self.push(value);
            },
            .set_local => {
                const idx = self.byte();
                const value = self.value_stack.at(0);
                self.setLocal(idx, value);
            },
            .set_local_array => {
                const local_idx = self.byte();
                const index_amount = self.byte();
                std.debug.assert(index_amount > 0);

                var array = self.getLocal(local_idx);

                for (0..index_amount - 1) |_| {
                    const idx = self.pop().int;
                    if (idx < 0) {
                        std.debug.panic("IndexUnderflow: {d}\n", .{idx});
                    } else if (idx > array.array.len) {
                        std.debug.panic("IndexOverflow: {d}, array len: {d}\n", .{ idx, array.array.len });
                    }
                    array = array.array.items[@intCast(idx)];
                }

                const last_idx = self.pop().int;
                array.array.items[@intCast(last_idx)] = self.value_stack.at(0);
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
            .jump_if_true => {
                const distance = self.short();
                const value = self.value_stack.at(0);
                if (value.isTrue()) self.ip += distance;
            },
            .jump_if_false => {
                const distance = self.short();
                const value = self.value_stack.at(0);
                if (!value.isTrue()) self.ip += distance;
            },
            .call => self.call(),
            .@"return" => {
                const ret_value = self.pop();
                const frame = self.call_stack.pop();
                self.ip = frame.ret_addr;
                self.value_stack.stack_top = frame.stack_bottom;
                self.push(ret_value);
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
    const rhs = self.pop();
    const lhs = self.pop();

    const result = std.fmt.allocPrint(self.gc, "{}{}", .{ lhs, rhs }) catch oom();
    self.push(.{ .string = result });
}

fn arithmeticInt(self: *VM, op: OpCode) void {
    const rhs = self.pop();
    const lhs = self.pop();

    const right: Integer = rhs.int;
    const left: Integer = lhs.int;

    const result: FlowValue = switch (op) {
        .add_i => .{ .int = left + right },
        .sub_i => .{ .int = left - right },
        .mul_i => .{ .int = left * right },
        .div_i => .{ .int = @divTrunc(left, right) },
        .mod_i => .{ .int = @mod(left, right) },
        else => unreachable,
    };

    self.push(result);
}

fn arithmeticFloat(self: *VM, op: OpCode) void {
    const rhs = self.pop();
    const lhs = self.pop();

    const right: Float = rhs.float;
    const left: Float = lhs.float;

    const result: FlowValue = switch (op) {
        .add_f => .{ .float = left + right },
        .sub_f => .{ .float = left - right },
        .mul_f => .{ .float = left * right },
        .div_f => .{ .float = left / right },
        .mod_f => .{ .float = @mod(left, right) },
        else => unreachable,
    };

    self.push(result);
}

fn comparison(self: *VM, op: OpCode) void {
    const rhs = self.pop();
    const lhs = self.pop();

    const is_float = rhs == .float or lhs == .float;
    if (is_float) {
        const right: Float = if (rhs == .float) rhs.float else @floatFromInt(rhs.int);
        const left: Float = if (lhs == .float) lhs.float else @floatFromInt(lhs.int);

        switch (op) {
            .lower => self.push(.{ .bool = left < right }),
            .lower_equal => self.push(.{ .bool = left <= right }),
            .greater => self.push(.{ .bool = left > right }),
            .greater_equal => self.push(.{ .bool = left >= right }),
            else => unreachable,
        }
        return;
    }

    const left = lhs.int;
    const right = rhs.int;
    switch (op) {
        .lower => self.push(.{ .bool = left < right }),
        .lower_equal => self.push(.{ .bool = left <= right }),
        .greater => self.push(.{ .bool = left > right }),
        .greater_equal => self.push(.{ .bool = left >= right }),
        else => unreachable,
    }
}

fn call(self: *VM) void {
    const value = self.pop();
    switch (value) {
        .builtin_fn => self.callBuiltin(value),
        .function => self.callFlowFunction(value),
        else => unreachable,
    }
}

fn callBuiltin(self: *VM, value: FlowValue) void {
    const arg_count = value.builtin_fn.arg_types.len;
    const args = self.value_stack.stack[self.value_stack.stack_top - arg_count .. self.value_stack.stack_top];
    const result = value.builtin_fn.function(self.gc, args);
    for (0..arg_count) |_| {
        _ = self.pop();
    }

    self.push(result);
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

fn push(self: *VM, value: FlowValue) void {
    self.value_stack.push(value);
}

fn pop(self: *VM) FlowValue {
    return self.value_stack.pop();
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
const definitions = @import("shared").definitions;
const Integer = definitions.Integer;
const Float = definitions.Float;
const FlowValue = definitions.FlowValue;
const builtins = @import("shared").builtins;
const Stack = @import("shared").Stack;
const oom = @import("shared").oom;
const debug_options = @import("debug_options");

const panic = std.debug.panic;
