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
    self.call_stack.push(.{
        .stack_bottom = 0,
        .ret_addr = self.ip,
    });

    switch (@import("vm_options").run_mode) {
        .loop => self.runWhileSwitch(),
        .@"switch" => self.runSwitchContinue(),
    }
}

fn loadConstants(self: *VM) void {
    var constants_counter: usize = 0;
    while (self.ip < self.code.len) {
        const op = self.instruction();
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

fn runWhileSwitch(self: *VM) void {
    while (self.ip < self.code.len) {
        const op = self.instruction();
        switch (op) {
            .true => self.push(.{ .bool = true }),
            .false => self.push(.{ .bool = false }),
            .null => self.push(.null),
            .pop => _ = self.pop(),
            .add_i, .sub_i, .mul_i, .div_i, .mod_i => self.arithmeticInt(op),
            .add_f, .sub_f, .mul_f, .div_f, .mod_f => self.arithmeticFloat(op),
            .concat => self.concat(),
            .lower, .lower_equal, .greater, .greater_equal => self.comparison(op),
            .array => self.array(),
            .clone => self.clone(),
            .index => self.index(),
            .append => self.append(),
            .constant => self.constant(),
            .negate_i => self.negateInt(),
            .negate_f => self.negateFloat(),
            .not => self.not(),
            .equal => self.equal(),
            .unequal => self.unequal(),
            .get_builtin => self.getBuiltin(),
            .get_global => self.getGlobal(),
            .set_global => self.setGlobal(),
            .set_global_array => self.setGlobalArray(),
            .get_local => self.getLocal(),
            .set_local => self.setLocal(),
            .set_local_array => self.setLocalArray(),
            .jump => self.jump(),
            .jump_back => self.jumpBack(),
            .jump_if_true => self.jumpIfTrue(),
            .jump_if_false => self.jumpIfFalse(),
            .call => self.call(),
            .@"return" => self.returnInstr(),
            .function => self.function(),
            .eof => break,

            .string,
            .string_long,
            .integer,
            .float,
            .constants_done,
            => panic("Illegal Instruction at {x:0>4}: {}\n", .{ self.ip, op }),
        }
    }
}

fn runSwitchContinue(self: *VM) void {
    loop: switch (self.instruction()) {
        .true => {
            self.push(.{ .bool = true });
            continue :loop self.instruction();
        },
        .false => {
            self.push(.{ .bool = false });
            continue :loop self.instruction();
        },
        .null => {
            self.push(.null);
            continue :loop self.instruction();
        },
        .pop => {
            _ = self.pop();
            continue :loop self.instruction();
        },
        .add_i, .sub_i, .mul_i, .div_i, .mod_i => |op| {
            self.arithmeticInt(op);
            continue :loop self.instruction();
        },
        .add_f, .sub_f, .mul_f, .div_f, .mod_f => |op| {
            self.arithmeticFloat(op);
            continue :loop self.instruction();
        },
        .concat => {
            self.concat();
            continue :loop self.instruction();
        },
        .lower, .lower_equal, .greater, .greater_equal => |op| {
            self.comparison(op);
            continue :loop self.instruction();
        },
        .array => {
            self.array();
            continue :loop self.instruction();
        },
        .clone => {
            self.clone();
            continue :loop self.instruction();
        },
        .index => {
            self.index();
            continue :loop self.instruction();
        },
        .append => {
            self.append();
            continue :loop self.instruction();
        },
        .constant => {
            self.constant();
            continue :loop self.instruction();
        },
        .negate_i => {
            self.negateInt();
            continue :loop self.instruction();
        },
        .negate_f => {
            self.negateFloat();
            continue :loop self.instruction();
        },
        .not => {
            self.not();
            continue :loop self.instruction();
        },
        .equal => {
            self.equal();
            continue :loop self.instruction();
        },
        .unequal => {
            self.unequal();
            continue :loop self.instruction();
        },
        .get_builtin => {
            self.getBuiltin();
            continue :loop self.instruction();
        },
        .get_global => {
            self.getGlobal();
            continue :loop self.instruction();
        },
        .set_global => {
            self.setGlobal();
            continue :loop self.instruction();
        },
        .set_global_array => {
            self.setGlobalArray();
            continue :loop self.instruction();
        },
        .get_local => {
            self.getLocal();
            continue :loop self.instruction();
        },
        .set_local => {
            self.setLocal();
            continue :loop self.instruction();
        },
        .set_local_array => {
            self.setLocalArray();
            continue :loop self.instruction();
        },
        .jump => {
            self.jump();
            continue :loop self.instruction();
        },
        .jump_back => {
            self.jumpBack();
            continue :loop self.instruction();
        },
        .jump_if_true => {
            self.jumpIfTrue();
            continue :loop self.instruction();
        },
        .jump_if_false => {
            self.jumpIfFalse();
            continue :loop self.instruction();
        },
        .call => {
            self.call();
            continue :loop self.instruction();
        },
        .@"return" => {
            self.returnInstr();
            continue :loop self.instruction();
        },
        .function => {
            self.function();
            continue :loop self.instruction();
        },
        .eof => {},

        .string,
        .string_long,
        .integer,
        .float,
        .constants_done,
        => |op| panic("Illegal Instruction at {x:0>4}: {}\n", .{ self.ip, op }),
    }
}

inline fn concat(self: *VM) void {
    const rhs = self.pop();
    const lhs = self.pop();

    const result = std.fmt.allocPrint(self.gc, "{}{}", .{ lhs, rhs }) catch oom();
    self.push(.{ .string = result });
}

inline fn arithmeticInt(self: *VM, op: OpCode) void {
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

inline fn arithmeticFloat(self: *VM, op: OpCode) void {
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

inline fn comparison(self: *VM, op: OpCode) void {
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

inline fn array(self: *VM) void {
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
}

inline fn clone(self: *VM) void {
    const value = self.pop();
    self.push(value.clone(self.gc));
}

inline fn index(self: *VM) void {
    const idx = self.pop().int;
    const arr = self.pop().array;
    if (idx < 0) {
        std.debug.panic("IndexUnderflow: {d}\n", .{idx});
    } else if (idx > arr.len) {
        std.debug.panic("IndexOverflow: {d}, array len: {d}\n", .{ idx, arr.len });
    }
    self.push(arr.items[@intCast(idx)]);
}

inline fn append(self: *VM) void {
    const value = self.pop();
    var arr = &self.value_stack.stack[self.value_stack.stack_top - 1];

    if (arr.array.cap >= arr.array.len + 1) {
        arr.array.items[arr.array.len] = value;
        arr.array.len += 1;
    } else {
        if (arr.array.cap == 0) arr.array.cap = 1;
        const new_values = self.gc.alloc(FlowValue, arr.array.cap * 2) catch oom();
        @memcpy(new_values[0..arr.array.len], arr.array.items[0..arr.array.len]);
        new_values[arr.array.len] = value;
        arr.array.len += 1;
        arr.array.cap *= 2;
        arr.array.items = new_values.ptr;
    }
}

inline fn constant(self: *VM) void {
    const c = self.constants[self.byte()];
    self.push(c);
}

inline fn negateInt(self: *VM) void {
    const value = self.pop();
    const negated: FlowValue = .{ .int = -value.int };
    self.push(negated);
}

inline fn negateFloat(self: *VM) void {
    const value = self.pop();
    const negated: FlowValue = .{ .float = -value.float };
    self.push(negated);
}

inline fn not(self: *VM) void {
    const value = self.pop();
    self.push(.{ .bool = !value.isTrue() });
}

inline fn equal(self: *VM) void {
    const rhs = self.pop();
    const lhs = self.pop();
    const res = lhs.equals(&rhs);
    self.push(.{ .bool = res });
}

inline fn unequal(self: *VM) void {
    const rhs = self.pop();
    const lhs = self.pop();
    const res = lhs.equals(&rhs);
    self.push(.{ .bool = !res });
}

inline fn getBuiltin(self: *VM) void {
    const name = self.pop();
    self.push(.{ .builtin_fn = builtins.get(name.string).? });
}

inline fn getGlobal(self: *VM) void {
    const idx = self.byte();
    const value = self.globals[idx];
    self.push(value);
}

inline fn setGlobal(self: *VM) void {
    const idx = self.byte();
    const value = self.value_stack.at(0);
    self.globals[idx] = value;

    if (self.globals_count <= idx) {
        self.globals_count = idx + 1;
    }
}

inline fn setGlobalArray(self: *VM) void {
    const global_idx = self.byte();
    const index_amount = self.byte();
    assert(index_amount > 0);

    var arr = self.globals[global_idx];

    for (0..index_amount - 1) |_| {
        const idx = self.pop().int;
        if (idx < 0) {
            std.debug.panic("IndexUnderflow: {d}\n", .{idx});
        } else if (idx > arr.array.len) {
            std.debug.panic("IndexOverflow: {d}, array len: {d}\n", .{ idx, arr.array.len });
        }
        arr = arr.array.items[@intCast(idx)];
    }

    const last_idx = self.pop().int;
    arr.array.items[@intCast(last_idx)] = self.value_stack.at(0);
}

inline fn getLocal(self: *VM) void {
    const idx = self.byte();
    const frame = self.call_stack.at(0);
    const value = self.value_stack.stack[frame.stack_bottom + idx];
    self.push(value);
}

inline fn setLocal(self: *VM) void {
    const idx = self.byte();
    const value = self.value_stack.at(0);
    const frame = self.call_stack.at(0);
    self.value_stack.stack[frame.stack_bottom + idx] = value;
}

inline fn setLocalArray(self: *VM) void {
    const local_idx = self.byte();
    const index_amount = self.byte();
    assert(index_amount > 0);

    const frame = self.call_stack.at(0);
    var arr = self.value_stack.stack[frame.stack_bottom + local_idx];

    for (0..index_amount - 1) |_| {
        const idx = self.pop().int;
        if (idx < 0) {
            std.debug.panic("IndexUnderflow: {d}\n", .{idx});
        } else if (idx > arr.array.len) {
            std.debug.panic("IndexOverflow: {d}, array len: {d}\n", .{ idx, arr.array.len });
        }
        arr = arr.array.items[@intCast(idx)];
    }

    const last_idx = self.pop().int;
    arr.array.items[@intCast(last_idx)] = self.value_stack.at(0);
}

inline fn jump(self: *VM) void {
    const distance = self.short();
    self.ip += distance;
}

inline fn jumpBack(self: *VM) void {
    const distance = self.short();
    self.ip -= distance;
}

inline fn jumpIfTrue(self: *VM) void {
    const distance = self.short();
    const value = self.value_stack.at(0);
    self.ip += distance * @intFromBool(value.isTrue());
}

inline fn jumpIfFalse(self: *VM) void {
    const distance = self.short();
    const value = self.value_stack.at(0);
    self.ip += distance * @intFromBool(!value.isTrue());
}

inline fn call(self: *VM) void {
    const value = self.pop();
    switch (value) {
        .builtin_fn => self.callBuiltin(value),
        .function => self.callFlowFunction(value),
        else => unreachable,
    }
}

inline fn callBuiltin(self: *VM, value: FlowValue) void {
    const arg_count = value.builtin_fn.arg_types.len;
    const args = self.value_stack.stack[self.value_stack.stack_top - arg_count .. self.value_stack.stack_top];

    // NOTE: Running a builtin function should not trigger a GC
    const gc: *GC = @ptrCast(@alignCast(self.gc.ptr));
    gc.disable();
    const result = value.builtin_fn.function(self.gc, args);
    gc.enable();

    for (0..arg_count) |_| {
        _ = self.pop();
    }

    self.push(result);
}

inline fn callFlowFunction(self: *VM, value: FlowValue) void {
    const arg_count = value.function.arg_count;
    const stack_bottom = self.value_stack.stack_top - arg_count;
    self.call_stack.push(.{
        .ret_addr = self.ip,
        .stack_bottom = stack_bottom,
    });
    for (value.function.closed_values) |closed_value| {
        self.push(closed_value);
    }
    self.ip = value.function.start_ip;
}

inline fn returnInstr(self: *VM) void {
    const ret_value = self.pop();
    const frame = self.call_stack.pop();
    self.ip = frame.ret_addr;
    self.value_stack.stack_top = frame.stack_bottom;
    self.push(ret_value);
}

inline fn function(self: *VM) void {
    const argc = self.byte();
    const closed_values_count = self.byte();
    const end = self.short();

    const closed_values = self.gc.alloc(FlowValue, closed_values_count) catch oom();
    for (closed_values) |*value| {
        value.* = self.pop();
    }

    self.push(.{
        .function = .{
            .arg_count = argc,
            .closed_values = closed_values,
            .start_ip = self.ip,
        },
    });

    self.ip += end - 1;
}

inline fn push(self: *VM, value: FlowValue) void {
    self.value_stack.push(value);
}

inline fn pop(self: *VM) FlowValue {
    return self.value_stack.pop();
}

inline fn instruction(self: *VM) OpCode {
    const op: OpCode = @enumFromInt(self.code[self.ip]);
    defer self.ip += 1;

    if (comptime debug_options.stack) {
        self.value_stack.dump();
    }
    if (comptime debug_options.bytecode) {
        std.debug.print("{x:0>4} {}\n", .{ self.ip, op });
    }

    return op;
}

inline fn short(self: *VM) u16 {
    return std.mem.bytesToValue(u16, &.{ self.byte(), self.byte() });
}

inline fn byte(self: *VM) u8 {
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
const assert = std.debug.assert;

const stdout = std.io.getStdOut().writer();

const GC = @import("gc/Simple.zig");

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
