const STACK_SIZE = 1024;

gpa: Allocator,
gc: Allocator,
code: []const u8,
ip: usize,
value_stack: Stack(FlowValueRef),
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
        .value_stack = .init(gpa.alloc(FlowValueRef, STACK_SIZE) catch oom()),
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
    for (self.value_stack.stack) |*value| {
        value.* = .{ .direct = .null };
    }
    for (0..self.globals.len) |idx| {
        self.globals[idx] = .null;
    }

    self.loadConstants();
    self.call_stack.push(.{
        .stack_bottom = 0,
        .ret_addr = self.ip,
    });

    switch (comptime @import("vm_options").run_mode) {
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
        switch (self.instruction()) {
            .true => self.pushTrue(),
            .false => self.pushFalse(),
            .null => self.pushNull(),
            .pop => self.popSilent(),

            .add_i => self.addInt(),
            .sub_i => self.subInt(),
            .mul_i => self.mulInt(),
            .div_i => self.divInt(),
            .mod_i => self.modInt(),

            .add_f => self.addFloat(),
            .sub_f => self.subFloat(),
            .mul_f => self.mulFloat(),
            .div_f => self.divFloat(),
            .mod_f => self.modFloat(),

            .equal => self.equal(),
            .unequal => self.unequal(),

            .greater_i => self.greaterInt(),
            .greater_equal_i => self.greaterEqualInt(),
            .lower_i => self.lowerInt(),
            .lower_equal_i => self.lowerEqualInt(),

            .greater_f => self.greaterFloat(),
            .greater_equal_f => self.greaterEqualFloat(),
            .lower_f => self.lowerFloat(),
            .lower_equal_f => self.lowerEqualFloat(),

            .concat => self.concat(),
            .array => self.array(),
            .clone => self.clone(),
            .index => self.index(),
            .append => self.append(),
            .constant => self.constant(),
            .negate_i => self.negateInt(),
            .negate_f => self.negateFloat(),
            .not => self.not(),
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

            inline .string,
            .string_long,
            .integer,
            .float,
            .constants_done,
            => self.illegalInstruction(),
        }
    }
}

fn runSwitchContinue(self: *VM) void {
    loop: switch (self.instruction()) {
        .true => {
            self.pushTrue();
            continue :loop self.instruction();
        },
        .false => {
            self.pushFalse();
            continue :loop self.instruction();
        },
        .null => {
            self.pushNull();
            continue :loop self.instruction();
        },
        .pop => {
            self.popSilent();
            continue :loop self.instruction();
        },
        .add_i => {
            self.addInt();
            continue :loop self.instruction();
        },
        .sub_i => {
            self.subInt();
            continue :loop self.instruction();
        },
        .mul_i => {
            self.mulInt();
            continue :loop self.instruction();
        },
        .div_i => {
            self.divInt();
            continue :loop self.instruction();
        },
        .mod_i => {
            self.modInt();
            continue :loop self.instruction();
        },
        .add_f => {
            self.addFloat();
            continue :loop self.instruction();
        },
        .sub_f => {
            self.subFloat();
            continue :loop self.instruction();
        },
        .mul_f => {
            self.mulFloat();
            continue :loop self.instruction();
        },
        .div_f => {
            self.divFloat();
            continue :loop self.instruction();
        },
        .mod_f => {
            self.modFloat();
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
        .greater_i => {
            self.greaterInt();
            continue :loop self.instruction();
        },
        .greater_equal_i => {
            self.greaterEqualInt();
            continue :loop self.instruction();
        },
        .lower_i => {
            self.lowerInt();
            continue :loop self.instruction();
        },
        .lower_equal_i => {
            self.lowerEqualInt();
            continue :loop self.instruction();
        },
        .greater_f => {
            self.greaterFloat();
            continue :loop self.instruction();
        },
        .greater_equal_f => {
            self.greaterEqualFloat();
            continue :loop self.instruction();
        },
        .lower_f => {
            self.lowerFloat();
            continue :loop self.instruction();
        },
        .lower_equal_f => {
            self.lowerEqualFloat();
            continue :loop self.instruction();
        },
        .concat => {
            self.concat();
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
        => self.illegalInstruction(),
    }
}

fn illegalInstruction(self: *VM) void {
    @branchHint(.cold);
    panic("Illegal Instruction at {x:0>4}\n", .{self.ip});
}

fn pushTrue(self: *VM) void {
    self.pushValue(.{ .bool = true });
}

fn pushFalse(self: *VM) void {
    self.pushValue(.{ .bool = false });
}

fn pushNull(self: *VM) void {
    self.pushValue(.null);
}

fn popSilent(self: *VM) void {
    self.popAmount(1);
}

fn addInt(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .int = lhs.int + rhs.int });
}
fn subInt(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .int = lhs.int - rhs.int });
}
fn mulInt(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .int = lhs.int * rhs.int });
}
fn divInt(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .int = @divTrunc(lhs.int, rhs.int) });
}
fn modInt(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .int = @mod(lhs.int, rhs.int) });
}

fn addFloat(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .float = lhs.float + rhs.float });
}
fn subFloat(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .float = lhs.float - rhs.float });
}
fn mulFloat(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .float = lhs.float * rhs.float });
}
fn divFloat(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .float = lhs.float / rhs.float });
}
fn modFloat(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .float = @mod(lhs.float, rhs.float) });
}

fn lowerInt(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .bool = lhs.int < rhs.int });
}
fn lowerEqualInt(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .bool = lhs.int <= rhs.int });
}
fn greaterInt(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .bool = lhs.int > rhs.int });
}
fn greaterEqualInt(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .bool = lhs.int >= rhs.int });
}
fn lowerFloat(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .bool = lhs.float < rhs.float });
}
fn lowerEqualFloat(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .bool = lhs.float <= rhs.float });
}
fn greaterFloat(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .bool = lhs.float > rhs.float });
}
fn greaterEqualFloat(self: *VM) void {
    const rhs = self.popNullSafe();
    const lhs = self.popNullSafe();
    self.pushValue(.{ .bool = lhs.float >= rhs.float });
}

fn concat(self: *VM) void {
    const rhs = self.pop();
    const lhs = self.pop();

    const result = std.fmt.allocPrint(self.gc, "{}{}", .{ lhs, rhs }) catch oom();
    self.pushValue(.{ .string = result });
}

fn array(self: *VM) void {
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
    self.pushValue(.{ .array = arr });
}

fn clone(self: *VM) void {
    const value = self.pop();
    self.pushValue(value.clone(self.gc));
}

fn index(self: *VM) void {
    const idx = self.pop().int;
    const arr = self.pop().array;
    if (idx < 0) {
        @branchHint(.unlikely);
        std.debug.panic("IndexUnderflow: {d}\n", .{idx});
    } else if (idx > arr.len) {
        @branchHint(.unlikely);
        std.debug.panic("IndexOverflow: {d}, array len: {d}\n", .{ idx, arr.len });
    }
    self.pushValue(arr.items[@intCast(idx)]);
}

fn append(self: *VM) void {
    const value = self.pop();
    var arr = self.top().deref();

    if (arr.array.cap >= arr.array.len + 1) {
        arr.array.items[arr.array.len] = value;
        arr.array.len += 1;
    } else {
        if (arr.array.cap == 0) arr.array.cap = 1;
        const new_values = self.gc.realloc(arr.array.items[0..arr.array.len], arr.array.cap * 2) catch oom();
        new_values[arr.array.len] = value;
        arr.array.len += 1;
        arr.array.cap *= 2;
        arr.array.items = new_values.ptr;
    }
}

fn constant(self: *VM) void {
    self.pushRef(&self.constants[self.byte()]);
}

fn negateInt(self: *VM) void {
    const value = self.popNullSafe();
    const negated: FlowValue = .{ .int = -value.int };
    self.pushValue(negated);
}

fn negateFloat(self: *VM) void {
    const value = self.popNullSafe();
    const negated: FlowValue = .{ .float = -value.float };
    self.pushValue(negated);
}

fn not(self: *VM) void {
    const value = self.pop();
    self.pushValue(.{ .bool = !value.isTrue() });
}

fn equal(self: *VM) void {
    const rhs = self.pop();
    const lhs = self.pop();
    const res = lhs.equals(&rhs);
    self.pushValue(.{ .bool = res });
}

fn unequal(self: *VM) void {
    const rhs = self.pop();
    const lhs = self.pop();
    const res = lhs.equals(&rhs);
    self.pushValue(.{ .bool = !res });
}

fn getBuiltin(self: *VM) void {
    const name = self.pop();
    self.pushValue(.{ .builtin_fn = builtins.get(name.string).? });
}

fn getGlobal(self: *VM) void {
    const idx = self.byte();
    const value = self.globals[idx];
    self.pushValue(value);
}

fn setGlobal(self: *VM) void {
    const idx = self.byte();
    const value = self.top();
    self.globals[idx] = value.deref();

    if (self.globals_count <= idx) {
        self.globals_count = idx + 1;
    }
}

fn setGlobalArray(self: *VM) void {
    const global_idx = self.byte();
    const index_amount = self.byte();
    assert(index_amount > 0);

    var arr = self.globals[global_idx];

    for (0..index_amount - 1) |_| {
        const idx = self.pop().int;
        if (idx < 0) {
            @branchHint(.unlikely);
            std.debug.panic("IndexUnderflow: {d}\n", .{idx});
        } else if (idx > arr.array.len) {
            @branchHint(.unlikely);
            std.debug.panic("IndexOverflow: {d}, array len: {d}\n", .{ idx, arr.array.len });
        }
        arr = arr.array.items[@intCast(idx)];
    }

    const last_idx = self.pop().int;
    arr.array.items[@intCast(last_idx)] = self.top().deref();
}

fn getLocal(self: *VM) void {
    const idx = self.byte();
    const frame = self.call_stack.at(0);
    const value = self.value_stack.stack[frame.stack_bottom + idx];
    self.pushValue(value.deref());
}

fn setLocal(self: *VM) void {
    const idx = self.byte();
    const value = self.value_stack.at(0);
    const frame = self.call_stack.at(0);
    self.value_stack.stack[frame.stack_bottom + idx] = value;
}

fn setLocalArray(self: *VM) void {
    const local_idx = self.byte();
    const index_amount = self.byte();
    assert(index_amount > 0);

    const frame = self.call_stack.at(0);
    var arr = self.value_stack.stack[frame.stack_bottom + local_idx].deref();

    for (0..index_amount - 1) |_| {
        const idx = self.pop().int;
        if (idx < 0) {
            @branchHint(.unlikely);
            std.debug.panic("IndexUnderflow: {d}\n", .{idx});
        } else if (idx > arr.array.len) {
            @branchHint(.unlikely);
            std.debug.panic("IndexOverflow: {d}, array len: {d}\n", .{ idx, arr.array.len });
        }
        arr = arr.array.items[@intCast(idx)];
    }

    const last_idx = self.pop().int;
    arr.array.items[@intCast(last_idx)] = self.top().deref();
}

fn jump(self: *VM) void {
    const distance = self.short();
    self.ip += distance;
}

fn jumpBack(self: *VM) void {
    const distance = self.short();
    self.ip -= distance;
}

fn jumpIfTrue(self: *VM) void {
    const distance = self.short();
    const value = self.top().deref();
    self.ip += distance * @intFromBool(value.isTrue());
}

fn jumpIfFalse(self: *VM) void {
    const distance = self.short();
    const value = self.top().deref();
    self.ip += distance * @intFromBool(!value.isTrue());
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
    const args = self.gpa.alloc(FlowValue, arg_count) catch oom();
    defer self.gpa.free(args);

    const stack_view = self.value_stack.stack[self.value_stack.stack_top - arg_count .. self.value_stack.stack_top];
    for (args, stack_view) |*a, v| {
        a.* = v.deref();
    }

    // NOTE: Running a builtin function should not trigger a GC
    const gc: *GC = @ptrCast(@alignCast(self.gc.ptr));
    gc.disable();
    const result = value.builtin_fn.function(self.gc, args);
    gc.enable();

    self.popAmount(@intCast(arg_count));

    self.pushValue(result);
}

fn callFlowFunction(self: *VM, value: FlowValue) void {
    const arg_count = value.function.arg_count;
    const stack_bottom = self.value_stack.stack_top - arg_count;
    self.call_stack.push(.{
        .ret_addr = self.ip,
        .stack_bottom = stack_bottom,
    });
    for (value.function.closed_values) |closed_value| {
        self.pushValue(closed_value);
    }
    self.ip = value.function.start_ip;
}

fn returnInstr(self: *VM) void {
    const ret_value = self.pop();
    const frame = self.call_stack.pop();
    self.ip = frame.ret_addr;
    self.value_stack.stack_top = frame.stack_bottom;
    self.pushValue(ret_value);
}

fn function(self: *VM) void {
    const argc = self.byte();
    const closed_values_count = self.byte();
    const end = self.short();

    const closed_values = self.gc.alloc(FlowValue, closed_values_count) catch oom();
    for (closed_values) |*value| {
        value.* = self.pop();
    }

    self.pushValue(.{
        .function = .{
            .arg_count = argc,
            .closed_values = closed_values,
            .start_ip = self.ip,
        },
    });

    self.ip += end - 1;
}

fn pushValue(self: *VM, value: FlowValue) void {
    self.value_stack.push(.{ .direct = value });
}

fn pushRef(self: *VM, ref: *FlowValue) void {
    self.value_stack.push(.{ .pointer = ref });
}

fn pop(self: *VM) FlowValue {
    return self.value_stack.pop().deref();
}

fn popNullSafe(self: *VM) FlowValue {
    const val = self.pop();
    if (val == .null) @panic("Illegal Null Value");
    return val;
}

fn popAmount(self: *VM, count: u8) void {
    assert(self.value_stack.stack_top - count >= 0);
    self.value_stack.stack_top -= count;
}

fn top(self: *VM) FlowValueRef {
    return self.value_stack.at(0);
}

fn instruction(self: *VM) OpCode {
    defer self.ip += 1;
    const op: OpCode = @enumFromInt(self.code[self.ip]);

    if (comptime debug_options.stack) {
        self.value_stack.dump();
    }
    if (comptime debug_options.bytecode) {
        std.debug.print("{x:0>4} {}\n", .{ self.ip, op });
    }

    return op;
}

fn instructionNumber(self: *VM) u8 {
    defer self.ip += 1;
    return self.code[self.ip];
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

const FlowValueRef = union(enum) {
    direct: FlowValue,
    pointer: *FlowValue,

    pub fn deref(self: FlowValueRef) FlowValue {
        return switch (self) {
            .direct => |v| v,
            .pointer => |p| p.*,
        };
    }

    pub fn format(self: FlowValueRef, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .direct => |v| try writer.print("{}", .{v}),
            .pointer => |p| try writer.print("{}", .{p.*}),
        }
    }
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
