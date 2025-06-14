ip: usize,
code: []const u8,
constants: [256]FlowValue,
constant_counter: usize,
function_counter: usize,
writer: std.io.AnyWriter,

pub fn dump(writer: std.io.AnyWriter, code: []const u8) void {
    var dumper: Dumper = .{
        .ip = 0,
        .code = code,
        .writer = writer,
        .constants = undefined,
        .constant_counter = 0,
        .function_counter = 0,
    };
    dumper.runDump();
}

fn runDump(self: *Dumper) void {
    while (self.ip < self.code.len) {
        defer self.writer.writeAll("\n") catch unreachable;
        self.writer.print("{x:0>4} ", .{self.ip}) catch unreachable;
        const op = self.instruction();
        switch (op) {
            .constant => {
                const idx = self.byte();
                const value = self.constants[idx];
                self.printInstruction("OP_CONSTANT", "{d: <10}{}", .{ idx, value });
            },
            .function => {
                defer self.ip += 2;
                defer self.function_counter += 1;
                const arg_count = self.byte();
                const line_count = std.mem.bytesToValue(u16, self.code[self.ip .. self.ip + 2]);
                self.printInstruction("OP_FUNCTION", "{d: <10}{{{d}}} [{x:0>4}]", .{ arg_count, self.function_counter, self.ip + line_count });
            },
            .integer => self.constantInstruction("OP_INTEGER", .int),
            .float => self.constantInstruction("OP_FLOAT", .float),
            .string => self.constantInstruction("OP_STRING", .string),
            .string_long => self.constantInstruction("OP_STRING_LONG", .string_long),
            .get_local => self.variableInstruction("OP_GET_LOCAL"),
            .set_local => self.variableInstruction("OP_SET_LOCAL"),
            .jump_if_false => self.jumpInstruction("OP_JUMP_IF_FALSE", true),
            .jump_if_true => self.jumpInstruction("OP_JUMP_IF_TRUE", true),
            .jump_back => self.jumpInstruction("OP_JUMP_BACK", false),
            .jump => self.jumpInstruction("OP_JUMP", true),
            .call => self.printInstruction("OP_CALL", "", .{}),
            .array => self.printInstruction("OP_ARRAY", "", .{}),
            .index => self.printInstruction("OP_INDEX", "", .{}),
            .get_builtin => self.variableInstruction("OP_GET_BUILTIN"),
            .get_global => self.variableInstruction("OP_GET_GLOBAL"),
            .set_global => self.variableInstruction("OP_SET_GLOBAL"),
            .true => self.printInstruction("OP_TRUE", "", .{}),
            .false => self.printInstruction("OP_FALSE", "", .{}),
            .null => self.printInstruction("OP_NULL", "", .{}),
            .pop => self.printInstruction("OP_POP", "", .{}),
            .not => self.printInstruction("OP_NOT", "", .{}),
            .concat => self.printInstruction("OP_CONCAT", "", .{}),
            .negate_i => self.printInstruction("OP_NEGATE_I", "", .{}),
            .negate_f => self.printInstruction("OP_NEGATE_F", "", .{}),
            .add_i => self.printInstruction("OP_ADD_I", "", .{}),
            .sub_i => self.printInstruction("OP_SUB_I", "", .{}),
            .div_i => self.printInstruction("OP_DIV_I", "", .{}),
            .mul_i => self.printInstruction("OP_MUL_I", "", .{}),
            .mod_i => self.printInstruction("OP_MOD_I", "", .{}),
            .add_f => self.printInstruction("OP_ADD_F", "", .{}),
            .sub_f => self.printInstruction("OP_SUB_F", "", .{}),
            .div_f => self.printInstruction("OP_DIV_F", "", .{}),
            .mul_f => self.printInstruction("OP_MUL_F", "", .{}),
            .mod_f => self.printInstruction("OP_MOD_F", "", .{}),
            .lower => self.printInstruction("OP_LOWER", "", .{}),
            .lower_equal => self.printInstruction("OP_LOWER_EQUAL", "", .{}),
            .greater => self.printInstruction("OP_GREATER", "", .{}),
            .greater_equal => self.printInstruction("OP_GREATER_EQUAL", "", .{}),
            .equal => self.printInstruction("OP_EQUAL", "", .{}),
            .unequal => self.printInstruction("OP_UNEQUAL", "", .{}),
            .@"return" => self.printInstruction("OP_RETURN", "", .{}),
            .constants_done => self.printInstruction("OP_CONSTANTS_DONE", "", .{}),
            .functions_done => self.printInstruction("OP_FUNCTIONS_DONE", "", .{}),
        }
    }
}

fn printInstruction(self: *Dumper, op: []const u8, comptime fmt: []const u8, args: anytype) void {
    self.writer.print("{s: <24} ", .{op}) catch unreachable;
    self.writer.print(fmt, args) catch unreachable;
}

fn jumpInstruction(self: *Dumper, op: []const u8, forward: bool) void {
    defer self.ip += 2;
    const jump_len = std.mem.bytesToValue(u16, self.code[self.ip .. self.ip + 2]);
    const target = if (forward) self.ip + jump_len + 2 else self.ip - jump_len + 2;
    self.printInstruction(op, "-> {x:0>4}", .{target});
}

fn variableInstruction(self: *Dumper, op: []const u8) void {
    const operand = self.byte();
    self.printInstruction(op, "{d}", .{operand});
}

fn constantInstruction(self: *Dumper, op: []const u8, flow_type: enum { int, float, string, string_long }) void {
    defer self.constant_counter += 1;
    switch (flow_type) {
        .int, .float => {
            defer self.ip += 8;
            const bytes = self.code[self.ip .. self.ip + 8];
            if (flow_type == .int) {
                self.constants[self.constant_counter] = .{ .int = std.mem.bytesToValue(Integer, bytes) };
            } else {
                self.constants[self.constant_counter] = .{ .float = std.mem.bytesToValue(Float, bytes) };
            }
        },
        .string => {
            const len = self.code[self.ip];
            self.ip += 1;
            self.constants[self.constant_counter] = .{ .string = self.code[self.ip .. self.ip + len] };
            self.ip += len;
        },
        .string_long => {
            const bytes = self.code[self.ip .. self.ip + 4];
            self.ip += 4;
            const len = std.mem.bytesToValue(u32, bytes);
            self.constants[self.constant_counter] = .{ .string = self.code[self.ip .. self.ip + len] };
            self.ip += len;
        },
    }
    self.printInstruction(op, "{d: <10}{}", .{ self.constant_counter, self.constants[self.constant_counter] });
}

fn instruction(self: *Dumper) OpCode {
    defer self.ip += 1;
    return @enumFromInt(self.code[self.ip]);
}

fn byte(self: *Dumper) u8 {
    defer self.ip += 1;
    return self.code[self.ip];
}

const Dumper = @This();

const std = @import("std");

const Integer = @import("../definitions.zig").Integer;
const Float = @import("../definitions.zig").Float;
const OpCode = @import("../byte_code.zig").OpCode;
const FlowValue = @import("../definitions.zig").FlowValue;
