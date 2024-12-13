ip: usize,
code: []const u8,
constants: [256]FlowValue = undefined,
constant_counter: usize = 0,

pub fn dump(code: []const u8) void {
    var dumper: Dumper = .{
        .ip = 0,
        .code = code,
    };
    dumper.runDump();
}

fn runDump(self: *Dumper) void {
    while (self.ip < self.code.len) {
        defer std.debug.print("\n", .{});
        std.debug.print("{x:0>4} ", .{self.ip});
        const op = self.instruction();
        switch (op) {
            .constant => {
                const idx = self.byte();
                const value = self.constants[idx];
                printInstruction("OP_CONSTANT", "{d: <10}{}", .{ idx, value });
            },
            .function => {
                defer self.ip += 2;
                const name_idx = self.byte();
                const name = self.constants[name_idx];
                const arg_count = self.byte();
                const line_count = std.mem.bytesToValue(u16, self.code[self.ip .. self.ip + 2]);
                printInstruction("OP_FUNCTION", "{d: <10}{{{s}}} [{x:0>4}]", .{ arg_count, name, self.ip + line_count });
            },
            .integer => self.constantInstruction("OP_INTEGER", .int),
            .float => self.constantInstruction("OP_FLOAT", .float),
            .string => self.constantInstruction("OP_STRING", .string),
            .string_long => self.constantInstruction("OP_STRING_LONG", .string_long),
            .get_local => self.localInstruction("OP_GET_LOCAL"),
            .set_local => self.localInstruction("OP_SET_LOCAL"),
            .jump_if_false => self.jumpInstruction("OP_JUMP_IF_FALSE", true),
            .jump_if_true => self.jumpInstruction("OP_JUMP_IF_TRUE", true),
            .jump_back => self.jumpInstruction("OP_JUMP_BACK", false),
            .jump => self.jumpInstruction("OP_JUMP", true),
            .call => printInstruction("OP_CALL", "", .{}),
            .create_global => printInstruction("OP_CREATE_GLOBAL", "", .{}),
            .get_global => printInstruction("OP_GET_GLOBAL", "", .{}),
            .set_global => printInstruction("OP_SET_GLOBAL", "", .{}),
            .true => printInstruction("OP_TRUE", "", .{}),
            .false => printInstruction("OP_FALSE", "", .{}),
            .null => printInstruction("OP_NULL", "", .{}),
            .pop => printInstruction("OP_POP", "", .{}),
            .not => printInstruction("OP_NOT", "", .{}),
            .negate => printInstruction("OP_NEGATE", "", .{}),
            .concat => printInstruction("OP_CONCAT", "", .{}),
            .add_i => printInstruction("OP_ADD_I", "", .{}),
            .sub_i => printInstruction("OP_SUB_I", "", .{}),
            .div_i => printInstruction("OP_DIV_I", "", .{}),
            .mul_i => printInstruction("OP_MUL_I", "", .{}),
            .mod_i => printInstruction("OP_MOD_I", "", .{}),
            .add_f => printInstruction("OP_ADD_F", "", .{}),
            .sub_f => printInstruction("OP_SUB_F", "", .{}),
            .div_f => printInstruction("OP_DIV_F", "", .{}),
            .mul_f => printInstruction("OP_MUL_F", "", .{}),
            .mod_f => printInstruction("OP_MOD_F", "", .{}),
            .lower => printInstruction("OP_LOWER", "", .{}),
            .lower_equal => printInstruction("OP_LOWER_EQUAL", "", .{}),
            .greater => printInstruction("OP_GREATER", "", .{}),
            .greater_equal => printInstruction("OP_GREATER_EQUAL", "", .{}),
            .equal => printInstruction("OP_EQUAL", "", .{}),
            .unequal => printInstruction("OP_UNEQUAL", "", .{}),
            .@"return" => printInstruction("OP_RETURN", "", .{}),
            .constants_done => printInstruction("OP_CONSTANTS_DONE", "", .{}),
            .functions_done => printInstruction("OP_FUNCTIONS_DONE", "", .{}),
        }
    }
}

fn printInstruction(op: []const u8, comptime fmt: []const u8, args: anytype) void {
    std.debug.print("{s: <24} ", .{op});
    std.debug.print(fmt, args);
}

fn jumpInstruction(self: *Dumper, op: []const u8, forward: bool) void {
    defer self.ip += 2;
    const jump_len = std.mem.bytesToValue(u16, self.code[self.ip .. self.ip + 2]);
    const target = if (forward) self.ip + jump_len + 2 else self.ip - jump_len + 2;
    printInstruction(op, "-> {x:0>4}", .{target});
}

fn localInstruction(self: *Dumper, op: []const u8) void {
    const operand = self.byte();
    printInstruction(op, "{d}", .{operand});
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
    printInstruction(op, "{d: <10}{}", .{ self.constant_counter, self.constants[self.constant_counter] });
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

const Integer = @import("shared").definitions.Integer;
const Float = @import("shared").definitions.Float;
const OpCode = @import("shared").OpCode;
const FlowValue = @import("shared").definitions.FlowValue;
