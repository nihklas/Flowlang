ip: usize,
code: []const u8,
constants: [256]Value = undefined,

pub fn dump(code: []const u8) void {
    var dumper: Dumper = .{
        .ip = 0,
        .code = code,
    };
    dumper.runDump();
}

fn runDump(self: *Dumper) void {
    var constant_counter: usize = 0;
    var constant_block: bool = true;
    while (self.ip < self.code.len) {
        defer std.debug.print("\n", .{});
        std.debug.print("{x:0>4} ", .{self.ip});
        const op = self.instruction();
        switch (op) {
            .integer => {
                defer constant_counter += 1;
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const int = std.mem.bytesToValue(Integer, bytes);
                self.constants[constant_counter] = .{ .int = int };
                printInstruction("OP_INTEGER", "{d: <10}{d}", .{ constant_counter, int });
            },
            .float => {
                defer constant_counter += 1;
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const float = std.mem.bytesToValue(Float, bytes);
                self.constants[constant_counter] = .{ .float = float };
                printInstruction("OP_FLOAT", "{d: <10}{d}", .{ constant_counter, float });
            },
            .string => {
                defer constant_counter += 1;
                const len = self.code[self.ip];
                self.ip += 1;
                self.constants[constant_counter] = .{ .string = self.code[self.ip .. self.ip + len] };
                printInstruction("OP_STRING", "{d: <10}{s}", .{
                    constant_counter,
                    self.constants[constant_counter].string,
                });
                self.ip += len;
            },
            .string_long => {
                defer constant_counter += 1;
                const bytes = self.code[self.ip .. self.ip + 4];
                self.ip += 4;
                const len = std.mem.bytesToValue(u32, bytes);
                self.constants[constant_counter] = .{ .string = self.code[self.ip .. self.ip + len] };
                printInstruction("OP_STRING_LONG", "{d: <10}{s}", .{
                    constant_counter,
                    self.constants[constant_counter].string,
                });
                self.ip += len;
            },
            .constant => {
                defer self.ip += 1;
                const idx = self.code[self.ip];
                const value = self.constants[idx];
                printInstruction("OP_CONSTANT", "{d: <10}{}", .{ idx, value });
            },
            .jump_if_false => {
                defer self.ip += 2;
                const jump_len = std.mem.bytesToValue(u16, self.code[self.ip .. self.ip + 2]);
                printInstruction("OP_JUMP_IF_FALSE", "-> {x:0>4}", .{jump_len + self.ip + 2});
            },
            .jump => {
                defer self.ip += 2;
                const jump_len = std.mem.bytesToValue(u16, self.code[self.ip .. self.ip + 2]);
                printInstruction("OP_JUMP", "-> {x:0>4}", .{jump_len + self.ip + 2});
            },
            .jump_back => {
                defer self.ip += 2;
                const jump_len = std.mem.bytesToValue(u16, self.code[self.ip .. self.ip + 2]);
                printInstruction("OP_JUMP_BACK", "-> {x:0>4}", .{self.ip - jump_len + 2});
            },
            .get_local => {
                defer self.ip += 1;
                const local_idx = self.code[self.ip];
                printInstruction("OP_GET_LOCAL", "{d}", .{local_idx});
            },
            .set_local => {
                defer self.ip += 1;
                const local_idx = self.code[self.ip];
                printInstruction("OP_SET_LOCAL", "{d}", .{local_idx});
            },
            .call => printInstruction("OP_CALL", "", .{}),
            .create_global => printInstruction("OP_CREATE_GLOBAL", "", .{}),
            .get_global => printInstruction("OP_GET_GLOBAL", "", .{}),
            .set_global => printInstruction("OP_SET_GLOBAL", "", .{}),
            .true => printInstruction("OP_TRUE", "", .{}),
            .false => printInstruction("OP_FALSE", "", .{}),
            .null => printInstruction("OP_NULL", "", .{}),
            .print => printInstruction("OP_PRINT", "", .{}),
            .pop => printInstruction("OP_POP", "", .{}),
            .not => printInstruction("OP_NOT", "", .{}),
            .negate => printInstruction("OP_NEGATE", "", .{}),
            .concat => printInstruction("OP_CONCAT", "", .{}),
            .add => printInstruction("OP_ADD", "", .{}),
            .sub => printInstruction("OP_SUB", "", .{}),
            .div => printInstruction("OP_DIV", "", .{}),
            .mul => printInstruction("OP_MUL", "", .{}),
            .mod => printInstruction("OP_MOD", "", .{}),
            .lower => printInstruction("OP_LOWER", "", .{}),
            .lower_equal => printInstruction("OP_LOWER_EQUAL", "", .{}),
            .greater => printInstruction("OP_GREATER", "", .{}),
            .greater_equal => printInstruction("OP_GREATER_EQUAL", "", .{}),
            .equal => printInstruction("OP_EQUAL", "", .{}),
            .unequal => printInstruction("OP_UNEQUAL", "", .{}),
            .constants_done => {
                constant_block = false;
                printInstruction("OP_CONSTANTS_DONE", "", .{});
            },
        }
    }
}

fn printInstruction(op: []const u8, comptime fmt: []const u8, args: anytype) void {
    std.debug.print("{s: <24} ", .{op});
    std.debug.print(fmt, args);
}

fn instruction(self: *Dumper) OpCode {
    defer self.ip += 1;
    return @enumFromInt(self.code[self.ip]);
}

fn byte(self: *Dumper) u8 {
    defer self.ip += 1;
    return self.code[self.ip];
}

fn panic(op: []const u8, msg: []const u8) void {
    std.debug.print("Illegal Instruction: {s}, {s}\n", .{ op, msg });
    @panic("Illegal Instruction");
}

const Dumper = @This();

const std = @import("std");

const Integer = @import("../definitions.zig").Integer;
const Float = @import("../definitions.zig").Float;
const OpCode = @import("../byte_code.zig").OpCode;
const Value = @import("../definitions.zig").FlowValue;
