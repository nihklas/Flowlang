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
                if (!constant_block) panic("OP_INTEGER", "not allowed outside constant definitions\n");
                defer constant_counter += 1;
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const int = std.mem.bytesToValue(Integer, bytes);
                self.constants[constant_counter] = .{ .int = int };
                printInstruction("OP_INTEGER", "{d: <10}{d}", .{ constant_counter, int });
            },
            .float => {
                if (!constant_block) panic("OP_FLOAT", "not allowed outside constant definitions\n");
                defer constant_counter += 1;
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const float = std.mem.bytesToValue(Float, bytes);
                self.constants[constant_counter] = .{ .float = float };
                printInstruction("OP_FLOAT", "{d: <10}{d}", .{ constant_counter, float });
            },
            .string => @panic("Not Yet Supported"),
            .constant => {
                if (constant_block) panic("OP_CONSTANT", "not allowed in constants definitions\n");
                defer self.ip += 1;
                const idx = self.code[self.ip];
                const value = self.constants[idx];
                printInstruction("OP_CONSTANT", "{d: <10}{}", .{ idx, value });
            },
            .true => printInstruction("OP_TRUE", "", .{}),
            .false => printInstruction("OP_FALSE", "", .{}),
            .null => printInstruction("OP_NULL", "", .{}),
            .print => printInstruction("OP_PRINT", "", .{}),
            .pop => printInstruction("OP_POP", "", .{}),
            .not => printInstruction("OP_NOT", "", .{}),
            .negate => printInstruction("OP_NEGATE", "", .{}),
            .add => printInstruction("OP_ADD", "", .{}),
            .sub => printInstruction("OP_SUB", "", .{}),
            .div => printInstruction("OP_DIV", "", .{}),
            .mul => printInstruction("OP_MUL", "", .{}),
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
