ip: usize,
code: []const u8,

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
                printInstruction("OP_INTEGER", "{d: <10}{d: >3}", .{ int, constant_counter });
            },
            .float => {
                defer constant_counter += 1;
                defer self.ip += 8;
                const bytes = self.code[self.ip .. self.ip + 8];
                const float = std.mem.bytesToValue(Float, bytes);
                printInstruction("OP_FLOAT", "{d: <10}{d: >3}", .{ float, constant_counter });
            },
            .constant => {
                if (constant_block) panic("OP_CONSTANT", "not allowed in constants definitions\n");
                defer self.ip += 1;
                const idx = self.code[self.ip];
                printInstruction("OP_CONSTANT", "{d}", .{idx});
            },
            .true => printInstruction("OP_TRUE", "", .{}),
            .false => printInstruction("OP_FALSE", "", .{}),
            .null => printInstruction("OP_NULL", "", .{}),
            .print => printInstruction("OP_PRINT", "", .{}),
            .pop => printInstruction("OP_POP", "", .{}),
            .constants_done => {
                constant_block = false;
                printInstruction("OP_CONSTANTS_DONE", "", .{});
            },
            else => panic(@tagName(op), ""),
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
