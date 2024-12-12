pub const OpCode = enum(u8) {
    // literals
    true,
    false,
    null,

    // constant definitions
    integer,
    float,
    string, // len as 1 byte operand
    string_long, // len as 4 byte operand

    // operations
    pop,
    constant,

    negate,
    not,

    concat,
    add,
    sub,
    div,
    mul,
    mod,

    equal,
    unequal,
    greater,
    greater_equal,
    lower,
    lower_equal,

    // variables
    create_global,
    get_global,
    set_global,

    get_local, // index as 1 byte operand
    set_local, // index as 1 byte operand

    // functions
    function, // operands: 1 byte for constant idx name, 1 byte for arg count, 2 byte for line count
    call,
    @"return",

    // jumps
    jump, // jmp len as 2 byte operand
    jump_back, // jmp len as 2 byte operand
    jump_if_true, // jmp len as 2 byte operand
    jump_if_false, // jmp len as 2 byte operand

    // markers
    constants_done,
    functions_done,

    pub fn raw(op: OpCode) u8 {
        return @intFromEnum(op);
    }

    pub fn format(self: OpCode, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll("OP_");
        const name = @tagName(self);
        for (name) |c| {
            try writer.writeByte(std.ascii.toUpper(c));
        }
    }
};

const std = @import("std");
