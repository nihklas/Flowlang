pub const OpCode = enum(u8) {
    // literals
    true,
    false,
    null,

    array,
    index,

    // constant definitions
    integer,
    float,
    string, // len as 1 byte operand
    string_long, // len as 4 byte operand

    // operations
    pop,
    constant,

    negate_i,
    negate_f,
    not,

    concat,

    add_i,
    sub_i,
    div_i,
    mul_i,
    mod_i,

    add_f,
    sub_f,
    div_f,
    mul_f,
    mod_f,

    equal,
    unequal,
    greater,
    greater_equal,
    lower,
    lower_equal,

    // variables
    get_global,
    set_global,
    set_global_array, // 2 operands: 1 byte index, 1 byte amount if arr_index

    get_local, // index as 1 byte operand
    set_local, // index as 1 byte operand
    set_local_array, // 2 operands: 1 byte index, 1 byte amount if arr_index

    append,
    clone,

    // functions
    function, // operands: 1 byte for constant idx name, 1 byte for arg count, 2 byte for line count
    get_builtin,
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
