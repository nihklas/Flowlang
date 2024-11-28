// TODO: After all Instructions are added, try turning them into packed structs and stuff
// Arithmetic operations can encode the types of the operands in the extra bits
pub const OpCode = enum(u8) {
    // literals
    true,
    false,
    null,

    // constant definitions
    integer,
    float,
    string,
    string_long,

    // operations
    pop,
    print,
    constant,

    negate,
    not,

    concat,
    add,
    sub,
    div,
    mul,

    equal,
    unequal,
    greater,
    greater_equal,
    lower,
    lower_equal,

    // markers
    constants_done,

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
