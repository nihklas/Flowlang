pub const OpCode = enum(u8) {
    // literals
    true,
    false,
    null,

    // constant definitions
    integer,
    float,
    string,

    // operations
    pop,
    print,
    constant,

    negate,
    not,

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
};
