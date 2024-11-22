pub const OpCode = enum(u8) {
    // Data
    true,
    false,
    null,
    integer,
    float,
    string,

    // Operations
    pop,
    print,

    pub fn raw(op: OpCode) u8 {
        return @intFromEnum(op);
    }
};
