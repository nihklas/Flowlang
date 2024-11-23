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
    constant,

    // Markers
    constants_done,

    pub fn raw(op: OpCode) u8 {
        return @intFromEnum(op);
    }
};
