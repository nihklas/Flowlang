pub fn dump(writer: anytype, fir: *const FIR) !void {
    _ = writer;
    _ = fir;
}

const FIR = @import("../ir/FIR.zig");
