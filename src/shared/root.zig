pub const OpCode = @import("byte_code.zig").OpCode;
pub const definitions = @import("definitions.zig");
pub const Stack = @import("stack.zig").Stack;
pub const BytecodeDumper = @import("debug/BytecodeDumper.zig");
pub const builtins = @import("builtins.zig").builtins;

pub fn oom() noreturn {
    const std = @import("std");

    std.io.getStdErr().writeAll("Out Of Memory! Proceeding to panic") catch {};
    std.debug.panic("OOM", .{});
}
