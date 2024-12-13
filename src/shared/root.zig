pub const OpCode = @import("byte_code.zig").OpCode;
pub const definitions = @import("definitions.zig");
pub const Stack = @import("stack.zig").Stack;

pub fn oom() noreturn {
    std.io.getStdErr().writeAll("Out Of Memory! Proceeding to panic") catch {};
    std.debug.panic("OOM", .{});
}

pub const builtins: std.StaticStringMap(definitions.BuiltinFunction) = blk: {
    var functions: []const struct { []const u8, definitions.BuiltinFunction } = &.{};

    for (std.meta.declarations(flow_std)) |decl| {
        functions = functions ++ .{.{ decl.name, @field(flow_std, decl.name) }};
    }

    if (@import("extensions").enabled) {
        for (std.meta.declarations(extensions)) |decl| {
            functions = functions ++ .{.{ decl.name, @field(extensions, decl.name) }};
        }
    }

    break :blk .initComptime(functions);
};

const std = @import("std");
const flow_std = @import("flow_std");
const extensions = if (@import("extensions").enabled) @import("flow_ext") else {};
