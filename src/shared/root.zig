pub const OpCode = @import("byte_code.zig").OpCode;
pub const definitions = @import("definitions.zig");
pub const Stack = @import("stack.zig").Stack;

pub const debug = struct {
    pub const BytecodeDumper = @import("debug/BytecodeDumper.zig");
};

pub fn oom() noreturn {
    std.debug.panic("Out Of Memory! Proceeding to panic", .{});
}

pub const builtins: std.StaticStringMap(definitions.BuiltinFunction) = blk: {
    var functions: []const struct { []const u8, definitions.BuiltinFunction } = &.{};

    for (std.meta.declarations(flow_std)) |decl| {
        const builtin = @field(flow_std, decl.name);

        if (builtin.ret_type.type == .null and builtin.ret_type.order > 0) {
            @compileError(std.fmt.comptimePrint("Return Type of builtin '{s}' cannot be an array of null", .{decl.name}));
        }

        for (builtin.arg_types, 0..) |arg, i| {
            if (arg.type == .null and arg.order > 0) {
                @compileError(std.fmt.comptimePrint("Argument Type of builtin '{s}' at index {d} cannot be an array of null", .{ decl.name, i }));
            }
        }
        functions = functions ++ .{.{ decl.name, builtin }};
    }

    if (@import("extensions").enabled) {
        for (std.meta.declarations(extensions)) |decl| {
            const builtin = @field(extensions, decl.name);

            if (builtin.ret_type.type == .null and builtin.ret_type.order > 0) {
                @compileError(std.fmt.comptimePrint("Return Type of extension-builtin '{s}' cannot be an array of null", .{decl.name}));
            }

            for (builtin.arg_types, 0..) |arg, i| {
                if (arg.type == .null and arg.order > 0) {
                    @compileError(std.fmt.comptimePrint("Argument Type of extension-builtin '{s}' at index {d} cannot be an array of null", .{ decl.name, i }));
                }
            }
            functions = functions ++ .{.{ decl.name, builtin }};
        }
    }

    break :blk .initComptime(functions);
};

const std = @import("std");
const flow_std = @import("flow_std");
const extensions = if (@import("extensions").enabled) @import("flow_ext") else {};
