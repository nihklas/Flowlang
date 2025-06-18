type: Type,
line: u32,
column: u32,
lexeme: []const u8,

pub const Type = enum {
    // Single Character
    @",",
    @".",
    @"+",
    @"-",
    @"*",
    @"/",
    @"%",
    @"!",
    @"=",
    @":",
    @";",
    @"(",
    @")",
    @"{",
    @"}",
    @">",
    @"<",
    @"[",
    @"]",

    // Two Characters
    @"==",
    @"<=",
    @">=",
    @"!=",
    @"->",
    @"<-",
    @"+=",
    @"-=",
    @"*=",
    @"/=",
    @"%=",
    @".=",
    @"[]",

    // Keywords
    @"and",
    @"or",
    @"var",
    @"const",
    @"if",
    @"else",
    @"for",
    @"return",
    @"break",
    @"continue",
    func,
    flow,
    null,
    true,
    false,
    // Types
    bool,
    int,
    float,
    string,
    void,

    // Literals
    identifier,
    number,
    string_literal,

    EOF,
};

pub const ReservedKeywords = std.StaticStringMap(Type).initComptime(.{
    .{ "and", .@"and" },
    .{ "or", .@"or" },
    .{ "var", .@"var" },
    .{ "const", .@"const" },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "for", .@"for" },
    .{ "break", .@"break" },
    .{ "continue", .@"continue" },
    .{ "func", .func },
    .{ "flow", .flow },
    .{ "null", .null },
    .{ "void", .void },
    .{ "true", .true },
    .{ "false", .false },
    .{ "bool", .bool },
    .{ "int", .int },
    .{ "float", .float },
    .{ "string", .string },
    .{ "return", .@"return" },
});

pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    try writer.print("Token[{{{s}}} \"{s}\" {d}/{d}]", .{
        @tagName(self.type),
        self.lexeme,
        self.line,
        self.column,
    });
}

const std = @import("std");
