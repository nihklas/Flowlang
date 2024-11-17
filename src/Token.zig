type: Type,
location: struct { line: usize, column: usize },
lexeme: []const u8,

pub const Type = enum {
    // Single Character
    @"+",
    @"-",
    @"*",
    @"/",
    @"!",
    @"=",
    @":",
    @";",
    @"(",
    @")",
    @"{",
    @"}",

    // Two Characters
    @"==",
    @"<=",
    @">=",
    @"!=",
    @"->",
    @"<-",

    // Keywords
    @"and",
    @"or",
    @"var",
    @"const",
    @"if",
    @"else",
    @"for",
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
    channel,

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
    .{ "func", .func },
    .{ "flow", .flow },
    .{ "null", .null },
    .{ "true", .true },
    .{ "false", .false },
    .{ "bool", .bool },
    .{ "int", .int },
    .{ "float", .float },
    .{ "string", .string },
    .{ "channel", .channel },
});

const std = @import("std");
