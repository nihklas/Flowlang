alloc: Allocator,
input: []const u8,
tokens: std.ArrayList(Token),
start: usize = 0,
current: usize = 0,
line: usize = 1,
column: usize = 1,
lexeme_column: usize = 0,
has_error: bool = false,

pub fn scan(alloc: Allocator, input: []const u8) ![]const Token {
    var scanner: Scanner = .{
        .input = input,
        .alloc = alloc,
        .tokens = .init(alloc),
    };
    try scanner.scanTokens();
    return try scanner.tokens.toOwnedSlice();
}

fn scanTokens(self: *Scanner) !void {
    while (!self.isAtEnd()) {
        self.start = self.current;
        self.lexeme_column = self.column;
        try self.nextToken();
    }

    try self.tokens.append(.{
        .type = .EOF,
        .lexeme = "",
        .location = .{
            .line = self.line,
            .column = self.lexeme_column,
        },
    });
}

fn nextToken(self: *Scanner) !void {
    const c = self.advance();
    switch (c) {
        ' ', '\t', '\r' => {},
        '\n' => {
            self.line += 1;
            self.column = 1;
            self.lexeme_column = 1;
        },
        '+' => try self.makeToken(.@"+"),
        '*' => try self.makeToken(.@"*"),
        ':' => try self.makeToken(.@":"),
        ';' => try self.makeToken(.@";"),
        '(' => try self.makeToken(.@"("),
        ')' => try self.makeToken(.@")"),
        '{' => try self.makeToken(.@"{"),
        '}' => try self.makeToken(.@"}"),
        '-' => try self.makeToken(if (self.match('>')) .@"->" else .@"-"),
        '=' => try self.makeToken(if (self.match('=')) .@"==" else .@"="),
        '!' => try self.makeToken(if (self.match('=')) .@"!=" else .@"="),
        '>' => try self.makeToken(if (self.match('=')) .@">=" else .@">"),
        '"' => try self.string(),
        '0'...'9' => try self.number(),
        '/' => {
            if (self.match('/')) {
                while (!self.check('\n')) _ = self.advance();
            } else {
                try self.makeToken(.@"/");
            }
        },
        '<' => {
            if (self.match('-')) {
                try self.makeToken(.@"<-");
            } else if (self.match('=')) {
                try self.makeToken(.@"<=");
            } else {
                try self.makeToken(.@"<");
            }
        },
        else => try self.keywordOrIdentifier(),
    }
}

fn string(self: *Scanner) !void {
    while (!self.check('"') and !self.isAtEnd()) {
        if (self.check('\n')) {
            self.line += 1;
            self.column = 0;
        }
        _ = self.advance();
    }

    if (self.isAtEnd()) {
        self.has_error = true;
        std.debug.print("Syntax Error: unterminated string\n", .{});
    }

    _ = self.advance(); // closing "

    const value = self.input[self.start + 1 .. self.current - 1];
    try self.makeTokenWithValue(.string_literal, value);
}

fn number(self: *Scanner) !void {
    while (std.ascii.isDigit(self.peek())) {
        _ = self.advance();
    }

    if (self.check('.') and std.ascii.isDigit(self.peekNext())) {
        _ = self.advance();
        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }
    }

    try self.makeToken(.number);
}

fn keywordOrIdentifier(self: *Scanner) !void {
    while (std.ascii.isAlphanumeric(self.peek())) {
        _ = self.advance();
    }

    const text = self.input[self.start..self.current];
    const token_type: Token.Type = Token.ReservedKeywords.get(text) orelse .identifier;
    try self.makeToken(token_type);
}

fn advance(self: *Scanner) u8 {
    defer self.column += 1;
    defer self.current += 1;
    return self.input[self.current];
}

fn match(self: *Scanner, expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.input[self.current] != expected) return false;

    self.current += 1;
    return true;
}

fn peek(self: *Scanner) u8 {
    if (self.isAtEnd()) return 0;
    return self.input[self.current];
}

fn peekNext(self: *Scanner) u8 {
    if (self.current + 1 >= self.input.len) return 0;
    return self.input[self.current + 1];
}

fn check(self: *Scanner, expected: u8) bool {
    if (self.isAtEnd()) return false;
    return self.input[self.current] == expected;
}

fn isAtEnd(self: *Scanner) bool {
    return self.current >= self.input.len;
}

fn makeToken(self: *Scanner, token_type: Token.Type) !void {
    const lexeme = self.input[self.start..self.current];
    try self.tokens.append(.{
        .type = token_type,
        .lexeme = lexeme,
        .location = .{
            .line = self.line,
            .column = self.lexeme_column,
        },
    });
}

fn makeTokenWithValue(self: *Scanner, token_type: Token.Type, value: []const u8) !void {
    try self.tokens.append(.{
        .type = token_type,
        .lexeme = value,
        .location = .{
            .line = self.line,
            .column = self.lexeme_column,
        },
    });
}

const Scanner = @This();

const std = @import("std");
const Token = @import("Token.zig");
const Allocator = std.mem.Allocator;
const testing = std.testing;

fn expectTokensEqual(actual: []const Token, expected: []const Token) !void {
    try testing.expect(actual.len == expected.len);
    if (actual.len == expected.len) {
        for (expected, actual) |e, a| {
            try testing.expectEqual(e.type, a.type);
            try testing.expectEqual(e.location, a.location);
            try testing.expectEqualSlices(u8, e.lexeme, a.lexeme);
        }
    }
}

test scan {
    const alloc = testing.allocator;

    const input =
        \\var name: string = "FlowLang"; // This will be ignored
        \\const num: float = 12.34;
        \\
    ;

    const tokens = try scan(alloc, input);
    defer alloc.free(tokens);

    const expected: []const Token = &.{
        .{ .type = .@"var", .lexeme = "var", .location = .{ .line = 1, .column = 1 } },
        .{ .type = .identifier, .lexeme = "name", .location = .{ .line = 1, .column = 5 } },
        .{ .type = .@":", .lexeme = ":", .location = .{ .line = 1, .column = 9 } },
        .{ .type = .string, .lexeme = "string", .location = .{ .line = 1, .column = 11 } },
        .{ .type = .@"=", .lexeme = "=", .location = .{ .line = 1, .column = 18 } },
        .{ .type = .string_literal, .lexeme = "FlowLang", .location = .{ .line = 1, .column = 20 } },
        .{ .type = .@";", .lexeme = ";", .location = .{ .line = 1, .column = 30 } },

        .{ .type = .@"const", .lexeme = "const", .location = .{ .line = 2, .column = 1 } },
        .{ .type = .identifier, .lexeme = "num", .location = .{ .line = 2, .column = 7 } },
        .{ .type = .@":", .lexeme = ":", .location = .{ .line = 2, .column = 10 } },
        .{ .type = .float, .lexeme = "float", .location = .{ .line = 2, .column = 12 } },
        .{ .type = .@"=", .lexeme = "=", .location = .{ .line = 2, .column = 18 } },
        .{ .type = .number, .lexeme = "12.34", .location = .{ .line = 2, .column = 20 } },
        .{ .type = .@";", .lexeme = ";", .location = .{ .line = 2, .column = 25 } },

        .{ .type = .EOF, .lexeme = "", .location = .{ .line = 3, .column = 1 } },
    };

    try expectTokensEqual(tokens, expected);
}
