alloc: Allocator,
input: []const u8,
tokens: std.ArrayList(Token),
start: usize = 0,
current: usize = 0,
line: usize = 1,
column: usize = 0,
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
        try self.nextToken();
    }
    self.column += 1;

    try self.tokens.append(.{
        .type = .EOF,
        .lexeme = "",
        .location = .{
            .line = self.line,
            .column = self.column,
        },
    });
}

fn nextToken(self: *Scanner) !void {
    const c = self.advance();
    switch (c) {
        '+' => try self.makeToken(.@"+"),
        '*' => try self.makeToken(.@"*"),
        '/' => try self.makeToken(.@"/"),
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
        '<' => {
            if (self.match('-')) {
                try self.makeToken(.@"<-");
            } else if (self.match('=')) {
                try self.makeToken(.@"<=");
            } else {
                try self.makeToken(.@"<");
            }
        },
        else => {
            // TODO: Keywords and Literals
        },
    }
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
            .column = self.column,
        },
    });
}

const Scanner = @This();

const std = @import("std");
const Token = @import("Token.zig");
const Allocator = std.mem.Allocator;

test scan {
    const testing = std.testing;
    const alloc = testing.allocator;

    const input =
        \\var name: string = "FlowLang";
    ;

    const tokens = try scan(alloc, input);
    defer alloc.free(tokens);

    const expected_tokens: []const Token = &.{
        .{ .type = .@"var", .lexeme = "var", .location = .{ .line = 1, .column = 1 } },
        .{ .type = .identifier, .lexeme = "name", .location = .{ .line = 1, .column = 5 } },
        .{ .type = .@":", .lexeme = ":", .location = .{ .line = 1, .column = 9 } },
        .{ .type = .string, .lexeme = "string", .location = .{ .line = 1, .column = 11 } },
        .{ .type = .@"=", .lexeme = "=", .location = .{ .line = 1, .column = 18 } },
        .{ .type = .string_literal, .lexeme = "FlowLang", .location = .{ .line = 1, .column = 21 } },
        .{ .type = .@";", .lexeme = ";", .location = .{ .line = 1, .column = 30 } },
        .{ .type = .EOF, .lexeme = "", .location = .{ .line = 1, .column = 31 } },
    };

    try testing.expectEqualSlices(Token, expected_tokens, tokens);
}
