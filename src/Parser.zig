tokens: []const Token,
alloc: Allocator,

pub fn createAST(alloc: Allocator, tokens: []const Token) ![]const *Stmt {
    var parser: Parser = .{
        .tokens = tokens,
        .alloc = alloc,
    };
    return try parser.parse();
}

fn parse(self: *Parser) ![]const *Stmt {
    _ = self;
    return error.NotImplementedYet;
}

fn program(self: *Parser) ![]const *Stmt {
    _ = self;
    return error.NotImplementedYet;
}

const Parser = @This();

const std = @import("std");
const Token = @import("Token.zig");
const ast = @import("ast.zig");

const Allocator = std.mem.Allocator;
const Expr = ast.Expr;
const Stmt = ast.Stmt;
