tokens: []const Token,
alloc: Allocator,
current: usize = 0,
has_error: bool = false,

pub fn createAST(alloc: Allocator, tokens: []const Token) ParserError![]const *Stmt {
    var parser: Parser = .{
        .tokens = tokens,
        .alloc = alloc,
    };
    return try parser.parse();
}

fn parse(self: *Parser) ParserError![]const *Stmt {
    var stmt_list: std.ArrayList(*Stmt) = .init(self.alloc);
    errdefer {
        for (stmt_list.items) |stmt| {
            stmt.destroy(self.alloc);
        }
        stmt_list.deinit();
    }

    while (!self.check(.EOF)) {
        // TODO: Catch Errors and get into workable state
        const stmt = try self.declaration();

        try stmt_list.append(stmt);
    }

    try self.consume(.EOF, "Expected EOF");

    return try stmt_list.toOwnedSlice();
}

fn declaration(self: *Parser) ParserError!*Stmt {
    return self.statement();
}

fn statement(self: *Parser) ParserError!*Stmt {
    return self.expressionStatement();
}

fn expressionStatement(self: *Parser) ParserError!*Stmt {
    const expr = try self.expression();
    try self.consume(.@";", "Expected ';' after statement");

    return Stmt.createExpr(self.alloc, expr);
}

fn expression(self: *Parser) ParserError!*Expr {
    return self.assignment();
}

fn assignment(self: *Parser) ParserError!*Expr {
    return self.orExpr();
}

fn orExpr(self: *Parser) ParserError!*Expr {
    return self.andExpr();
}

fn andExpr(self: *Parser) ParserError!*Expr {
    return self.equality();
}

fn equality(self: *Parser) ParserError!*Expr {
    return self.comparison();
}

fn comparison(self: *Parser) ParserError!*Expr {
    return self.term();
}

fn term(self: *Parser) ParserError!*Expr {
    return self.factor();
}

fn factor(self: *Parser) ParserError!*Expr {
    return self.unary();
}

fn unary(self: *Parser) ParserError!*Expr {
    if (self.matchEither(.@"-", .@"!")) |op| {
        const expr = try self.unary();
        return Expr.createUnary(self.alloc, op, expr);
    }

    return self.call();
}

fn call(self: *Parser) ParserError!*Expr {
    return self.primary();
}

fn primary(self: *Parser) ParserError!*Expr {
    // TODO: Need to create some kind of typing for ast
    _ = self;
    return ParserError.NotImplementedYet;
}

fn consume(self: *Parser, expected: Token.Type, msg: []const u8) ParserError!void {
    if (!self.check(expected)) {
        error_reporter.reportError(self.peek(), "Syntax Error: {s}", .{msg});
        return ParserError.SyntaxError;
    }

    self.advance();
}

fn match(self: *Parser, expected: Token.Type) ?Token {
    if (self.check(expected)) {
        return self.advance();
    }

    return null;
}

fn matchEither(self: *Parser, expected1: Token.Type, expected2: Token.Type) ?Token {
    if (self.check(expected1) or self.check(expected2)) {
        return self.advance();
    }

    return null;
}

fn check(self: *Parser, expected: Token.Type) bool {
    if (self.isAtEnd()) return false;
    return self.peek().type == expected;
}

fn advance(self: *Parser) Token {
    if (!self.isAtEnd()) self.current += 1;
    return self.previous();
}

fn isAtEnd(self: *Parser) bool {
    return self.peek().type == .EOF;
}

fn peek(self: *Parser) Token {
    return self.tokens[self.current];
}

fn previous(self: *Parser) Token {
    return self.tokens[self.current - 1];
}

const ParserError = error{
    SyntaxError,
    NotImplementedYet,
};

const Parser = @This();

const std = @import("std");
const Token = @import("Token.zig");
const ast = @import("ast.zig");
const error_reporter = @import("error_reporter.zig");

const Allocator = std.mem.Allocator;
const Expr = ast.Expr;
const Stmt = ast.Stmt;
