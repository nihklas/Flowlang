tokens: []const Token,
alloc: Allocator,
current: usize = 0,
has_error: bool = false,

pub fn createAST(alloc: Allocator, tokens: []const Token) ![]const *Stmt {
    var parser: Parser = .{
        .tokens = tokens,
        .alloc = alloc,
    };
    return try parser.parse();
}

fn parse(self: *Parser) ![]const *Stmt {
    var stmt_list: std.ArrayList(*Stmt) = .init(self.alloc);
    errdefer {
        for (stmt_list.items) |stmt| {
            stmt.destroy(self.alloc);
        }
        stmt_list.deinit();
    }

    while (!self.check(.EOF)) {
        if (self.declaration()) |stmt| {
            stmt_list.append(stmt) catch @panic("OOM");
        } else |_| {
            self.has_error = true;
            self.recover();
        }
    }

    try self.consume(.EOF, "Expected EOF");

    if (self.has_error) {
        return error.ParseError;
    }

    return stmt_list.toOwnedSlice() catch @panic("OOM");
}

fn declaration(self: *Parser) ParserError!*Stmt {
    return self.statement();
}

fn statement(self: *Parser) ParserError!*Stmt {
    return self.expressionStatement();
}

fn expressionStatement(self: *Parser) ParserError!*Stmt {
    const expr = try self.expression();
    errdefer expr.destroy(self.alloc);

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
        errdefer expr.destroy(self.alloc);
        return Expr.createUnary(self.alloc, op, expr);
    }

    return self.call();
}

fn call(self: *Parser) ParserError!*Expr {
    return self.primary();
}

fn primary(self: *Parser) ParserError!*Expr {
    if (self.match(.null)) |token| {
        return Expr.createLiteral(self.alloc, token, .null);
    }
    if (self.matchEither(.true, .false)) |token| {
        return Expr.createLiteral(self.alloc, token, .{ .bool = token.type == .true });
    }

    if (self.match(.number)) |token| {
        if (std.fmt.parseInt(ast.Integer, token.lexeme, 10)) |value| {
            return Expr.createLiteral(self.alloc, token, .{ .int = value });
        } else |err| switch (err) {
            error.InvalidCharacter => {
                const value = std.fmt.parseFloat(ast.Float, token.lexeme) catch {
                    error_reporter.reportError(token, "Could not convert '{s}' to float", .{token.lexeme});
                    return ParserError.SyntaxError;
                };
                return Expr.createLiteral(self.alloc, token, .{ .float = value });
            },
            error.Overflow => {
                error_reporter.reportError(token, "Could not convert '{s}' to int", .{token.lexeme});
                return ParserError.SyntaxError;
            },
        }
    }

    if (self.match(.string_literal)) |token| {
        return Expr.createLiteral(self.alloc, token, .{ .string = token.lexeme });
    }

    if (self.match(.@"(")) |_| {
        const expr = try self.expression();
        errdefer expr.destroy(self.alloc);

        try self.consume(.@")", "Expected ')' after Expression");

        return Expr.createGrouping(self.alloc, expr);
    }

    error_reporter.reportError(self.peek(), "Unexpected Token. Expected Literal, got {s}", .{@tagName(self.peek().type)});
    return ParserError.UnexpectedToken;
}

fn recover(self: *Parser) void {
    recover: switch (self.advance().type) {
        .@";" => {},
        .@"if",
        .@"for",
        .@"var",
        .func,
        .@"return",
        .print,
        .EOF,
        => self.current -= 1,
        else => {
            continue :recover self.advance().type;
        },
    }
}

fn consume(self: *Parser, expected: Token.Type, msg: []const u8) ParserError!void {
    if (!self.check(expected)) {
        error_reporter.reportError(self.peek(), "SyntaxError: {s}", .{msg});
        return ParserError.SyntaxError;
    }

    _ = self.advance();
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

const testing = std.testing;
const testing_alloc = testing.allocator;
const Scanner = @import("Scanner.zig");

test "Expression Statement with Literals" {
    const input =
        \\1;
        \\1.2;
        \\"test";
        \\true;
        \\false;
        \\null;
        \\
    ;

    const tokens = try Scanner.scan(testing_alloc, input);
    defer testing_alloc.free(tokens);

    const program = try createAST(testing_alloc, tokens);
    defer testing_alloc.free(program);
    defer for (program) |stmt| {
        stmt.destroy(testing_alloc);
    };

    try testing.expectEqual(6, program.len);

    for (program) |stmt| {
        // We only have expression statements
        try testing.expect(stmt.* == .expr);
        // ...and all of them are literals
        try testing.expect(stmt.expr.expr.* == .literal);
    }

    try testing.expect(program[0].expr.expr.literal.value == .int);
    try testing.expect(program[0].expr.expr.literal.value.int == 1);

    try testing.expect(program[1].expr.expr.literal.value == .float);
    try testing.expect(program[1].expr.expr.literal.value.float == 1.2);

    try testing.expect(program[2].expr.expr.literal.value == .string);
    try testing.expectEqualSlices(u8, "test", program[2].expr.expr.literal.value.string);

    try testing.expect(program[3].expr.expr.literal.value == .bool);
    try testing.expect(program[3].expr.expr.literal.value.bool == true);

    try testing.expect(program[4].expr.expr.literal.value == .bool);
    try testing.expect(program[4].expr.expr.literal.value.bool == false);

    try testing.expect(program[5].expr.expr.literal.value == .null);
    try testing.expect(program[5].expr.expr.literal.value.null == {});
}

test "Expression Statement with Grouping" {
    const input =
        \\(1);
        \\2;
        \\
    ;

    const tokens = try Scanner.scan(testing_alloc, input);
    defer testing_alloc.free(tokens);

    const program = try createAST(testing_alloc, tokens);
    defer testing_alloc.free(program);
    defer for (program) |stmt| {
        stmt.destroy(testing_alloc);
    };

    try testing.expectEqual(2, program.len);

    for (program) |stmt| {
        // We only have expression statements
        try testing.expect(stmt.* == .expr);
    }

    try testing.expect(program[0].expr.expr.* == .grouping);
    try testing.expect(program[0].expr.expr.grouping.expr.* == .literal);
    try testing.expect(program[0].expr.expr.grouping.expr.literal.value == .int);

    try testing.expect(program[1].expr.expr.* == .literal);
    try testing.expect(program[1].expr.expr.literal.value == .int);
    try testing.expect(program[1].expr.expr.literal.value.int == 2);
}

test "Expression Statement with Unary" {
    const input =
        \\!false;
        \\-5;
        \\
    ;

    const tokens = try Scanner.scan(testing_alloc, input);
    defer testing_alloc.free(tokens);

    const program = try createAST(testing_alloc, tokens);
    defer testing_alloc.free(program);
    defer for (program) |stmt| {
        stmt.destroy(testing_alloc);
    };

    try testing.expectEqual(2, program.len);

    for (program) |stmt| {
        // We only have expression statements
        try testing.expect(stmt.* == .expr);
        // ...and all of them are unary
        try testing.expect(stmt.expr.expr.* == .unary);
    }

    try testing.expect(program[0].expr.expr.unary.op.type == .@"!");
    try testing.expect(program[1].expr.expr.unary.op.type == .@"-");
}

const ParserError = error{
    SyntaxError,
    UnexpectedToken,
    NotImplementedYet,
    Panic,
};

const Parser = @This();

const std = @import("std");
const Token = @import("Token.zig");
const ast = @import("ast.zig");
const error_reporter = @import("error_reporter.zig");

const Allocator = std.mem.Allocator;
const Expr = ast.Expr;
const Stmt = ast.Stmt;
