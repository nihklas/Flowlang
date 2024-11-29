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
    if (self.matchEither(.@"var", .@"const")) |_| {
        return self.varDeclaration();
    }

    return self.statement();
}

fn varDeclaration(self: *Parser) ParserError!*Stmt {
    // var or const
    const keyword = self.previous();

    try self.consume(
        .identifier,
        std.fmt.allocPrint(self.alloc, "Expected identifier after {s}", .{
            @tagName(keyword.type),
        }) catch unreachable,
    );
    const name = self.previous();

    const type_hint = self.typeHint();

    const value = blk: {
        if (self.match(.@"=")) |_| {
            break :blk try self.expression();
        }
        break :blk null;
    };

    try self.consume(.@";", "Expected ';' after variable declaration");

    return Stmt.createVariable(self.alloc, name, type_hint, keyword.type == .@"const", value);
}

fn statement(self: *Parser) ParserError!*Stmt {
    if (self.match(.print)) |_| {
        return self.printStatement();
    }

    if (self.match(.@"{")) |_| {
        const stmts = try self.block();
        return Stmt.createBlock(self.alloc, stmts);
    }

    return self.expressionStatement();
}

fn block(self: *Parser) ParserError![]*Stmt {
    var stmt_list: std.ArrayList(*Stmt) = .init(self.alloc);
    errdefer {
        for (stmt_list.items) |stmt| {
            stmt.destroy(self.alloc);
        }
        stmt_list.deinit();
    }

    while (!self.check(.@"}") and !self.isAtEnd()) {
        const stmt = try self.declaration();
        stmt_list.append(stmt) catch @panic("OOM");
    }

    try self.consume(.@"}", "Expected '}' at the end of block");

    return stmt_list.toOwnedSlice() catch @panic("OOM");
}

fn printStatement(self: *Parser) ParserError!*Stmt {
    const expr = try self.expression();
    errdefer expr.destroy(self.alloc);

    try self.consume(.@";", "Expected ';' after value");

    return Stmt.createPrint(self.alloc, expr);
}

fn expressionStatement(self: *Parser) ParserError!*Stmt {
    const expr = try self.expression();
    errdefer expr.destroy(self.alloc);

    try self.consume(.@";", "Expected ';' after expression");

    return Stmt.createExpr(self.alloc, expr);
}

fn expression(self: *Parser) ParserError!*Expr {
    return self.assignment();
}

fn assignment(self: *Parser) ParserError!*Expr {
    if (self.check(.identifier) and self.checkNext(.@"=")) {
        const identifier = self.match(.identifier).?;
        _ = self.match(.@"=");

        const expr = try self.expression();
        return Expr.createAssignment(self.alloc, identifier, expr);
    }

    return self.concat();
}

fn concat(self: *Parser) ParserError!*Expr {
    var lhs = try self.orExpr();
    errdefer lhs.destroy(self.alloc);

    while (self.match(.@".")) |op| {
        const rhs = try self.orExpr();
        lhs = Expr.createBinary(self.alloc, lhs, op, rhs);
    }

    return lhs;
}

fn orExpr(self: *Parser) ParserError!*Expr {
    var lhs = try self.andExpr();
    errdefer lhs.destroy(self.alloc);

    while (self.match(.@"or")) |op| {
        const rhs = try self.andExpr();
        lhs = Expr.createLogical(self.alloc, lhs, op, rhs);
    }

    return lhs;
}

fn andExpr(self: *Parser) ParserError!*Expr {
    var lhs = try self.equality();
    errdefer lhs.destroy(self.alloc);

    while (self.match(.@"and")) |op| {
        const rhs = try self.equality();
        lhs = Expr.createLogical(self.alloc, lhs, op, rhs);
    }

    return lhs;
}

fn equality(self: *Parser) ParserError!*Expr {
    var lhs = try self.comparison();
    errdefer lhs.destroy(self.alloc);

    while (self.matchEither(.@"!=", .@"==")) |op| {
        const rhs = try self.comparison();
        lhs = Expr.createBinary(self.alloc, lhs, op, rhs);
    }

    return lhs;
}

fn comparison(self: *Parser) ParserError!*Expr {
    var lhs = try self.term();
    errdefer lhs.destroy(self.alloc);

    while (self.matchEither(.@"<", .@"<=") != null or self.matchEither(.@">=", .@">") != null) {
        const op = self.previous();
        const rhs = try self.term();
        lhs = Expr.createBinary(self.alloc, lhs, op, rhs);
    }

    return lhs;
}

fn term(self: *Parser) ParserError!*Expr {
    var lhs = try self.factor();
    errdefer lhs.destroy(self.alloc);

    while (self.matchEither(.@"+", .@"-")) |op| {
        const rhs = try self.factor();
        lhs = Expr.createBinary(self.alloc, lhs, op, rhs);
    }

    return lhs;
}

fn factor(self: *Parser) ParserError!*Expr {
    var lhs = try self.unary();
    errdefer lhs.destroy(self.alloc);

    while (self.matchEither(.@"/", .@"*")) |op| {
        const rhs = try self.unary();
        lhs = Expr.createBinary(self.alloc, lhs, op, rhs);
    }

    return lhs;
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
        if (std.fmt.parseInt(Integer, token.lexeme, 10)) |value| {
            return Expr.createLiteral(self.alloc, token, .{ .int = value });
        } else |err| switch (err) {
            error.InvalidCharacter => {
                const value = std.fmt.parseFloat(Float, token.lexeme) catch {
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

    if (self.match(.identifier)) |token| {
        return Expr.createVariable(self.alloc, token);
    }

    if (self.match(.@"(")) |_| {
        const expr = try self.expression();
        errdefer expr.destroy(self.alloc);

        try self.consume(.@")", "Expected ')' after Expression");

        return Expr.createGrouping(self.alloc, expr);
    }

    error_reporter.reportError(self.peek(), "UnexpectedToken: Expected Expression, got '{s}'", .{@tagName(self.peek().type)});
    return ParserError.UnexpectedToken;
}

fn typeHint(self: *Parser) ?Token {
    if (self.match(.@":")) |colon| {
        if (self.match(.string)) |string| return string;
        if (self.match(.int)) |int| return int;
        if (self.match(.float)) |float| return float;
        if (self.match(.bool)) |boolean| return boolean;

        error_reporter.reportError(colon, "Expected type after ':'", .{});
        self.has_error = true;
    }

    return null;
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
        else => continue :recover self.advance().type,
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

fn checkNext(self: *Parser, expected: Token.Type) bool {
    if (self.isAtEnd()) return false;
    return self.tokens[self.current + 1].type == expected;
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
    try testing.expectEqualStrings("test", program[2].expr.expr.literal.value.string);

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

test "Expression Statement with Factors" {
    const input =
        \\4 / 2;
        \\3 * 3;
        \\12 * 3 / 6;
        \\
    ;

    const tokens = try Scanner.scan(testing_alloc, input);
    defer testing_alloc.free(tokens);

    const program = try createAST(testing_alloc, tokens);
    defer testing_alloc.free(program);
    defer for (program) |stmt| {
        stmt.destroy(testing_alloc);
    };

    try testing.expectEqual(3, program.len);

    for (program) |stmt| {
        // We only have expression statements
        try testing.expect(stmt.* == .expr);
        // ...and all of them are binary
        try testing.expect(stmt.expr.expr.* == .binary);
    }

    {
        try testing.expect(program[0].expr.expr.binary.op.type == .@"/");
        try testing.expect(program[0].expr.expr.binary.lhs.* == .literal);
        try testing.expect(program[0].expr.expr.binary.lhs.literal.value == .int);
        try testing.expect(program[0].expr.expr.binary.lhs.literal.value.int == 4);
        try testing.expect(program[0].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[0].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[0].expr.expr.binary.rhs.literal.value.int == 2);
    }

    {
        try testing.expect(program[1].expr.expr.binary.op.type == .@"*");
        try testing.expect(program[1].expr.expr.binary.lhs.* == .literal);
        try testing.expect(program[1].expr.expr.binary.lhs.literal.value == .int);
        try testing.expect(program[1].expr.expr.binary.lhs.literal.value.int == 3);
        try testing.expect(program[1].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[1].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[1].expr.expr.binary.rhs.literal.value.int == 3);
    }

    {
        try testing.expect(program[2].expr.expr.binary.op.type == .@"/");
        try testing.expect(program[2].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[2].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[2].expr.expr.binary.rhs.literal.value.int == 6);

        {
            const lhs = program[2].expr.expr.binary.lhs;
            try testing.expect(lhs.* == .binary);
            try testing.expect(lhs.binary.op.type == .@"*");
            try testing.expect(lhs.binary.rhs.* == .literal);
            try testing.expect(lhs.binary.rhs.literal.value == .int);
            try testing.expect(lhs.binary.rhs.literal.value.int == 3);

            try testing.expect(lhs.binary.lhs.* == .literal);
            try testing.expect(lhs.binary.lhs.literal.value == .int);
            try testing.expect(lhs.binary.lhs.literal.value.int == 12);
        }
    }
}

test "Expression Statement with Terms" {
    const input =
        \\4 - 2;
        \\3 + 3;
        \\12 + 3 - 6;
        \\
    ;

    const tokens = try Scanner.scan(testing_alloc, input);
    defer testing_alloc.free(tokens);

    const program = try createAST(testing_alloc, tokens);
    defer testing_alloc.free(program);
    defer for (program) |stmt| {
        stmt.destroy(testing_alloc);
    };

    try testing.expectEqual(3, program.len);

    for (program) |stmt| {
        // We only have expression statements
        try testing.expect(stmt.* == .expr);
        // ...and all of them are binary
        try testing.expect(stmt.expr.expr.* == .binary);
    }

    {
        try testing.expect(program[0].expr.expr.binary.op.type == .@"-");
        try testing.expect(program[0].expr.expr.binary.lhs.* == .literal);
        try testing.expect(program[0].expr.expr.binary.lhs.literal.value == .int);
        try testing.expect(program[0].expr.expr.binary.lhs.literal.value.int == 4);
        try testing.expect(program[0].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[0].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[0].expr.expr.binary.rhs.literal.value.int == 2);
    }

    {
        try testing.expect(program[1].expr.expr.binary.op.type == .@"+");
        try testing.expect(program[1].expr.expr.binary.lhs.* == .literal);
        try testing.expect(program[1].expr.expr.binary.lhs.literal.value == .int);
        try testing.expect(program[1].expr.expr.binary.lhs.literal.value.int == 3);
        try testing.expect(program[1].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[1].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[1].expr.expr.binary.rhs.literal.value.int == 3);
    }

    {
        try testing.expect(program[2].expr.expr.binary.op.type == .@"-");
        try testing.expect(program[2].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[2].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[2].expr.expr.binary.rhs.literal.value.int == 6);

        {
            const lhs = program[2].expr.expr.binary.lhs;
            try testing.expect(lhs.* == .binary);
            try testing.expect(lhs.binary.op.type == .@"+");
            try testing.expect(lhs.binary.rhs.* == .literal);
            try testing.expect(lhs.binary.rhs.literal.value == .int);
            try testing.expect(lhs.binary.rhs.literal.value.int == 3);

            try testing.expect(lhs.binary.lhs.* == .literal);
            try testing.expect(lhs.binary.lhs.literal.value == .int);
            try testing.expect(lhs.binary.lhs.literal.value.int == 12);
        }
    }
}

test "Expression Statement with Comparisons" {
    const input =
        \\4 < 2;
        \\4 <= 2;
        \\4 >= 2;
        \\4 > 2;
        \\
    ;

    const tokens = try Scanner.scan(testing_alloc, input);
    defer testing_alloc.free(tokens);

    const program = try createAST(testing_alloc, tokens);
    defer testing_alloc.free(program);
    defer for (program) |stmt| {
        stmt.destroy(testing_alloc);
    };

    try testing.expectEqual(4, program.len);

    for (program) |stmt| {
        // We only have expression statements
        try testing.expect(stmt.* == .expr);
        // ...and all of them are binary
        try testing.expect(stmt.expr.expr.* == .binary);
    }

    {
        try testing.expect(program[0].expr.expr.binary.op.type == .@"<");
        try testing.expect(program[0].expr.expr.binary.lhs.* == .literal);
        try testing.expect(program[0].expr.expr.binary.lhs.literal.value == .int);
        try testing.expect(program[0].expr.expr.binary.lhs.literal.value.int == 4);
        try testing.expect(program[0].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[0].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[0].expr.expr.binary.rhs.literal.value.int == 2);
    }

    {
        try testing.expect(program[1].expr.expr.binary.op.type == .@"<=");
        try testing.expect(program[1].expr.expr.binary.lhs.* == .literal);
        try testing.expect(program[1].expr.expr.binary.lhs.literal.value == .int);
        try testing.expect(program[1].expr.expr.binary.lhs.literal.value.int == 4);
        try testing.expect(program[1].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[1].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[1].expr.expr.binary.rhs.literal.value.int == 2);
    }

    {
        try testing.expect(program[2].expr.expr.binary.op.type == .@">=");
        try testing.expect(program[2].expr.expr.binary.lhs.* == .literal);
        try testing.expect(program[2].expr.expr.binary.lhs.literal.value == .int);
        try testing.expect(program[2].expr.expr.binary.lhs.literal.value.int == 4);
        try testing.expect(program[2].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[2].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[2].expr.expr.binary.rhs.literal.value.int == 2);
    }

    {
        try testing.expect(program[3].expr.expr.binary.op.type == .@">");
        try testing.expect(program[3].expr.expr.binary.lhs.* == .literal);
        try testing.expect(program[3].expr.expr.binary.lhs.literal.value == .int);
        try testing.expect(program[3].expr.expr.binary.lhs.literal.value.int == 4);
        try testing.expect(program[3].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[3].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[3].expr.expr.binary.rhs.literal.value.int == 2);
    }
}

test "Expression Statement with Equality" {
    const input =
        \\4 != 2;
        \\4 == 2;
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
        // ...and all of them are binary
        try testing.expect(stmt.expr.expr.* == .binary);
    }

    {
        try testing.expect(program[0].expr.expr.binary.op.type == .@"!=");
        try testing.expect(program[0].expr.expr.binary.lhs.* == .literal);
        try testing.expect(program[0].expr.expr.binary.lhs.literal.value == .int);
        try testing.expect(program[0].expr.expr.binary.lhs.literal.value.int == 4);
        try testing.expect(program[0].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[0].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[0].expr.expr.binary.rhs.literal.value.int == 2);
    }

    {
        try testing.expect(program[1].expr.expr.binary.op.type == .@"==");
        try testing.expect(program[1].expr.expr.binary.lhs.* == .literal);
        try testing.expect(program[1].expr.expr.binary.lhs.literal.value == .int);
        try testing.expect(program[1].expr.expr.binary.lhs.literal.value.int == 4);
        try testing.expect(program[1].expr.expr.binary.rhs.* == .literal);
        try testing.expect(program[1].expr.expr.binary.rhs.literal.value == .int);
        try testing.expect(program[1].expr.expr.binary.rhs.literal.value.int == 2);
    }
}

test "Expression Statement with Logical" {
    const input =
        \\4 and 2;
        \\4 or 2;
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
        // ...and all of them are binary
        try testing.expect(stmt.expr.expr.* == .logical);
    }

    {
        try testing.expect(program[0].expr.expr.logical.op.type == .@"and");
        try testing.expect(program[0].expr.expr.logical.lhs.* == .literal);
        try testing.expect(program[0].expr.expr.logical.lhs.literal.value == .int);
        try testing.expect(program[0].expr.expr.logical.lhs.literal.value.int == 4);
        try testing.expect(program[0].expr.expr.logical.rhs.* == .literal);
        try testing.expect(program[0].expr.expr.logical.rhs.literal.value == .int);
        try testing.expect(program[0].expr.expr.logical.rhs.literal.value.int == 2);
    }

    {
        try testing.expect(program[1].expr.expr.logical.op.type == .@"or");
        try testing.expect(program[1].expr.expr.logical.lhs.* == .literal);
        try testing.expect(program[1].expr.expr.logical.lhs.literal.value == .int);
        try testing.expect(program[1].expr.expr.logical.lhs.literal.value.int == 4);
        try testing.expect(program[1].expr.expr.logical.rhs.* == .literal);
        try testing.expect(program[1].expr.expr.logical.rhs.literal.value == .int);
        try testing.expect(program[1].expr.expr.logical.rhs.literal.value.int == 2);
    }
}

test "Expression Statement with Assignment" {
    const input =
        \\name = "Hello";
        \\
    ;

    const tokens = try Scanner.scan(testing_alloc, input);
    defer testing_alloc.free(tokens);

    const program = try createAST(testing_alloc, tokens);
    defer testing_alloc.free(program);
    defer for (program) |stmt| {
        stmt.destroy(testing_alloc);
    };

    try testing.expectEqual(1, program.len);

    try testing.expect(program[0].* == .expr);
    try testing.expect(program[0].expr.expr.* == .assignment);

    try testing.expectEqualStrings("name", program[0].expr.expr.assignment.name.lexeme);
    try testing.expect(program[0].expr.expr.assignment.value.* == .literal);
    try testing.expect(program[0].expr.expr.assignment.value.literal.value == .string);
    try testing.expectEqualStrings("Hello", program[0].expr.expr.assignment.value.literal.value.string);
}

test "Print Statement" {
    const input =
        \\print 1;
        \\
    ;

    const tokens = try Scanner.scan(testing_alloc, input);
    defer testing_alloc.free(tokens);

    const program = try createAST(testing_alloc, tokens);
    defer testing_alloc.free(program);
    defer for (program) |stmt| {
        stmt.destroy(testing_alloc);
    };

    try testing.expectEqual(1, program.len);

    try testing.expect(program[0].* == .print);
    try testing.expect(program[0].print.expr.* == .literal);
    try testing.expect(program[0].print.expr.literal.value == .int);
    try testing.expect(program[0].print.expr.literal.value.int == 1);
}

test "Block" {
    const input =
        \\{
        \\    print 1;
        \\}
        \\
    ;

    const tokens = try Scanner.scan(testing_alloc, input);
    defer testing_alloc.free(tokens);

    const program = try createAST(testing_alloc, tokens);
    defer testing_alloc.free(program);
    defer for (program) |stmt| {
        stmt.destroy(testing_alloc);
    };

    try testing.expectEqual(1, program.len);

    try testing.expect(program[0].* == .block);
    try testing.expectEqual(1, program[0].block.stmts.len);

    try testing.expect(program[0].block.stmts[0].* == .print);
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
const definitions = @import("shared").definitions;
const Integer = definitions.Integer;
const Float = definitions.Float;

const Allocator = std.mem.Allocator;
const Expr = ast.Expr;
const Stmt = ast.Stmt;
