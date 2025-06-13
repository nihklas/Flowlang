tokens: []const Token,
arena: Allocator,
current: usize = 0,
has_error: bool = false,

pub fn createAST(arena: Allocator, tokens: []const Token) ![]const *Stmt {
    var parser: Parser = .{
        .tokens = tokens,
        .arena = arena,
    };
    return try parser.parse();
}

fn parse(self: *Parser) ![]const *Stmt {
    var stmt_list: std.ArrayList(*Stmt) = .init(self.arena);

    while (!self.isAtEnd()) {
        if (self.declaration()) |stmt| {
            stmt_list.append(stmt) catch oom();
        } else |_| {
            self.has_error = true;
            self.recover();
        }
    }

    try self.consume(.EOF, "Expected EOF");

    if (self.has_error) {
        return error.ParseError;
    }

    return stmt_list.toOwnedSlice() catch oom();
}

fn declaration(self: *Parser) ParserError!*Stmt {
    if (self.matchOneOf(&.{ .@"var", .@"const" })) |_| {
        return self.varDeclaration();
    }

    if (self.match(.func)) |_| {
        return self.funcDeclaration();
    }

    return self.statement();
}

fn varDeclaration(self: *Parser) ParserError!*Stmt {
    // var or const
    const keyword = self.previous();

    try self.consume(.identifier, "Expected variable identifier");

    const name = self.previous();

    const type_hint = blk: {
        if (self.match(.@":")) |colon| {
            const result = self.typeHint();

            if (result == null) {
                error_reporter.reportError(colon, "Expected typehint after ':'", .{});
                return ParserError.UnexpectedToken;
            }

            break :blk result;
        }

        break :blk null;
    };

    const value = blk: {
        if (self.match(.@"=")) |_| {
            break :blk try self.expression();
        }
        break :blk null;
    };

    try self.consume(.@";", "Expected ';' after variable declaration");

    return Stmt.createVariable(self.arena, name, type_hint, keyword.type == .@"const", value);
}

fn funcDeclaration(self: *Parser) ParserError!*Stmt {
    try self.consume(.identifier, "Expected identifier after 'func'");
    const name = self.previous();

    try self.consume(.@"(", "Expected '(' after function name");

    const params = try self.parameters();

    try self.consume(.@")", "Expected ')' after function parameters");
    const close_paren = self.previous();

    const type_hint: ast.TypeHint = blk: {
        if (self.check(.@"{")) {
            break :blk .{
                .token = .{
                    .type = .null,
                    .line = close_paren.line,
                    .column = close_paren.column,
                    .lexeme = "null",
                },
                .type = .null,
            };
        }

        const result = self.typeHint();
        if (result == null) {
            error_reporter.reportError(self.peek(), "Unexpected Token, expected either typehint or '{{'", .{});
            return error.UnexpectedToken;
        }
        break :blk result.?;
    };

    try self.consume(.@"{", "Expected '{' before function body");

    const body = try self.block();

    return Stmt.createFunction(self.arena, name, type_hint, params, body);
}

fn parameters(self: *Parser) ParserError![]*Stmt {
    if (self.check(.@")")) return &.{};

    var params: std.ArrayList(*Stmt) = .init(self.arena);
    defer params.deinit();

    while (!self.check(.@")") and !self.isAtEnd()) {
        if (self.param()) |parameter| {
            params.append(parameter) catch oom();
        }

        if (self.match(.@",") == null) {
            break;
        }
    }

    return params.toOwnedSlice() catch oom();
}

fn param(self: *Parser) ?*Stmt {
    if (self.match(.identifier)) |name| {
        self.consume(.@":", "Expected ':' in front of type hint") catch {
            self.has_error = true;
            return null;
        };
        const type_hint = self.typeHint();
        if (type_hint == null) {
            error_reporter.reportError(name, "Expected type hint after parameter name", .{});
            self.has_error = true;
            return null;
        }

        return Stmt.createVariable(self.arena, name, type_hint, true, null);
    }

    error_reporter.reportError(self.peek(), "Expected parameter name", .{});
    self.has_error = true;
    return null;
}

fn statement(self: *Parser) ParserError!*Stmt {
    if (self.match(.@"if")) |_| {
        return self.ifStatement();
    }

    if (self.match(.@"for")) |_| {
        return self.forStatement();
    }

    if (self.match(.@"return")) |_| {
        return self.returnStatement();
    }

    if (self.match(.@"break")) |token| {
        try self.consume(.@";", "Expected ';' after 'break'");
        return Stmt.createBreak(self.arena, token);
    }

    if (self.match(.@"continue")) |token| {
        try self.consume(.@";", "Expected ';' after 'continue'");
        return Stmt.createContinue(self.arena, token);
    }

    if (self.match(.@"{")) |_| {
        const stmts = try self.block();
        return Stmt.createBlock(self.arena, stmts);
    }

    return self.expressionStatement();
}

fn block(self: *Parser) ParserError![]*Stmt {
    var stmt_list: std.ArrayList(*Stmt) = .init(self.arena);

    while (!self.check(.@"}") and !self.isAtEnd()) {
        const stmt = try self.declaration();
        stmt_list.append(stmt) catch oom();
    }

    try self.consume(.@"}", "Expected '}' at the end of block");

    return stmt_list.toOwnedSlice() catch oom();
}

fn ifStatement(self: *Parser) ParserError!*Stmt {
    const condition = try self.expression();
    const then = blk: {
        const stmt = try self.statement();
        if (stmt.* == .block) break :blk stmt;

        const stmts = self.arena.alloc(*Stmt, 1) catch oom();
        stmts[0] = stmt;
        break :blk Stmt.createBlock(self.arena, stmts);
    };

    const else_branch = blk: {
        if (self.match(.@"else") == null) break :blk null;

        const stmt = try self.statement();
        if (stmt.* == .block) break :blk stmt;

        const stmts = self.arena.alloc(*Stmt, 1) catch oom();
        stmts[0] = stmt;
        break :blk Stmt.createBlock(self.arena, stmts);
    };

    return Stmt.createIf(self.arena, condition, then, else_branch);
}

fn returnStatement(self: *Parser) ParserError!*Stmt {
    const keyword = self.previous();
    const value = if (!self.check(.@";")) try self.expression() else null;

    try self.consume(.@";", "Expected ';' after return statement");
    return Stmt.createReturn(self.arena, keyword, value);
}

fn forStatement(self: *Parser) ParserError!*Stmt {
    const maybe_initializer: ?*Stmt = blk: {
        if (self.match(.@";")) |_| {
            break :blk null;
        }

        if (self.match(.@"var")) |_| {
            break :blk try self.varDeclaration();
        }

        break :blk try self.expressionStatement();
    };

    const condition: *Expr = blk: {
        if (self.match(.@";")) |t| {
            break :blk Expr.createLiteral(self.arena, t, .{ .bool = true });
        }

        const expr = try self.expression();
        try self.consume(.@";", "Expected ';' after loop condition");
        break :blk expr;
    };

    const maybe_increment = blk: {
        if (self.check(.@"{")) {
            break :blk null;
        }

        break :blk try self.expression();
    };

    try self.consume(.@"{", "Expected '{' before loop body");
    const body = try self.block();

    var outer_scope: std.ArrayList(*Stmt) = .init(self.arena);

    if (maybe_initializer) |initializer| {
        outer_scope.append(initializer) catch oom();
    }

    const loop = Stmt.createLoop(self.arena, condition, body);
    outer_scope.append(loop) catch oom();

    if (maybe_increment) |increment| {
        loop.loop.inc = increment;
    }

    const outer_scope_stmts = outer_scope.toOwnedSlice() catch oom();
    return Stmt.createBlock(self.arena, outer_scope_stmts);
}

fn expressionStatement(self: *Parser) ParserError!*Stmt {
    const expr = try self.expression();

    try self.consume(.@";", "Expected ';' after expression");

    return Stmt.createExpr(self.arena, expr);
}

fn expression(self: *Parser) ParserError!*Expr {
    return self.assignment();
}

fn assignment(self: *Parser) ParserError!*Expr {
    if (self.isAssignment()) {
        const identifier = self.match(.identifier).?;
        const op = self.advance();

        var expr = try self.expression();
        if (op.type != .@"=") {
            const identifier_expr = Expr.createVariable(self.arena, identifier);
            expr = Expr.createBinary(self.arena, identifier_expr, op, expr);
        }

        return Expr.createAssignment(self.arena, identifier, expr);
    }

    return self.concat();
}

fn concat(self: *Parser) ParserError!*Expr {
    var lhs = try self.orExpr();

    while (self.match(.@".")) |op| {
        const rhs = try self.expression();
        lhs = Expr.createBinary(self.arena, lhs, op, rhs);
    }

    return lhs;
}

fn orExpr(self: *Parser) ParserError!*Expr {
    var lhs = try self.andExpr();

    while (self.match(.@"or")) |op| {
        const rhs = try self.andExpr();
        lhs = Expr.createLogical(self.arena, lhs, op, rhs);
    }

    return lhs;
}

fn andExpr(self: *Parser) ParserError!*Expr {
    var lhs = try self.equality();

    while (self.match(.@"and")) |op| {
        const rhs = try self.equality();
        lhs = Expr.createLogical(self.arena, lhs, op, rhs);
    }

    return lhs;
}

fn equality(self: *Parser) ParserError!*Expr {
    var lhs = try self.comparison();

    while (self.matchOneOf(&.{ .@"!=", .@"==" })) |op| {
        const rhs = try self.comparison();
        lhs = Expr.createBinary(self.arena, lhs, op, rhs);
    }

    return lhs;
}

fn comparison(self: *Parser) ParserError!*Expr {
    var lhs = try self.term();

    while (self.matchOneOf(&.{ .@"<", .@"<=", .@">=", .@">" })) |op| {
        const rhs = try self.term();
        lhs = Expr.createBinary(self.arena, lhs, op, rhs);
    }

    return lhs;
}

fn term(self: *Parser) ParserError!*Expr {
    var lhs = try self.factor();

    while (self.matchOneOf(&.{ .@"+", .@"-" })) |op| {
        const rhs = try self.factor();
        lhs = Expr.createBinary(self.arena, lhs, op, rhs);
    }

    return lhs;
}

fn factor(self: *Parser) ParserError!*Expr {
    var lhs = try self.unary();

    while (self.matchOneOf(&.{ .@"*", .@"/", .@"%" })) |op| {
        const rhs = try self.unary();
        lhs = Expr.createBinary(self.arena, lhs, op, rhs);
    }

    return lhs;
}

fn unary(self: *Parser) ParserError!*Expr {
    if (self.matchOneOf(&.{ .@"-", .@"!" })) |op| {
        const expr = try self.unary();
        return Expr.createUnary(self.arena, op, expr);
    }

    return self.call();
}

fn call(self: *Parser) ParserError!*Expr {
    const expr = try self.index();

    if (self.match(.@"(") == null) {
        return expr;
    }

    const params: []*Expr = blk: {
        var params_list: std.ArrayList(*Expr) = .init(self.arena);
        defer params_list.deinit();

        while (!self.check(.@")") and !self.isAtEnd()) {
            params_list.append(try self.expression()) catch oom();

            if (self.match(.@",") == null) break;
        }

        break :blk params_list.toOwnedSlice() catch oom();
    };

    try self.consume(.@")", "Expected ')' after parameters");

    return Expr.createCall(self.arena, expr, params);
}

fn index(self: *Parser) ParserError!*Expr {
    var expr = try self.primary();

    while (self.match(.@"[")) |bracket| {
        const index_expr = try self.expression();
        try self.consume(.@"]", "Expected ']' after index");
        expr = Expr.createIndex(self.arena, expr, bracket, index_expr);
    }

    return expr;
}

fn primary(self: *Parser) ParserError!*Expr {
    if (self.match(.@"[")) |open_bracket| {
        const items: []*Expr = blk: {
            var items_list: std.ArrayList(*Expr) = .init(self.arena);
            defer items_list.deinit();

            while (!self.check(.@"]") and !self.isAtEnd()) {
                items_list.append(try self.expression()) catch oom();

                if (self.match(.@",") == null) break;
            }

            break :blk items_list.toOwnedSlice() catch oom();
        };
        try self.consume(.@"]", "Expected ']' after array literal");

        return Expr.createLiteral(self.arena, open_bracket, .{ .array = items });
    }

    if (self.match(.null)) |token| {
        return Expr.createLiteral(self.arena, token, .null);
    }
    if (self.matchOneOf(&.{ .true, .false })) |token| {
        return Expr.createLiteral(self.arena, token, .{ .bool = token.type == .true });
    }

    if (self.match(.number)) |token| {
        if (std.fmt.parseInt(Integer, token.lexeme, 10)) |value| {
            return Expr.createLiteral(self.arena, token, .{ .int = value });
        } else |err| switch (err) {
            error.InvalidCharacter => {
                const value = std.fmt.parseFloat(Float, token.lexeme) catch {
                    error_reporter.reportError(token, "Could not convert '{s}' to float", .{token.lexeme});
                    return ParserError.SyntaxError;
                };
                return Expr.createLiteral(self.arena, token, .{ .float = value });
            },
            error.Overflow => {
                error_reporter.reportError(token, "Could not convert '{s}' to int", .{token.lexeme});
                return ParserError.SyntaxError;
            },
        }
    }

    if (self.match(.string_literal)) |token| {
        return Expr.createLiteral(self.arena, token, .{ .string = token.lexeme });
    }

    if (self.match(.identifier)) |token| {
        return Expr.createVariable(self.arena, token);
    }

    if (self.match(.@"(")) |open_paren| {
        const expr = try self.expression();

        try self.consume(.@")", "Expected ')' after Expression");

        return Expr.createGrouping(self.arena, open_paren, expr);
    }

    error_reporter.reportError(self.peek(), "UnexpectedToken: Expected Expression, got '{s}'", .{@tagName(self.peek().type)});
    return ParserError.UnexpectedToken;
}

fn typeHint(self: *Parser) ?ast.TypeHint {
    var order: u8 = 0;
    while (self.match(.@"[") != null and self.match(.@"]") != null) {
        order += 1;
    }

    if (self.matchOneOf(&.{ .string, .int, .float, .bool })) |token| {
        return .{
            .type = .{
                .type = switch (token.type) {
                    .string => .string,
                    .int => .int,
                    .float => .float,
                    .bool => .bool,
                    else => unreachable,
                },
                .order = order,
            },
            .token = token,
        };
    }

    return null;
}

fn recover(self: *Parser) void {
    recover: switch (self.advance().type) {
        .@";", .EOF => {},
        .@"if",
        .@"for",
        .@"var",
        .func,
        .@"return",
        .@"{",
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

fn matchOneOf(self: *Parser, comptime expecteds: []const Token.Type) ?Token {
    inline for (expecteds) |t_type| {
        if (self.check(t_type)) return self.advance();
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

fn isAssignment(self: *Parser) bool {
    return self.check(.identifier) and (self.checkNext(.@"=") or
        self.checkNext(.@".=") or
        self.checkNext(.@"+=") or
        self.checkNext(.@"-=") or
        self.checkNext(.@"*=") or
        self.checkNext(.@"/=") or
        self.checkNext(.@"%="));
}

fn advance(self: *Parser) Token {
    if (self.isAtEnd()) return self.peek();

    defer self.current += 1;
    return self.peek();
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

    var testing_arena_state: std.heap.ArenaAllocator = .init(testing_alloc);
    defer testing_arena_state.deinit();
    const testing_arena = testing_arena_state.allocator();

    const program = try createAST(testing_arena, tokens);

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

    var testing_arena_state: std.heap.ArenaAllocator = .init(testing_alloc);
    defer testing_arena_state.deinit();
    const testing_arena = testing_arena_state.allocator();

    const program = try createAST(testing_arena, tokens);

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

    var testing_arena_state: std.heap.ArenaAllocator = .init(testing_alloc);
    defer testing_arena_state.deinit();
    const testing_arena = testing_arena_state.allocator();

    const program = try createAST(testing_arena, tokens);

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

    var testing_arena_state: std.heap.ArenaAllocator = .init(testing_alloc);
    defer testing_arena_state.deinit();
    const testing_arena = testing_arena_state.allocator();

    const program = try createAST(testing_arena, tokens);

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

    var testing_arena_state: std.heap.ArenaAllocator = .init(testing_alloc);
    defer testing_arena_state.deinit();
    const testing_arena = testing_arena_state.allocator();

    const program = try createAST(testing_arena, tokens);

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

    var testing_arena_state: std.heap.ArenaAllocator = .init(testing_alloc);
    defer testing_arena_state.deinit();
    const testing_arena = testing_arena_state.allocator();

    const program = try createAST(testing_arena, tokens);

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

    var testing_arena_state: std.heap.ArenaAllocator = .init(testing_alloc);
    defer testing_arena_state.deinit();
    const testing_arena = testing_arena_state.allocator();

    const program = try createAST(testing_arena, tokens);

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

    var testing_arena_state: std.heap.ArenaAllocator = .init(testing_alloc);
    defer testing_arena_state.deinit();
    const testing_arena = testing_arena_state.allocator();

    const program = try createAST(testing_arena, tokens);

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

    var testing_arena_state: std.heap.ArenaAllocator = .init(testing_alloc);
    defer testing_arena_state.deinit();
    const testing_arena = testing_arena_state.allocator();

    const program = try createAST(testing_arena, tokens);

    try testing.expectEqual(1, program.len);

    try testing.expect(program[0].* == .expr);
    try testing.expect(program[0].expr.expr.* == .assignment);

    try testing.expectEqualStrings("name", program[0].expr.expr.assignment.name.lexeme);
    try testing.expect(program[0].expr.expr.assignment.value.* == .literal);
    try testing.expect(program[0].expr.expr.assignment.value.literal.value == .string);
    try testing.expectEqualStrings("Hello", program[0].expr.expr.assignment.value.literal.value.string);
}

test "Block" {
    const input =
        \\{
        \\    1;
        \\}
        \\
    ;

    const tokens = try Scanner.scan(testing_alloc, input);
    defer testing_alloc.free(tokens);

    var testing_arena_state: std.heap.ArenaAllocator = .init(testing_alloc);
    defer testing_arena_state.deinit();
    const testing_arena = testing_arena_state.allocator();

    const program = try createAST(testing_arena, tokens);

    try testing.expectEqual(1, program.len);

    try testing.expect(program[0].* == .block);
    try testing.expectEqual(1, program[0].block.stmts.len);

    try testing.expect(program[0].block.stmts[0].* == .expr);
}

const ParserError = error{
    SyntaxError,
    UnexpectedToken,
    NotImplementedYet,
    Panic,
};

const Parser = @This();

const std = @import("std");
const Token = @import("ir/Token.zig");
const ast = @import("ir/ast.zig");
const error_reporter = @import("util/error_reporter.zig");
const definitions = @import("shared").definitions;
const oom = @import("shared").oom;
const Integer = definitions.Integer;
const Float = definitions.Float;

const Allocator = std.mem.Allocator;
const Expr = ast.Expr;
const Stmt = ast.Stmt;
