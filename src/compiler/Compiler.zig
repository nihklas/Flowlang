alloc: Allocator,
byte_code: std.ArrayList(u8),
constants: []const FlowValue,
loop_levels: Stack(LoopLevel, 256),

pub fn compile(alloc: Allocator, program: []const *Stmt, sema: *Sema) []const u8 {
    var compiler: Compiler = .{
        .alloc = alloc,
        .byte_code = .init(alloc),
        .constants = sema.constants.items,
        .loop_levels = .init(alloc),
    };
    defer compiler.loop_levels.deinit(alloc);

    compiler.compileConstants();
    compiler.compileFunctions(program);
    compiler.traverse(program);

    return compiler.byte_code.toOwnedSlice() catch oom();
}

fn compileConstants(self: *Compiler) void {
    for (self.constants) |c| switch (c) {
        .string => |string| {
            if (string.len > std.math.maxInt(u8)) {
                self.emitOpcode(.string_long);
                self.emitMultibyte(@as(u32, @intCast(string.len)));
            } else {
                self.emitOpcode(.string);
                self.emitByte(@intCast(string.len));
            }
            for (string) |char| {
                self.emitByte(char);
            }
        },
        .int => {
            self.emitOpcode(.integer);
            self.emitMultibyte(c.int);
        },
        .float => {
            self.emitOpcode(.float);
            self.emitMultibyte(c.float);
        },
        .bool, .null, .builtin_fn, .function => unreachable,
    };
    self.emitOpcode(.constants_done);
}

fn compileFunctions(self: *Compiler, program: []const *ast.Stmt) void {
    for (program) |stmt| {
        switch (stmt.*) {
            .function => |func| {
                self.emitOpcode(.function);
                self.emitByte(self.resolveConstant(.{ .string = func.name.lexeme }));
                self.emitByte(@intCast(func.params.len));
                // Reserve space for operand
                self.emitByte(0x00);
                self.emitByte(0x00);
                const op_idx = self.byte_code.items.len;

                for (func.body) |line| {
                    self.statement(line);
                }
                if (func.ret_type.type == .void) {
                    self.emitOpcode(.null);
                }
                self.emitOpcode(.@"return");

                const line_count = self.byte_code.items.len - op_idx + 1;
                const jump_length: u16 = @intCast(line_count);

                const bytes = std.mem.toBytes(jump_length);
                self.byte_code.items[op_idx - 2] = bytes[0];
                self.byte_code.items[op_idx - 1] = bytes[1];
            },
            else => {},
        }
    }

    self.emitOpcode(.functions_done);
}

fn traverse(self: *Compiler, program: []const *ast.Stmt) void {
    for (program) |stmt| {
        self.statement(stmt);
    }
}

fn statement(self: *Compiler, stmt: *Stmt) void {
    switch (stmt.*) {
        .expr => self.expressionStatement(stmt),
        .variable => self.varDeclaration(stmt),
        .@"if" => self.ifStatement(stmt),
        .block => self.blockStatement(stmt),
        .loop => self.loopStatement(stmt),
        .@"break" => self.breakStatement(),
        .@"continue" => self.continueStatement(),
        .@"return" => self.returnStatement(stmt),
        .function => {}, // This is not an error state, global functions are already handled
        else => panic("{s} is not yet implemented", .{@tagName(stmt.*)}),
    }
}

fn expressionStatement(self: *Compiler, stmt: *Stmt) void {
    self.expression(stmt.expr.expr);
    self.emitOpcode(.pop);
}

fn blockStatement(self: *Compiler, stmt: *Stmt) void {
    self.traverse(stmt.block.stmts);
    for (0..stmt.block.local_count) |_| {
        self.emitOpcode(.pop);
    }
}

fn returnStatement(self: *Compiler, stmt: *Stmt) void {
    self.expression(stmt.@"return".value);
    self.emitOpcode(.@"return");
}

fn varDeclaration(self: *Compiler, stmt: *Stmt) void {
    const variable = stmt.variable;

    if (variable.value) |value| {
        self.expression(value);
    } else {
        self.emitOpcode(.null);
    }

    if (variable.global) {
        self.emitConstant(.{ .string = variable.name.lexeme });
        self.emitOpcode(.create_global);
    }
}

fn loopStatement(self: *Compiler, stmt: *Stmt) void {
    const loop_start = self.byte_code.items.len;

    var break_statements: std.ArrayList(usize) = .init(self.alloc);
    defer break_statements.deinit();

    const loop_level: LoopLevel = .{
        .loop = stmt,
        .break_statements = &break_statements,
        .loop_start = loop_start,
    };

    self.loop_levels.push(loop_level);
    defer _ = self.loop_levels.pop();

    self.expression(stmt.loop.condition);

    const exit_jump = self.emitJump(.jump_if_false);
    self.emitOpcode(.pop);

    for (stmt.loop.body) |body| {
        self.statement(body);
    }

    self.emitLoop(loop_start);
    self.patchJump(exit_jump);
    self.emitOpcode(.pop);

    for (loop_level.break_statements.items) |break_stmt| {
        self.patchJump(break_stmt);
    }
}

fn breakStatement(self: *Compiler) void {
    const break_stmt = self.emitJump(.jump);
    var stmts = self.loop_levels.at(0);
    stmts.break_statements.append(break_stmt) catch oom();
}

fn continueStatement(self: *Compiler) void {
    const loop_level = self.loop_levels.at(0);

    if (loop_level.loop.loop.inc) |inc| {
        self.statement(inc);
    }

    self.emitLoop(loop_level.loop_start);
}

fn ifStatement(self: *Compiler, stmt: *Stmt) void {
    self.expression(stmt.@"if".condition);
    const jump_idx = self.emitJump(.jump_if_false);

    self.emitOpcode(.pop);
    self.statement(stmt.@"if".true_branch);

    const else_jump = self.emitJump(.jump);

    self.patchJump(jump_idx);

    self.emitOpcode(.pop);
    if (stmt.@"if".false_branch) |false_branch| {
        self.statement(false_branch);
    }

    self.patchJump(else_jump);
}

fn expression(self: *Compiler, expr: *Expr) void {
    switch (expr.*) {
        .literal => self.literalExpression(expr),
        .unary => self.unaryExpression(expr),
        .binary => self.binaryExpression(expr),
        .variable => self.variableExpression(expr),
        .assignment => self.assignmentExpression(expr),
        .grouping => self.expression(expr.grouping.expr),
        .call => self.callExpression(expr),
        else => panic("{s} is not yet implemented", .{@tagName(expr.*)}),
    }
}

fn literalExpression(self: *Compiler, expr: *Expr) void {
    switch (expr.literal.value) {
        .int => |int| self.emitConstant(.{ .int = int }),
        .float => |float| self.emitConstant(.{ .float = float }),
        .bool => |boolean| self.emitOpcode(if (boolean) .true else .false),
        .null => self.emitOpcode(.null),
        .string => |string| self.emitConstant(.{ .string = string }),
    }
}

fn unaryExpression(self: *Compiler, expr: *Expr) void {
    self.expression(expr.unary.expr);
    switch (expr.unary.op.type) {
        .@"!" => self.emitOpcode(.not),
        .@"-" => self.emitOpcode(.negate),
        else => unreachable,
    }
}

fn binaryExpression(self: *Compiler, expr: *Expr) void {
    const binary = expr.binary;
    self.expression(binary.lhs);
    self.expression(binary.rhs);
    switch (binary.op.type) {
        .@"+", .@"+=" => self.emitOpcode(.add),
        .@"-", .@"-=" => self.emitOpcode(.sub),
        .@"*", .@"*=" => self.emitOpcode(.mul),
        .@"/", .@"/=" => self.emitOpcode(.div),
        .@"%", .@"%=" => self.emitOpcode(.mod),

        .@"==" => self.emitOpcode(.equal),
        .@"!=" => self.emitOpcode(.unequal),

        .@"<" => self.emitOpcode(.lower),
        .@"<=" => self.emitOpcode(.lower_equal),
        .@">" => self.emitOpcode(.greater),
        .@">=" => self.emitOpcode(.greater_equal),

        .@".", .@".=" => self.emitOpcode(.concat),

        else => unreachable,
    }
}

fn variableExpression(self: *Compiler, expr: *Expr) void {
    if (expr.variable.global) {
        self.emitConstant(.{ .string = expr.variable.name.lexeme });
        self.emitOpcode(.get_global);
    } else {
        self.emitOpcode(.get_local);
        self.emitByte(expr.variable.local_idx);
    }
}

fn assignmentExpression(self: *Compiler, expr: *Expr) void {
    self.expression(expr.assignment.value);
    if (expr.assignment.global) {
        self.emitConstant(.{ .string = expr.assignment.name.lexeme });
        self.emitOpcode(.set_global);
    } else {
        self.emitOpcode(.set_local);
        self.emitByte(expr.assignment.local_idx);
    }
}

fn callExpression(self: *Compiler, expr: *Expr) void {
    for (expr.call.args) |arg| {
        self.expression(arg);
    }
    self.expression(expr.call.expr);
    self.emitOpcode(.call);
}

fn emitLoop(self: *Compiler, jump_idx: usize) void {
    const jump_to = self.byte_code.items.len - jump_idx + 3;

    if (jump_to > std.math.maxInt(u16)) {
        panic("Jump too long: {d}", .{jump_to});
    }

    self.emitOpcode(.jump_back);

    const jump_len: u16 = @intCast(jump_to);
    self.emitMultibyte(jump_len);
}

fn emitJump(self: *Compiler, jump_op: OpCode) usize {
    const jump_idx = self.byte_code.items.len;
    self.emitOpcode(jump_op);
    // Reserve space for jump distance
    self.emitByte(0x00);
    self.emitByte(0x00);
    return jump_idx;
}

fn patchJump(self: *Compiler, jump_idx: usize) void {
    // len - 1 to get the correct amount of instructions between jmp and now
    // another - 2 to account for the jmp length in the jmp instruction
    const exact_jump_length = self.byte_code.items.len - 3 - jump_idx;

    if (exact_jump_length > std.math.maxInt(u16)) {
        panic("Jump too long: {d}", .{exact_jump_length});
    }

    const jump_length: u16 = @intCast(exact_jump_length);

    const bytes = std.mem.toBytes(jump_length);
    self.byte_code.items[jump_idx + 1] = bytes[0];
    self.byte_code.items[jump_idx + 2] = bytes[1];
}

fn emitConstant(self: *Compiler, value: FlowValue) void {
    const idx = self.resolveConstant(value);
    self.emitOpcode(.constant);
    self.emitByte(@intCast(idx));
}

fn resolveConstant(self: *Compiler, value: FlowValue) u8 {
    return for (self.constants, 0..) |c, i| {
        if (c.equals(value)) break @intCast(i);
    } else unreachable;
}

fn emitOpcode(self: *Compiler, op: OpCode) void {
    self.byte_code.append(op.raw()) catch oom();
}

fn emitByte(self: *Compiler, byte: u8) void {
    self.byte_code.append(byte) catch oom();
}

fn emitMultibyte(self: *Compiler, value: anytype) void {
    const bytes = std.mem.toBytes(value);
    for (bytes) |byte| {
        self.byte_code.append(byte) catch oom();
    }
}

test "Expression Statement" {
    const input =
        \\1;
        \\1.2;
        \\true;
        \\false;
        \\null;
        \\
    ;

    const float_bytes = std.mem.toBytes(@as(f64, 1.2));

    // zig fmt: off
    const expected: []const u8 = &.{
        OpCode.integer.raw(), 1, 0, 0, 0, 0, 0, 0, 0,
        OpCode.float.raw(),
        float_bytes[0],
        float_bytes[1],
        float_bytes[2],
        float_bytes[3],
        float_bytes[4],
        float_bytes[5],
        float_bytes[6],
        float_bytes[7],

        OpCode.constants_done.raw(),
        OpCode.functions_done.raw(),

        OpCode.constant.raw(), 0, OpCode.pop.raw(),
        OpCode.constant.raw(), 1, OpCode.pop.raw(),

        OpCode.true.raw(), OpCode.pop.raw(),
        OpCode.false.raw(), OpCode.pop.raw(),
        OpCode.null.raw(), OpCode.pop.raw(),
    };
    // zig fmt: on

    try testBytecode(input, expected);
}

fn testBytecode(input: []const u8, expected_bytecode: []const u8) !void {
    const tokens = try Scanner.scan(testing_allocator, input);
    defer testing_allocator.free(tokens);

    const program = try Parser.createAST(testing_allocator, tokens);
    defer testing_allocator.free(program);
    defer for (program) |node| {
        node.destroy(testing_allocator);
    };

    var sema: Sema = .init(testing_allocator, program);
    defer sema.deinit();
    try sema.analyse();

    const bytecode = compile(testing_allocator, program, &sema);
    defer testing_allocator.free(bytecode);

    try testing.expectEqualSlices(u8, expected_bytecode, bytecode);
}

const LoopLevel = struct {
    loop: *Stmt,
    break_statements: *std.ArrayList(usize),
    loop_start: usize,
};

const Compiler = @This();

const Token = @import("Token.zig");
const OpCode = @import("shared").OpCode;
const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const Sema = @import("Sema.zig");
const ast = @import("ast.zig");
const Stmt = ast.Stmt;
const Expr = ast.Expr;
const FlowValue = @import("shared").definitions.FlowValue;
const Stack = @import("shared").Stack;
const oom = @import("shared").oom;

const std = @import("std");
const Allocator = std.mem.Allocator;

const testing = std.testing;
const testing_allocator = testing.allocator;
const panic = std.debug.panic;
