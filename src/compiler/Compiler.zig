// TODO: better compile step errors?

alloc: Allocator,
byte_code: std.ArrayList(u8),
constants: []const FlowValue,

pub fn compile(alloc: Allocator, program: []const *Stmt, constants: []const FlowValue) ![]const u8 {
    var compiler: Compiler = .{
        .alloc = alloc,
        .byte_code = .init(alloc),
        .constants = constants,
    };
    errdefer compiler.byte_code.deinit();

    compiler.compileConstants();
    compiler.traverse(program);

    return try compiler.byte_code.toOwnedSlice();
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
        .bool, .null => @panic("not a constant"),
    };
    self.emitOpcode(.constants_done);
}

fn traverse(self: *Compiler, program: []const *ast.Stmt) void {
    for (program) |stmt| {
        self.statement(stmt);
    }
}

fn statement(self: *Compiler, stmt: *Stmt) void {
    switch (stmt.*) {
        .expr => {
            self.expression(stmt.expr.expr);
            self.emitOpcode(.pop);
        },
        .print => {
            self.expression(stmt.print.expr);
            self.emitOpcode(.print);
        },
        .variable => self.varDeclaration(stmt),
        .@"if" => self.ifStatement(stmt),
        .block => {
            self.traverse(stmt.block.stmts);
            for (0..stmt.block.local_count) |_| {
                self.emitOpcode(.pop);
            }
        },
        .loop => self.loopStatement(stmt),
        else => @panic("Not yet implemented"),
    }
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

    self.expression(stmt.loop.condition);

    const exit_jump = self.emitJump(.jump_if_false);
    self.emitOpcode(.pop);

    self.statement(stmt.loop.body);

    self.emitLoop(loop_start);
    self.patchJump(exit_jump);
    self.emitOpcode(.pop);
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
        .literal => |literal| switch (literal.value) {
            .int => |int| self.emitConstant(.{ .int = int }),
            .float => |float| self.emitConstant(.{ .float = float }),
            .bool => |boolean| self.emitOpcode(if (boolean) .true else .false),
            .null => self.emitOpcode(.null),
            .string => |string| self.emitConstant(.{ .string = string }),
        },
        .unary => |unary| {
            self.expression(unary.expr);
            switch (unary.op.type) {
                .@"!" => self.emitOpcode(.not),
                .@"-" => self.emitOpcode(.negate),
                else => @panic("Wrong Operation"),
            }
        },
        .binary => |binary| {
            self.expression(binary.lhs);
            self.expression(binary.rhs);
            switch (binary.op.type) {
                .@"+" => self.emitOpcode(.add),
                .@"-" => self.emitOpcode(.sub),
                .@"*" => self.emitOpcode(.mul),
                .@"/" => self.emitOpcode(.div),

                .@"==" => self.emitOpcode(.equal),
                .@"!=" => self.emitOpcode(.unequal),

                .@"<" => self.emitOpcode(.lower),
                .@"<=" => self.emitOpcode(.lower_equal),
                .@">" => self.emitOpcode(.greater),
                .@">=" => self.emitOpcode(.greater_equal),

                .@"." => self.emitOpcode(.concat),

                else => @panic("Wrong Operation"),
            }
        },
        .variable => |variable| {
            if (variable.global) {
                self.emitConstant(.{ .string = variable.name.lexeme });
                self.emitOpcode(.get_global);
            } else {
                self.emitOpcode(.get_local);
                self.emitByte(variable.local_idx);
            }
        },
        .assignment => |assignment| {
            self.expression(assignment.value);
            if (assignment.global) {
                self.emitConstant(.{ .string = assignment.name.lexeme });
                self.emitOpcode(.set_global);
            } else {
                self.emitOpcode(.set_local);
                self.emitByte(assignment.local_idx);
            }
        },
        .grouping => self.expression(expr.grouping.expr),
        else => @panic("Not yet implemented"),
    }
}

fn emitLoop(self: *Compiler, jump_idx: usize) void {
    const jump_to = self.byte_code.items.len - jump_idx + 3;

    if (jump_to > std.math.maxInt(u16)) {
        @panic("Jump too long");
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

    // TODO: Do we need to allow bigger jumps?
    if (exact_jump_length > std.math.maxInt(u16)) {
        @panic("Jump too long");
    }

    const jump_length: u16 = @intCast(exact_jump_length);

    const bytes = std.mem.toBytes(jump_length);
    self.byte_code.items[jump_idx + 1] = bytes[0];
    self.byte_code.items[jump_idx + 2] = bytes[1];
}

fn emitConstant(self: *Compiler, value: FlowValue) void {
    const idx = for (self.constants, 0..) |c, i| {
        if (c.equals(value)) break i;
    } else {
        std.debug.print("Could not find constant {}\n", .{value});
        @panic("UnknownConstant");
    };
    self.emitOpcode(.constant);
    self.emitByte(@intCast(idx));
}

fn emitOpcode(self: *Compiler, op: OpCode) void {
    self.byte_code.append(op.raw()) catch @panic("OOM");
}

fn emitByte(self: *Compiler, byte: u8) void {
    self.byte_code.append(byte) catch @panic("OOM");
}

fn emitMultibyte(self: *Compiler, value: anytype) void {
    const bytes = std.mem.toBytes(value);
    for (bytes) |byte| {
        self.byte_code.append(byte) catch @panic("OOM");
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

        OpCode.constant.raw(), 0, OpCode.pop.raw(),
        OpCode.constant.raw(), 1, OpCode.pop.raw(),

        OpCode.true.raw(), OpCode.pop.raw(),
        OpCode.false.raw(), OpCode.pop.raw(),
        OpCode.null.raw(), OpCode.pop.raw(),
    };
    // zig fmt: on

    try testBytecode(input, expected);
}

test "Print Statement" {
    const input =
        \\print 1;
        \\
    ;

    const expected: []const u8 = &.{
        OpCode.integer.raw(),
        1,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        OpCode.constants_done.raw(),
        OpCode.constant.raw(),
        0,
        OpCode.print.raw(),
    };

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

    var sema: Sema = try .init(testing_allocator, program);
    defer sema.deinit();
    try sema.analyse();

    const bytecode = try compile(testing_allocator, program, sema.constants.items);
    defer testing_allocator.free(bytecode);

    try testing.expectEqualSlices(u8, expected_bytecode, bytecode);
}

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

const std = @import("std");
const Allocator = std.mem.Allocator;

const testing = std.testing;
const testing_allocator = testing.allocator;
