byte_code: std.ArrayList(u8),

pub fn compile(alloc: Allocator, program: []const *Stmt) ![]const u8 {
    var compiler: Compiler = .{
        .byte_code = .init(alloc),
    };

    try compiler.traverse(program);
    errdefer compiler.byte_code.deinit();

    return try compiler.byte_code.toOwnedSlice();
}

fn traverse(self: *Compiler, program: []const *ast.Stmt) !void {
    for (program) |stmt| {
        try self.statement(stmt);
    }
}

fn statement(self: *Compiler, stmt: *Stmt) !void {
    switch (stmt.*) {
        .expr => {
            try self.expression(stmt.expr.expr);
            self.emitOpcode(.pop);
        },
        .print => {
            try self.expression(stmt.print.expr);
            self.emitOpcode(.print);
        },
        else => return error.NotImplementedYet,
    }
}

fn expression(self: *Compiler, expr: *Expr) !void {
    switch (expr.*) {
        .literal => |literal| switch (literal.value) {
            .int => |int| {
                self.emitOpcode(.integer);
                self.emitMultibyte(i64, int);
            },
            .bool => |boolean| self.emitOpcode(if (boolean) .true else .false),
            .null => self.emitOpcode(.null),
            else => return error.NotImplementedYet,
        },
        else => return error.NotImplementedYet,
    }
}

fn emitOpcode(self: *Compiler, op: OpCode) void {
    self.byte_code.append(op.raw()) catch @panic("OOM");
}

fn emitMultibyte(self: *Compiler, T: type, value: T) void {
    const bytes = std.mem.toBytes(value);
    for (bytes) |byte| {
        self.byte_code.append(byte) catch @panic("OOM");
    }
}

test compile {
    const input =
        \\1;
        \\print 3;
        \\true;
        \\false;
        \\null;
        \\
    ;

    // zig fmt: off
    const expected: []const u8 = &.{
        OpCode.integer.raw(), 1, 0, 0, 0, 0, 0, 0, 0, OpCode.pop.raw(),
        OpCode.integer.raw(), 3, 0, 0, 0, 0, 0, 0, 0, OpCode.print.raw(),
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

    const bytecode = try compile(testing_allocator, program);
    defer testing_allocator.free(bytecode);

    try testing.expectEqualSlices(u8, expected_bytecode, bytecode);
}

const Compiler = @This();

const Token = @import("Token.zig");
const OpCode = @import("byte_code.zig").OpCode;
const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const ast = @import("ast.zig");
const Stmt = ast.Stmt;
const Expr = ast.Expr;

const std = @import("std");
const Allocator = std.mem.Allocator;

const testing = std.testing;
const testing_allocator = testing.allocator;
