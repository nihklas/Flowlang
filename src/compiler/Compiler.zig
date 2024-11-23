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

    try compiler.compileConstants();
    try compiler.traverse(program);

    return try compiler.byte_code.toOwnedSlice();
}

fn compileConstants(self: *Compiler) !void {
    for (self.constants) |c| switch (c) {
        .string => return error.NotImplementedYet,
        .int => {
            self.emitOpcode(.integer);
            self.emitMultibyte(c.int);
        },
        .float => {
            self.emitOpcode(.float);
            self.emitMultibyte(c.float);
        },
        .bool, .null => return error.NotAConstant,
    };
    self.emitOpcode(.constants_done);
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
            .int => |int| self.emitConstant(.{ .int = int }),
            .float => |float| self.emitConstant(.{ .float = float }),
            .bool => |boolean| self.emitOpcode(if (boolean) .true else .false),
            .null => self.emitOpcode(.null),
            else => return error.NotImplementedYet,
        },
        .unary => |unary| {
            try self.expression(unary.expr);
            switch (unary.op.type) {
                .@"!" => self.emitOpcode(.not),
                .@"-" => self.emitOpcode(.negate),
                else => @panic("Wrong Operation"),
            }
        },
        .binary => |binary| {
            try self.expression(binary.lhs);
            try self.expression(binary.rhs);
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

                else => @panic("Wrong Operation"),
            }
        },
        else => return error.NotImplementedYet,
    }
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

// TODO: Fix test to use sema for constants information

// test "Expression Statement" {
//     const input =
//         \\1;
//         \\1.2;
//         \\true;
//         \\false;
//         \\null;
//         \\
//     ;
//
//     const float_bytes = std.mem.toBytes(@as(f64, 1.2));
//
//     // zig fmt: off
//     const expected: []const u8 = &.{
//         OpCode.constants_done.raw(),
//         OpCode.integer.raw(), 1, 0, 0, 0, 0, 0, 0, 0, OpCode.pop.raw(),
//
//         OpCode.float.raw(),
//         float_bytes[0],
//         float_bytes[1],
//         float_bytes[2],
//         float_bytes[3],
//         float_bytes[4],
//         float_bytes[5],
//         float_bytes[6],
//         float_bytes[7],
//         OpCode.pop.raw(),
//
//         OpCode.true.raw(), OpCode.pop.raw(),
//         OpCode.false.raw(), OpCode.pop.raw(),
//         OpCode.null.raw(), OpCode.pop.raw(),
//     };
//     // zig fmt: on
//
//     try testBytecode(input, expected);
// }
//
// test "Print Statement" {
//     const input =
//         \\print 1;
//         \\
//     ;
//
//     const expected: []const u8 = &.{
//         OpCode.constants_done.raw(),
//         OpCode.integer.raw(),
//         1,
//         0,
//         0,
//         0,
//         0,
//         0,
//         0,
//         0,
//         OpCode.print.raw(),
//     };
//
//     try testBytecode(input, expected);
// }

fn testBytecode(input: []const u8, expected_bytecode: []const u8) !void {
    const tokens = try Scanner.scan(testing_allocator, input);
    defer testing_allocator.free(tokens);

    const program = try Parser.createAST(testing_allocator, tokens);
    defer testing_allocator.free(program);
    defer for (program) |node| {
        node.destroy(testing_allocator);
    };

    const bytecode = try compile(testing_allocator, program, &.{});
    defer testing_allocator.free(bytecode);

    try testing.expectEqualSlices(u8, expected_bytecode, bytecode);
}

const Compiler = @This();

const Token = @import("Token.zig");
const OpCode = @import("shared").OpCode;
const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const ast = @import("ast.zig");
const Stmt = ast.Stmt;
const Expr = ast.Expr;
const FlowValue = @import("shared").definitions.FlowValue;

const std = @import("std");
const Allocator = std.mem.Allocator;

const testing = std.testing;
const testing_allocator = testing.allocator;
