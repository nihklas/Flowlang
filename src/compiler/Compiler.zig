alloc: Allocator,
fir: *const FIR,
byte_code: std.ArrayListUnmanaged(u8),
local_count: usize,

pub fn init(alloc: Allocator, fir: *const FIR) Compiler {
    return .{
        .alloc = alloc,
        .fir = fir,
        .byte_code = .empty,
        .local_count = 0,
    };
}

pub fn compile(self: *Compiler) []const u8 {
    if (self.fir.entry == FIR.uninitialized_entry) {
        return &.{};
    }
    self.compileConstants();
    self.compileFunctions();
    self.compileBlock(self.fir.entry);

    return self.byte_code.toOwnedSlice(self.alloc) catch oom();
}

fn compileFunctions(self: *Compiler) void {
    // TODO:
    self.emitOpcode(.functions_done);
}

fn compileConstants(self: *Compiler) void {
    for (self.fir.constants.items) |constant| {
        switch (constant) {
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
                self.emitMultibyte(constant.int);
            },
            .float => {
                self.emitOpcode(.float);
                self.emitMultibyte(constant.float);
            },
            .bool, .null => {},
            .builtin_fn, .function => unreachable,
        }
    }

    self.emitOpcode(.constants_done);
}

fn compileBlock(self: *Compiler, starting_idx: usize) void {
    var maybe_node_idx: ?usize = starting_idx;
    while (maybe_node_idx) |node_idx| {
        defer maybe_node_idx = self.fir.nodes.items[node_idx].after;
        self.compileStmt(node_idx);
    }
}

fn compileStmt(self: *Compiler, node_idx: usize) void {
    const node = self.fir.nodes.items[node_idx];

    switch (node.kind) {
        .expr => {
            self.compileExpression(node.index);
            self.emitOpcode(.pop);
        },
        .pop => self.emitOpcode(.pop),
        .cond => self.compileCond(node.index),
        .loop => self.compileLoop(node.index),
        .global => self.compileGlobal(node.index),
        .local => self.compileLocal(node.index),
    }
}

fn compileCond(self: *Compiler, cond_idx: usize) void {
    const cond = self.fir.conds.items[cond_idx];
    self.compileExpression(cond.condition);
    const jump_idx = self.emitJump(.jump_if_false);
    self.emitOpcode(.pop);
    self.compileBlock(cond.true);

    const else_jump = self.emitJump(.jump);
    self.patchJump(jump_idx);
    self.emitOpcode(.pop);

    if (cond.false) |false_branch| {
        self.compileBlock(false_branch);
    }

    self.patchJump(else_jump);
}

fn compileLoop(self: *Compiler, loop_idx: usize) void {
    const loop = self.fir.loops.items[loop_idx];

    const loop_start = self.byte_code.items.len;
    self.compileExpression(loop.condition);
    const exit_jump = self.emitJump(.jump_if_false);
    self.emitOpcode(.pop);
    self.compileBlock(loop.body);
    self.emitLoop(loop_start);
    self.patchJump(exit_jump);
    self.emitOpcode(.pop);
}

fn compileGlobal(self: *Compiler, var_idx: usize) void {
    const global = self.fir.globals.items[var_idx];

    if (global.expr) |expr| {
        self.compileExpression(expr);
    } else {
        self.emitOpcode(.null);
    }

    self.emitOpcode(.set_global);
    self.emitByte(@intCast(var_idx));

    self.emitOpcode(.pop);
}

fn compileLocal(self: *Compiler, var_idx: usize) void {
    const local = self.fir.locals.items[var_idx];

    if (local.expr) |expr| {
        self.compileExpression(expr);
    } else {
        self.emitOpcode(.null);
    }

    self.local_count += 1;
}

fn compileExpression(self: *Compiler, expr_idx: usize) void {
    const expr = self.fir.exprs.items[expr_idx];
    switch (expr.op) {
        .literal => {
            const literal = self.fir.constants.items[expr.operands[0]];
            switch (literal) {
                .int, .float, .string => self.emitConstant(expr.operands[0]),
                .bool, .null, .function, .builtin_fn => unreachable,
            }
        },
        .true => self.emitOpcode(.true),
        .false => self.emitOpcode(.false),
        .null => self.emitOpcode(.null),
        .equal, .unequal, .less, .less_equal, .greater, .greater_equal, .add, .sub, .div, .mul, .mod, .concat => self.compileBinary(expr),
        .global => {
            self.emitOpcode(.get_global);
            self.emitByte(@intCast(expr.operands[0]));
        },
        .local => {
            const local = self.fir.locals.items[expr.operands[0]];
            self.emitOpcode(.get_local);
            self.emitByte(@intCast(local.stack_idx));
        },
    }
}

fn compileBinary(self: *Compiler, expr: FIR.Node.Expr) void {
    std.debug.assert(expr.operands.len == 2);

    self.compileExpression(expr.operands[0]);
    self.compileExpression(expr.operands[1]);

    switch (expr.op) {
        .equal => self.emitOpcode(.equal),
        .unequal => self.emitOpcode(.unequal),
        .less => self.emitOpcode(.lower),
        .less_equal => self.emitOpcode(.lower_equal),
        .greater => self.emitOpcode(.greater),
        .greater_equal => self.emitOpcode(.greater_equal),
        .concat => self.emitOpcode(.concat),
        .add => self.emitOpcode(if (expr.type == .int) .add_i else .add_f),
        .sub => self.emitOpcode(if (expr.type == .int) .sub_i else .sub_f),
        .div => self.emitOpcode(if (expr.type == .int) .div_i else .div_f),
        .mul => self.emitOpcode(if (expr.type == .int) .mul_i else .mul_f),
        .mod => self.emitOpcode(if (expr.type == .int) .mod_i else .mod_f),
        else => unreachable,
    }
}

//
// fn statement(self: *Compiler, stmt: *Stmt) void {
//     switch (stmt.*) {
//         .expr => self.expressionStatement(stmt),
//         .variable => self.varDeclaration(stmt),
//         .@"if" => self.ifStatement(stmt),
//         .block => self.blockStatement(stmt),
//         .loop => self.loopStatement(stmt),
//         .@"break" => self.breakStatement(),
//         .@"continue" => self.continueStatement(),
//         .@"return" => self.returnStatement(stmt),
//         .function => {}, // This is not an error state, global functions are already handled
//         else => panic("{s} is not yet implemented", .{@tagName(stmt.*)}),
//     }
// }
//
// fn expressionStatement(self: *Compiler, stmt: *Stmt) void {
//     self.expression(stmt.expr.expr);
//     self.emitOpcode(.pop);
// }
//
// fn blockStatement(self: *Compiler, stmt: *Stmt) void {
//     self.traverse(stmt.block.stmts);
//     for (0..stmt.block.local_count) |_| {
//         self.emitOpcode(.pop);
//     }
// }
//
// fn returnStatement(self: *Compiler, stmt: *Stmt) void {
//     if (stmt.@"return".value) |value| {
//         self.expression(value);
//     } else {
//         self.emitOpcode(.null);
//     }
//     self.emitOpcode(.@"return");
// }
//
// fn varDeclaration(self: *Compiler, stmt: *Stmt) void {
//     const variable = stmt.variable;
//
//     if (variable.value) |value| {
//         self.expression(value);
//     } else {
//         self.emitOpcode(.null);
//     }
//
//     if (variable.global) {
//         self.emitConstant(.{ .string = variable.name.lexeme });
//         self.emitOpcode(.create_global);
//     }
// }
//
// fn loopStatement(self: *Compiler, stmt: *Stmt) void {
//     const loop_start = self.byte_code.items.len;
//
//     var break_statements: std.ArrayList(usize) = .init(self.alloc);
//     defer break_statements.deinit();
//
//     const loop_level: LoopLevel = .{
//         .loop = stmt,
//         .break_statements = &break_statements,
//         .loop_start = loop_start,
//     };
//
//     self.loop_levels.push(loop_level);
//     defer _ = self.loop_levels.pop();
//
//     self.expression(stmt.loop.condition);
//
//     const exit_jump = self.emitJump(.jump_if_false);
//     self.emitOpcode(.pop);
//
//     for (stmt.loop.body) |body| {
//         self.statement(body);
//     }
//
//     self.emitLoop(loop_start);
//     self.patchJump(exit_jump);
//     self.emitOpcode(.pop);
//
//     for (loop_level.break_statements.items) |break_stmt| {
//         self.patchJump(break_stmt);
//     }
// }
//
// fn breakStatement(self: *Compiler) void {
//     const break_stmt = self.emitJump(.jump);
//     var stmts = self.loop_levels.at(0);
//     stmts.break_statements.append(break_stmt) catch oom();
// }
//
// fn continueStatement(self: *Compiler) void {
//     const loop_level = self.loop_levels.at(0);
//
//     if (loop_level.loop.loop.inc) |inc| {
//         self.statement(inc);
//     }
//
//     self.emitLoop(loop_level.loop_start);
// }
//
// fn ifStatement(self: *Compiler, stmt: *Stmt) void {
//     self.expression(stmt.@"if".condition);
//     const jump_idx = self.emitJump(.jump_if_false);
//
//     self.emitOpcode(.pop);
//     self.statement(stmt.@"if".true_branch);
//
//     const else_jump = self.emitJump(.jump);
//
//     self.patchJump(jump_idx);
//
//     self.emitOpcode(.pop);
//     if (stmt.@"if".false_branch) |false_branch| {
//         self.statement(false_branch);
//     }
//
//     self.patchJump(else_jump);
// }
//
// fn expression(self: *Compiler, expr: *Expr) void {
//     switch (expr.*) {
//         .literal => self.literalExpression(expr),
//         .unary => self.unaryExpression(expr),
//         .binary => self.binaryExpression(expr),
//         .logical => self.logicalExpression(expr),
//         .variable => self.variableExpression(expr),
//         .assignment => self.assignmentExpression(expr),
//         .grouping => self.expression(expr.grouping.expr),
//         .call => self.callExpression(expr),
//         else => panic("{s} is not yet implemented", .{@tagName(expr.*)}),
//     }
// }
//
// fn literalExpression(self: *Compiler, expr: *Expr) void {
//     switch (expr.literal.value) {
//         .int => |int| self.emitConstant(.{ .int = int }),
//         .float => |float| self.emitConstant(.{ .float = float }),
//         .bool => |boolean| self.emitOpcode(if (boolean) .true else .false),
//         .null => self.emitOpcode(.null),
//         .string => |string| self.emitConstant(.{ .string = string }),
//         .function, .builtin_fn => unreachable,
//     }
// }
//
// fn unaryExpression(self: *Compiler, expr: *Expr) void {
//     self.expression(expr.unary.expr);
//     switch (expr.unary.op.type) {
//         .@"!" => self.emitOpcode(.not),
//         .@"-" => self.emitOpcode(.negate),
//         else => unreachable,
//     }
// }
//
// fn binaryExpression(self: *Compiler, expr: *Expr) void {
//     const binary = expr.binary;
//     self.expression(binary.lhs);
//     self.expression(binary.rhs);
//     switch (binary.op.type) {
//         .@"-", .@"-=", .@"*", .@"*=", .@"/", .@"/=", .@"%", .@"%=", .@"+", .@"+=" => self.emitArithmetic(expr),
//
//         .@"==" => self.emitOpcode(.equal),
//         .@"!=" => self.emitOpcode(.unequal),
//
//         .@"<" => self.emitOpcode(.lower),
//         .@"<=" => self.emitOpcode(.lower_equal),
//         .@">" => self.emitOpcode(.greater),
//         .@">=" => self.emitOpcode(.greater_equal),
//
//         .@".", .@".=" => self.emitOpcode(.concat),
//
//         else => unreachable,
//     }
// }
//
// fn logicalExpression(self: *Compiler, expr: *Expr) void {
//     self.expression(expr.logical.lhs);
//
//     const jump_idx = self.emitJump(if (expr.logical.op.type == .@"and") .jump_if_false else .jump_if_true);
//
//     self.emitOpcode(.pop);
//     self.expression(expr.logical.rhs);
//
//     self.patchJump(jump_idx);
// }
//
// fn variableExpression(self: *Compiler, expr: *Expr) void {
//     if (expr.variable.global) {
//         self.emitConstant(.{ .string = expr.variable.name.lexeme });
//         self.emitOpcode(.get_global);
//     } else {
//         self.emitOpcode(.get_local);
//         self.emitByte(expr.variable.local_idx);
//     }
// }
//
// fn assignmentExpression(self: *Compiler, expr: *Expr) void {
//     self.expression(expr.assignment.value);
//     if (expr.assignment.global) {
//         self.emitConstant(.{ .string = expr.assignment.name.lexeme });
//         self.emitOpcode(.set_global);
//     } else {
//         self.emitOpcode(.set_local);
//         self.emitByte(expr.assignment.local_idx);
//     }
// }
//
// fn callExpression(self: *Compiler, expr: *Expr) void {
//     for (expr.call.args) |arg| {
//         self.expression(arg);
//     }
//     self.expression(expr.call.expr);
//     self.emitOpcode(.call);
// }

// fn emitArithmetic(self: *Compiler, expr: *Expr) void {
//     const binary = expr.binary;
//
//     switch (binary.op.type) {
//         .@"+", .@"+=" => self.emitOpcode(if (binary.type == .int) .add_i else .add_f),
//         .@"-", .@"-=" => self.emitOpcode(if (binary.type == .int) .sub_i else .sub_f),
//         .@"*", .@"*=" => self.emitOpcode(if (binary.type == .int) .mul_i else .mul_f),
//         .@"/", .@"/=" => self.emitOpcode(if (binary.type == .int) .div_i else .div_f),
//         .@"%", .@"%=" => self.emitOpcode(if (binary.type == .int) .mod_i else .mod_f),
//         else => unreachable,
//     }
// }
//

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

fn emitConstant(self: *Compiler, idx: usize) void {
    self.emitOpcode(.constant);
    self.emitByte(@intCast(idx));
}

// fn resolveConstant(self: *Compiler, value: FlowValue) u8 {
//     return for (self.constants.items, 0..) |c, i| {
//         if (c.equals(value)) break @intCast(i);
//     } else {
//         self.constants.append(self.alloc, value) catch oom();
//         if (self.constants.items.len > std.math.maxInt(u8)) {
//             @panic("Too many constants");
//         }
//         return @intCast(self.constants.items.len - 1);
//     };
// }

fn emitMultibyte(self: *Compiler, value: anytype) void {
    const bytes = std.mem.toBytes(value);
    for (bytes) |byte| {
        self.emitByte(byte);
    }
}

fn emitOpcode(self: *Compiler, op: OpCode) void {
    self.emitByte(op.raw());
}

fn emitByte(self: *Compiler, byte: u8) void {
    self.byte_code.append(self.alloc, byte) catch oom();
}

const Compiler = @This();

const OpCode = @import("shared").OpCode;
const FIR = @import("ir/FIR.zig");
const FlowValue = @import("shared").definitions.FlowValue;
const Stack = @import("shared").StaticStack;
const oom = @import("shared").oom;
const panic = std.debug.panic;

const std = @import("std");
const Allocator = std.mem.Allocator;
