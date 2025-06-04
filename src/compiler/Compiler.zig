alloc: Allocator,
fir: *const FIR,
byte_code: std.ArrayListUnmanaged(u8),
local_count: usize,
loop_levels: std.ArrayListUnmanaged(LoopLevel),

const LoopLevel = struct {
    loop_start: usize,
    breaks: std.ArrayListUnmanaged(usize),
    inc: ?usize,
};

pub fn init(alloc: Allocator, fir: *const FIR) Compiler {
    return .{
        .alloc = alloc,
        .fir = fir,
        .byte_code = .empty,
        .local_count = 0,
        .loop_levels = .empty,
    };
}

pub fn compile(self: *Compiler) []const u8 {
    if (self.fir.entry == FIR.uninitialized_entry) {
        return &.{};
    }
    self.compileConstants();
    self.compileFunctions();
    self.compileBlock(self.fir.entry);
    defer self.loop_levels.deinit(self.alloc);

    return self.byte_code.toOwnedSlice(self.alloc) catch oom();
}

fn compileFunctions(self: *Compiler) void {
    for (self.fir.globals.items) |global| {
        if (global.type != .function) continue;
        const func = self.fir.functions.items[global.extra_idx];

        self.emitOpcode(.function);
        self.emitByte(@intCast(func.param_count));
        self.emitByte(0x00);
        self.emitByte(0x00);
        const op_idx = self.byte_code.items.len;

        if (func.ret_type == .null) {
            self.emitOpcode(.null);
        }
        self.compileBlock(func.body);

        const line_count = self.byte_code.items.len - op_idx + 1;
        const jump_length: u16 = @intCast(line_count);

        const bytes = std.mem.toBytes(jump_length);
        self.byte_code.items[op_idx - 2] = bytes[0];
        self.byte_code.items[op_idx - 1] = bytes[1];
    }
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
        .@"break" => {
            var breaks = &self.loop_levels.items[self.loop_levels.items.len - 1].breaks;
            breaks.append(self.alloc, self.emitJump(.jump)) catch oom();
        },
        .@"continue" => {
            const loop = self.loop_levels.getLast();
            if (loop.inc) |inc| {
                self.compileExpression(inc);
                self.emitOpcode(.pop);
            }
            self.emitLoop(loop.loop_start);
        },
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

    self.loop_levels.append(self.alloc, .{
        .loop_start = loop_start,
        .breaks = .empty,
        .inc = loop.inc,
    }) catch oom();
    defer std.debug.assert(self.loop_levels.pop() != null);
    defer {
        var breaks = &self.loop_levels.items[self.loop_levels.items.len - 1].breaks;
        defer breaks.deinit(self.alloc);

        for (breaks.items) |break_jump| {
            self.patchJump(break_jump);
        }
    }

    self.compileExpression(loop.condition);
    const exit_jump = self.emitJump(.jump_if_false);
    self.emitOpcode(.pop);
    self.compileBlock(loop.body);
    if (loop.inc) |inc| {
        self.compileExpression(inc);
        self.emitOpcode(.pop);
    }
    self.emitLoop(loop_start);
    self.patchJump(exit_jump);
    self.emitOpcode(.pop);
}

fn compileGlobal(self: *Compiler, var_idx: usize) void {
    const global = self.fir.globals.items[var_idx];

    if (global.type == .function) {
        return;
    } else if (global.expr) |expr| {
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

    if (local.type == .function) {
        @panic("Local functions not yet allowed");
    } else if (local.expr) |expr| {
        self.compileExpression(expr);
    } else {
        self.emitOpcode(.null);
    }

    self.local_count += 1;
}

fn compileExpression(self: *Compiler, expr_idx: usize) void {
    const expr = self.fir.exprs.items[expr_idx];
    switch (expr.op) {
        .builtin_fn => {
            const literal = self.fir.constants.items[expr.operands[0]];
            std.debug.assert(literal == .string);
            self.emitConstant(expr.operands[0]);
            self.emitOpcode(.get_builtin);
        },
        .literal => {
            const literal = self.fir.constants.items[expr.operands[0]];
            std.debug.assert(literal == .int or literal == .float or literal == .string);
            self.emitConstant(expr.operands[0]);
        },
        .true => self.emitOpcode(.true),
        .false => self.emitOpcode(.false),
        .null => self.emitOpcode(.null),
        .equal, .unequal, .less, .less_equal, .greater, .greater_equal, .add, .sub, .div, .mul, .mod, .concat => self.compileBinary(expr),
        .@"and", .@"or" => self.compileLogical(expr),
        .not, .negate => self.compileUnary(expr),
        .global => {
            self.emitOpcode(.get_global);
            self.emitByte(@intCast(expr.operands[0]));
        },
        .local => {
            const local = self.fir.locals.items[expr.operands[0]];
            self.emitOpcode(.get_local);
            self.emitByte(@intCast(local.stack_idx));
        },
        .assign_global => {
            self.compileExpression(expr.operands[0]);
            self.emitOpcode(.set_global);
            self.emitByte(@intCast(expr.operands[1]));
        },
        .assign_local => {
            const local = self.fir.locals.items[expr.operands[1]];
            self.compileExpression(expr.operands[0]);
            self.emitOpcode(.set_local);
            self.emitByte(@intCast(local.stack_idx));
        },
        .call => {
            for (1..expr.operands.len) |idx| {
                self.compileExpression(expr.operands[idx]);
            }
            self.compileExpression(expr.operands[0]);
            self.emitOpcode(.call);
        },
    }
}

fn compileUnary(self: *Compiler, expr: FIR.Node.Expr) void {
    std.debug.assert(expr.operands.len == 1);

    self.compileExpression(expr.operands[0]);

    switch (expr.op) {
        .not => self.emitOpcode(.not),
        .negate => self.emitOpcode(if (expr.type == .int) .negate_i else .negate_f),
        else => unreachable,
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

fn compileLogical(self: *Compiler, expr: FIR.Node.Expr) void {
    std.debug.assert(expr.operands.len == 2);

    self.compileExpression(expr.operands[0]);

    const jump = self.emitJump(if (expr.op == .@"and") .jump_if_false else .jump_if_true);

    self.emitOpcode(.pop);
    self.compileExpression(expr.operands[1]);
    self.patchJump(jump);
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

fn emitConstant(self: *Compiler, idx: usize) void {
    self.emitOpcode(.constant);
    self.emitByte(@intCast(idx));
}

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
