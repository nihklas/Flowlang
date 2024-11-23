pub fn Stack(comptime T: type, size: usize, comptime panic_on_overflow: bool) type {
    return struct {
        const Self = @This();
        stack: []T,
        stack_top: usize = 0,
        alloc: Allocator,

        fn StackReturn(comptime RT: type) type {
            return if (panic_on_overflow) RT else (!RT);
        }

        pub fn init(alloc: Allocator) !Self {
            return .{
                .stack = try alloc.alloc(T, size),
                .alloc = alloc,
            };
        }

        pub fn deinit(self: *Self) void {
            self.alloc.free(self.stack);
            self.stack_top = 0;
            self.* = undefined;
        }

        pub fn push(self: *Self, value: T) StackReturn(void) {
            if (self.stack_top == size) {
                if (panic_on_overflow) @panic("Stackoverflow") else return error.Stackoverflow;
            }
            defer self.stack_top += 1;
            self.stack[self.stack_top] = value;
        }

        pub fn pop(self: *Self) StackReturn(T) {
            if (self.stack_top == 0) {
                if (panic_on_overflow) @panic("Stackoverflow") else return error.Stackoverflow;
            }
            self.stack_top -= 1;
            return self.stack[self.stack_top];
        }

        pub fn at(self: *Self, offset: usize) StackReturn(T) {
            const i = self.stack_top - offset;
            if (i > self.stack_top or i < 0) {
                if (panic_on_overflow) @panic("Stackoverflow") else return error.Stackoverflow;
            }
            return self.stack[i];
        }
    };
}

const std = @import("std");
const Allocator = std.mem.Allocator;
