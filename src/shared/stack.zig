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

        pub fn dump(self: *Self) void {
            var stack_counter: usize = 0;
            while (stack_counter < self.stack_top) : (stack_counter += 1) {
                std.debug.print("[ {} ] ", .{self.stack[stack_counter]});
            }
            if (stack_counter == 0) {
                std.debug.print("[ ]", .{});
            }
            std.debug.print("\n", .{});
        }

        pub fn push(self: *Self, value: T) StackReturn(void) {
            if (self.stack_top == size) {
                if (panic_on_overflow) panic("Stackoverflow", .{}) else return error.Stackoverflow;
            }
            defer self.stack_top += 1;
            self.stack[self.stack_top] = value;
        }

        pub fn pop(self: *Self) StackReturn(T) {
            if (self.stack_top == 0) {
                if (panic_on_overflow) panic("Stackoverflow", .{}) else return error.Stackoverflow;
            }
            self.stack_top -= 1;
            return self.stack[self.stack_top];
        }

        /// 0 is at the top, 1 is the second from the top, and so on...
        pub fn at(self: *Self, offset: usize) StackReturn(T) {
            const i = self.stack_top - 1 - offset;
            if (i > self.stack_top or i < 0) {
                if (panic_on_overflow) panic("Stackoverflow", .{}) else return error.Stackoverflow;
            }
            return self.stack[i];
        }

        pub fn setAt(self: *Self, offset: usize, value: T) StackReturn(void) {
            const i = self.stack_top - 1 - offset;
            if (i > self.stack_top or i < 0) {
                if (panic_on_overflow) panic("Stackoverflow", .{}) else return error.Stackoverflow;
            }
            self.stack[i] = value;
        }
    };
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
