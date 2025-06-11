pub fn Stack(comptime T: type) type {
    return struct {
        const Self = @This();
        stack: []T,
        stack_top: usize = 0,

        /// Assumes `stack` to be empty
        pub fn init(stack: []T) Self {
            return .{
                .stack = stack,
            };
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

        pub fn push(self: *Self, value: T) void {
            if (self.stack_top == self.stack.len) panic("Stackoverflow", .{});
            defer self.stack_top += 1;
            self.stack[self.stack_top] = value;
        }

        pub fn pop(self: *Self) T {
            if (self.stack_top == 0) panic("Stackoverflow", .{});
            self.stack_top -= 1;
            return self.stack[self.stack_top];
        }

        /// 0 is at the top, 1 is the second from the top, and so on...
        pub fn at(self: *Self, offset: usize) T {
            const i = self.stack_top - 1 - offset;
            if (i > self.stack_top or i < 0) panic("Stackoverflow", .{});
            return self.stack[i];
        }

        pub fn setAt(self: *Self, offset: usize, value: T) void {
            const i = self.stack_top - 1 - offset;
            if (i > self.stack_top or i < 0) panic("Stackoverflow", .{});
            self.stack[i] = value;
        }
    };
}

const std = @import("std");
const panic = std.debug.panic;
const oom = @import("root.zig").oom;
