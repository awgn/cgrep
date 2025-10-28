// Zig test example file

const std = @import("std");

// Regular function (not a test)
fn add(a: i32, b: i32) i32 {
    return a + b;
}

// Regular function
fn multiply(a: i32, b: i32) i32 {
    return a * b;
}

// Test block 1
test "addition works correctly" {
    const result = add(2, 3);
    try std.testing.expectEqual(@as(i32, 5), result);
}

// Another regular function
fn subtract(a: i32, b: i32) i32 {
    return a - b;
}

// Test block 2
test "multiplication works" {
    const result = multiply(4, 5);
    try std.testing.expectEqual(@as(i32, 20), result);
    try std.testing.expect(result > 0);
}

// Helper struct (not a test)
const Calculator = struct {
    value: i32,

    pub fn init(initial: i32) Calculator {
        return Calculator{ .value = initial };
    }

    pub fn increment(self: *Calculator) void {
        self.value += 1;
    }
};

// Test block 3
test "Calculator increment" {
    var calc = Calculator.init(10);
    calc.increment();
    try std.testing.expectEqual(@as(i32, 11), calc.value);
}

// Test block 4 with nested braces
test "nested operations" {
    const x = blk: {
        const a = 5;
        const b = 10;
        break :blk a + b;
    };
    try std.testing.expectEqual(@as(i32, 15), x);
}

// Regular function at the end
fn divide(a: i32, b: i32) i32 {
    return @divTrunc(a, b);
}