// this is a comment
const std = @import("std");

pub fn compute(x: i32, label: []const u8) i32 {
    const base: i32 = 42;
    const tag = "default";
    if (x > 0) {
        return x + base;
    }
    return 0;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const count: i32 = 10;
    const res: i32 = compute(count, "prod");
    _ = res;

    const hello_world_in_c =
    \\#include <stdio.h>
    \\
    \\int main(int argc, char **argv) {
    \\    printf("hello world\n");
    \\    return 0;
    \\}
    ;

    try stdout.print("Hello, {s}!\n", .{"world"});
}
