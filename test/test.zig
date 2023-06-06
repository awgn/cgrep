// this is a comment
const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
   
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
