const std = @import("std");
const process = std.process;
const mem = std.mem;
const heap = std.heap;

const Io = @import("io.zig");
const memory = @import("memory.zig");

pub fn main() !void {
    var output = Io.stderr;
    const stderr = output.writer();
    defer output.flush();

    var vm = Vm.init();
    defer vm.deinit();

    const args = try process.argsAlloc(heap.page_allocator);
    if (args.len == 1) {
        try repl(vm);
    } else if (args.len == 2) {
        try runFile(vm, args[1]);
    } else {
        stderr.print("Usage: zlox [path]\n", .{});
        process.exit(64);
    }
}

fn repl(vm: Vm) !void {
    var output = Io.stdout;
    const stdout = output.writer();
    var input = Io.stdin;
    const stdin = input.reader();
    var buf: [1024]u8 = undefined;

    while (true) {
        try stdout.print("> ");
        try output.flush();

        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');
        if (line) |l| {
            try interpret(vm, line);
        } else {
            stdout.print("\n");
            output.flush();
            break;
        }
    }
}
