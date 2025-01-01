const Io = @This();

const std = @import("std");
const io = std.io;
const fs = std.fs;

pub const stdout_file = io.getStdOut();
pub const stderr_file = io.getStdErr();
pub const stdin_file = io.getStdIn();
pub var stdout = io.bufferedWriter(stdout_file.writer());
pub var stderr = io.bufferedWriter(stderr_file.writer());
pub var stdin = io.bufferedReader(stdin_file.reader());
