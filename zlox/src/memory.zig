const std = @import("std");
const Allocator = std.mem.allocator;
const PageAllocator = std.heap.PageAllocator;

pub const vtable = Allocator.VTable {
    .alloc = alloc,
    .resize = resize,
    .free = free,
};



pub const ManagedAllocator = struct {
    const Self = @This();
    allocator: Allocator,

    pub fn init(allocator: Allocator) Self {
        return Self{ .allocator = allocator };
    }

    
};
