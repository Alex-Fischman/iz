const std = @import("std");

/// standard
fn print(string: []const u8) void {
    std.debug.print("{s}", .{string});
}

fn Buffer(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: [*]T,
        mem: usize,
        len: usize,

        const allocator = std.heap.page_allocator;
        const init_mem = 16;

        fn init() !Self {
            const slice = try allocator.alloc(T, init_mem * @sizeOf(T));
            return Self{
                .ptr = slice.ptr,
                .mem = slice.len / @sizeOf(T),
                .len = 0,
            };
        }

        fn resize(self: *Self, size: usize) !void {
            if (size <= self.mem) return;
            const slice = try allocator.realloc(self.allocation(), size * @sizeOf(T));
            self.ptr = slice.ptr;
            self.mem = slice.len / @sizeOf(T);
        }

        fn data(self: *const Self) []T {
            return self.ptr[0..self.len];
        }

        fn allocation(self: *const Self) []T {
            return self.ptr[0..self.mem];
        }

        fn deinit(self: *Self) void {
            allocator.free(self.allocation());
        }

        fn push(self: *Self, x: T) !void {
            if (self.len == self.mem) try self.resize(self.mem * 2);
            self.ptr[self.len] = x;
            self.len += 1;
        }

        fn extend(self: *Self, xs: []const T) !void {
            for (xs) |x| try self.push(x);
        }

        fn reserve(self: *Self, size: usize) !void {
            try self.resize(size);
            self.len = size;
        }
    };
}

fn format_usize(buffer: *Buffer(u8), x: usize) !void {
    if (x >= 10) try format_usize(buffer, x / 10);
    try buffer.push(@truncate('0' + (x % 10)));
}

fn format_string_escaped(buffer: *Buffer(u8), xs: []const u8) !void {
    for (xs) |x| switch (x) {
        '\n' => try buffer.extend("\\n"),
        '\t' => try buffer.extend("\\t"),
        '\"' => try buffer.extend("\\\""),
        else => try buffer.push(x),
    };
}

/// compiler
const Source = struct {
    name: []u8,
    text: []u8,
};

const Span = struct {
    source: *const Source,
    slice: []const u8,
};

fn format_error(buffer: *Buffer(u8), span: Span) !void {
    var row: usize = 1;
    var col: usize = 1;
    var ptr = span.source.text.ptr;
    while (ptr != span.slice.ptr) : (ptr += 1) {
        if (ptr[0] == '\n') {
            col = 1;
            row += 1;
        } else {
            col += 1;
        }
    }

    try buffer.push('"');
    try format_string_escaped(buffer, span.slice);
    try buffer.push('"');
    try buffer.extend(" at ");
    try buffer.extend(span.source.name);
    try buffer.push(':');
    try format_usize(buffer, row);
    try buffer.push(':');
    try format_usize(buffer, col);
}

pub fn main() !void {
    const args = std.os.argv;
    if (args.len != 2) {
        print("usage: ./iz [file.iz]\n");
        std.process.exit(1);
    }
    const name = std.mem.span(args[1]);

    const file = try std.fs.cwd().openFile(name, .{});
    defer file.close();
    const stat = try file.stat();

    var text = try Buffer(u8).init();
    try text.reserve(stat.size);
    _ = try file.readAll(text.data());

    var source = Source{
        .name = name,
        .text = text.data(),
    };

    var spans = try Buffer(Span).init();
    for (0..text.len) |i| {
        try spans.push(Span{
            .source = &source,
            .slice = text.ptr[i .. i + 1],
        });
    }

    var out = try Buffer(u8).init();
    for (spans.data()) |span| {
        try format_error(&out, span);
        try out.push('\n');
    }
    print(out.data());
}
