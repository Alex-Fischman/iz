const builtin = @import("builtin");
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

        fn data(self: *const Self) []T {
            return self.ptr[0..self.len];
        }

        fn allocation(self: *const Self) []T {
            return self.ptr[0..self.mem];
        }

        const allocator = if (builtin.is_test) std.testing.allocator else std.heap.page_allocator;
        const init_mem = 16;

        fn init() !Self {
            const slice = try allocator.alloc(T, init_mem);
            return Self{
                .ptr = slice.ptr,
                .mem = init_mem,
                .len = 0,
            };
        }

        fn resize(self: *Self, size: usize) !void {
            if (size <= self.mem) return;
            const slice = try allocator.realloc(self.allocation(), size);
            self.ptr = slice.ptr;
            self.mem = size;
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
    name: []const u8,
    text: []const u8,
};

const Span = struct {
    source: *const Source,
    slice: []const u8,
};

fn format_span(buffer: *Buffer(u8), span: Span) !void {
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

const Tree = struct {
    // each node maintains a doubly linked list of its children
    // invariant: each node except the root is always pointed at twice
    const Node = struct {
        span: Span,
        // siblings
        next: ?usize = null,
        prev: ?usize = null,
        // children
        head: ?usize = null,
        last: ?usize = null,
    };

    nodes: Buffer(Node),

    const root = 0;

    fn init(source: *const Source) !Tree {
        var nodes = try Buffer(Node).init();
        try nodes.push(Node{
            .span = Span{
                .source = source,
                .slice = source.text.ptr[0..0],
            },
        });
        return Tree{
            .nodes = nodes,
        };
    }

    fn deinit(self: *Tree) void {
        self.nodes.deinit();
    }

    fn push_child(self: *Tree, parent: usize, span: Span) !void {
        const i = self.nodes.len;
        try self.nodes.push(Node{
            .span = span,
            .prev = self.nodes.data()[parent].last,
        });

        if (self.nodes.data()[parent].last) |last| {
            self.nodes.data()[last].next = i;
        } else {
            self.nodes.data()[parent].head = i;
        }

        self.nodes.data()[parent].last = i;
    }
};

fn format_node(buffer: *Buffer(u8), tree: Tree, node: usize, depth: usize) !void {
    const n = tree.nodes.data()[node];

    for (0..depth) |_| try buffer.push('\t');
    try format_span(buffer, n.span);
    try buffer.push('\n');

    if (n.head) |head| try format_node(buffer, tree, head, depth + 1);
    if (n.next) |next| try format_node(buffer, tree, next, depth);
}

const Token = enum {
    Whitespace,
    Bracket,
    Comment,
    Identifier,
};

fn token(char: u8) Token {
    return switch (char) {
        ' ', '\n', '\t' => Token.Whitespace,
        '(', ')', '{', '}', '[', ']' => Token.Bracket,
        '#' => Token.Comment,
        else => Token.Identifier,
    };
}

fn compile(source: Source) !void {
    var tree = try Tree.init(&source);
    defer tree.deinit();

    var i: usize = 0;
    while (i < source.text.len) {
        var j = i + 1;
        switch (token(source.text.ptr[i])) {
            Token.Whitespace => {},
            Token.Bracket => {
                try tree.push_child(Tree.root, Span{
                    .source = &source,
                    .slice = source.text.ptr[i..j],
                });
            },
            Token.Comment => {
                while (source.text.ptr[j] != '\n') {
                    j += 1;
                }
            },
            Token.Identifier => {
                while (token(source.text.ptr[j]) == Token.Identifier) {
                    j += 1;
                }
                try tree.push_child(Tree.root, Span{
                    .source = &source,
                    .slice = source.text.ptr[i..j],
                });
            },
        }
        i = j;
    }

    var out = try Buffer(u8).init();
    defer out.deinit();
    try format_node(&out, tree, Tree.root, 0);
    print(out.data());
}

/// interface
fn compile_file(name: []const u8) !void {
    const file = try std.fs.cwd().openFile(name, .{});
    const stat = try file.stat();

    var text = try Buffer(u8).init();
    defer text.deinit();
    try text.reserve(stat.size);
    _ = try file.readAll(text.data());

    file.close();

    try compile(Source{
        .name = name,
        .text = text.data(),
    });
}

pub fn main() !void {
    const args = std.os.argv;
    if (args.len != 2) {
        print("usage: ./iz [file.iz]\n");
        std.process.exit(1);
    }
    try compile_file(std.mem.span(args[1]));
}

test "scratch" {
    try compile_file("scratch.iz");
}
