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

    try buffer.extend(span.source.name);
    try buffer.push(':');
    try format_usize(buffer, row);
    try buffer.push(':');
    try format_usize(buffer, col);
    try buffer.extend(": ");
    try buffer.extend(span.slice);
}

const Tree = struct {
    // each node maintains a doubly linked list of its children
    // invariant: each node except the root is always pointed at twice
    const Node = struct {
        span: Span,
        // siblings
        next: ?*Node = null,
        prev: ?*Node = null,
        // children
        head: ?*Node = null,
        last: ?*Node = null,
    };

    nodes: Buffer(Node),

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

    fn root(self: Tree) *Node {
        return &self.nodes.data()[0];
    }

    fn push_child(self: *Tree, parent: *Node, span: Span) !void {
        try self.nodes.push(Node{
            .span = span,
            .prev = parent.last,
        });
        const new = &self.nodes.data()[self.nodes.len - 1];

        if (parent.last) |last| {
            last.next = new;
        } else {
            parent.head = new;
        }

        parent.last = new;
    }
};

fn format_node(buffer: *Buffer(u8), node: Tree.Node, depth: usize) !void {
    for (0..depth) |_| try buffer.push('\t');
    try format_span(buffer, node.span);
    try buffer.push('\n');

    if (node.head) |head| try format_node(buffer, head.*, depth + 1);
    if (node.next) |next| try format_node(buffer, next.*, depth);
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

const ErrorMessage = struct {
    span: Span,
    message: []const u8,
};

fn format_err(buffer: *Buffer(u8), error_message: ErrorMessage) !void {
    try buffer.extend("error: ");
    try buffer.extend(error_message.message);
    try buffer.extend(" at ");
    try format_span(buffer, error_message.span);
    try buffer.push('\n');
}

const Tag = enum { Ok, Err };

fn Result(comptime T: type) type {
    return union(Tag) {
        Ok: T,
        Err: ErrorMessage,
    };
}

fn Ok(payload: anytype) Result(@TypeOf(payload)) {
    return Result(@TypeOf(payload)){ .Ok = payload };
}

fn Err(comptime T: type, span: Span, message: []const u8) Result(T) {
    return Result(T){ .Err = ErrorMessage{
        .span = span,
        .message = message,
    } };
}

fn parse_brackets_internal(n: ?*Tree.Node, parent: *Tree.Node) !Result(?*Tree.Node) {
    var node = n;
    while (node) |start| {
        if (std.mem.eql(u8, start.span.slice, ")")) return Ok(node);

        if (std.mem.eql(u8, start.span.slice, "(")) {
            const result = try parse_brackets_internal(start.next, parent);
            const end = switch (result) {
                Tag.Err => return result,
                Tag.Ok => |payload| payload orelse
                    return Err(?*Tree.Node, start.span, "missing end bracket"),
            };

            if (!std.mem.eql(u8, end.span.slice, ")")) return Err(?*Tree.Node, end.span, "bracket mismatch");

            start.span.slice = start.span.slice.ptr[0 .. 1 + @intFromPtr(end.span.slice.ptr) - @intFromPtr(start.span.slice.ptr)];

            start.next = end.next;
            if (start.next) |next| next.prev = start;

            if (start.next != end) {
                const head = start.next.?;
                const last = end.prev.?;

                start.head = head;
                start.last = last;

                last.next = null;
                head.prev = null;
            }

            if (parent.last == end) parent.last = end.prev;
        }

        node = start.next;
    }
    return Ok(node);
}

fn parse_brackets(tree: *Tree) !Result(void) {
    const result = try parse_brackets_internal(tree.root().head, tree.root());
    switch (result) {
        Tag.Err => |payload| return Err(void, payload.span, payload.message),
        Tag.Ok => |payload| {
            if (payload) |node| return Err(void, node.span, "extra end bracket");
            return Ok({});
        },
    }
}

fn compile(source: Source) !Result(void) {
    var tree = try Tree.init(&source);
    defer tree.deinit();

    var i: usize = 0;
    while (i < source.text.len) {
        var j = i + 1;
        switch (token(source.text.ptr[i])) {
            Token.Whitespace => {},
            Token.Bracket => {
                try tree.push_child(tree.root(), Span{
                    .source = &source,
                    .slice = source.text.ptr[i..j],
                });
            },
            Token.Comment => {
                while (source.text.ptr[j] != '\n') j += 1;
            },
            Token.Identifier => {
                while (token(source.text.ptr[j]) == Token.Identifier) j += 1;
                try tree.push_child(tree.root(), Span{
                    .source = &source,
                    .slice = source.text.ptr[i..j],
                });
            },
        }
        i = j;
    }

    const parse_result = try parse_brackets(&tree);
    switch (parse_result) {
        Tag.Ok => {},
        Tag.Err => return parse_result,
    }

    var out = try Buffer(u8).init();
    defer out.deinit();
    try format_node(&out, tree.root().*, 0);
    print(out.data());

    return Ok({});
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

    const result = try compile(Source{
        .name = name,
        .text = text.data(),
    });
    switch (result) {
        Tag.Ok => {},
        Tag.Err => |payload| {
            var out = try Buffer(u8).init();
            defer out.deinit();
            try format_err(&out, payload);
            try format_span(&out, payload.span);
            print(out.data());
        },
    }
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
