const std = @import("std");

fn panic(comptime format: []const u8, args: anytype) noreturn {
    std.debug.panic(format ++ "\n", args);
}

const Source = struct { name: []const u8, text: []const u8 };
const Token = struct {
    slice: []const u8,
    source: *const Source,
};

const Tree = struct {
    token: Token,
    children: std.ArrayList(Tree),

    fn init(allocator: std.mem.Allocator, token: Token) Tree {
        return Tree{
            .token = token,
            .children = std.ArrayList(Tree).init(allocator),
        };
    }

    fn deinit(self: *Tree) void {
        self.children.deinit();
    }
};

fn printTree(tree: *const Tree, indent: usize) void {
    var i: usize = 0;
    while (i < indent) : (i += 1) std.debug.print("\t", .{});
    std.debug.print("{s}\n", .{tree.token.slice});
    for (tree.children.items) |child| printTree(&child, indent + 1);
}

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    defer if (gpa.deinit()) {
        panic("leak detected", .{});
    };

    var args = std.process.args();
    _ = args.skip(); // executable name
    const name = args.next() orelse panic("pass a .iz file", .{});
    const file = std.fs.cwd().openFile(name, .{}) catch panic("could not open {s}", .{name});
    const stat = file.stat() catch panic("could not get size of {s}", .{name});
    const text = allocator.alloc(u8, stat.size) catch panic("could not allocate a buffer for {s}", .{name});
    defer allocator.free(text);
    const written = file.readAll(text) catch panic("could not read from {s}", .{name});
    std.debug.assert(stat.size == written);
    const source = Source{ .name = name, .text = text };

    var tree = Tree.init(allocator, Token{ .slice = text[0..0], .source = &source });
    defer tree.deinit();

    const utf8View = std.unicode.Utf8View.init(text) catch panic("{s} was not UTF8", .{name});
    var utf8Iterator = utf8View.iterator();
    while (utf8Iterator.nextCodepointSlice()) |slice| {
        const token = Token{ .slice = slice, .source = &source };
        tree.children.append(Tree.init(allocator, token)) catch unreachable;
    }

    remove_comments(&tree);

    printTree(&tree, 0);
}

fn remove_comments(tree: *Tree) void {
    var i: usize = 0;
    while (i < tree.children.items.len) : (i +%= 1) {
        if (std.mem.eql(u8, tree.children.items[i].token.slice, "#")) {
            var j = i;
            while (j < tree.children.items.len and !std.mem.eql(u8, tree.children.items[j].token.slice, "\n")) j += 1;
            tree.children.replaceRange(i, j - i + 1, &[0]Tree{}) catch unreachable;
            i -%= 1;
        }
    }
}
