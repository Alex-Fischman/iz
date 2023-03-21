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
    data: std.StringHashMap([]u8),
    children: std.ArrayList(Tree),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) Tree {
        return Tree{
            .data = std.StringHashMap([]u8).init(allocator),
            .children = std.ArrayList(Tree).init(allocator),
            .allocator = allocator,
        };
    }

    fn deinit(self: *Tree) void {
        var items = self.data.valueIterator();
        while (items.next()) |item| self.allocator.free(item.*);
        self.data.deinit();
        var i: usize = 0;
        while (i < self.children.items.len) : (i += 1) self.children.items[i].deinit();
        self.children.deinit();
    }

    fn put(self: *Tree, comptime T: type, x: T) void {
        const slice: []u8 = self.allocator.alloc(u8, @sizeOf(T)) catch unreachable;
        std.mem.copy(u8, slice, std.mem.asBytes(&x));
        self.data.put(@typeName(T), slice) catch unreachable;
    }

    fn get(self: Tree, comptime T: type) ?*T {
        return if (self.data.get(@typeName(T))) |bytes| @alignCast(@alignOf(T), std.mem.bytesAsValue(T, bytes[0..@sizeOf(T)])) else null;
    }
};

fn printTree(tree: Tree, indent: usize) void {
    var i: usize = 0;
    while (i < indent) : (i += 1) std.debug.print("\t", .{});
    if (tree.get(Token)) |token| std.debug.print("{s}\n", .{token.slice});
    for (tree.children.items) |child| printTree(child, indent + 1);
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

    var tree = Tree.init(allocator);
    defer tree.deinit();

    const utf8View = std.unicode.Utf8View.init(text) catch panic("{s} was not UTF8", .{name});
    var utf8Iterator = utf8View.iterator();
    while (utf8Iterator.nextCodepointSlice()) |slice| {
        var child = Tree.init(allocator);
        child.put(Token, Token{ .slice = slice, .source = &source });
        tree.children.append(child) catch unreachable;
    }

    remove_comments(&tree);

    printTree(tree, 0);
}

fn remove_comments(tree: *Tree) void {
    var i: usize = 0;
    while (i < tree.children.items.len) : (i +%= 1) {
        if (std.mem.eql(u8, tree.children.items[i].get(Token).?.slice, "#")) {
            var j = i;
            while (j < tree.children.items.len and !std.mem.eql(u8, tree.children.items[j].get(Token).?.slice, "\n")) j += 1;

            var k = i;
            while (k <= j) : (k += 1) tree.children.items[k].deinit();

            tree.children.replaceRange(i, j - i + 1, &[0]Tree{}) catch unreachable;
            i -%= 1;
        }
    }
}
