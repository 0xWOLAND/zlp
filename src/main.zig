const std = @import("std");
const root = @import("root.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Get command line arguments
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    // Skip program name
    _ = args.skip();

    // Get the example file path
    const example_path = args.next() orelse {
        std.debug.print("Usage: {s} <example_file>\n", .{args.next().?});
        std.process.exit(1);
    };

    // Read the example file
    const file = try std.fs.cwd().openFile(example_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const file_buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(file_buffer);

    const bytes_read = try file.read(file_buffer);
    if (bytes_read != file_size) {
        std.debug.print("Error: Could not read entire file\n", .{});
        std.process.exit(1);
    }

    // Parse and evaluate the lambda expression
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const expr = try root.Expr.init(arena.allocator(), file_buffer);
    const reduced = try expr.reduce(&arena);

    std.debug.print("Original expression: {s}\n", .{file_buffer});
    std.debug.print("Reduced expression: ", .{});
    try printExpr(reduced);
    std.debug.print("\n", .{});
}

fn printExpr(expr: *root.Expr) !void {
    switch (expr.*) {
        .Variable => |i| {
            std.debug.print("{d}", .{i});
        },
        .Application => |app| {
            std.debug.print("(", .{});
            try printExpr(app.lhs);
            std.debug.print(" ", .{});
            try printExpr(app.rhs);
            std.debug.print(")", .{});
        },
        .Lambda => |lam| {
            std.debug.print("\\{s} . ", .{lam.name});
            try printExpr(lam.body);
        },
    }
}
