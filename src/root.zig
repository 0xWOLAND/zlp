const std = @import("std");

pub const Expr = union(enum) {
    Variable: usize,
    Application: struct {
        lhs: *Expr,
        rhs: *Expr,
    },
    Lambda: struct {
        name: []const u8,
        body: *Expr,
    },

    pub fn init(allocator: std.mem.Allocator, input: []const u8) !*Expr {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();
        var ctx = std.ArrayList([]const u8).init(allocator);
        defer ctx.deinit();
        var parser = Parser{ .arena = &arena, .input = input, .pos = 0, .context = &ctx };
        return try parser.parseTerm();
    }

    pub fn deinit(self: *Expr, arena: *std.heap.ArenaAllocator) void {
        _ = self;
        arena.deinit();
    }

    pub fn reduce(self: *Expr, arena: *std.heap.ArenaAllocator) !*Expr {
        switch (self.*) {
            .Variable => return self,
            .Application => |app| {
                const lhs = try app.lhs.reduce(arena);
                const rhs = try app.rhs.reduce(arena);

                if (lhs.* == .Lambda) {
                    return try substitute(lhs.Lambda.body, 0, rhs, arena);
                }

                const new = try arena.allocator().create(Expr);
                new.* = .{ .Application = .{ .lhs = lhs, .rhs = rhs } };
                return new;
            },
            .Lambda => |lam| {
                const body = try lam.body.reduce(arena);
                const expr = try arena.allocator().create(Expr);
                expr.* = .{ .Lambda = .{ .name = lam.name, .body = body } };
                return expr;
            },
        }
    }

    fn substitute(expr: *Expr, idx: usize, value: *Expr, arena: *std.heap.ArenaAllocator) !*Expr {
        switch (expr.*) {
            .Variable => |i| {
                if (i == idx) return value;
                const newIndex = if (i > idx) i - 1 else i;
                const v = try arena.allocator().create(Expr);
                v.* = .{ .Variable = newIndex };
                return v;
            },
            .Application => |app| {
                const lhs = try substitute(app.lhs, idx, value, arena);
                const rhs = try substitute(app.rhs, idx, value, arena);
                const new = try arena.allocator().create(Expr);
                new.* = .{ .Application = .{ .lhs = lhs, .rhs = rhs } };
                return new;
            },
            .Lambda => |lam| {
                const shifted = try shift(lam.body, 1, 0, arena);
                const body = try substitute(shifted, idx + 1, shifted, arena);
                const new = try arena.allocator().create(Expr);
                new.* = .{ .Lambda = .{ .name = lam.name, .body = body } };
                return new;
            },
        }
    }

    fn shift(e: *Expr, d: usize, c: usize, arena: *std.heap.ArenaAllocator) !*Expr {
        switch (e.*) {
            .Variable => |i| {
                const ni = if (i < c) i else i + d;
                const v = try arena.allocator().create(Expr);
                v.* = .{ .Variable = ni };
                return v;
            },
            .Application => |app| {
                const lhs = try shift(app.lhs, d, c, arena);
                const rhs = try shift(app.rhs, d, c, arena);
                const a = try arena.allocator().create(Expr);
                a.* = .{ .Application = .{ .lhs = lhs, .rhs = rhs } };
                return a;
            },
            .Lambda => |lam| {
                // bump cutoff for nested scope
                const body = try shift(lam.body, d, c + 1, arena);
                const l = try arena.allocator().create(Expr);
                l.* = .{ .Lambda = .{ .name = lam.name, .body = body } };
                return l;
            },
        }
    }
};

const ParseError = error{
    UnexpectedChar,
    UnexpectedEOF,
    InvalidVariable,
    OutOfMemory,
};

const Parser = struct {
    arena: *std.heap.ArenaAllocator,
    input: []const u8,
    pos: usize,
    context: *std.ArrayList([]const u8),

    fn peek(self: *Parser) ?u8 {
        return if (self.pos < self.input.len) self.input[self.pos] else null;
    }

    fn consume(self: *Parser) ?u8 {
        const ch = self.peek() orelse return null;
        self.pos += 1;
        return ch;
    }

    fn skipWhitespace(self: *Parser) void {
        while (self.peek()) |ch| {
            if (!std.ascii.isWhitespace(ch)) break;
            _ = self.consume();
        }
    }

    fn parseTerm(self: *Parser) ParseError!*Expr {
        self.skipWhitespace();
        return switch (self.peek() orelse return ParseError.UnexpectedEOF) {
            '\\' => self.parseLambda(),
            '(' => self.parseParen(),
            else => |ch| if (std.ascii.isLower(ch)) self.parseVariable() else ParseError.UnexpectedChar,
        };
    }

    fn parseVariable(self: *Parser) ParseError!*Expr {
        self.skipWhitespace();
        const start = self.pos;
        _ = self.consume() orelse return ParseError.UnexpectedEOF;

        while (self.peek()) |ch| {
            if (std.ascii.isAlphanumeric(ch)) {
                _ = self.consume();
            } else {
                break;
            }
        }

        const name = self.input[start..self.pos];

        // look up De Bruijn index in context
        var found: ?usize = null;
        for (self.context.items, 0..) |n, i| {
            if (std.mem.eql(u8, n, name)) {
                found = i;
                break;
            }
        }

        const i = found orelse return ParseError.InvalidVariable;
        const v = try self.arena.allocator().create(Expr);
        v.* = .{ .Variable = i };
        return v;
    }

    fn parseLambda(self: *Parser) ParseError!*Expr {
        _ = self.consume(); // skip '\'
        const start = self.pos;
        _ = self.consume() orelse return ParseError.UnexpectedEOF;
        while (self.peek()) |ch| {
            if (std.ascii.isAlphanumeric(ch)) {
                _ = self.consume();
            } else {
                break;
            }
        }
        const name = self.input[start..self.pos];

        self.skipWhitespace();
        if (self.consume() != '.') return ParseError.UnexpectedChar;

        try self.context.append(name); // push param
        const body = try self.parseTerm();
        _ = self.context.pop(); // pop param

        const l = try self.arena.allocator().create(Expr);
        l.* = .{ .Lambda = .{ .name = name, .body = body } };
        return l;
    }

    fn parseParen(self: *Parser) ParseError!*Expr {
        _ = self.consume();
        const e = try self.parseTerm();
        self.skipWhitespace();
        if (self.consume() != ')') return ParseError.UnexpectedChar;
        return self.parseApplication(e);
    }

    fn parseApplication(self: *Parser, left: *Expr) ParseError!*Expr {
        self.skipWhitespace();
        if (self.peek() == null or self.peek() == ')') return left;
        const r = try self.parseTerm();
        const a = try self.arena.allocator().create(Expr);
        a.* = .{ .Application = .{ .lhs = left, .rhs = r } };
        return self.parseApplication(a);
    }
};

test "test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const expr = try Expr.init(arena.allocator(), "\\x . x");
    const reduced = try expr.reduce(&arena);
    try std.testing.expect(@TypeOf(reduced.*) == Expr);
}

test "identity function" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const expr = try Expr.init(arena.allocator(), "\\x . x");
    const reduced = try expr.reduce(&arena);
    try std.testing.expect(reduced.* == .Lambda);
    try std.testing.expectEqualStrings("x", reduced.Lambda.name);
}

test "application" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const expr = try Expr.init(arena.allocator(), "(\\x . x) (\\y . y)");
    const reduced = try expr.reduce(&arena);
    try std.testing.expect(reduced.* == .Lambda);
    try std.testing.expectEqualStrings("y", reduced.Lambda.name);
}

test "nested lambda" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const expr = try Expr.init(arena.allocator(), "\\x . \\y . x");
    const reduced = try expr.reduce(&arena);
    try std.testing.expect(reduced.* == .Lambda);
    try std.testing.expectEqualStrings("x", reduced.Lambda.name);
    try std.testing.expect(reduced.Lambda.body.* == .Lambda);
    try std.testing.expectEqualStrings("y", reduced.Lambda.body.Lambda.name);
}

test "beta reduction" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const expr = try Expr.init(arena.allocator(), "(\\x . \\y . y) (\\z . z)");
    const reduced = try expr.reduce(&arena);
    try std.testing.expect(reduced.* == .Lambda);
    try std.testing.expectEqualStrings("y", reduced.Lambda.name);
}
