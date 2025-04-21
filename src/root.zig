const std = @import("std");

pub const Expr = union(enum) {
    Variable: []const u8,
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
        var parser = Parser{ .arena = &arena, .input = input, .pos = 0 };
        const expr = try parser.parseTerm();
        return expr;
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
                    return try substitute(lhs.Lambda.body, lhs.Lambda.name, rhs, arena);
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

    fn substitute(expr: *Expr, name: []const u8, value: *Expr, arena: *std.heap.ArenaAllocator) !*Expr {
        switch (expr.*) {
            .Variable => |v| {
                if (std.mem.eql(u8, v, name)) {
                    return value;
                }
                return expr;
            },
            .Application => |app| {
                const lhs = try substitute(app.lhs, name, value, arena);
                const rhs = try substitute(app.rhs, name, value, arena);
                const new = try arena.allocator().create(Expr);
                new.* = .{ .Application = .{ .lhs = lhs, .rhs = rhs } };
                return new;
            },
            .Lambda => |lam| {
                const body = try substitute(lam.body, name, value, arena);
                const new = try arena.allocator().create(Expr);
                new.* = .{ .Lambda = .{ .name = lam.name, .body = body } };
                return new;
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
        if (!std.ascii.isLower(self.peek() orelse return ParseError.UnexpectedEOF))
            return ParseError.InvalidVariable;

        _ = self.consume();
        while (self.peek()) |ch| {
            if (std.ascii.isAlphanumeric(ch)) {
                _ = self.consume();
            } else {
                break;
            }
        }

        const name = self.input[start..self.pos];
        const expr = try self.arena.allocator().create(Expr);
        expr.* = .{ .Variable = name };
        return expr;
    }

    fn parseLambda(self: *Parser) ParseError!*Expr {
        _ = self.consume(); // skip '\\'
        const param_expr = try self.parseVariable();
        const param = param_expr.Variable;

        self.skipWhitespace();
        if (self.consume() != '.') return ParseError.UnexpectedChar;

        const body = try self.parseTerm();
        const expr = try self.arena.allocator().create(Expr);
        expr.* = .{ .Lambda = .{ .name = param, .body = body } };
        return expr;
    }

    fn parseParen(self: *Parser) ParseError!*Expr {
        _ = self.consume(); // skip '('
        const term = try self.parseTerm();

        self.skipWhitespace();
        if (self.consume() != ')') {
            return ParseError.UnexpectedChar;
        }

        return self.parseApplication(term);
    }

    fn parseApplication(self: *Parser, left: *Expr) ParseError!*Expr {
        self.skipWhitespace();
        const next = self.peek();
        if (next == null or next == ')') return left;

        const right = try self.parseTerm();
        const expr = try self.arena.allocator().create(Expr);
        expr.* = .{ .Application = .{ .lhs = left, .rhs = right } };
        return self.parseApplication(expr);
    }
};

test "test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const expr = try Expr.init(arena.allocator(), "(\\x . x) y");
    const reduced = try expr.reduce(&arena);
    try std.testing.expect(@TypeOf(reduced.*) == Expr);
}
