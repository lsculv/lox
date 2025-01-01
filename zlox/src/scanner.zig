const std = @import("std");
const ascii = std.ascii;

const TokenType = enum {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Percent,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
};

const Token = struct {
    token_type: TokenType,
    start: usize,
    length: usize,
    line: usize,
};

const Scanner = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,

    const Self = @This();

    pub fn new(source: []const u8) Self {
        return Scanner{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    fn isValidIdent(c: u8) bool {
        return ascii.isAlphabetic(c) or (c == '_');
    }

    fn isAtEnd(self: *Self) bool {
        return self.source[self.current] == 0;
    }

    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn peek(self: *Self) u8 {
        return self.source[self.current];
    }

    fn peekNext(self: *Self) u8 {
        if (self.isAtEnd()) {
            return 0;
        }
        return self.source[self.current + 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }

        if (self.source[self.current] != expected) {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn makeToken(self: *Self, token_type: TokenType) Token {
        return Token{
            .token_type = token_type,
            .start = self.start,
            .length = self.current - self.start,
            .line = self.line,
        };
    }

    fn errorToken(self: *Self, message: []const u8) Token {
        return Token{
            .token_type = .Error,
            .start = message,
            .length = message.len,
            .line = self.line,
        };
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => self.advance(),
                '\n' => {
                    self.line += 1;
                    self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // A '//' comment goes until the end of the line.
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            self.advance();
                        }
                    } else if (self.peekNext() == '*') {
                        // A '/*' comment goes until '*/'
                        self.advance();
                        self.advance();
                        while (!(self.peek() == '*' and self.peekNext() == '/') and !self.isAtEnd()) {
                            if (self.peek() == '\n') {
                                self.line += 1;
                            }
                            self.advance();
                        }
                        self.advance();
                        self.advance();
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn checkKeyword(self: *Self, start: usize, length: usize, rest: []const u8, token_type: TokenType) TokenType {
        if (self.current - self.start == start + length and self.source[(self.start + start)..(self.start + start + length)] == rest) {
            return token_type;
        }

        return .Identifier;
    }

    fn identifierType(self: *Self) TokenType {
        return switch (self.source[self.start]) {
            'a' => self.checkKeyword(1, 2, "nd", .And),
            'c' => self.checkKeyword(1, 4, "lass", .Class),
            'e' => self.checkKeyword(1, 3, "lse", .Else),
            'f' =>
            // check to see if there is a second letter first
            if (self.current - self.start > 1) {
                switch (self.source[self.start + 1]) {
                    'a' => self.checkKeyword(2, 3, "lse", .False),
                    'o' => self.checkKeyword(2, 1, "r", .For),
                    'u' => self.checkKeyword(2, 1, "n", .Fun),
                }
            },
            'i' => self.checkKeyword(1, 1, "f", .If),
            'n' => self.checkKeyword(1, 2, "il", .Nil),
            'o' => self.checkKeyword(1, 1, "r", .Or),
            'p' => self.checkKeyword(1, 4, "rint", .Print),
            'r' => self.checkKeyword(1, 5, "eturn", .Return),
            's' => self.checkKeyword(1, 4, "uper", .Super),
            't' =>
            // check to see if there is a second letter first
            if (self.current - self.start > 1) {
                switch (self.source[self.start + 1]) {
                    'h' => self.checkKeyword(2, 2, "is", .This),
                    'r' => self.checkKeyword(2, 2, "ue", .True),
                }
            },

            'v' => self.checkKeyword(1, 2, "ar", .Var),
            'w' => self.checkKeyword(1, 4, "hile", .While),
            else => .Identifier,
        };
    }

    fn identifier(self: *Self) Token {
        while (self.isValidIdent(self.peek()) or ascii.isDigit(self.peek())) {
            self.advance();
        }
        return self.makeToken(self.identifierType());
    }

    fn number(self: *Self) Token {
        while (ascii.isDigit(self.peek())) {
            self.advance();
        }

        // Look for fractional part
        if (self.peek() == '.' and ascii.isDigit(self.peekNext())) {
            // Consume the '.'
            self.advance();

            while (ascii.isDigit(self.peek())) {
                self.advance();
            }
        }

        return self.makeToken(.Number);
    }
};
