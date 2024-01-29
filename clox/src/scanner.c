#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
    const char* start;
    const char* current;
    int line;
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

static bool isAlpha(char c) {
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c == '_');
}

static bool isDigit(char c) {
    return '0' <= c && c <= '9';
}

static bool isAtEnd(void) {
    return *scanner.current == '\0';
}

static char advance(void) {
    scanner.current++;
    return scanner.current[-1];
}

static char peek(void) {
    return *scanner.current;
}

static char peekNext(void) {
    if (isAtEnd())
        return '\0';
    return scanner.current[1];
}

static bool match(char expected) {
    if (isAtEnd()) {
        return false;
    }
    if (*scanner.current != expected) {
        return false;
    }
    scanner.current++;
    return true;
}

static Token makeToken(TokenType type) {
    Token token;
    token.type = type;
    token.start = scanner.start;
    token.length = (uint32_t)(scanner.current - scanner.start);
    token.line = scanner.line;
    return token;
}

static Token errorToken(const char* message) {
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (uint32_t)strlen(message);
    token.line = scanner.line;
    return token;
}

inline char* tokenTypeName(TokenType type) {
    switch (type) {
    case TOKEN_LEFT_PAREN: return "TOKEN_LEFT_PAREN";
    case TOKEN_RIGHT_PAREN: return "TOKEN_RIGHT_PAREN";
    case TOKEN_LEFT_BRACE: return "TOKEN_LEFT_BRACE";
    case TOKEN_RIGHT_BRACE: return "TOKEN_RIGHT_BRACE";
    case TOKEN_COMMA: return "TOKEN_COMMA";
    case TOKEN_DOT: return "TOKEN_DOT";
    case TOKEN_MINUS: return "TOKEN_MINUS";
    case TOKEN_PLUS: return "TOKEN_PLUS";
    case TOKEN_SEMICOLON: return "TOKEN_SEMICOLON";
    case TOKEN_SLASH: return "TOKEN_SLASH";
    case TOKEN_STAR: return "TOKEN_STAR";
    case TOKEN_PERCENT: return "TOKEN_PERCENT";
    case TOKEN_BANG: return "TOKEN_BANG";
    case TOKEN_BANG_EQUAL: return "TOKEN_BANG_EQUAL";
    case TOKEN_EQUAL: return "TOKEN_EQUAL";
    case TOKEN_EQUAL_EQUAL: return "TOKEN_EQUAL_EQUAL";
    case TOKEN_GREATER: return "TOKEN_GREATER";
    case TOKEN_GREATER_EQUAL: return "TOKEN_GREATER_EQUAL";
    case TOKEN_LESS: return "TOKEN_LESS";
    case TOKEN_LESS_EQUAL: return "TOKEN_LESS_EQUAL";
    case TOKEN_IDENTIFIER: return "TOKEN_IDENTIFIER";
    case TOKEN_STRING: return "TOKEN_STRING";
    case TOKEN_NUMBER: return "TOKEN_NUMBER";
    case TOKEN_AND: return "TOKEN_AND";
    case TOKEN_CLASS: return "TOKEN_CLASS";
    case TOKEN_ELSE: return "TOKEN_ELSE";
    case TOKEN_FALSE: return "TOKEN_FALSE";
    case TOKEN_FOR: return "TOKEN_FOR";
    case TOKEN_FUN: return "TOKEN_FUN";
    case TOKEN_IF: return "TOKEN_IF";
    case TOKEN_NIL: return "TOKEN_NIL";
    case TOKEN_OR: return "TOKEN_OR";
    case TOKEN_PRINT: return "TOKEN_PRINT";
    case TOKEN_RETURN: return "TOKEN_RETURN";
    case TOKEN_SUPER: return "TOKEN_SUPER";
    case TOKEN_THIS: return "TOKEN_THIS";
    case TOKEN_TRUE: return "TOKEN_TRUE";
    case TOKEN_VAR: return "TOKEN_VAR";
    case TOKEN_WHILE: return "TOKEN_WHILE";
    case TOKEN_ERROR: return "TOKEN_ERROR";
    case TOKEN_EOF: return "TOKEN_EOF";
    default: return "UNKNOWN_TOKEN";
    }
}

static void skipWhitespace(void) {
    for (;;) {
        char c = peek();
        switch (c) {

        case ' ':
        case '\r':
        case '\t': advance(); break;
        case '\n':
            scanner.line++;
            advance();
            break;
        case '/':
            if (peekNext() == '/') {
                // A '//' comment goes until the end of the line.
                while (peek() != '\n' && !isAtEnd()) {
                    advance();
                }
            } else if (peekNext() == '*') {
                // A '/*' comment goes until '*/'
                advance();
                advance();
                while (!(peek() == '*' && peekNext() == '/') && !isAtEnd()) {
                    if (peek() == '\n') {
                        scanner.line++;
                    }
                    advance();
                }
                advance();
                advance();
            } else {
                return;
            }
            break;
        default: return;
        }
    }
}

static TokenType checkKeyword(uint32_t start, uint32_t length, const char* rest, TokenType type) {
    // First check that the identifier lengths are the same.
    // Then check that all characters past the first match,
    // as we already expect the caller to check the first character matches
    if (scanner.current - scanner.start == start + length &&
        memcmp(scanner.start + start, rest, length) == 0) {
        return type;
    }

    return TOKEN_IDENTIFIER;
}

static TokenType identifierType(void) {
    switch (scanner.start[0]) {
    case 'a': return checkKeyword(1, 2, "nd", TOKEN_AND);
    case 'c': return checkKeyword(1, 4, "lass", TOKEN_CLASS);
    case 'e': return checkKeyword(1, 3, "lse", TOKEN_ELSE);
    case 'f':
        // check to see if there is a second letter first
        if (scanner.current - scanner.start > 1) {
            switch (scanner.start[1]) {
            case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE);
            case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR);
            case 'u': return checkKeyword(2, 1, "n", TOKEN_FUN);
            }
        }
        break;
    case 'i': return checkKeyword(1, 1, "f", TOKEN_IF);
    case 'n': return checkKeyword(1, 2, "il", TOKEN_NIL);
    case 'o': return checkKeyword(1, 1, "r", TOKEN_OR);
    case 'p': return checkKeyword(1, 4, "rint", TOKEN_PRINT);
    case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
    case 's': return checkKeyword(1, 4, "uper", TOKEN_SUPER);
    case 't':
        // check to see if there is a second letter first
        if (scanner.current - scanner.start > 1) {
            switch (scanner.start[1]) {
            case 'h': return checkKeyword(2, 2, "is", TOKEN_THIS);
            case 'r': return checkKeyword(2, 2, "ue", TOKEN_TRUE);
            }
        }
        break;

    case 'v': return checkKeyword(1, 2, "ar", TOKEN_VAR);
    case 'w': return checkKeyword(1, 4, "hile", TOKEN_WHILE);
    }
    return TOKEN_IDENTIFIER;
}

static Token identifier(void) {
    while (isAlpha(peek()) || isDigit(peek())) {
        advance();
    }
    return makeToken(identifierType());
}

static Token number(void) {
    while (isDigit(peek())) {
        advance();
    }

    // Look for a fractional part
    if (peek() == '.' && isDigit(peekNext())) {
        // consume the '.'
        advance();

        while (isDigit(peek())) {
            advance();
        }
    }

    return makeToken(TOKEN_NUMBER);
}

static Token string(void) {
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') {
            scanner.line++;
        }
        advance();
    }

    if (isAtEnd()) {
        return errorToken("Unterminated string literal.");
    }

    // consume the closing quote
    advance();
    return makeToken(TOKEN_STRING);
}

Token scanToken(void) {
    skipWhitespace();
    scanner.start = scanner.current;

    if (isAtEnd()) {
        return makeToken(TOKEN_EOF);
    }

    char c = advance();
    if (isDigit(c)) {
        return number();
    }
    if (isAlpha(c)) {
        return identifier();
    }

    switch (c) {
    case '(': return makeToken(TOKEN_LEFT_PAREN);
    case ')': return makeToken(TOKEN_RIGHT_PAREN);
    case '{': return makeToken(TOKEN_LEFT_BRACE);
    case '}': return makeToken(TOKEN_RIGHT_BRACE);
    case ';': return makeToken(TOKEN_SEMICOLON);
    case ',': return makeToken(TOKEN_COMMA);
    case '.': return makeToken(TOKEN_DOT);
    case '-': return makeToken(TOKEN_MINUS);
    case '+': return makeToken(TOKEN_PLUS);
    case '/': return makeToken(TOKEN_SLASH);
    case '*': return makeToken(TOKEN_STAR);
    case '%': return makeToken(TOKEN_PERCENT);
    case '!': return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
    case '=': return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
    case '<': return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
    case '>': return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
    case '"': return string();
    }

    char buffer[50] = {0}; // This should always be more than enough.
    size_t max_len = sizeof buffer;
    snprintf(buffer, max_len, "Unexpected character '%c'.", c);
    return errorToken(buffer);
}
