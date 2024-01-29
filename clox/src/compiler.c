#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "scanner.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif /* ifdef DEBUG_PRINT_CODE */

typedef struct {
    Token current;
    Token previous;
    bool had_error;
    bool panic_mode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * / %
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool can_assign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
    bool is_captured;
} Local;

typedef struct {
    uint8_t index;
    bool is_local;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
    TYPE_METHOD,
    TYPE_INITIALIZER,
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    ObjFunction* function;
    FunctionType type;
    Local locals[UINT8_COUNT];
    int local_count;
    Upvalue upvalues[UINT8_COUNT];
    int scope_depth;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
    bool has_superclass;
} ClassCompiler;

Parser parser;
Compiler* current = NULL;
ClassCompiler* current_class = NULL;

static Chunk* currentChunk(void) {
    return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panic_mode) {
        return;
    }
    parser.panic_mode = true;
    eprintf("[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        eprintf(" at EOF");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing
    } else {
        eprintf(" at '%.*s'", (int)token->length, token->start);
    }

    eprintf(": %s\n", message);
    parser.had_error = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance(void) {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) {
            break;
        }

        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) {
        return false;
    }
    advance();
    return true;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int loop_start) {
    emitByte(OP_LOOP);

    // We need to add 2 to jump over the OP_LOOP operands.
    int offset = currentChunk()->count - loop_start + 2;
    if (offset > UINT16_MAX) {
        error("Loop body too large.");
    }

    emitByte((uint8_t)((offset >> 8) & 0xff));
    emitByte((uint8_t)(offset & 0xff));
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitReturn(void) {
    if (current->type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }
    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
    // -2 to adjust for the bytecode of the jump offset itself.
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (uint8_t)((jump >> 8) & 0xff);
    currentChunk()->code[offset + 1] = (uint8_t)(jump & 0xff);
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    // Mark the enclosing compiler as the one that is currently being used.
    // This allows us to break out of this compiler to enclosing one when we
    // are finished compiling a function body for example.
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->local_count = 0;
    compiler->scope_depth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT) {
        current->function->name =
            copyString(parser.previous.start, parser.previous.length);
    }

    Local* local = &current->locals[current->local_count++];
    local->depth = 0;
    local->is_captured = false;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler(void) {
    emitReturn();
    ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
    if (!parser.had_error) {
        disassembleChunk(currentChunk(),
                         function->name != NULL ? function->name->chars : "<script>");
    }
#endif /* ifdef DEBUG_PRINT_CODE */

    // Break out of the current compiler to the enclosing compiler one level up
    current = current->enclosing;
    return function;
}

static void beginScope(void) {
    current->scope_depth++;
}

static void endScope(void) {
    current->scope_depth--;

    while (current->local_count > 0 &&
           current->locals[current->local_count - 1].depth > current->scope_depth) {
        if (current->locals[current->local_count - 1].is_captured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            // Instead of popping the locals individually, we could use a new `OP_POPN`
            // instruction that can pop multiple values off the stack at once.
            emitByte(OP_POP);
        }
        current->local_count--;
    }
}

static void expression(void);
static void statement(void);
static void declaration(void);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) {
        return false;
    }

    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
    assert(compiler->local_count > 0);
    for (int i = compiler->local_count - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read a local variable in its own initializer.");
            }
            return i;
        }
    }

    // This happens when we don't have a local variable that matched the name
    // given. The happy case of this is a global variable.
    return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool is_local) {
    int upvalue_count = compiler->function->upvalue_count;

    // Check if the upvalue already exists.
    for (int i = 0; i < upvalue_count; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->is_local == is_local) {
            return i;
        }
    }

    if (upvalue_count == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalue_count].is_local = is_local;
    compiler->upvalues[upvalue_count].index = index;
    return compiler->function->upvalue_count++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) {
        return -1;
    }

    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].is_captured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

static void addLocal(Token name) {
    if (current->local_count == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    // Initialize the next available local in the compiler's array.
    Local* local = &current->locals[current->local_count++];
    local->name = name;
    local->depth = -1;
    local->is_captured = false;
}

static void declareVariable(void) {
    if (current->scope_depth == 0) {
        return;
    }

    Token* name = &parser.previous;
    assert(current->local_count > 0);
    for (int i = current->local_count - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scope_depth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }
    addLocal(*name);
}

static uint8_t parseVariable(const char* error_message) {
    consume(TOKEN_IDENTIFIER, error_message);

    declareVariable();
    if (current->scope_depth > 0) {
        return 0;
    }

    return identifierConstant(&parser.previous);
}

static void markInitialized(void) {
    if (current->scope_depth == 0) {
        return;
    }
    current->locals[current->local_count - 1].depth = current->scope_depth;
}

static void defineVariable(uint8_t global) {
    if (current->scope_depth > 0) {
        markInitialized();
        return;
    }
    emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList(void) {
    uint8_t arg_count = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (arg_count == 255) {
                error("Can't have more than 255 arguments.");
            }
            arg_count++;

        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return arg_count;
}

static void and_(bool can_assign) {
    UNUSED(can_assign);
    int end_jump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(end_jump);
}

static void binary(bool can_assign) {
    UNUSED(can_assign);
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
    case TOKEN_BANG_EQUAL: emitBytes(OP_EQUAL, OP_NOT); break;
    case TOKEN_EQUAL_EQUAL: emitByte(OP_EQUAL); break;
    case TOKEN_GREATER: emitByte(OP_GREATER); break;
    case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
    case TOKEN_LESS: emitByte(OP_LESS); break;
    case TOKEN_LESS_EQUAL: emitBytes(OP_GREATER, OP_NOT); break;
    case TOKEN_PLUS: emitByte(OP_ADD); break;
    case TOKEN_MINUS: emitByte(OP_SUBTRACT); break;
    case TOKEN_STAR: emitByte(OP_MULTIPLY); break;
    case TOKEN_SLASH: emitByte(OP_DIVIDE); break;
    case TOKEN_PERCENT: emitByte(OP_MOD); break;
    default: return; //__builtin_unreachable();
    }
}

static void call(bool can_assign) {
    UNUSED(can_assign);
    uint8_t arg_count = argumentList();
    emitBytes(OP_CALL, arg_count);
}

static void dot(bool can_assign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (can_assign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else if (match(TOKEN_LEFT_PAREN)) {
        uint8_t arg_count = argumentList();
        emitBytes(OP_INVOKE, name);
        emitByte(arg_count);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
    }
}

static void literal(bool can_assign) {
    UNUSED(can_assign);
    switch (parser.previous.type) {
    case TOKEN_FALSE: emitByte(OP_FALSE); break;
    case TOKEN_TRUE: emitByte(OP_TRUE); break;
    case TOKEN_NIL: emitByte(OP_NIL); break;
    default: __builtin_unreachable();
    }
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block(void) {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after function name.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalue_count; i++) {
        emitByte(compiler.upvalues[i].is_local ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void method(void) {
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = identifierConstant(&parser.previous);

    FunctionType type = TYPE_METHOD;
    if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }
    function(type);

    emitBytes(OP_METHOD, constant);
}

static void variable(bool can_assign);
static void namedVariable(Token name, bool can_assign);

static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (uint32_t)strlen(text);
    return token;
}

static void super_(bool can_assign) {
    UNUSED(can_assign);
    if (current_class == NULL) {
        error("Can't use 'super' outside of a class.");
    } else if (!current_class->has_superclass) {
        error("Can't use 'super' in a class with no superclass.");
    }

    consume(TOKEN_DOT, "Expect '.' after 'super'.");
    consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
    uint8_t name = identifierConstant(&parser.previous);

    namedVariable(syntheticToken("this"), false);
    if (match(TOKEN_LEFT_PAREN)) {
        uint8_t arg_count = argumentList();
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_SUPER_INVOKE, name);
        emitByte(arg_count);
    } else {
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_GET_SUPER, name);
    }
}

static void classDeclaration(void) {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token class_name = parser.previous;
    uint8_t name_constant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(OP_CLASS, name_constant);
    defineVariable(name_constant);

    ClassCompiler class_compiler;
    class_compiler.enclosing = current_class;
    class_compiler.has_superclass = false;
    current_class = &class_compiler;

    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name.");
        variable(false);

        if (identifiersEqual(&class_name, &parser.previous)) {
            error("A class can't inherit from itself.");
        }

        beginScope();
        addLocal(syntheticToken("super"));
        defineVariable(0);

        namedVariable(class_name, false);
        emitByte(OP_INHERIT);
        class_compiler.has_superclass = true;
    }

    namedVariable(class_name, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OP_POP);

    if (class_compiler.has_superclass) {
        endScope();
    }

    current_class = current_class->enclosing;
}

static void funDeclaration(void) {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}

static void varDeclaration(void) {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
    defineVariable(global);
}

static void expressionStatement(void) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void forStatement(void) {
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(TOKEN_SEMICOLON)) {
        // Empty initializer.
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loop_start = currentChunk()->count;
    int exit_jump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        exit_jump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP); // Pop the condition.
    }

    if (!match(TOKEN_SEMICOLON)) {
        // We want to skip the increment on the first pass through the loop.
        int body_jump = emitJump(OP_JUMP);
        int increment_start = currentChunk()->count;
        expression();
        // We only evaluate the expression in the increment for the side effect
        // (e.g. assignment) so we can discard the value it leaves on the stack.
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for loop clauses.");

        // We want to jump to the before the increment each time before so
        // that it gets executed each time.
        emitLoop(loop_start);
        loop_start = increment_start;
        patchJump(body_jump);
    }

    statement();
    emitLoop(loop_start);

    if (exit_jump != -1) {
        patchJump(exit_jump);
        emitByte(OP_POP);
    }

    endScope();
}

static void ifStatement(void) {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    // We must evaluate the statement inside the 'then' branch first
    // in order to know how far to offset the instruction pointer.
    int then_jump = emitJump(OP_JUMP_IF_FALSE);

    // We need to pop the condition value that's left on the stack.
    // this will be skipped over if we skip over the 'then' branch.
    emitByte(OP_POP);
    statement();

    int else_jump = emitJump(OP_JUMP);

    // Now we patch the jump value with what it should be.
    patchJump(then_jump);

    // We need to pop the condition value that's left on the stack.
    // This will be skipped over if we evaluate the 'then' branch.
    // which skips over the 'else' branch.
    emitByte(OP_POP);

    if (match(TOKEN_ELSE)) {
        statement();
    }
    patchJump(else_jump);
}

static void printStatement(void) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void returnStatement(void) {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer.");
        }
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

static void whileStatement(void) {
    int loop_start = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exit_jump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    emitLoop(loop_start);

    patchJump(exit_jump);
    emitByte(OP_POP);
}

static void synchronize(void) {
    parser.panic_mode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) {
            return;
        }

        // Return only when we reach a token that looks like a statement boundary
        switch (parser.current.type) {
        case TOKEN_CLASS:
        case TOKEN_FUN:
        case TOKEN_VAR:
        case TOKEN_FOR:
        case TOKEN_IF:
        case TOKEN_WHILE:
        case TOKEN_PRINT:
        case TOKEN_RETURN: return;
        default:; // Do nothing
        }

        advance();
    }
}

static void declaration(void) {
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panic_mode) {
        synchronize();
    }
}

static void statement(void) {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else {
        expressionStatement();
    }
}

static void grouping(bool can_assign) {
    UNUSED(can_assign);
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after expression.");
}

static void number(bool can_assign) {
    UNUSED(can_assign);
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void or_(bool can_assign) {
    UNUSED(can_assign);
    int else_jump = emitJump(OP_JUMP_IF_FALSE);
    int end_jump = emitJump(OP_JUMP);

    patchJump(else_jump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(end_jump);
}

static void string(bool can_assign) {
    UNUSED(can_assign);
    // Take the strings characters directly from the lexeme.
    // + 1 and - 2 properly trim the quotation marks from the lexeme.
    emitConstant(
        OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void namedVariable(Token name, bool can_assign) {
    uint8_t get_op, set_op;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        get_op = OP_GET_LOCAL;
        set_op = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        get_op = OP_GET_UPVALUE;
        set_op = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        get_op = OP_GET_GLOBAL;
        set_op = OP_SET_GLOBAL;
    }

    if (can_assign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(set_op, (uint8_t)arg);
    } else {
        emitBytes(get_op, (uint8_t)arg);
    }
}

static void variable(bool can_assign) {
    namedVariable(parser.previous, can_assign);
}

static void this_(bool can_assign) {
    if (current_class == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    UNUSED(can_assign);
    variable(false);
}

static void unary(bool can_assign) {
    UNUSED(can_assign);
    TokenType operatorType = parser.previous.type;

    // Compile the operand
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction
    switch (operatorType) {
    case TOKEN_BANG: emitByte(OP_NOT); break;
    case TOKEN_MINUS: emitByte(OP_NEGATE); break;
    default: return; //__builtin_unreachable();
    }
}

// clang-format off
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_PERCENT]       = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     and_,   PREC_NONE},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
    [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};
// clang-format on

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefix_rule = getRule(parser.previous.type)->prefix;
    if (prefix_rule == NULL) {
        error("Expect expression.");
        return;
    }

    bool can_assign = precedence <= PREC_ASSIGNMENT;
    prefix_rule(can_assign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infix_rule = getRule(parser.previous.type)->infix;
        infix_rule(can_assign);
    }

    if (can_assign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

// We need this wrapper over the table because our parser is recursive
// and we need to handle the declaration cycle of having our table
// contain function pointers to functions that use the table.
static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.had_error = false;
    parser.panic_mode = false;

    advance();
    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.had_error ? NULL : function;
}

void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}
