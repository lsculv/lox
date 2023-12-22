#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

VM vm;

static Value clockNative(int arg_count, Value* args) {
    UNUSED(arg_count);
    UNUSED(args);
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
    vm.sp = vm.stack;
    vm.frame_count = 0;
    vm.open_upvalues = NULL;
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fputs("\n", stderr);

    for (int i = vm.frame_count - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        eprintf("[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL) {
            eprintf("script\n");
        } else {
            eprintf("%s()", function->name->chars);
        }
    }

    CallFrame* frame = &vm.frames[vm.frame_count - 1];
    size_t instruction = frame->ip - frame->closure->function->chunk.code - 1;
    int line = frame->closure->function->chunk.lines[instruction];
    eprintf("[line %d] in script\n", line);
    resetStack();
}

static void defineNative(const char* name, NativeFn function) {
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void initVM() {
    resetStack();
    vm.objects = NULL;
    vm.bytes_allocated = 0;
    vm.next_gc = 1024 * 1024;

    vm.gray_count = 0;
    vm.gray_capacity = 0;
    vm.gray_stack = NULL;

    initTable(&vm.globals);
    initTable(&vm.strings);

    vm.initString = NULL;
    vm.initString = copyString("init", 4);

    defineNative("clock", clockNative);
}
void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    vm.initString = NULL;
    freeObjects();
}

void push(Value value) {
    *vm.sp = value;
    vm.sp++;
}

Value pop() {
    vm.sp--;
    return *vm.sp;
}

static Value peek(int distance) {
    return vm.sp[-1 - distance];
}

static bool call(ObjClosure* closure, int arg_count) {
    if (arg_count != closure->function->arity) {
        runtimeError("Expected %d arguments but got %d.", closure->function->arity,
                     arg_count);
        return false;
    }

    if (vm.frame_count == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame* frame = &vm.frames[vm.frame_count++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm.sp - arg_count - 1;
    return true;
}

static bool callValue(Value callee, int arg_count) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
        case OBJ_BOUND_METHOD: {
            ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
            vm.sp[-arg_count - 1] = bound->receiver;
            return call(bound->method, arg_count);
        }
        case OBJ_CLASS: {
            ObjClass* class_ = AS_CLASS(callee);
            vm.sp[-arg_count - 1] = OBJ_VAL(newInstance(class_));
            Value initializer;
            if (tableGet(&class_->methods, vm.initString, &initializer)) {
                return call(AS_CLOSURE(initializer), arg_count);
            } else if (arg_count != 0) {
                runtimeError("Expected 0 arguments but got %d.", arg_count);
                return false;
            }
            return true;
        }
        case OBJ_CLOSURE: return call(AS_CLOSURE(callee), arg_count);
        case OBJ_NATVIE: {
            NativeFn native = AS_NATIVE(callee);
            Value result = native(arg_count, vm.sp - arg_count);
            vm.sp -= arg_count + 1;
            push(result);
            return true;
        }
        default: break; // Non callable object type.
        }
    }

    runtimeError("Can only call functions and classes.");
    return false;
}

static bool invokeFromClass(ObjClass* class_, ObjString* name, int arg_count) {
    Value method;
    if (!tableGet(&class_->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    return call(AS_CLOSURE(method), arg_count);
}

static bool invoke(ObjString* name, int arg_count) {
    Value receiver = peek(arg_count);

    if (!IS_INSTANCE(receiver)) {
        runtimeError("Only instances have methods.");
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);

    Value value;
    if (!tableGet(&instance->fields, name, &value)) {
        vm.sp[-arg_count - 1] = value;
        return callValue(value, arg_count);
    }

    return invokeFromClass(instance->class_, name, arg_count);
}

static bool bindMethod(ObjClass* class_, ObjString* name) {
    Value method;
    if (!tableGet(&class_->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));

    pop();
    push(OBJ_VAL(bound));
    return true;
}

static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prev_upvalue = NULL;
    ObjUpvalue* upvalue = vm.open_upvalues;
    while (upvalue != NULL && upvalue->location > local) {
        prev_upvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location > local) {
        return upvalue;
    }

    ObjUpvalue* created_upvalue = newUpvalue(local);
    created_upvalue->next = upvalue;

    if (prev_upvalue == NULL) {
        vm.open_upvalues = created_upvalue;
    } else {
        prev_upvalue->next = created_upvalue;
    }

    return created_upvalue;
}

static void closeUpvalues(Value* last) {
    while (vm.open_upvalues != NULL && vm.open_upvalues->location >= last) {
        ObjUpvalue* upvalue = vm.open_upvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.open_upvalues = upvalue->next;
    }
}

static void defineMethod(ObjString* name) {
    Value method = peek(0);
    ObjClass* class_ = AS_CLASS(peek(1));
    tableSet(&class_->methods, name, method);
    pop();
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    ObjString* b = AS_STRING(peek(0));
    ObjString* a = AS_STRING(peek(1));

    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    // Remove string a and string b from our stack.
    pop();
    pop();
    push(OBJ_VAL(result));
}

static InterpretResult run() {
    CallFrame* frame = &vm.frames[vm.frame_count - 1];
#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_STRING() (AS_STRING(READ_CONSTANT()))
#define BINARY_OP(value_type, op)                                     \
    do {                                                              \
        /* first check if both operands are numbers */                \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {             \
            runtimeError("Operands must be numbers.");                \
            return INTERPRET_RUNTIME_ERROR;                           \
        }                                                             \
        /* get both values and take the inner value */                \
        double b = AS_NUMBER(pop());                                  \
        double a = AS_NUMBER(pop());                                  \
        /* push the value after performing the passed in operation */ \
        push(value_type(a op b));                                     \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        disassembleInstruction(&frame->closure->function->chunk,
                               (int)(frame->ip - frame->closure->function->chunk.code));
        eprintf("          ");
        // print when the stack is empty
        if (vm.stack == vm.sp) {
            eprintf("*empty*");
        }
        for (Value* slot = vm.stack; slot < vm.sp; slot++) {
            eprintf("[ ");
            printValue(stderr, *slot);
            eprintf(" ]");
        }

        eprintf("\n");
#endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
        case OP_CONSTANT: {
            Value constant = READ_CONSTANT();
            push(constant);
            break;
        }
        case OP_NIL: push(NIL_VAL()); break;
        case OP_TRUE: push(BOOL_VAL(true)); break;
        case OP_FALSE: push(BOOL_VAL(false)); break;
        case OP_POP: pop(); break;
        case OP_SET_LOCAL: {
            uint8_t slot = READ_BYTE();
            frame->slots[slot] = peek(0);
            break;
        }
        case OP_GET_LOCAL: {
            uint8_t slot = READ_BYTE();
            push(frame->slots[slot]);
            break;
        }
        case OP_GET_GLOBAL: {
            ObjString* name = READ_STRING();
            Value value;
            if (!tableGet(&vm.globals, name, &value)) {
                runtimeError("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(value);
            break;
        }
        case OP_DEFINE_GLOBAL: {
            ObjString* name = READ_STRING();
            tableSet(&vm.globals, name, peek(0));
            pop();
            break;
        }
        case OP_SET_GLOBAL: {
            ObjString* name = READ_STRING();
            if (tableSet(&vm.globals, name, peek(0))) {
                tableDelete(&vm.globals, name);
                runtimeError("Undefined variable '%s'.", name->chars);
            }
            break;
        }
        case OP_GET_UPVALUE: {
            uint8_t slot = READ_BYTE();
            push(*frame->closure->upvalues[slot]->location);
            break;
        }
        case OP_SET_UPVALUE: {
            uint8_t slot = READ_BYTE();
            *frame->closure->upvalues[slot]->location = peek(0);
            break;
        }
        case OP_GET_SUPER: {
            ObjString* name = READ_STRING();
            ObjClass* superclass = AS_CLASS(pop());

            if (!bindMethod(superclass, name)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_EQUAL: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(valuesEqual(a, b)));
            break;
        }
        case OP_GET_PROPERTY: {
            if (!IS_INSTANCE(peek(0))) {
                runtimeError("Only instances have properties.");
                return INTERPRET_RUNTIME_ERROR;
            }
            ObjInstance* instance = AS_INSTANCE(peek(0));
            ObjString* name = READ_STRING();

            Value value;
            if (tableGet(&instance->fields, name, &value)) {
                pop(); // pop the Instance.
                push(value);
                break;
            } else {
                // javascript-like behavior
                pop(); // pop the Instance.
                push(NIL_VAL());
                break;
            }

            if (!bindMethod(instance->class_, name)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            break;

            // Original behavior.
            // runtimeError("Undefined property %s", name->chars);
            // return INTERPRET_RUNTIME_ERROR;
        }
        case OP_SET_PROPERTY: {
            if (!IS_INSTANCE(peek(1))) {
                runtimeError("Only instances have fields.");
                return INTERPRET_RUNTIME_ERROR;
            }
            ObjInstance* instance = AS_INSTANCE(peek(1));
            tableSet(&instance->fields, READ_STRING(), peek(0));
            Value value = pop();
            pop();
            push(value);
            break;
        }
        case OP_GREATER: BINARY_OP(BOOL_VAL, >); break;
        case OP_LESS: BINARY_OP(BOOL_VAL, <); break;
        case OP_ADD: {
            if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                concatenate();
            } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                double b = AS_NUMBER(pop());
                double a = AS_NUMBER(pop());
                push(NUMBER_VAL(a + b));
            } else {
                runtimeError("Operands must be two numbers or two strings.");
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
        case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
        case OP_DIVIDE: BINARY_OP(NUMBER_VAL, /); break;
        case OP_MOD: {
            if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {
                runtimeError("Operands must be numbers.");
                return INTERPRET_RUNTIME_ERROR;
            }
            double b = AS_NUMBER(pop());
            double a = AS_NUMBER(pop());
            push(NUMBER_VAL(fmod(a, b)));
            break;
        }
        case OP_NOT: *(vm.sp - 1) = BOOL_VAL(isFalsey(*(vm.sp - 1))); break;
        case OP_NEGATE:
            if (!IS_NUMBER(peek(0))) {
                runtimeError("Operand must be a number.");
                return INTERPRET_RUNTIME_ERROR;
            }
            *(vm.sp - 1) = NUMBER_VAL(-AS_NUMBER(*(vm.sp - 1)));
            break;
        case OP_PRINT: {
            printValue(stdout, pop());
            printf("\n");
            break;
        }
        case OP_JUMP: {
            uint16_t offset = READ_SHORT();
            frame->ip += offset;
            break;
        }
        case OP_JUMP_IF_FALSE: {
            uint16_t offset = READ_SHORT();
            if (isFalsey(peek(0))) {
                frame->ip += offset;
            }
            break;
        }
        case OP_LOOP: {
            uint16_t offset = READ_SHORT();
            frame->ip -= offset;
            break;
        }
        case OP_CALL: {
            int arg_count = READ_BYTE();
            if (!callValue(peek(arg_count), arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            frame = &vm.frames[vm.frame_count - 1];
            break;
        }
        case OP_INVOKE: {
            ObjString* method = READ_STRING();
            int arg_count = READ_BYTE();
            if (!invoke(method, arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            frame = &vm.frames[vm.frame_count - 1];
            break;
        }
        case OP_SUPER_INVOKE: {
            ObjString* method = READ_STRING();
            int arg_count = READ_BYTE();
            ObjClass* superclass = AS_CLASS(pop());
            if (!invokeFromClass(superclass, method, arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            frame = &vm.frames[vm.frame_count - 1];
            break;
        }
        case OP_CLOSURE: {
            ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
            ObjClosure* closure = newClosure(function);
            push(OBJ_VAL(closure));
            for (int i = 0; i < closure->upvalue_count; i++) {
                uint8_t is_local = READ_BYTE();
                uint8_t index = READ_BYTE();
                if (is_local) {
                    closure->upvalues[i] = captureUpvalue(frame->slots + index);
                } else {
                    closure->upvalues[i] = frame->closure->upvalues[index];
                }
            }
            break;
        }
        case OP_CLOSE_UPVALUE: {
            closeUpvalues(vm.sp - 1);
            pop();
            break;
        }
        case OP_CLASS: {
            push(OBJ_VAL(newClass(READ_STRING())));
            break;
        }
        case OP_INHERIT: {
            Value superclass = peek(1);
            if (!IS_CLASS(superclass)) {
                runtimeError("Superclass must be a class.");
                return INTERPRET_RUNTIME_ERROR;
            }
            ObjClass* subclass = AS_CLASS(peek(0));
            tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);
            pop(); // subclass
            break;
        }
        case OP_METHOD: {
            defineMethod(READ_STRING());
            break;
        }
        case OP_RETURN: {
            Value result = pop();
            closeUpvalues(frame->slots);
            vm.frame_count--;
            if (vm.frame_count == 0) {
                pop();
                return INTERPRET_OK;
            }

            vm.sp = frame->slots;
            push(result);
            frame = &vm.frames[vm.frame_count - 1];
            break;
        }
        }
    }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);
    if (function == NULL) {
        return INTERPRET_COMPILE_ERROR;
    }

    push(OBJ_VAL(function));
    ObjClosure* closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);

    return run();
}