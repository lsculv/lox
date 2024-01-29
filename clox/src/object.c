#include <stdio.h>
#include <string.h>

#include "common.h"
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) ((type*)allocateObject(sizeof(type), objectType))

static Obj* allocateObject(size_t size, ObjType type) {
    // This doesn't just allocate sizeof(Obj) as we need extra room for the payload
    // that any Obj can contain. For example, an ObjString as extra fields like `length`
    // that we need to make room for.
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;
    object->is_marked = false;

    // Insert our newly allocated object into the front of the objects list.
    object->next = vm.objects;
    vm.objects = object;

#ifdef DEBUG_LOG_GC
    eprintf("%p allocate %zu for %d\n", (void*)object, size, type);
#endif
    return object;
}

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method) {
    ObjBoundMethod* bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

ObjClass* newClass(ObjString* name) {
    ObjClass* class_ = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    class_->name = name;
    initTable(&class_->methods);
    return class_;
}

ObjInstance* newInstance(ObjClass* class_) {
    ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->class_ = class_;
    initTable(&instance->fields);
    return instance;
}

ObjClosure* newClosure(ObjFunction* function) {
    ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalue_count);
    for (int i = 0; i < function->upvalue_count; i++) {
        upvalues[i] = NULL;
    }

    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalue_count = function->upvalue_count;
    return closure;
}

ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalue_count = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjNative* newNative(NativeFn function) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATVIE);
    native->function = function;
    return native;
}

static ObjString* allocateString(char* chars, uint32_t length, uint32_t hash) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

    push(OBJ_VAL(string));
    tableSet(&vm.strings, string, NIL_VAL());
    pop();

    return string;
}

static uint32_t hashString(const char* key, uint32_t length) {
    uint32_t hash = 216613626lu;
    for (uint32_t i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

/// Owns the underlying memory in the `chars` parameter.
ObjString* takeString(char* chars, uint32_t length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }
    return allocateString(chars, length, hash);
}

/// This function assumes that it cannot take ownership of the underlying memory.
ObjString* copyString(const char* chars, uint32_t length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        return interned;
    }
    // Allocated string on heap with enough space for the chars and the trailing '\0'.
    char* heapChars = ALLOCATE(char, length + 1);
    // Copy the chars passed in to the heap.
    // This is so an ObjString owns the underlying memory and we can free it later
    // without freeing the characters in the source code string.
    memcpy(heapChars, chars, length);
    // Add the null terminator.
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

ObjUpvalue* newUpvalue(Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->closed = NIL_VAL();
    upvalue->location = slot;
    upvalue->next = NULL;
    return upvalue;
}

static void printFunction(FILE* restrict stream, ObjFunction* function) {
    if (function->name == NULL) {
        fprintf(stream, "<script>");
        return;
    }
    fprintf(stream, "<fun %s>", function->name->chars);
}

void printObject(FILE* restrict stream, Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_CLOSURE: {
        printFunction(stream, AS_CLOSURE(value)->function);
        break;
    }
    case OBJ_FUNCTION: printFunction(stream, AS_FUNCTION(value)); break;
    case OBJ_STRING: fprintf(stream, "%s", AS_CSTRING(value)); break;
    case OBJ_NATVIE: fprintf(stream, "<native fn>"); break;
    case OBJ_UPVALUE: fprintf(stream, "upvalue"); break;
    case OBJ_CLASS: fprintf(stream, "%s", AS_CLASS(value)->name->chars); break;
    case OBJ_INSTANCE:
        fprintf(stream, "%s instance", AS_INSTANCE(value)->class_->name->chars);
        break;
    case OBJ_BOUND_METHOD: {
        printFunction(stream, AS_BOUND_METHOD(value)->method->function);
        break;
    };
        default: __builtin_unreachable();
    }
}
