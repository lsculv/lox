#include <stdio.h>
#include <string.h>

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

    // Insert our newly allocated object into the front of the objects list.
    object->next = vm.objects;
    vm.objects = object;
    return object;
}

ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjNative* newNative(NativeFn function) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATVIE);
    native->function = function;
    return native;
}

static ObjString* allocateString(char* chars, int length, uint32_t hash) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    tableSet(&vm.strings, string, NIL_VAL());
    return string;
}

static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 216613626lu;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

/// Owns the underlying memory in the `chars` parameter.
ObjString* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }
    return allocateString(chars, length, hash);
}

/// This function assumes that it cannot take ownership of the underlying memory.
ObjString* copyString(const char* chars, int length) {
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

static void printFunction(FILE* restrict stream, ObjFunction* function) {
    if (function->name == NULL) {
        fprintf(stream, "<script>");
        return;
    }
    fprintf(stream, "<fun %s>", function->name->chars);
}

void printObject(FILE* restrict stream, Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_FUNCTION: printFunction(stream, AS_FUNCTION(value)); break;
    case OBJ_STRING: fprintf(stream, "%s", AS_CSTRING(value)); break;
    case OBJ_NATVIE: fprintf(stream, "<native fn>"); break;
    }
}
