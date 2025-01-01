#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include "debug.h"
#include <stdio.h>
#endif

#define GC_HEAP_GROWTH_FACTOR 2

void* reallocate(void* mem, size_t old_size, size_t new_size) {
    vm.bytes_allocated += new_size - old_size;
    if (new_size > old_size) {
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif
        if (vm.bytes_allocated > vm.next_gc) {
            collectGarbage();
        }
    }

    if (new_size == 0) {
        free(mem);
        return NULL;
    }

    void* result = realloc(mem, new_size);
    if (result == NULL) {
        eprintf("Error: `reallocate` function failed to realloc memory.\n{ "
                "mem: %p, old_size: %lu, new_size: %lu }",
                mem, old_size, new_size);
        exit(1);
    }
    return result;
}

void markObject(Obj* object) {
    if (object == NULL) {
        return;
    }
    if (object->is_marked) {
        return;
    }

#ifdef DEBUG_LOG_GC
    eprintf("%p mark ", (void*)object);
    printValue(stderr, OBJ_VAL(object));
    eprintf("\n");
#endif
    object->is_marked = true;

    if (vm.gray_capacity <= vm.gray_count) {
        vm.gray_capacity = GROW_CAPACITY(vm.gray_capacity);
        // Note that we use the system `realloc` instead of our `reallocate` function
        // we would normally use for a dynamic array. This is so we don't recursively
        // initiate a GC while doing a GC. That would be bad.
        vm.gray_stack =
            (Obj**)realloc(vm.gray_stack, sizeof(Obj*) * (size_t)vm.gray_capacity);

        // This would be bad
        if (vm.gray_stack == NULL) {
            eprintf("The world is ending... stopping now.\n");
            exit(1);
        }
    }

    vm.gray_stack[vm.gray_count++] = object;
}

void markValue(Value value) {
    if (IS_OBJ(value)) {
        markObject(AS_OBJ(value));
    }
}

static void markArray(ValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}

static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    eprintf("%p blacken ", (void*)object);
    printValue(stderr, OBJ_VAL(object));
    eprintf("\n");
#endif
    switch (object->type) {
    case OBJ_NATIVE:
    case OBJ_STRING: break;
    case OBJ_UPVALUE: {
        markValue(((ObjUpvalue*)object)->closed);
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* function = (ObjFunction*)object;
        markObject((Obj*)function->name);
        markArray(&function->chunk.constants);
        break;
    }
    case OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*)object;
        markObject((Obj*)closure->function);
        for (int i = 0; i < closure->upvalue_count; i++) {
            markObject((Obj*)closure->upvalues[i]);
        }
        break;
    }
    case OBJ_CLASS: {
        ObjClass* class_ = (ObjClass*)object;
        markObject((Obj*)class_->name);
        markTable(&class_->methods);
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*)object;
        markObject((Obj*)instance->class_);
        markTable(&instance->fields);
        break;
    }
    case OBJ_BOUND_METHOD: {
        ObjBoundMethod* bound = (ObjBoundMethod*)object;
        markValue(bound->receiver);
        markObject((Obj*)bound->method);
        break;
    }
    default: __builtin_unreachable();
    }
}

static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    eprintf("%p free type %d\n", (void*)object, object->type);
#endif

    switch (object->type) {
    case OBJ_CLOSURE: {
        ObjClosure* closure = (ObjClosure*)object;
        FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalue_count);
        FREE(ObjClosure, object);
        break;
    }
    case OBJ_FUNCTION: {
        ObjFunction* function = (ObjFunction*)object;
        freeChunk(&function->chunk);
        FREE(ObjFunction, object);
        break;
    }
    case OBJ_STRING: {
        ObjString* string = (ObjString*)object;
        FREE_ARRAY(char, string->chars, string->length + 1);
        FREE(ObjString, object);
        break;
    }
    case OBJ_NATIVE: {
        FREE(ObjNative, object);
        break;
    }
    case OBJ_UPVALUE: {
        FREE(ObjUpvalue, object);
        break;
    }
    case OBJ_CLASS: {
        ObjClass* class_ = (ObjClass*)object;
        freeTable(&class_->methods);
        FREE(ObjClass, object);
        break;
    }
    case OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*)object;
        freeTable(&instance->fields);
        FREE(ObjInstance, object);
        break;
    }
    case OBJ_BOUND_METHOD: {
        FREE(ObjBoundMethod, object);
        break;
    }
    default: __builtin_unreachable();
    }
}

static void markRoots(void) {
    for (Value* slot = vm.stack; slot < vm.sp; slot++) {
        markValue(*slot);
    }

    for (int i = 0; i < vm.frame_count; i++) {
        markObject((Obj*)vm.frames[i].closure);
    }

    for (ObjUpvalue* upvalue = vm.open_upvalues; upvalue != NULL;
         upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
    }

    markTable(&vm.globals);
    markCompilerRoots();
    markObject((Obj*)vm.initString);
}

static void traceReferences(void) {
    while (vm.gray_count > 0) {
        Obj* object = vm.gray_stack[--vm.gray_count];
        blackenObject(object);
    }
}

static void sweep(void) {
    Obj* previous = NULL;
    Obj* object = vm.objects;
    while (object != NULL) {
        if (object->is_marked) {
            object->is_marked = false;
            previous = object;
            object = object->next;
        } else {
            Obj* unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
    eprintf("-- gc begin\n");
    size_t before = vm.bytes_allocated;
#endif

    markRoots();
    traceReferences();
    tableRemoveWhite(&vm.strings);
    sweep();

    vm.next_gc = vm.bytes_allocated * GC_HEAP_GROWTH_FACTOR;

#ifdef DEBUG_LOG_GC
    eprintf("-- gc end\n");
    eprintf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
            before - vm.bytes_allocated, before, vm.bytes_allocated, vm.next_gc);
#endif
}

void freeObjects() {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }

    free(vm.gray_stack);
}
