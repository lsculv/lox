#include <stdio.h>
#include <stdlib.h>

#include "memory.h"
#include "vm.h"

void* reallocate(void* mem, size_t old_size, size_t new_size) {
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

static void freeObject(Obj* object) {
    switch (object->type) {
    case OBJ_STRING: {
        ObjString* string = (ObjString*)object;
        FREE_ARRAY(char, string->chars, string->length + 1);
        FREE(ObjString, object);
        break;
    }
    }
}

void freeObjects() {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }
}
