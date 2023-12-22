#include <math.h>
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array); // zero out old value array
}

void writeValueArray(ValueArray* array, Value value) {
    if (array->capacity <= array->count) {
        int old_capacity = array->capacity;
        array->capacity = GROW_CAPACITY(old_capacity);
        array->values = GROW_ARRAY(Value, array->values, old_capacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void printValue(FILE* restrict stream, Value value) {
#ifdef NAN_BOXING
    if (IS_BOOL(value)) {
        fprintf(stream, AS_BOOL(value) ? "true" : "false");
    } else if (IS_NIL(value)) {
        fprintf(stream, "nil");
    } else if (IS_NUMBER(value)) {
        double number = AS_NUMBER(value);
        if (fmod(number, 1) == 0) {
            fprintf(stream, "%.0f", number);
        } else {
            fprintf(stream, "%g", number);
        }
    } else if (IS_OBJ(value)) {
        printObject(stream, value);
    }
#else
    switch (value.type) {
    case VAL_BOOL: fprintf(stream, AS_BOOL(value) ? "true" : "false"); break;
    case VAL_NIL: fprintf(stream, "nil"); break;
    case VAL_NUMBER: {
        double number = AS_NUMBER(value);
        // Check if the value is integer-like
        if (fmod(number, 1) == 0) {
            fprintf(stream, "%.0f", number);
        } else {
            fprintf(stream, "%g", number);
        }
        break;
    }
    case VAL_OBJ: printObject(stream, value); break;
    }
#endif /* ifdef NAN_BOXING */
}

bool valuesEqual(Value a, Value b) {
#ifdef NAN_BOXING
    if (IS_NUMBER(a) && IS_NUMBER(b)) {
        return AS_NUMBER(a) == AS_NUMBER(b);
    }
    return a == b;
#else
    if (a.type != b.type) {
        return false;
    }

    switch (a.type) {
    case VAL_BOOL: return AS_BOOL(a) == AS_BOOL(b);
    case VAL_NIL: return true;
    case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
    // We get to compare strings using `==` instead of `memcmp`
    // because we intern all strings.
    case VAL_OBJ: return AS_OBJ(a) == AS_OBJ(b);
    default: return false; //__builtin_unreachable();
    }
#endif /* ifdef NAN_BOXING */
}
