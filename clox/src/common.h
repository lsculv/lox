#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION

#define eprintf(format, ...) fprintf(stderr, format __VA_OPT__(, ) __VA_ARGS__)

#endif
