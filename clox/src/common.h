#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Debug Defines
#ifdef DEBUG_MODE
#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION
#define DEBUG_STRESS_GC
#define DEBUG_LOG_GC
#endif

#define UINT8_COUNT (UINT8_MAX + 1)
#define eprintf(format, ...) fprintf(stderr, format __VA_OPT__(, ) __VA_ARGS__)
#define UNUSED(value) ((void)(value))

#endif
