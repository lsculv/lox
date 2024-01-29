#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "vm.h"

static void repl(void) {
    char line[1024];
    for (;;) {
        printf("> ");

        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }

        interpret(line);
    }
}

static char* readFile(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        eprintf("Error: Could not open file '%s': %s.\n", path, strerror(errno));
        exit(74);
    }

    // get the length of the stream and then place the read cursor back to the start,
    // checking for errors.
    if (fseek(file, 0L, SEEK_END) != 0) {
        eprintf("Error: Could not seek on file '%s': %s", path, strerror(errno));
        exit(74);
    }

    long fileSize = ftell(file);
    if (fileSize == -1) {
        eprintf("Error: Could not get length of file '%s': %s", path, strerror(errno));
    }

    if (fseek(file, 0L, SEEK_SET) != 0) {
        eprintf("Error: Could not seek on file '%s': %s", path, strerror(errno));
        exit(74);
    }

    // add one to include the null terminator
    // this memory will be freed by the caller
    char* buffer = (char*)malloc((size_t)fileSize + 1);
    if (buffer == NULL) {
        eprintf("Error: Failed to allocate memory to read file '%s': %s.\n", path,
                strerror(errno));
        exit(74);
    }

    size_t bytesRead = fread(buffer, sizeof(char), (size_t)fileSize, file);
    if (bytesRead < (size_t)fileSize) {
        eprintf("Error: Could not read file '%s': '%s'.\n", path, strerror(errno));
    }
    buffer[bytesRead] = '\0';

    fclose(file);
    return buffer;
}

static void runFile(const char* path) {
    char* source = readFile(path);
    InterpretResult result = interpret(source);
    free(source);

    if (result == INTERPRET_COMPILE_ERROR) {
        exit(65);
    }
    if (result == INTERPRET_RUNTIME_ERROR) {
        exit(70);
    }
}

int main(int argc, const char* argv[]) {
    initVM();

    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        runFile(argv[1]);
    } else {
        eprintf("Usage: clox [path]\n");
        exit(64);
    }

    freeVM();
    return 0;
}
