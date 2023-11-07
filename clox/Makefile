build: src/*.c src/*.h
	cc -o clox -Wall -Wextra -pedantic-errors -O3 src/*.c

debug: src/*.c src/*.h
	cc -o clox -Wall -Wextra -pedantic-errors -g -DDEBUG_PRINT_CODE -DDEBUG_TRACE_EXECUTION src/*.c

fmt: src/*.c src/*.h
	fd '.*\.(c|h)' | xargs clang-format --verbose -i 
