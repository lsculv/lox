build: src/*.c src/*.h
	cc -o clox -Wall -Wextra -pedantic-errors -g src/*.c

fmt: src/*.c src/*.h
	fd '.*\.(c|h)' | xargs clang-format --verbose -i 
