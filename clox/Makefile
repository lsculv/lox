BUILDDIR=build
TARGETS=$(BUILDDIR)/chunk.o $(BUILDDIR)/compiler.o $(BUILDDIR)/debug.o $(BUILDDIR)/main.o $(BUILDDIR)/memory.o $(BUILDDIR)/object.o $(BUILDDIR)/scanner.o $(BUILDDIR)/table.o $(BUILDDIR)/value.o $(BUILDDIR)/vm.o 
SOURCEDIR=src
CFLAGS=-Wall -Wextra -lm
RELEASE_FLAGS=-O3
DEBUG_FLAGS=-g -DDEBUG_PRINT_CODE -DDEBUG_TRACE_EXECUTION

.PHONY: all
all: clox

clox: $(TARGETS)
	cc $(CFLAGS) $(RELEASE_FLAGS) -o $(BUILDDIR)/$@ $^

debug: $(TARGETS)
	cc $(CFLAGS) $(DEBUG_FLAGS) -o $(BUILDDIR)/clox $^

$(BUILDDIR)/%.o: $(SOURCEDIR)/%.c | $(BUILDDIR)
	cc $(CFLAGS) -c $< -o $@

$(BUILDDIR):
	mkdir $(BUILDDIR)

.PHONY: fmt
fmt:
	fd '.*\.(c|h)' | xargs clang-format --verbose -i 

.PHONY: clean
clean: 
	rm -rf $(BUILDDIR)

