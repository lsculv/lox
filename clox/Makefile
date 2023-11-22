SOURCE_DIR=src
BUILD_DIR=target
TEST_DIR=tests

RELEASE_DIR=$(BUILD_DIR)/release
DEBUG_DIR=$(BUILD_DIR)/debug

OBJECTS=chunk.o compiler.o debug.o main.o memory.o object.o scanner.o table.o value.o vm.o 

RELEASE_TARGETS=$(addprefix $(RELEASE_DIR)/, $(OBJECTS))
DEBUG_TARGETS=$(addprefix $(DEBUG_DIR)/, $(OBJECTS))

CFLAGS=-Wall -Wextra -Werror -lm
RELEASE_FLAGS=-O2
DEBUG_FLAGS=-g -DDEBUG_MODE

.PHONY: all
all: release 

release: $(RELEASE_TARGETS)
	cc $(CFLAGS) $(RELEASE_FLAGS) -o $(RELEASE_DIR)/clox $^

debug: $(DEBUG_TARGETS)
	cc $(CFLAGS) $(DEBUG_FLAGS) -o $(DEBUG_DIR)/clox $^

$(RELEASE_DIR)/%.o: $(SOURCE_DIR)/%.c | $(RELEASE_DIR)
	cc $(CFLAGS) $(RELEASE_FLAGS) -c $< -o $@

$(DEBUG_DIR)/%.o: $(SOURCE_DIR)/%.c | $(DEBUG_DIR)
	cc $(CFLAGS) $(DEBUG_FLAGS) -c $< -o $@

$(RELEASE_DIR):
	mkdir -p $@

$(DEBUG_DIR):
	mkdir -p $@

.PHONY: fmt
fmt:
	fd -e 'c' -e 'h' | xargs clang-format --verbose -i 

.PHONY: clean
clean: 
	rm -rf $(BUILD_DIR)

.PHONY: run
run: release 
	./$(RELEASE_DIR)/clox

.PHONY: rund
rund: debug 
	./$(DEBUG_DIR)/clox
