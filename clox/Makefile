BIN_NAME=clox

SOURCE_DIR=src
HEADER_DIR=include
BUILD_DIR=target
TEST_DIR=tests

OBJECTS=$(patsubst $(SOURCE_DIR)/%.c,%.o,$(wildcard $(SOURCE_DIR)/*.c))

RELEASE_DIR=$(BUILD_DIR)/release
PROFILE_DIR=$(BUILD_DIR)/profile
DEBUG_DIR=$(BUILD_DIR)/debug

RELEASE_OBJECTS=$(addprefix $(RELEASE_DIR)/, $(OBJECTS))
PROFILE_OBJECTS=$(addprefix $(PROFILE_DIR)/, $(OBJECTS))
DEBUG_OBJECTS=$(addprefix $(DEBUG_DIR)/, $(OBJECTS))

CFLAGS=-Wall -Wextra -Werror -I $(HEADER_DIR) -lm
ifndef NO_NAN_BOXING
	CFLAGS += -DNAN_BOXING
endif
RELEASE_FLAGS=-O2
PROFILE_FLAGS=-O2 -g
DEBUG_FLAGS=-g -DDEBUG_MODE

.PHONY: release profile debug
release: RELEASE
profile: PROFILE
debug: DEBUG

.PHONY: all
all: release profile debug

define build_target
$1: $$($(1)_OBJECTS)
	cc $$(CFLAGS) $$($1_FLAGS) -o $$($1_DIR)/$(BIN_NAME) $$^
endef

define build_object
$$($1_DIR)/%.o: $$(SOURCE_DIR)/%.c | $$($1_DIR)
	cc $$(CFLAGS) $$($1_FLAGS) -c $$< -o $$@
endef

$(eval $(call build_target,RELEASE))
$(eval $(call build_target,PROFILE))
$(eval $(call build_target,DEBUG))

$(eval $(call build_object,RELEASE))
$(eval $(call build_object,PROFILE))
$(eval $(call build_object,DEBUG))

$(RELEASE_DIR) $(PROFILE_DIR) $(DEBUG_DIR):
	mkdir -p $@

.PHONY: fmt
fmt:
	fd -e 'c' -e 'h' | xargs clang-format --verbose -i 

.PHONY: clean
clean: 
	rm -rf $(BUILD_DIR)

.PHONY: run
run: release 
	./$(RELEASE_DIR)/$(BIN_NAME)

.PHONY: rund
rund: debug 
	./$(DEBUG_DIR)/$(BIN_NAME)
