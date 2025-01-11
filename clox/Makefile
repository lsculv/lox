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

$(RELEASE_OBJECTS): | $(RELEASE_DIR)
$(PROFILE_OBJECTS): | $(PROFILE_DIR)
$(DEBUG_OBJECTS): | $(DEBUG_DIR)

DEPENDENCY_FILES=$(patsubst %.o,%.d,$(RELEASE_OBJECTS) $(PROFILE_OBJECTS) $(DEBUG_OBJECTS))

CFLAGS=-I $(HEADER_DIR) -lm \
	-Wall -Wextra -Wpedantic -Wformat=2 -Wformat-overflow=2 \
	-Wformat-truncation=2 -Wformat-security -Wnull-dereference \
	-Wstack-protector -Wtrampolines -Walloca -Wvla \
	-Warray-bounds=2 -Wdouble-promotion -Wshift-overflow=2 \
	-Wcast-qual -Wstringop-overflow=4 -Wconversion \
	-Wlogical-op -Wduplicated-cond -Wduplicated-branches \
	-Wformat-signedness -Wshadow -Wstrict-overflow=2 -Wundef \
	-Wstrict-prototypes -Wstack-usage=1000000 \
	-Wcast-align=strict -D_FORTIFY_SOURCE=3 -fstack-protector-strong \
	-fstack-clash-protection -fPIE -fsanitize=bounds \
	-fsanitize-undefined-trap-on-error -Wl,-z,relro -Wl,-z,now \
	-Wl,-z,noexecstack -Wl,-z,separate-code
ifndef NO_NAN_BOXING
	CFLAGS += -DNAN_BOXING
endif
RELEASE_FLAGS=-O3
PROFILE_FLAGS=-O3 -gdwarf
DEBUG_FLAGS=-Og -ggdb -fsanitize=undefined -DDEBUG_MODE
ifdef ASAN
	DEBUG_FLAGS += -fsanitize=address
endif

.PHONY: release profile debug
release: RELEASE
profile: PROFILE
debug: DEBUG

.PHONY: all
all: release profile debug

-include $(DEPENDENCY_FILES)

define build_target
$1: $$($(1)_OBJECTS)
	cc $$(CFLAGS) $$($1_FLAGS) -o $$($1_DIR)/$(BIN_NAME) $$^
endef

define build_object
$$($1_DIR)/%.o: $$(SOURCE_DIR)/%.c | $$($1_DIR)
	cc $$(CFLAGS) $$($1_FLAGS) -MMD -MP -c $$< -o $$@
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
	find . -name '*.c' -o -name '*.h' | xargs clang-format --verbose -i 

.PHONY: clean
clean: 
	rm -rf $(BUILD_DIR)

.PHONY: run
run: release 
	./$(RELEASE_DIR)/$(BIN_NAME)

.PHONY: rund
rund: debug 
	./$(DEBUG_DIR)/$(BIN_NAME)

