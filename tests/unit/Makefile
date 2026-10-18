# Unit tests built with the PC's compiler (pure C parts of src/, and the
# Python ROM converter). See DEVNOTES.md.

CFLAGS := -std=c11 -Wall -Wextra -Werror -O1 -g -fsanitize=address,undefined
BUILD := build

all: test

$(BUILD)/test_states: test_states.c ../../src/states.c ../../src/states.h
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -o $@ test_states.c ../../src/states.c

test: $(BUILD)/test_states
	$(BUILD)/test_states

clean:
	rm -rf $(BUILD)

.PHONY: all test clean
