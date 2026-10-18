# PC test harness: CEmu's core (built here with its debugger support, for the
# execution counters) + fc-headless. See DEVNOTES.md, "PC testing".
#
#   make            build build/fc-headless
#   make test       run the PC tests (needs FC_ROM, see fctest.py)

CEMU ?= $(HOME)/ti84ce/build/tools/CEmu
BUILD := build

CORE_SRC := $(filter-out $(CEMU)/core/os/os-emscripten.c $(CEMU)/core/os/os-win32.c, \
              $(wildcard $(CEMU)/core/*.c $(CEMU)/core/os/*.c $(CEMU)/core/usb/*.c \
                         $(CEMU)/core/debug/*.c $(CEMU)/core/debug/zdis/*.c))
CORE_OBJ := $(patsubst $(CEMU)/core/%.c,$(BUILD)/core/%.o,$(CORE_SRC))

CFLAGS := -O2 -std=gnu11 -fPIC -DDEBUG_SUPPORT -w
CXXFLAGS := -O2 -std=c++11 -Wall -Wextra -Wno-unused-parameter -DDEBUG_SUPPORT \
            -I$(CEMU) -I$(CEMU)/tests/autotester

all: $(BUILD)/fc-headless

$(BUILD)/core/%.o: $(CEMU)/core/%.c
	@mkdir -p $(dir $@)
	gcc $(CFLAGS) -c $< -o $@

$(BUILD)/libcemucore-debug.a: $(CORE_OBJ)
	ar rcs $@ $^

$(BUILD)/autotester.o: $(CEMU)/tests/autotester/autotester.cpp
	@mkdir -p $(BUILD)
	g++ $(CXXFLAGS) -DGLOB_SUPPORT -c $< -o $@

$(BUILD)/fc-headless.o: fc-headless.cpp
	@mkdir -p $(BUILD)
	g++ $(CXXFLAGS) -c $< -o $@

$(BUILD)/fc-headless: $(BUILD)/fc-headless.o $(BUILD)/autotester.o $(BUILD)/libcemucore-debug.a
	g++ -o $@ $^ -lpthread

test: all
	python3 fctest.py all

clean:
	rm -rf $(BUILD)

.PHONY: all test clean
