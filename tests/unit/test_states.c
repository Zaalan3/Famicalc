/* Unit tests for src/states.c (slot names, state header), built with the PC's
 * compiler: make -C tests/unit */
#include <stdio.h>
#include <string.h>

#include "../../src/states.h"

static int failures, checks;

#define CHECK(cond, ...) do { \
	checks++; \
	if (!(cond)) { \
		failures++; \
		printf("FAIL %s:%d: ", __FILE__, __LINE__); \
		printf(__VA_ARGS__); \
		printf("\n"); \
	} \
} while (0)

static int valid_name_char(char c) {
	return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

static void test_suffixes(void) {
	static char seen[NUM_SLOTS][3];
	char s[3];

	for (unsigned int slot = 0; slot < NUM_SLOTS; slot++) {
		slot_suffix(slot, s);
		CHECK(strlen(s) == 2, "slot %u: suffix '%s' is not 2 characters", slot, s);
		CHECK(valid_name_char(s[0]) && valid_name_char(s[1]), "slot %u: bad characters '%s'", slot, s);
		CHECK(suffix_slot(s) == (int)slot, "slot %u -> '%s' -> %d", slot, s, suffix_slot(s));
		// a ROM's own files: NAME, NAMEP0-PF, NAMEC0-CF, NAMESV
		CHECK(!(s[0] == 'P' || s[0] == 'C' || !strcmp(s, "SV")), "slot %u: '%s' clashes with ROM files", slot, s);
		for (unsigned int other = 0; other < slot; other++)
			CHECK(strcmp(seen[other], s), "slots %u and %u share '%s'", other, slot, s);
		memcpy(seen[slot], s, 3);
	}
}

static void test_known_names(void) {
	char s[3];
	slot_suffix(0, s);
	CHECK(!strcmp(s, "S0"), "slot 0 is '%s' (old FamiCalc states are NAMES0)", s);
	slot_suffix(9, s);
	CHECK(!strcmp(s, "S9"), "slot 9 is '%s'", s);
	slot_suffix(10, s);
	CHECK(!strcmp(s, "10"), "slot 10 is '%s'", s);
	slot_suffix(99, s);
	CHECK(!strcmp(s, "99"), "slot 99 is '%s'", s);
	slot_suffix(100, s);
	CHECK(!strcmp(s, "a0"), "slot 100 is '%s'", s);
	slot_suffix(999, s);
	CHECK(!strcmp(s, "yz"), "slot 999 is '%s'", s);
}

static void test_not_states(void) {
	const char *names[] = { "SV", "P0", "PF", "C0", "CF", "S", "", "SAA", "00", "05", "zz", "z0",
	                        "A0", "S:", "1a", "Ab" };
	for (unsigned int i = 0; i < sizeof names / sizeof *names; i++)
		CHECK(suffix_slot(names[i]) == -1, "'%s' parsed as slot %d", names[i], suffix_slot(names[i]));
}

static void test_header(void) {
	struct state_header h;
	CHECK(sizeof h == STATE_HEADER_SIZE, "header is %zu bytes", sizeof h);
	state_header_init(&h, 59, 41, 23, 17, 10, 2026);
	CHECK(state_has_header(&h), "initialized header not recognized");
	CHECK(state_header_year(&h) == 2026, "year %u", state_header_year(&h));
	CHECK(h.hour == 23 && h.minute == 41 && h.second == 59 && h.day == 17 && h.month == 10, "time fields");
	CHECK(h.version == STATE_VERSION && h.flags == 0, "version/flags");

	// old states start with an LZ literal run: 0x00-0x7F
	for (int b = 0; b < 0x80; b++) {
		unsigned char old[3] = { (unsigned char)b, 'S', 'T' };
		CHECK(!state_has_header(old), "old state starting with %02X taken for a header", b);
	}
}

int main(void) {
	test_suffixes();
	test_known_names();
	test_not_states();
	test_header();
	printf("%s: %d checks, %d failed\n", failures ? "FAIL" : "PASS", checks, failures);
	return failures != 0;
}
