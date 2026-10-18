#include <string.h>

#include "states.h"

static const char base36[] = "0123456789abcdefghijklmnopqrstuvwxyz";

void slot_suffix(unsigned int slot, char out[3]) {
	if (slot < 10) {
		out[0] = 'S';
		out[1] = '0' + slot;
	} else if (slot < 100) {
		out[0] = '0' + slot / 10;
		out[1] = '0' + slot % 10;
	} else {
		slot -= 100;
		out[0] = 'a' + slot / 36;
		out[1] = base36[slot % 36];
	}
	out[2] = 0;
}

int suffix_slot(const char *suffix) {
	char a, b;
	const char *pos;

	if (!suffix[0] || !suffix[1] || suffix[2])
		return -1;
	a = suffix[0];
	b = suffix[1];
	if (a == 'S')
		return b >= '0' && b <= '9' ? b - '0' : -1;
	if (a >= '1' && a <= '9')
		return b >= '0' && b <= '9' ? (a - '0') * 10 + (b - '0') : -1;
	if (a >= 'a' && a <= 'y' && (pos = strchr(base36, b))) {
		int slot = 100 + (a - 'a') * 36 + (int)(pos - base36);
		return slot < NUM_SLOTS ? slot : -1;
	}
	return -1;
}

void state_header_init(struct state_header *h, uint8_t second, uint8_t minute, uint8_t hour,
                       uint8_t day, uint8_t month, unsigned int year) {
	memset(h, 0, sizeof *h);
	h->magic[0] = 0xFC;
	h->magic[1] = 'S';
	h->magic[2] = 'T';
	h->version = STATE_VERSION;
	h->second = second;
	h->minute = minute;
	h->hour = hour;
	h->day = day;
	h->month = month;
	h->year_lo = year & 0xFF;
	h->year_hi = year >> 8;
}

bool state_has_header(const void *data) {
	const uint8_t *d = data;
	return d[0] == 0xFC && d[1] == 'S' && d[2] == 'T';
}

unsigned int state_header_year(const struct state_header *h) {
	return h->year_lo | (unsigned int)h->year_hi << 8;
}
