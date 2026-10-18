#ifndef STATES_H
#define STATES_H

#include <stdbool.h>
#include <stdint.h>

/*
	Save state slots and the save state file header.
	Plain C without calculator headers, so tests/unit can build it on a PC.
*/

#define NUM_SLOTS 1000		// slots 0..999

/*
	A state is an appvar named ROMNAME + a two character suffix:
	  slots 0-9      S0 .. S9   (the names FamiCalc has always used)
	  slots 10-99    10 .. 99
	  slots 100-999  a0 .. yz   (lowercase letter + digit or lowercase letter)
	ROM names have at most 6 characters, so the name fits in 8. None of these
	suffixes is used by a ROM's own files (P0-PF, C0-CF, SV).
*/
void slot_suffix(unsigned int slot, char out[3]);
// suffix -> slot, or -1 if it isn't a state suffix
int suffix_slot(const char *suffix);

/*
	State file layout:
	  16 byte header (states written by FamiCalc 0.3 and later), then
	  the LZ compressed state (src/savestate.asm).
	Older states have no header. Their first byte is an LZ literal run
	(0x00-0x7F), never 0xFC.
*/
#define STATE_HEADER_SIZE 16
#define STATE_VERSION 1

struct state_header {
	uint8_t magic[3];		// FC 'S' 'T'
	uint8_t version;		// STATE_VERSION
	uint8_t flags;			// reserved, 0
	uint8_t second, minute, hour;
	uint8_t day, month;
	uint8_t year_lo, year_hi;	// 0 if the clock wasn't set
	uint8_t reserved[4];
};

void state_header_init(struct state_header *h, uint8_t second, uint8_t minute, uint8_t hour,
                       uint8_t day, uint8_t month, unsigned int year);
bool state_has_header(const void *data);
unsigned int state_header_year(const struct state_header *h);

#endif
