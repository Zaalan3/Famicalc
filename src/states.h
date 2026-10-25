#ifndef STATES_H
#define STATES_H

#include <stdbool.h>
#include <stdint.h>

/*
	Save state slots and the save state file header.
	Plain C without calculator headers, so tests/unit can build it on a PC.
*/

#define NUM_SLOTS 1000		// slots 0..999
#define AUTO_SLOT 1000		// the auto save (Save on quit), after them

/*
	A state is an appvar named ROMNAME + a two character suffix:
	  slots 0-9      S0 .. S9   (the names FamiCalc has always used)
	  slots 10-99    10 .. 99
	  slots 100-999  a0 .. yz   (lowercase letter + digit or lowercase letter)
	  the auto save  AU
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
	uint8_t flags;			// STATE_PICTURE
	uint8_t second, minute, hour;
	uint8_t day, month;
	uint8_t year_lo, year_hi;	// 0 if the clock wasn't set
	uint8_t picture[3];		// where the picture starts in the file (24 bits)
	uint8_t reserved;
};

/*
	A picture of the game (pictures.h) may follow the compressed state:
	FamiCalc 0.3 loads such files too (it reads only the compressed data).
*/
#define STATE_PICTURE 0x01
void state_header_set_picture(struct state_header *h, uint32_t offset);
// the picture's offset in the file, 0: none
uint32_t state_header_picture(const struct state_header *h);

void state_header_init(struct state_header *h, uint8_t second, uint8_t minute, uint8_t hour,
                       uint8_t day, uint8_t month, unsigned int year);
bool state_has_header(const void *data);
unsigned int state_header_year(const struct state_header *h);
// the header has a plausible date (the clock was set)
bool state_header_dated(const struct state_header *h);
// a number that grows with the header's date and time (to the minute), 0 if undated
uint32_t state_header_stamp(const struct state_header *h);
// the date and time of a stamp back in a header
void state_stamp_header(uint32_t stamp, struct state_header *h);
// "Oct 17, 2026" and "11:41 PM" (buffers of 13 and 9 characters)
void state_date_text(const struct state_header *h, char *out);
void state_time_text(const struct state_header *h, char *out);

#endif
