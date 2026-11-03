#include <stdint.h>
#include <string.h>
#include <fileioc.h>

#include "menu.h"
#include "pictures.h"

#define LCD_PALETTE ((volatile uint16_t *)0xE30200)

// pic_capture: ui.asm

static void load_colors(const uint8_t *colors) {
	memcpy((void *)LCD_PALETTE, colors, 2 * PIC_COLORS);
}

void pic_colors(const struct picture *pic) {
	load_colors((const uint8_t *)pic->palette);
}

void pic_draw(const struct picture *pic, uint8_t *dest) {
	const uint8_t *p = pic->pixels;
	load_colors((const uint8_t *)pic->palette);
	for (unsigned int y = 0; y < PIC_H; y++, p += PIC_W, dest += 320)
		memcpy(dest, p, PIC_W);
}

/* Stored pictures: the colors (PIC_COLORS little endian words), then runs:
   0x00-0x9F a pixel, 0xA0-0xFF the last pixel again 2-97 times. */
#define RUNS 0xA0

static ti_var_t out;
static unsigned int out_size, chunk_n;
static uint8_t chunk[64];

static void put(uint8_t token) {
	out_size++;
	if (!out)
		return;
	chunk[chunk_n++] = token;
	if (chunk_n == sizeof chunk) {
		ti_Write(chunk, sizeof chunk, 1, out);
		chunk_n = 0;
	}
}

// the last pixel again n more times
static void repeat(uint8_t last, unsigned int n) {
	if (n == 1)
		put(last);
	else if (n)
		put(RUNS - 2 + n);
}

unsigned int pic_write(const uint8_t *pixels, ti_var_t f) {
	int last = -1;
	unsigned int again = 0;
	out = f;
	out_size = chunk_n = 0;
	for (unsigned int i = 0; i < PIC_W * PIC_H; i++) {
		uint8_t v = *pixels++;
		if (v == last && again < 255 - RUNS + 2) {
			again++;
			continue;
		}
		repeat(last, again);
		again = 0;
		put(v);
		last = v;
	}
	repeat(last, again);
	if (f && chunk_n)
		ti_Write(chunk, chunk_n, 1, f);
	return out_size;
}

extern void pic_draw_runs(const uint8_t *runs, uint8_t *dest);	// ui.asm

void pic_draw_stored(const uint8_t *data, uint8_t *dest) {
	load_colors(data);
	pic_draw_runs(data + 2 * PIC_COLORS, dest);
}
