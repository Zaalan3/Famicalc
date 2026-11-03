#include <stdint.h>
#include <string.h>
#include <fileioc.h>

#include "menu.h"
#include "pictures.h"

#define VBUFFER ((const uint8_t *)0xD42000)	// the normal picture: 256 bytes per line
#define LCD_PALETTE ((volatile uint16_t *)0xE30200)

extern uint8_t spi_scroll_sent;		// spi.asm: the panel shows column x + S at x
extern unsigned int scroll_want_w;	// spi.asm: the wide picture's rotation, in its columns (0-319)

// pixel x, y of the picture from the frame on screen
static uint8_t frame_pixel(unsigned int x, unsigned int y) {
	if (wide_buffer) {
		// 300 of its 320 columns are shown (10-309), rotated by S_w (at most 626)
		unsigned int c = 10 + x * 5 / 2 + scroll_want_w;
		if (c >= 320)
			c -= 320;
		return wide_buffer[2 * y * 320 + c];
	}
	// columns 8-247 are shown, rotated by S
	return VBUFFER[2 * y * 256 + ((8 + 2 * x + spi_scroll_sent) & 255)];
}

// pic_capture: ui.asm (the same pixels as frame_pixel, ~20 cycles each)

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
	for (unsigned int y = 0; y < PIC_H; y++)
		for (unsigned int x = 0; x < PIC_W; x++) {
			uint8_t v = pixels ? *pixels++ : frame_pixel(x, y);
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

extern void pic_sums(const uint8_t *dest, unsigned int sums[3]);	// ui.asm

void pic_average(const uint8_t *dest, uint8_t rgb[3]) {
	unsigned int sums[3];
	pic_sums(dest, sums);
	for (unsigned int k = 0; k < 3; k++)
		rgb[k] = sums[k] / 60;		// / 480 * 8
}
