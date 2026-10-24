#include <stdint.h>
#include <string.h>
#include <fileioc.h>

#include "menu.h"
#include "pictures.h"

#define VBUFFER ((const uint8_t *)0xD42000)	// the normal picture: 256 bytes per line
#define LCD_PALETTE ((volatile uint16_t *)0xE30200)

extern uint8_t spi_scroll_sent;		// spi.asm: the panel shows column x + S at x

// pixel x, y of the picture from the frame on screen
static uint8_t frame_pixel(unsigned int x, unsigned int y) {
	if (wide_buffer)
		// 300 of its 320 columns are shown (10-309)
		return wide_buffer[2 * y * 320 + 10 + x * 5 / 2];
	// columns 8-247 are shown, rotated by S
	return VBUFFER[2 * y * 256 + ((8 + 2 * x + spi_scroll_sent) & 255)];
}

void pic_capture(struct picture *pic) {
	uint8_t *p = pic->pixels;
	for (unsigned int i = 0; i < PIC_COLORS; i++)
		pic->palette[i] = LCD_PALETTE[i];
	for (unsigned int y = 0; y < PIC_H; y++)
		for (unsigned int x = 0; x < PIC_W; x++)
			*p++ = frame_pixel(x, y);
}

static void load_colors(const uint8_t *colors) {
	for (unsigned int i = 0; i < PIC_COLORS; i++, colors += 2)
		LCD_PALETTE[i] = colors[0] | colors[1] << 8;
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

void pic_average(const uint8_t *dest, uint8_t rgb[3]) {
	unsigned int sum[3] = { 0, 0, 0 };
	const volatile uint8_t *palette = (const volatile uint8_t *)LCD_PALETTE;
	// every 4th pixel of every 7th line: 480. The palette's 1555 entries
	// byte by byte (16 bit shifts are library calls)
	for (unsigned int y = 0; y < PIC_H; y += 7, dest += 320 * 7)
		for (unsigned int x = 0; x < PIC_W; x += 4) {
			const volatile uint8_t *c = palette + 2 * dest[x];
			uint8_t lo = c[0], hi = c[1];
			sum[0] += hi >> 2 & 31;
			sum[1] += (hi & 3) << 3 | lo >> 5;
			sum[2] += lo & 31;
		}
	for (unsigned int k = 0; k < 3; k++)
		rgb[k] = sum[k] / 60;		// / 480 * 8
}
