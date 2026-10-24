#ifndef PICTURES_H
#define PICTURES_H

#include <stdbool.h>
#include <stdint.h>
#include <fileioc.h>

/*
	Pictures of the game: the frame on screen at half size (every other
	pixel of every other line), with the colors it was shown in.
	Pixels are the frame buffer's own values (0x00-0x9F, indices into the
	LCD palette), so a picture keeps the game's palette entries with it.
	A state keeps one after its compressed data (main.c saveToSlot).
*/
#define PIC_W 120
#define PIC_H 112
#define PIC_COLORS 0xA0			// LCD palette entries a frame uses

struct picture {
	uint16_t palette[PIC_COLORS];
	uint8_t pixels[PIC_W * PIC_H];
};

// the frame on screen (the normal picture with its rotation, or the wide one)
void pic_capture(struct picture *pic);
// draws it at dest (320 bytes per line) and loads its colors into the palette
void pic_draw(const struct picture *pic, uint8_t *dest);
void pic_colors(const struct picture *pic);

// where the pause menu leaves the picture for a state it saves: the end of
// the normal frame buffer (unused by the wide picture; cleared before the
// normal one is drawn again)
#define PICTURE_SPOT ((struct picture *)(0xD42000 + 256 * 224 - sizeof(struct picture)))

// Stored: its colors, then its pixels in runs. pic_write writes the runs of
// pixels (NULL: the frame on screen) to f (0: only counts); returns their size.
unsigned int pic_write(const uint8_t *pixels, ti_var_t f);
void pic_draw_stored(const uint8_t *data, uint8_t *dest);

// the average color of a picture drawn at dest, 0-255 per channel
void pic_average(const uint8_t *dest, uint8_t rgb[3]);

#endif
