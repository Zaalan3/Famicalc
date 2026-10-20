#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "icons.h"
#include "menu.h"
#include "states.h"

/*
	The ROM list and the pause menu. Both fill the whole 320x240 screen
	(ui_screen_full: 8 bpp, frame buffer at MENU_FB) in the style of
	PocketSNES Nspire's menus: a title bar with a stripe in the Famicom's
	colors, rows with icons, a help box about the selected row and key caps
	at the bottom. Palette entries 0xA0-0xA6 (gradients) and 0xE0-0xF2; the
	game's own are 0x00-0x9F.
	The in-game labels (FPS counter, messages) are drawn by ui.asm with the
	same font metrics and colors.
*/

extern const uint8_t ui_font[128 * 8];
extern void ui_glyph(uint8_t *dest, const uint8_t *glyph, uint8_t left, uint8_t color);
extern void ui_fill(uint8_t *dest, unsigned int width, unsigned int height, uint8_t color);
extern void ui_screen_full(uint8_t *frame_buffer);
extern void ui_screen_game(void);
extern void spiUnlock(void);
extern void spiLock(void);
extern uint8_t render_cache[];
extern unsigned int ui_stride;		// ui_fill's and ui_glyph's bytes per line

#define MENU_FB ((uint8_t *)0xD42000)
#define FB_W 320
#define FB_H 240
// the game's frame buffer (256 bytes per line, columns 8-247 are shown)
#define VBUFFER ((uint8_t *)0xD42000)
#define VBUFFER_SIZE (256 * 224)
// the menu's frame buffer runs into the emulator's memory from here on;
// the pause menu keeps a copy at the start of render_cache
#define EMU_PAGE ((uint8_t *)0xD50000)
#define EMU_COVERED (MENU_FB + FB_W * FB_H - EMU_PAGE)
// lines drawn off screen, then copied (see band_begin), after that copy
#define BAND_BUFFER (render_cache + 20 * 1024)
#define BAND_MAX 40

#define KEYS ((volatile uint16_t *)0xF50010)
#define LCD_PALETTE ((volatile uint16_t *)0xE30200)

// palette entries: 1555, 5 bits per channel
#define RGB(r, g, b) ((((r) >> 3) << 10) | (((g) >> 3) << 5) | ((b) >> 3))
#define RGB5(r, g, b) ((r) << 10 | (g) << 5 | (b))

// keep in sync with the C_ constants in vars.inc
enum {
	C_SHADOW = 0xE0, C_BG, C_EDGE, C_CHIP, C_RED, C_CHIP_EDGE, C_TEXT, C_DIM, C_SEL, C_GOLD,
	C_GREEN, C_FAINT, C_ERROR, C_ICON, C_SEL_EDGE, C_CREAM, C_STRIPE_GOLD, C_BAR_TEXT, C_FOOTER,
	C_END
};
static const uint16_t menu_colors[C_END - C_SHADOW] = {
	RGB(8, 4, 8),		// shadow (black)
	RGB5(2, 1, 2),		// help box, in-game labels
	RGB5(9, 5, 7),		// box and footer edges
	RGB5(7, 4, 6),		// key caps
	RGB(228, 52, 70),	// Famicom red
	RGB5(11, 7, 9),		// key cap edge
	RGB(246, 240, 230),	// text
	RGB(188, 169, 172),	// secondary text
	RGB5(12, 4, 6),		// selected row
	RGB(255, 210, 122),	// values (gold)
	RGB(110, 220, 146),	// green
	RGB(122, 100, 108),	// disabled
	RGB(255, 123, 123),	// error
	RGB(168, 136, 143),	// icons
	RGB(242, 194, 94),	// selected row's edge
	RGB(239, 230, 210),	// stripe: cream
	RGB(232, 178, 74),	// stripe: gold
	RGB(191, 170, 176),	// title bar text
	RGB5(4, 2, 3),		// footer
};

// gradients: steps of one level in every channel, so the bands keep their hue
#define G_BG 0xA0
#define BG_LEVELS 4
#define G_BAR (G_BG + BG_LEVELS)
#define BAR_LEVELS 3
static const uint16_t gradient_colors[BG_LEVELS + BAR_LEVELS] = {
	RGB5(5, 3, 4), RGB5(4, 2, 3), RGB5(3, 1, 2), RGB5(2, 0, 1),	// background, top to bottom
	RGB5(8, 5, 7), RGB5(7, 4, 6), RGB5(6, 3, 5),			// title bar
};

#define BAR_H 22		// title bar, then the 2 line stripe
#define BG_TOP (BAR_H + 2)
#define FOOT_Y 223
#define ROW_X 5
#define ROW_W 304
#define ROW_H 15

/* ---------------------------------------------------------------- drawing */

// proportional spacing for the 8x8 font (also used by ui.asm)
uint8_t glyph_left[128], glyph_width[128];

void ui_font_init(void) {
	for (unsigned int c = 0; c < 128; c++) {
		uint8_t bits = 0;
		for (unsigned int r = 0; r < 8; r++)
			bits |= ui_font[c * 8 + r];
		if (!bits) {
			glyph_left[c] = 0;
			glyph_width[c] = 3;	// space
			continue;
		}
		uint8_t left = 0, right = 7;
		while (!(bits & (0x80 >> left)))
			left++;
		while (!(bits & (0x80 >> right)))
			right--;
		glyph_left[c] = left;
		glyph_width[c] = right - left + 1;
	}
}

void menu_palette(void) {
	for (unsigned int i = 0; i < C_END - C_SHADOW; i++)
		LCD_PALETTE[C_SHADOW + i] = menu_colors[i];
	for (unsigned int i = 0; i < BG_LEVELS + BAR_LEVELS; i++)
		LCD_PALETTE[G_BG + i] = gradient_colors[i];
}

// Drawing goes to the screen, or (between band_begin and band_end) to a
// few lines in BAND_BUFFER that are copied to the screen in one go, so a
// row being drawn again never shows half drawn.
static uint8_t *canvas = MENU_FB;	// holds the lines canvas_y to canvas_y + canvas_h - 1
static int canvas_y, canvas_h = FB_H;

static void band_begin(int y, int h) {
	canvas = BAND_BUFFER;
	canvas_y = y;
	canvas_h = h;
}

static void band_end(void) {
	memcpy(MENU_FB + canvas_y * FB_W, BAND_BUFFER, canvas_h * FB_W);
	canvas = MENU_FB;
	canvas_y = 0;
	canvas_h = FB_H;
}

static uint8_t *at(int x, int y) {
	return canvas + (y - canvas_y) * FB_W + x;
}

static void fill(int x, int y, int w, int h, uint8_t color) {
	if (x < 0) { w += x; x = 0; }
	if (y < canvas_y) { h -= canvas_y - y; y = canvas_y; }
	if (x + w > FB_W) w = FB_W - x;
	if (y + h > canvas_y + canvas_h) h = canvas_y + canvas_h - y;
	if (w <= 0 || h <= 0)
		return;
	ui_fill(at(x, y), w, h, color);
}

// rectangle with its corner pixels left out
static void round_rect(int x, int y, int w, int h, uint8_t color) {
	fill(x + 1, y, w - 2, 1, color);
	fill(x, y + 1, w, h - 2, color);
	fill(x + 1, y + h - 1, w - 2, 1, color);
}

static void round_frame(int x, int y, int w, int h, uint8_t color) {
	fill(x + 1, y, w - 2, 1, color);
	fill(x + 1, y + h - 1, w - 2, 1, color);
	fill(x, y + 1, 1, h - 2, color);
	fill(x + w - 1, y + 1, 1, h - 2, color);
}

// a shape with round ends: inset[r] pixels left out at both ends of line r
static void pill(int x, int y, int w, const uint8_t *inset, int h, uint8_t color) {
	for (int r = 0; r < h; r++)
		fill(x + inset[r], y + r, w - 2 * inset[r], 1, color);
}

// the background: a gradient below the title bar
static void background(int x, int y, int w, int h) {
	for (int k = 0; k < BG_LEVELS; k++) {
		int top = k ? BG_TOP + k * (FB_H - BG_TOP) / BG_LEVELS : 0;
		int bottom = BG_TOP + (k + 1) * (FB_H - BG_TOP) / BG_LEVELS;
		if (top < y)
			top = y;
		if (bottom > y + h)
			bottom = y + h;
		if (top < bottom)
			fill(x, top, w, bottom - top, G_BG + k);
	}
}

static void icon(unsigned int id, int x, int y, uint8_t color) {
	if (y < canvas_y || y + 12 > canvas_y + canvas_h)
		return;
	uint8_t *p = at(x, y);
	for (unsigned int r = 0; r < 12; r++, p += FB_W) {
		uint8_t *q = p;
		for (uint16_t bits = icon_rows[id][r]; bits; bits <<= 1, q++)
			if (bits & 0x8000)
				*q = color;
	}
}

static int char_width(char c) {
	return glyph_width[(uint8_t)c & 0x7F] + 1;
}

static int text_width(const char *s) {
	int w = 0;
	while (*s)
		w += char_width(*s++);
	return w ? w - 1 : 0;
}

// proportional text, transparent background; bold draws each glyph twice,
// a pixel apart. Returns the x after it.
static int text_ex(int x, int y, const char *s, uint8_t color, bool bold) {
	bool shown = y >= canvas_y && y + 8 <= canvas_y + canvas_h;
	for (; *s; s++) {
		uint8_t c = (uint8_t)*s & 0x7F;
		uint8_t w = glyph_width[c];
		if (shown && x >= 0 && x + w + bold <= FB_W) {
			ui_glyph(at(x, y), ui_font + c * 8, glyph_left[c], color);
			if (bold)
				ui_glyph(at(x + 1, y), ui_font + c * 8, glyph_left[c], color);
		}
		x += w + 1 + bold;
	}
	return x;
}

static int text(int x, int y, const char *s, uint8_t color) {
	return text_ex(x, y, s, color, false);
}

static void text_right(int right, int y, const char *s, uint8_t color) {
	text(right - text_width(s), y, s, color);
}

// copies s into buf, shortened with "..." to fit in width pixels
static const char *fit(const char *s, int width, char *buf, size_t size) {
	size_t n = strlen(s);
	if (text_width(s) <= width)
		return s;
	if (n > size - 4)
		n = size - 4;
	memcpy(buf, s, n);
	do {
		strcpy(buf + n, "...");
	} while (n-- && text_width(buf) > width);
	return buf;
}

// a key cap like [2nd]; lit: the gold one of a selected row
#define CAP_H 13
static int key_cap(int x, int y, const char *key, bool lit) {
	int w = text_width(key) + 8;
	round_rect(x, y, w, CAP_H, lit ? C_SEL_EDGE : C_CHIP);
	fill(x + 2, y + CAP_H - 1, w - 4, 1, lit ? C_GOLD : C_CHIP_EDGE);	// the cap's lower edge
	text(x + 4, y + 3, key, lit ? C_SEL : C_TEXT);
	return w;
}

// a key cap followed by what it does; returns the x after both
static int key_hint(int x, int y, const char *key, const char *what) {
	x += key_cap(x, y, key, false) + 4;
	return text(x, y + 3, what, C_DIM) + 10;
}

struct hint {
	const char *key, *what;
};

static void footer(const struct hint *hints, unsigned int count, const char *right) {
	fill(0, FOOT_Y, FB_W, FB_H - FOOT_Y, C_FOOTER);
	fill(0, FOOT_Y, FB_W, 1, C_EDGE);
	int x = 6;
	for (unsigned int i = 0; i < count; i++)
		x = key_hint(x, FOOT_Y + 3, hints[i].key, hints[i].what);
	if (right)
		text_right(314, FOOT_Y + 6, right, C_FAINT);
}

// the title bar: "FamiCalc", the stripe, and a text on the right
static void title_bar(const char *right) {
	char buf[48];
	for (int k = 0; k < BAR_LEVELS; k++)
		fill(0, k * BAR_H / BAR_LEVELS, FB_W, (k + 1) * BAR_H / BAR_LEVELS - k * BAR_H / BAR_LEVELS,
		     G_BAR + k);
	fill(0, BAR_H, 160, 2, C_RED);
	fill(160, BAR_H, 80, 2, C_STRIPE_GOLD);
	fill(240, BAR_H, 80, 2, C_CREAM);
	text_ex(text_ex(10, 7, "Fami", C_TEXT, true), 7, "Calc", C_RED, true);
	if (right)
		text_right(310, 8, fit(right, 200, buf, sizeof buf), C_BAR_TEXT);
}

// a row's background; the selected row has a gold edge, or a gold frame
// while it asks to confirm
static void row_back(int y, bool on, bool asking) {
	background(0, y, FB_W, ROW_H);
	if (!on)
		return;
	round_rect(ROW_X, y, ROW_W, ROW_H, C_SEL);
	if (asking)
		round_frame(ROW_X, y, ROW_W, ROW_H, C_SEL_EDGE);
	else
		fill(ROW_X, y + 3, 2, ROW_H - 6, C_SEL_EDGE);
}

// on/off switch, its right end at right
static void toggle(int right, int y, bool on) {
	static const uint8_t track[10] = { 3, 1, 1, 0, 0, 0, 0, 1, 1, 3 };
	static const uint8_t knob[8] = { 2, 1, 0, 0, 0, 0, 1, 2 };
	pill(right - 20, y, 20, track, 10, on ? C_GREEN : C_FAINT);
	pill(on ? right - 9 : right - 19, y + 1, 8, knob, 8, C_TEXT);
}

// a box with a frame (the help box)
static void box(int y, int h) {
	round_rect(ROW_X, y, ROW_W, h, C_EDGE);
	round_rect(ROW_X + 1, y + 1, ROW_W - 2, h - 2, C_BG);
}

/* ---------------------------------------------------------------- text helpers */

static char *put_uint(char *p, unsigned int v, int digits) {
	char tmp[8];
	int n = 0;
	do {
		tmp[n++] = '0' + v % 10;
		v /= 10;
	} while (v || n < digits);
	while (n)
		*p++ = tmp[--n];
	*p = 0;
	return p;
}

static char *put_str(char *p, const char *s) {
	while ((*p = *s++))
		p++;
	return p;
}

// "Oct 17, 2026, 11:41 PM"
static char *put_date_time(char *p, const struct state_header *h) {
	state_date_text(h, p);
	p = put_str(p + strlen(p), ", ");
	state_time_text(h, p);
	return p + strlen(p);
}

/* ---------------------------------------------------------------- timing, keys */

// waits for the next LCD frame (about 1/60 s)
extern void lcd_wait_frame(void);
#define wait_frame lcd_wait_frame

static uint16_t keys_now[8], keys_last[8];

static void read_keys(void) {
	for (unsigned int g = 1; g < 8; g++) {
		keys_last[g] = keys_now[g];
		keys_now[g] = KEYS[g];
	}
}

// ignore keys that are already down when a menu opens
static void reset_keys(void) {
	read_keys();
	read_keys();
}

static bool any_key_down(void) {
	return keys_now[1] | keys_now[2] | keys_now[3] | keys_now[4] | keys_now[5] | keys_now[6] |
	       keys_now[7];
}

static bool is_down(unsigned int g, unsigned int bit) {
	return keys_now[g] & bit;
}

static bool pressed(unsigned int g, unsigned int bit) {
	return (keys_now[g] & bit) && !(keys_last[g] & bit);
}

#define K_2ND 1, 1 << 5
#define K_MODE 1, 1 << 6
#define K_DEL 1, 1 << 7
#define K_ALPHA 2, 1 << 7
#define K_ENTER 6, 1 << 0
#define K_CLEAR 6, 1 << 6
#define K_DOWN 7, 1 << 0
#define K_LEFT 7, 1 << 1
#define K_RIGHT 7, 1 << 2
#define K_UP 7, 1 << 3

// held arrow keys repeat; returns the step (0 = no step this frame)
static unsigned int repeat_frames[4];
static int arrow(unsigned int which, unsigned int g, unsigned int bit) {
	if (!is_down(g, bit)) {
		repeat_frames[which] = 0;
		return 0;
	}
	unsigned int n = repeat_frames[which]++;
	if (n == 0)
		return 1;
	if (n < 18 || (n - 18) % 3)
		return 0;
	return n > 90 ? 10 : 1;		// held for 1.5 s: tens
}

static int digit_pressed(void) {
	static const uint8_t group[10] = { 3, 3, 4, 5, 3, 4, 5, 3, 4, 5 };
	static const uint8_t bit[10] = { 0, 1, 1, 1, 2, 2, 2, 3, 3, 3 };
	for (int d = 0; d < 10; d++)
		if (pressed(group[d], 1 << bit[d]))
			return d;
	return -1;
}

/* ---------------------------------------------------------------- ROM list */

#define LIST_Y (BG_TOP + 4)
#define LIST_ROWS 10
#define INFO_Y 183
#define INFO_H 38

static const char *mapper_name(uint8_t mapper) {
	switch (mapper) {
	case 0: return "NROM";
	case 1: return "MMC1";
	case 2: return "UxROM";
	case 3: return "CNROM";
	case 4: return "MMC3";
	case 7: return "AxROM";
	}
	return NULL;
}

// one row of the list: the number key that starts it (the first ten), the
// title and how many states it has; the scroll bar next to it
static void draw_rom_row(struct rom *roms, unsigned int count, unsigned int i, unsigned int sel,
                         unsigned int top) {
	char buf[48], *p;
	int y = LIST_Y + (i - top) * ROW_H;
	bool on = i == sel;

	band_begin(y, ROW_H);
	row_back(y, on, false);
	if (count > LIST_ROWS) {
		int track = LIST_ROWS * ROW_H - 2;
		int len = track * LIST_ROWS / count;
		fill(312, LIST_Y + 1, 3, track, C_CHIP);
		fill(312, LIST_Y + 1 + (track - len) * top / (count - LIST_ROWS), 3, len, C_FAINT);
	}
	if (i < count) {
		const char *title = roms[i].header->description[0] ? roms[i].header->description : roms[i].name;
		if (i < 10) {
			char key[2] = { (char)('0' + (i + 1) % 10), 0 };
			key_cap(11, y + 1, key, on);
		}
		if (roms[i].states) {
			p = put_uint(buf, roms[i].states, 1);
			put_str(p, roms[i].states == 1 ? " save" : " saves");
			text_right(300, y + 4, buf, on ? C_GOLD : C_FAINT);
		}
		text(30, y + 4, fit(title, roms[i].states ? 206 : 266, buf, sizeof buf), on ? C_TEXT : C_DIM);
	}
	band_end();
}

// the selected game's files and newest state
static void draw_rom_info(struct rom *rom, const char *error) {
	char buf[64], *p;
	const struct romheader *h = rom->header;
	const char *mapper = mapper_name(h->mapper);

	band_begin(INFO_Y - 1, INFO_H + 2);
	background(0, INFO_Y - 1, FB_W, INFO_H + 2);
	box(INFO_Y, INFO_H);
	text(11, INFO_Y + 4, fit(h->description[0] ? h->description : rom->name, 292, buf, sizeof buf), C_TEXT);

	p = put_str(buf, rom->name);
	p = put_str(p, "  \x07  ");
	if (mapper) {
		p = put_str(p, mapper);
	} else {
		p = put_str(p, "mapper ");
		p = put_uint(p, h->mapper, 1);
	}
	p = put_str(p, "  \x07  ");
	p = put_uint(p, h->prgsize * 16, 1);
	p = put_str(p, "K");
	if (h->chrsize) {
		p = put_str(p, " + ");
		p = put_uint(p, h->chrsize * 8, 1);
		p = put_str(p, "K");
	}
	text(11, INFO_Y + 15, buf, C_DIM);

	if (error) {
		text(11, INFO_Y + 26, error, C_ERROR);
	} else if (!rom->states) {
		text(11, INFO_Y + 26, "No save states yet", C_FAINT);
	} else if (rom->newest) {
		struct state_header newest;
		state_stamp_header(rom->newest, &newest);
		p = put_str(buf, "Newest: slot ");
		p = put_uint(p, rom->newest_slot, 3);
		p = put_str(p, ", ");
		put_date_time(p, &newest);
		text(11, INFO_Y + 26, buf, C_FAINT);
	} else {
		text(11, INFO_Y + 26, "Saved by an older FamiCalc", C_FAINT);
	}
	band_end();
}

static void draw_rom_list(struct rom *roms, unsigned int count, unsigned int sel, unsigned int top,
                          const char *error, bool all) {
	static const struct hint hints[] = { { "2nd", "Play" }, { "\x1E\x1F", "Move" }, { "del", "Quit" } };
	char buf[16];

	if (all) {
		char *p = put_str(buf, "v");
		put_str(p, version_string);
		background(0, 0, FB_W, FB_H);
		title_bar(buf);
		p = put_uint(buf, count, 1);
		put_str(p, count == 1 ? " game" : " games");
		if (count) {
			footer(hints, 3, buf);
		} else {
			footer(hints + 2, 1, NULL);
			box(LIST_Y + 4, 64);
			text(15, LIST_Y + 12, "No games found", C_TEXT);
			text(15, LIST_Y + 27, "Convert .nes files with FamiCalc's", C_DIM);
			text(15, LIST_Y + 39, "converter and send all of the files it", C_DIM);
			text(15, LIST_Y + 51, "makes to the calculator.", C_DIM);
			return;
		}
	}
	for (unsigned int row = 0; row < LIST_ROWS; row++)
		draw_rom_row(roms, count, top + row, sel, top);
	draw_rom_info(&roms[sel], error);
}

int rom_menu(struct rom *roms, unsigned int count, unsigned int selected, const char *error) {
	unsigned int top = 0;
	bool redraw = true, all = true;
	int d;

	if (selected >= count)
		selected = 0;
	// draw it while the panel still shows the last picture, then show it
	spiLock();
	ui_stride = FB_W;
	menu_palette();
	reset_keys();

	for (;;) {
		int step;
		if (selected < top)
			top = selected;
		if (selected >= top + LIST_ROWS)
			top = selected - LIST_ROWS + 1;
		if (redraw) {
			draw_rom_list(roms, count, selected, top, error, all);
			if (all)
				ui_screen_full(MENU_FB);
			redraw = all = false;
		}
		wait_frame();
		read_keys();

		if (pressed(K_DEL) || pressed(K_CLEAR))
			return -1;
		if (!count)
			continue;
		// number keys start the first ten games
		if ((d = digit_pressed()) >= 0 && (unsigned int)(d + 9) % 10 < count) {
			selected = (d + 9) % 10;
			draw_rom_list(roms, count, selected, selected < top ? selected : top, NULL, false);
			wait_frame();
			goto play;
		}
		if (pressed(K_2ND) || pressed(K_ENTER))
			goto play;
		if ((step = arrow(0, K_UP)) && selected) {
			selected = selected > (unsigned int)step ? selected - step : 0;
			redraw = true;
		}
		if ((step = arrow(1, K_DOWN)) && selected + 1 < count) {
			selected = selected + step < count ? selected + step : count - 1;
			redraw = true;
		}
		if (redraw)
			error = NULL;
	}
play:
	// black, so the rows outside the game's window don't keep the list
	fill(0, 0, FB_W, FB_H, C_SHADOW);
	wait_frame();
	wait_frame();
	return selected;
}

/* ---------------------------------------------------------------- pause menu */

enum {
	ITEM_RESUME, ITEM_SLOT, ITEM_SAVE, ITEM_LOAD, ITEM_DELETE, ITEM_FPS, ITEM_SCREEN,
	ITEM_RESET, ITEM_GAMES, ITEM_QUIT, ITEM_COUNT
};
static const char *const item_labels[ITEM_COUNT] = {
	"Resume game", "State slot", "Save state", "Load state", "Delete state", "Show FPS", "Picture",
	"Reset game", "Change game", "Quit FamiCalc"
};
static const uint8_t item_icons[ITEM_COUNT] = {
	ICON_PLAY, ICON_SLOTS, ICON_SAVE, ICON_LOAD, ICON_TRASH, ICON_SPEED, ICON_SCREEN,
	ICON_RESTART, ICON_CART, ICON_POWER
};
// rows in groups, 4 lines apart
static const uint8_t item_y[ITEM_COUNT] = { 28, 47, 62, 77, 92, 111, 126, 145, 160, 175 };
#define HELP_Y 194
#define HELP_H 27

static uint8_t *const emu_backup = render_cache;

static uint8_t used[NUM_SLOTS / 8 + 1];
static unsigned int used_count, free_kb;
static struct slot_info info;

static bool item_off(unsigned int i) {
	return (i == ITEM_LOAD || i == ITEM_DELETE) && !info.used;
}

static void draw_item(unsigned int i, unsigned int sel, int confirm) {
	char buf[8];
	int y = item_y[i];
	bool on = i == sel;
	uint8_t value = on ? C_GOLD : C_DIM;

	band_begin(y, ROW_H);
	row_back(y, on, (int)i == confirm);
	icon(item_icons[i], 12, y + 2, on ? C_TEXT : item_off(i) ? C_FAINT : C_ICON);
	text(32, y + 4, item_labels[i], on ? C_TEXT : item_off(i) ? C_FAINT : C_DIM);
	switch (i) {
	case ITEM_SLOT: {
		// [dot if used] < 012 >
		int x = 300 - text_width("\x10");
		put_uint(buf, save_slot, 3);
		text(x, y + 4, "\x10", on ? C_GOLD : C_FAINT);
		x -= 4 + text_width(buf);
		text(x, y + 4, buf, on ? C_GOLD : C_TEXT);
		x -= 4 + text_width("\x11");
		text(x, y + 4, "\x11", on ? C_GOLD : C_FAINT);
		if (info.used)
			text(x - 6 - text_width("\x07"), y + 4, "\x07", C_GREEN);
		break;
	}
	case ITEM_LOAD:
	case ITEM_DELETE:
		if (!info.used)
			text_right(300, y + 4, "empty slot", C_FAINT);
		break;
	case ITEM_FPS:
		toggle(300, y + 2, show_fps);
		break;
	case ITEM_SCREEN:
		if (on) {
			int x = 300 - text_width("\x10");
			text(x, y + 4, "\x10", C_GOLD);
			x -= 4 + text_width(wide_buffer ? "Wide" : "Normal");
			text(x, y + 4, wide_buffer ? "Wide" : "Normal", value);
			text(x - 4 - text_width("\x11"), y + 4, "\x11", C_GOLD);
		} else {
			text_right(300, y + 4, wide_buffer ? "Wide" : "Normal", value);
		}
		break;
	}
	band_end();
}

// "slot 012"
static char *put_slot(char *p, unsigned int slot) {
	p = put_str(p, "slot ");
	return put_uint(p, slot, 3);
}

// what the selected slot holds: "Slot 012: saved Oct 17, 2026, 11:41 PM"
static void slot_status(char *p) {
	p = put_str(p, "Slot ");
	p = put_uint(p, save_slot, 3);
	if (!info.used) {
		put_str(p, " is empty");
	} else if (info.dated) {
		p = put_str(p, ": saved ");
		put_date_time(p, &info.header);
	} else {
		put_str(p, state_has_header(&info.header) ? ": saved" : ": saved by an older FamiCalc");
	}
}

// the help box: what the selected row does, a question to confirm, or what just happened
static void draw_help(unsigned int sel, int confirm, const char *note, bool note_bad) {
	char line1[64], line2[64], fitted[64], *p;
	const char *l1 = line1, *l2 = line2;
	uint8_t c1 = C_TEXT;

	line1[0] = line2[0] = 0;
	slot_status(line2);
	if (confirm >= 0) {
		c1 = C_GOLD;
		switch (confirm) {
		case ITEM_SAVE:
			p = put_str(line1, "Save over ");
			p = put_slot(p, save_slot);
			put_str(p, "?");
			break;
		case ITEM_DELETE:
			p = put_str(line1, "Delete ");
			p = put_slot(p, save_slot);
			put_str(p, "? It can't be undone.");
			break;
		default:
			l1 = confirm == ITEM_RESET ? "Reset the game?" : confirm == ITEM_GAMES ? "Leave this game?" :
			     "Quit FamiCalc?";
			l2 = "Progress since your last save is lost.";
			break;
		}
	} else if (note) {
		l1 = note;
		c1 = note_bad ? C_ERROR : C_GOLD;
	} else {
		switch (sel) {
		case ITEM_RESUME:
			l1 = "Close the menu and keep playing.";
			p = put_uint(line2, used_count, 1);
			p = put_str(p, used_count == 1 ? " save state, " : " save states, ");
			p = put_uint(p, free_kb, 1);
			put_str(p, "K of archive free.");
			break;
		case ITEM_SLOT:
			slot_status(line1);
			l2 = "Left/right or the number keys pick a slot.";
			break;
		case ITEM_SAVE:
			p = put_str(line1, "Saves the game here, in ");
			p = put_slot(p, save_slot);
			put_str(p, ".");
			break;
		case ITEM_LOAD:
			l1 = info.used ? "Goes back to the moment it was saved." : "Nothing to load: this slot is empty.";
			break;
		case ITEM_DELETE:
			l1 = info.used ? "Deletes this save state." : "Nothing to delete: this slot is empty.";
			break;
		case ITEM_FPS:
			l1 = "Shows frames per second in the corner:";
			l2 = "the game's speed, then the frames drawn.";
			break;
		case ITEM_SCREEN:
			l1 = "Normal shows the NES picture as it is;";
			l2 = "Wide stretches it to the full width.";
			break;
		case ITEM_RESET:
			l1 = "Starts over, like the NES's reset button.";
			l2 = "Progress since your last save is lost.";
			break;
		case ITEM_GAMES:
			l1 = "Back to the list of games.";
			l2 = "Progress since your last save is lost.";
			break;
		case ITEM_QUIT:
			l1 = "Back to the calculator.";
			l2 = "Progress since your last save is lost.";
			break;
		}
	}

	band_begin(HELP_Y - 1, HELP_H + 2);
	background(0, HELP_Y - 1, FB_W, HELP_H + 2);
	box(HELP_Y, HELP_H);
	text(11, HELP_Y + 5, fit(l1, 296, fitted, sizeof fitted), c1);
	text(11, HELP_Y + 16, fit(l2, 296, fitted, sizeof fitted), C_DIM);
	band_end();
}

static void draw_pause_footer(unsigned int sel, int confirm) {
	static const struct hint ask[] = { { "2nd", "Yes" }, { "clear", "No" } };
	static const struct hint slot[] = {
		{ "\x1E\x1F", "Move" }, { "2nd", "OK" }, { "\x11\x10", "Slot" }, { "clear", "Back" }
	};
	static const struct hint change[] = {
		{ "\x1E\x1F", "Move" }, { "2nd", "OK" }, { "\x11\x10", "Change" }, { "clear", "Back" }
	};
	static const struct hint plain[] = { { "\x1E\x1F", "Move" }, { "2nd", "OK" }, { "clear", "Back" } };

	band_begin(FOOT_Y, FB_H - FOOT_Y);
	if (confirm >= 0)
		footer(ask, 2, NULL);
	else if (sel >= ITEM_SLOT && sel <= ITEM_DELETE)
		footer(slot, 4, NULL);
	else if (sel == ITEM_FPS || sel == ITEM_SCREEN)
		footer(change, 4, NULL);
	else
		footer(plain, 3, NULL);
	band_end();
}

static void draw_pause(unsigned int sel, int confirm, const char *note, bool note_bad) {
	for (unsigned int i = 0; i < ITEM_COUNT; i++)
		draw_item(i, sel, confirm);
	draw_help(sel, confirm, note, note_bad);
	draw_pause_footer(sel, confirm);
}

static void select_slot(unsigned int slot) {
	save_slot = slot;
	slot_info(slot, &info);
}

// The menu's frame buffer overlaps the emulator's memory: keep a copy.
// The panel keeps showing the game while the menu is drawn.
static void open_screen(unsigned int sel) {
	memcpy(emu_backup, EMU_PAGE, EMU_COVERED);
	spiLock();
	ui_stride = FB_W;
	menu_palette();
	background(0, 0, FB_W, FB_H);
	title_bar(game_title);
	draw_pause(sel, -1, NULL, false);
	ui_screen_full(MENU_FB);
}

static void close_screen(void) {
	// black while the emulator's memory goes back, then the game's window
	fill(0, 0, FB_W, FB_H, C_SHADOW);
	wait_frame();
	wait_frame();
	spiLock();
	memcpy(EMU_PAGE, emu_backup, EMU_COVERED);
	memset(VBUFFER, 0, VBUFFER_SIZE);
	if (wide_buffer)
		memset(wide_buffer, 0, WIDE_SIZE);
	ui_screen_game();
}

// the wide picture's buffer takes free RAM from the JIT's cache
static bool toggle_wide(const char **note) {
	if (set_wide(!wide_buffer))
		return true;
	*note = "Not enough free RAM for the wide picture.";
	return false;
}

uint8_t pause_menu(void) {
	unsigned int sel = ITEM_RESUME;
	int confirm = -1;
	bool redraw = false, note_bad = false;
	unsigned int typed = 0, typed_frames = 0;
	const char *note = NULL;
	char note_buf[48];
	uint8_t result = MENU_RESUME;
	bool screen_changed = false;

	reset_keys();
	used_count = scan_states(used);
	free_kb = archive_free_kb();
	select_slot(save_slot);
	open_screen(sel);

	for (;;) {
		int step, d;
		if (redraw) {
			draw_pause(sel, confirm, note, note_bad);
			redraw = false;
		}
		wait_frame();
		read_keys();
		if (typed_frames && !--typed_frames)
			typed = 0;

		if (pressed(K_CLEAR) || pressed(K_ALPHA) || pressed(K_MODE)) {
			if (confirm < 0)
				break;
			confirm = -1;	// "No"
			redraw = true;
			continue;
		}

		if ((step = arrow(0, K_UP))) {
			sel = (sel + ITEM_COUNT - 1) % ITEM_COUNT;
			confirm = -1;
			note = NULL;
			redraw = true;
		}
		if ((step = arrow(1, K_DOWN))) {
			sel = (sel + 1) % ITEM_COUNT;
			confirm = -1;
			note = NULL;
			redraw = true;
		}

		// slot: left/right on the state rows, number keys anywhere
		if (sel >= ITEM_SLOT && sel <= ITEM_DELETE) {
			int dir = 0;
			if ((step = arrow(2, K_LEFT)))
				dir = -step;
			else if ((step = arrow(3, K_RIGHT)))
				dir = step;
			if (dir) {
				select_slot((save_slot + NUM_SLOTS + dir) % NUM_SLOTS);
				confirm = -1;
				note = NULL;
				redraw = true;
			}
		} else if (sel == ITEM_FPS && (arrow(2, K_LEFT) || arrow(3, K_RIGHT))) {
			show_fps = !show_fps;
			redraw = true;
		} else if (sel == ITEM_SCREEN && (arrow(2, K_LEFT) || arrow(3, K_RIGHT))) {
			note = NULL;
			if (toggle_wide(&note))
				screen_changed = !screen_changed;
			note_bad = true;
			redraw = true;
		}
		if ((d = digit_pressed()) >= 0) {
			typed = typed >= 100 ? (unsigned int)d : typed * 10 + d;
			typed_frames = 90;
			select_slot(typed);
			confirm = -1;
			note = NULL;
			redraw = true;
		}

		if (pressed(K_DEL) && info.used) {
			sel = ITEM_DELETE;
			confirm = ITEM_DELETE;
			redraw = true;
		}

		if (pressed(K_2ND) || pressed(K_ENTER)) {
			redraw = true;
			note = NULL;
			note_bad = false;
			switch (sel) {
			case ITEM_RESUME:
				goto done;
			case ITEM_SLOT:
				sel = ITEM_SAVE;
				break;
			case ITEM_SAVE:
				if (info.used && confirm != ITEM_SAVE) {
					confirm = ITEM_SAVE;
					break;
				}
				result = MENU_SAVE;
				goto done;
			case ITEM_LOAD:
				if (!info.used) {
					note = "Nothing to load: this slot is empty.";
					break;
				}
				result = MENU_LOAD;
				goto done;
			case ITEM_DELETE:
				if (!info.used)
					break;
				if (confirm != ITEM_DELETE) {
					confirm = ITEM_DELETE;
					break;
				}
				confirm = -1;
				if (delete_state(save_slot)) {
					char *p = put_str(note_buf, "Deleted ");
					p = put_slot(p, save_slot);
					put_str(p, ".");
					note = note_buf;
					used_count = scan_states(used);
					free_kb = archive_free_kb();
				} else {
					note = "Couldn't delete it.";
					note_bad = true;
				}
				slot_info(save_slot, &info);
				break;
			case ITEM_FPS:
				show_fps = !show_fps;
				break;
			case ITEM_SCREEN:
				if (toggle_wide(&note))
					screen_changed = !screen_changed;
				note_bad = true;
				break;
			case ITEM_RESET:
			case ITEM_GAMES:
			case ITEM_QUIT:
				if (confirm != (int)sel) {
					confirm = sel;
					break;
				}
				exit_reason = sel == ITEM_RESET ? EXIT_RESET : sel == ITEM_GAMES ? EXIT_ROM_LIST : EXIT_QUIT;
				result = MENU_LEAVE;
				goto done;
			}
			if (confirm != (int)sel)
				confirm = -1;
		}
	}
done:
	// a new picture size moves the JIT's cache: restart the emulator from
	// its state (save and load do that anyway)
	if (screen_changed && result == MENU_RESUME) {
		state_reload_only = 1;
		result = MENU_RELOAD;
	}
	// let go of the keys first, so the game doesn't get them (or reopen the menu)
	do {
		wait_frame();
		read_keys();
	} while (any_key_down());
	close_screen();
	return result;
}
