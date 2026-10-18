#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "menu.h"
#include "states.h"

/*
	Menus, drawn straight into the emulator's 8 bpp frame buffer
	(256x224, the panel shows columns 8-247) with palette entries 0xE0-0xEC.
	The ROM list fills the screen; the pause menu is a panel over the
	dimmed game.
*/

extern const uint8_t ui_font[128 * 8];
extern void ui_glyph(uint8_t *dest, const uint8_t *glyph, uint8_t left, uint8_t color);
extern void ui_fill(uint8_t *dest, unsigned int width, unsigned int height, uint8_t color);
extern void spiUnlock(void);

#define FB ((uint8_t *)0xD42000)
#define FB_W 256
#define FB_H 224
#define VIS_X0 8
#define VIS_X1 248

#define KEYS ((volatile uint16_t *)0xF50010)
#define LCD_PALETTE ((volatile uint16_t *)0xE30200)
#define LCD_RIS (*(volatile uint8_t *)0xE30020)
#define LCD_ICR (*(volatile uint8_t *)0xE30028)

#define RGB(r, g, b) ((((r) >> 3) << 10) | (((g) >> 3) << 5) | ((b) >> 3))

enum {
	C_SHADOW = 0xE0, C_BG, C_BAND, C_EDGE, C_RED, C_RED_DARK, C_TEXT, C_DIM, C_SEL, C_GOLD,
	C_GREEN, C_FAINT, C_ERROR, C_END
};
static const uint16_t menu_colors[C_END - C_SHADOW] = {
	RGB(0, 0, 0),		// shadow
	RGB(22, 22, 30),	// panel
	RGB(38, 38, 52),	// bands, key caps
	RGB(72, 72, 96),	// border
	RGB(192, 34, 54),	// Famicom red
	RGB(120, 18, 34),	// dark red
	RGB(244, 236, 218),	// text
	RGB(156, 150, 144),	// secondary text
	RGB(94, 30, 46),	// selected row
	RGB(246, 192, 72),	// gold
	RGB(112, 210, 130),	// green
	RGB(100, 98, 110),	// disabled
	RGB(255, 112, 112),	// error
};

/* ---------------------------------------------------------------- drawing */

static uint8_t glyph_left[128], glyph_width[128];

static void font_metrics(void) {
	static bool ready;
	if (ready)
		return;
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
	ready = true;
}

static void fill(int x, int y, int w, int h, uint8_t color) {
	if (x < 0) { w += x; x = 0; }
	if (y < 0) { h += y; y = 0; }
	if (x + w > FB_W) w = FB_W - x;
	if (y + h > FB_H) h = FB_H - y;
	if (w <= 0 || h <= 0)
		return;
	ui_fill(FB + y * FB_W + x, w, h, color);
}

// rectangle with its corner pixels left out
static void round_rect(int x, int y, int w, int h, uint8_t color) {
	fill(x + 1, y, w - 2, 1, color);
	fill(x, y + 1, w, h - 2, color);
	fill(x + 1, y + h - 1, w - 2, 1, color);
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

// proportional text, transparent background; scale 1 or 2; returns the x after it
static int text_scaled(int x, int y, const char *s, uint8_t color, int scale) {
	for (; *s; s++) {
		uint8_t c = (uint8_t)*s & 0x7F;
		const uint8_t *g = ui_font + c * 8;
		uint8_t w = glyph_width[c];
		if (x < 0 || x + w * scale > FB_W || y < 0 || y + 8 * scale > FB_H) {
			// off screen
		} else if (scale == 1) {
			ui_glyph(FB + y * FB_W + x, g, glyph_left[c], color);
		} else {
			uint8_t *row = FB + y * FB_W + x;
			for (unsigned int r = 0; r < 8; r++) {
				uint8_t bits = g[r] << glyph_left[c];
				for (unsigned int i = 0; bits; i++, bits <<= 1) {
					if (bits & 0x80) {
						uint8_t *p = row + i * scale;
						p[0] = color;
						if (scale == 2) {
							p[1] = color;
							p[FB_W] = color;
							p[FB_W + 1] = color;
						}
					}
				}
				row += FB_W * scale;
			}
		}
		x += (w + 1) * scale;
	}
	return x;
}

static int text(int x, int y, const char *s, uint8_t color) {
	return text_scaled(x, y, s, color, 1);
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

// a key cap like [2nd] followed by its meaning; returns the x after both
static int key_hint(int x, int y, const char *key, const char *what) {
	int w = text_width(key) + 8;
	round_rect(x, y - 3, w, 13, C_BAND);
	text(x + 4, y, key, C_TEXT);
	return text(x + w + 4, y, what, C_DIM) + 10;
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

static const char months[12][4] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

// "Oct 17 2026, 11:41 PM"
static void format_date(char *p, const struct state_header *h) {
	unsigned int hour = h->hour % 12;
	p = put_str(p, months[h->month - 1]);
	*p++ = ' ';
	p = put_uint(p, h->day, 1);
	*p++ = ' ';
	p = put_uint(p, state_header_year(h), 4);
	p = put_str(p, ", ");
	p = put_uint(p, hour ? hour : 12, 1);
	*p++ = ':';
	p = put_uint(p, h->minute, 2);
	put_str(p, h->hour < 12 ? " AM" : " PM");
}

/* ---------------------------------------------------------------- palette, timing, keys */

static uint16_t saved_palette[256];

// menu colors; dim = also darken the game's colors behind the pause menu
static void set_palette(bool dim) {
	for (unsigned int i = 0; i < 256; i++)
		saved_palette[i] = LCD_PALETTE[i];
	if (dim) {
		for (unsigned int i = 0; i < C_SHADOW; i++) {
			uint16_t c = saved_palette[i];
			unsigned int r = (c >> 10 & 31) * 3 / 8, g = (c >> 5 & 31) * 3 / 8, b = (c & 31) * 3 / 8;
			LCD_PALETTE[i] = r << 10 | g << 5 | b;
		}
	}
	for (unsigned int i = 0; i < C_END - C_SHADOW; i++)
		LCD_PALETTE[C_SHADOW + i] = menu_colors[i];
}

static void restore_palette(void) {
	for (unsigned int i = 0; i < C_SHADOW; i++)
		LCD_PALETTE[i] = saved_palette[i];
}

// waits for the next LCD frame (about 1/60 s)
static void wait_frame(void) {
	LCD_ICR = 8;
	while (!(LCD_RIS & 8))
		;
}

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

#define LIST_Y 40
#define LIST_ROW 14
#define LIST_ROWS 9

static void draw_rom_list(struct rom *roms, unsigned int count, unsigned int sel, unsigned int top,
                          const char *error) {
	char buf[48], *p;

	fill(0, 0, FB_W, FB_H, C_BG);
	// header
	fill(0, 0, FB_W, 30, C_RED);
	fill(0, 30, FB_W, 2, C_RED_DARK);
	text_scaled(18, 8, "FamiCalc", C_TEXT, 2);
	p = put_str(buf, "v");
	put_str(p, version_string);
	text_right(238, 15, buf, C_TEXT);

	if (!count) {
		text(18, 50, "No games found.", C_TEXT);
		text(18, 70, "Convert .nes files with FamiCalc's", C_DIM);
		text(18, 82, "converter and send all of the files", C_DIM);
		text(18, 94, "it makes to the calculator.", C_DIM);
		key_hint(18, 208, "del", "Quit");
		return;
	}

	for (unsigned int row = 0; row < LIST_ROWS && top + row < count; row++) {
		unsigned int i = top + row;
		int y = LIST_Y + row * LIST_ROW;
		bool on = i == sel;
		const char *title = roms[i].header->description[0] ? roms[i].header->description : roms[i].name;
		if (on) {
			round_rect(12, y - 3, 232, 14, C_SEL);
			fill(12, y - 2, 2, 12, C_GOLD);
		}
		if (roms[i].states) {
			p = put_uint(buf, roms[i].states, 1);
			put_str(p, roms[i].states == 1 ? " save" : " saves");
			text_right(238, y, buf, on ? C_GOLD : C_DIM);
		}
		text(20, y, fit(title, roms[i].states ? 168 : 214, buf, sizeof buf), on ? C_TEXT : C_DIM);
	}
	// scroll marks
	if (top)
		text(124, LIST_Y - 9, "\x1E", C_DIM);
	if (top + LIST_ROWS < count)
		text(124, LIST_Y + LIST_ROWS * LIST_ROW - 3, "\x1F", C_DIM);

	// details of the selected game
	fill(8, 172, 240, 1, C_BAND);
	{
		const struct romheader *h = roms[sel].header;
		p = put_str(buf, roms[sel].name);
		p = put_str(p, "  \x07  mapper ");
		p = put_uint(p, h->mapper, 1);
		p = put_str(p, "  \x07  ");
		p = put_uint(p, h->prgsize * 16, 1);
		p = put_str(p, "K");
		if (h->chrsize) {
			p = put_str(p, " + ");
			p = put_uint(p, h->chrsize * 8, 1);
			p = put_str(p, "K");
		}
		text(18, 181, buf, C_DIM);
	}
	if (error)
		text(18, 194, error, C_ERROR);
	key_hint(key_hint(18, 208, "2nd", "Play"), 208, "del", "Quit");
}

int rom_menu(struct rom *roms, unsigned int count, unsigned int selected, const char *error) {
	unsigned int top = 0;
	bool redraw = true;

	font_metrics();
	set_palette(false);
	spiUnlock();
	reset_keys();
	if (selected >= count)
		selected = 0;

	for (;;) {
		int step;
		if (selected < top)
			top = selected;
		if (selected >= top + LIST_ROWS)
			top = selected - LIST_ROWS + 1;
		if (redraw) {
			draw_rom_list(roms, count, selected, top, error);
			redraw = false;
		}
		wait_frame();
		read_keys();

		if (pressed(K_DEL) || pressed(K_CLEAR))
			return -1;
		if (!count)
			continue;
		if (pressed(K_2ND) || pressed(K_ENTER))
			return selected;
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
}

/* ---------------------------------------------------------------- pause menu */

enum {
	ITEM_RESUME, ITEM_SLOT, ITEM_SAVE, ITEM_LOAD, ITEM_DELETE, ITEM_FPS,
	ITEM_RESET, ITEM_GAMES, ITEM_QUIT, ITEM_COUNT
};
static const char *const item_labels[ITEM_COUNT] = {
	"Resume", "State slot", "Save state", "Load state", "Delete state", "Show FPS",
	"Reset game", "Change game", "Quit FamiCalc"
};

#define PX 24
#define PY 18
#define PW 208
#define PH 188
#define ROW_Y (PY + 27)
#define ROW_H 13

static uint8_t used[NUM_SLOTS / 8 + 1];
static unsigned int used_count, free_kb;
static struct slot_info info;

static void draw_pause(unsigned int sel, int confirm, const char *note) {
	char buf[48], *p;
	bool has = info.used;

	// panel with a drop shadow
	round_rect(PX + 3, PY + 3, PW, PH, C_SHADOW);
	round_rect(PX, PY, PW, PH, C_EDGE);
	round_rect(PX + 1, PY + 1, PW - 2, PH - 2, C_BG);
	fill(PX + 1, PY + 1, PW - 2, 18, C_RED);
	fill(PX + 1, PY + 19, PW - 2, 1, C_RED_DARK);
	text(PX + 9, PY + 6, "Paused", C_TEXT);
	text_right(PX + PW - 9, PY + 6,
	           fit(game_title, 130, buf, sizeof buf), C_TEXT);

	for (unsigned int i = 0; i < ITEM_COUNT; i++) {
		int y = ROW_Y + i * ROW_H;
		bool on = i == sel;
		bool off = (i == ITEM_LOAD || i == ITEM_DELETE) && !has;
		uint8_t color = off ? C_FAINT : on ? C_TEXT : C_DIM;
		const char *label = item_labels[i];

		if (on) {
			round_rect(PX + 5, y - 3, PW - 10, 13, C_SEL);
			fill(PX + 5, y - 2, 2, 11, C_GOLD);
		}
		if ((int)i == confirm) {
			p = put_str(buf, i == ITEM_SAVE ? "Overwrite slot " : i == ITEM_DELETE ? "Delete slot " :
			                 i == ITEM_RESET ? "Reset the game" : i == ITEM_GAMES ? "Leave this game" :
			                 "Quit FamiCalc");
			if (i == ITEM_SAVE || i == ITEM_DELETE)
				p = put_uint(p, save_slot, 3);
			put_str(p, "?");
			text(PX + 14, y, buf, C_GOLD);
			text_right(PX + PW - 12, y, "2nd: yes", C_GOLD);
			continue;
		}
		text(PX + 14, y, label, color);

		switch (i) {
		case ITEM_SLOT: {
			int x = PX + PW - 12;
			uint8_t num = on ? C_GOLD : C_TEXT;
			put_uint(buf, save_slot, 3);
			x -= text_width("\x10");
			text(x, y, "\x10", C_DIM);
			x -= 4 + text_width(buf);
			text(x, y, buf, num);
			x -= 4 + text_width("\x11");
			text(x, y, "\x11", C_DIM);
			if (has)
				text(x - 4 - text_width("\x07"), y, "\x07", C_GREEN);
			break;
		}
		case ITEM_LOAD:
			if (!has)
				text_right(PX + PW - 12, y, "empty", C_FAINT);
			break;
		case ITEM_FPS:
			text_right(PX + PW - 12, y, show_fps ? "On" : "Off", on ? C_GOLD : C_TEXT);
			break;
		}
	}

	// slot details and archive space
	fill(PX + 8, PY + PH - 39, PW - 16, 1, C_BAND);
	p = put_str(buf, "Slot ");
	p = put_uint(p, save_slot, 3);
	p = put_str(p, ": ");
	if (note)
		put_str(p, note);
	else if (!has)
		put_str(p, "empty");
	else if (info.dated)
		format_date(p, &info.header);
	else
		put_str(p, state_has_header(&info.header) ? "saved" : "saved (old format)");
	text(PX + 10, PY + PH - 31, buf, note ? C_GOLD : C_TEXT);
	p = put_uint(buf, used_count, 1);
	p = put_str(p, used_count == 1 ? " state saved  \x07  " : " states saved  \x07  ");
	p = put_uint(p, free_kb, 1);
	put_str(p, "K free");
	text(PX + 10, PY + PH - 17, buf, C_DIM);
}

static void select_slot(unsigned int slot) {
	save_slot = slot;
	slot_info(slot, &info);
}

uint8_t pause_menu(void) {
	unsigned int sel = ITEM_RESUME;
	int confirm = -1;
	bool redraw = true;
	unsigned int typed = 0, typed_frames = 0;
	const char *note = NULL;
	uint8_t result = MENU_RESUME;

	font_metrics();
	set_palette(true);
	spiUnlock();
	reset_keys();
	used_count = scan_states(used);
	free_kb = archive_free_kb();
	select_slot(save_slot);

	for (;;) {
		int step, d;
		if (redraw) {
			draw_pause(sel, confirm, note);
			redraw = false;
		}
		wait_frame();
		read_keys();
		if (typed_frames && !--typed_frames)
			typed = 0;

		if (pressed(K_CLEAR) || pressed(K_ALPHA) || pressed(K_MODE))
			break;

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
					note = "nothing saved here";
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
					note = "deleted";
					used_count = scan_states(used);
					free_kb = archive_free_kb();
				} else {
					note = "couldn't delete";
				}
				slot_info(save_slot, &info);
				break;
			case ITEM_FPS:
				show_fps = !show_fps;
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
	// let go of the keys first, so the game doesn't get them (or reopen the menu)
	do {
		wait_frame();
		read_keys();
	} while (keys_now[1] | keys_now[2] | keys_now[3] | keys_now[4] | keys_now[5] | keys_now[6] | keys_now[7]);
	restore_palette();
	return result;
}
