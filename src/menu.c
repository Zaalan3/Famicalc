#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <sys/power.h>
#include <sys/rtc.h>

#include "icons.h"
#include "menu.h"
#include "pictures.h"
#include "states.h"

/*
	FamiCalc's screens: the library (the list of games) and the pause menu.
	One layout everywhere (see DEVNOTES.md): a header with the screen's
	title and the time, a list on the left, a detail pane on the right (a
	picture and what the selected row does), key hints at the bottom.
	Every change draws the whole screen again while the panel keeps showing
	the last one (spiLock), then shows it between two of the LCD's transfers
	(ui_screen_full).
	Palette: 0x00-0x9F the picture shown (its own colors), 0xA0-0xC1 these
	screens' (P_ below; the in-game labels use some of them, vars.inc).
*/

extern int ui_text(const struct ui_font *font, uint8_t *dest, const char *s, uint8_t color);
extern int ui_text_width(const struct ui_font *font, const char *s);
extern void ui_fill(uint8_t *dest, unsigned int width, unsigned int height, uint8_t color);
extern void ui_screen_full(uint8_t *frame_buffer);
extern void ui_screen_game(void);
extern void spiLock(void);
extern void lcd_wait_frame(void);
extern uint8_t render_cache[];
extern unsigned int ui_stride;		// ui_fill's and ui_text's bytes per line

#define FB ((uint8_t *)0xD42000)
#define FB_W 320
#define FB_H 240
// the game's frame buffer (256 bytes per line, columns 8-247 are shown)
#define VBUFFER ((uint8_t *)0xD42000)
#define VBUFFER_SIZE (256 * 224)
// the menu's frame buffer runs into the emulator's memory from here on;
// the pause menu keeps a copy at the start of render_cache
#define EMU_PAGE ((uint8_t *)0xD50000)
#define EMU_COVERED (FB + FB_W * FB_H - EMU_PAGE)
// the game's picture when the pause menu opened, after that copy
#define NOW ((struct picture *)(render_cache + EMU_COVERED))
// a dialog keeps the colors it dims here (after NOW; not on the stack: the
// eZ80 reaches only 127 bytes of a stack frame cheaply)
#define DIALOG_SAVED ((uint16_t *)(render_cache + 40000))

#define KEYS ((volatile uint16_t *)0xF50010)
#define KEY_MODE (*(volatile uint8_t *)0xF50000)
#define LCD_PALETTE ((volatile uint16_t *)0xE30200)

// palette entries: 1555, 5 bits per channel
#define RGB(r, g, b) ((((r) >> 3) << 10) | (((g) >> 3) << 5) | ((b) >> 3))

/* ---------------------------------------------------------------- tokens */

// keep P_SURFACE..P_BLACK in sync with vars.inc
enum {
	P_BG = 0xA0,		// the background's 8 bands, top to bottom
	P_SURFACE = 0xA8, P_SURFACE_HI, P_SURFACE_HI2, P_STROKE, P_TEXT1, P_TEXT2, P_TEXT3, P_ACCENT,
	P_GREEN, P_SHADOW, P_INK, P_BLACK, P_PAD_EDGE, P_PAD_BODY, P_PAD_BAND, P_PAD_DARK, P_PAD_RED,
	P_PAD_KEY, P_KEY_NUM,
	// a dialog's own copies, which stay bright while everything else dims
	P_D_SURFACE, P_D_SURFACE_HI, P_D_TEXT1, P_D_TEXT2, P_D_ACCENT, P_D_INK, P_D_SHADOW,
	P_END
};
static const uint16_t ui_colors[P_END - P_SURFACE] = {
	RGB(30, 32, 38),	// surface: cards, empty panes
	RGB(44, 47, 56),	// the focused row, key caps
	RGB(60, 64, 75),	// a focused key cap, switches off
	RGB(50, 54, 63),	// hairlines
	RGB(243, 244, 246),	// text
	RGB(166, 170, 180),	// secondary text
	RGB(108, 113, 124),	// labels, hints
	RGB(240, 80, 90),	// accent (FamiCalc red)
	RGB(74, 222, 128),	// on
	RGB(5, 6, 8),		// shadows
	RGB(12, 13, 16),	// text on white
	RGB(0, 0, 0),		// black (around the game's picture)
	RGB(150, 153, 160), RGB(196, 199, 206), RGB(28, 29, 33), RGB(64, 66, 72), RGB(196, 46, 56),
	RGB(100, 102, 110),	// the NES pad's edge, body, band, D-pad, A and B, Select and Start
	RGB(112, 116, 126),	// the keypad's number keys
	RGB(30, 32, 38), RGB(44, 47, 56), RGB(243, 244, 246), RGB(166, 170, 180), RGB(240, 80, 90),
	RGB(12, 13, 16), RGB(5, 6, 8),
};

// The background: a dark gradient, tinted by the picture shown (a quarter of
// its color at the top, an eighth at the bottom). 8 bit math only: wider
// shifts are library calls on the eZ80.
static const uint8_t bg_top[3] = { 24, 26, 31 }, bg_bottom[3] = { 12, 13, 16 };

__attribute__((noinline)) static void tint(const uint8_t *rgb) {
	uint8_t c[3], step[3];
	volatile uint8_t *entry = (volatile uint8_t *)&LCD_PALETTE[P_BG];
	for (unsigned int i = 0; i < 3; i++) {
		uint8_t top = bg_top[i], bottom = bg_bottom[i];
		if (rgb) {
			top += (rgb[i] >> 2) - (top >> 2);
			bottom += (rgb[i] >> 3) - (bottom >> 3);
		}
		c[i] = top;
		// the top is always the lighter (see the colors above); rounded
		step[i] = (uint8_t)(top - bottom + 4) >> 3;
	}
	for (unsigned int k = 0; k < 8; k++, entry += 2) {
		// 1555: rrrrrggg gggbbbbb
		uint8_t r = c[0] >> 3, g = c[1] >> 3, b = c[2] >> 3;
		entry[0] = (uint8_t)(g << 5) | b;
		entry[1] = (uint8_t)(r << 2) | g >> 3;
		for (unsigned int i = 0; i < 3; i++)
			c[i] -= step[i];
	}
}

void menu_palette(void) {
	for (unsigned int i = 0; i < P_END - P_SURFACE; i++)
		LCD_PALETTE[P_SURFACE + i] = ui_colors[i];
	tint(NULL);
}

/* ---------------------------------------------------------------- fonts */

extern const uint8_t font_body_widths[128], font_body_bits[], font_display_widths[128], font_display_bits[];
static uint16_t body_offsets[128], display_offsets[128];
const struct ui_font font_body = { 10, font_body_widths, font_body_bits, body_offsets };
const struct ui_font font_display = { 14, font_display_widths, font_display_bits, display_offsets };
#define BODY (&font_body)
#define DISPLAY (&font_display)

static void font_offsets(const struct ui_font *f, uint16_t *offsets) {
	unsigned int at = 0;
	for (unsigned int c = 0; c < 128; c++) {
		offsets[c] = at;
		at += (f->widths[c] * f->rows + 7) / 8;
	}
}

void fonts_init(void) {
	font_offsets(BODY, body_offsets);
	font_offsets(DISPLAY, display_offsets);
}

/* ---------------------------------------------------------------- grid */

#define M 16			// outer margin
#define ROW 18			// list pitch
#define TOP 44			// content
#define LIST_X 8		// the focus pill; text at M
#define LIST_W 166
#define LIST_ROWS 9
#define PANE_X 184		// the detail pane, at the right margin
#define PANE_W 120
#define PANE_H 112
#define CAP_Y (TOP + PANE_H + 10)
#define FOOT_Y 219

/* ---------------------------------------------------------------- drawing */

static uint8_t *at(int x, int y) {
	return FB + y * FB_W + x;
}

static void fill(int x, int y, int w, int h, uint8_t color) {
	if (w > 0 && h > 0)
		ui_fill(at(x, y), w, h, color);
}

// a rectangle with corners of radius r (1-6): each row's inset
static const uint8_t insets[] = { 1, 2, 1, 3, 1, 1, 4, 2, 1, 1, 5, 3, 2, 1, 1, 6, 4, 3, 2, 1, 1 };

static void rrect(int x, int y, int w, int h, uint8_t color, int r) {
	const uint8_t *in = insets + r * (r - 1) / 2;
	for (int i = 0; i < r; i++) {
		fill(x + in[i], y + i, w - 2 * in[i], 1, color);
		fill(x + in[i], y + h - 1 - i, w - 2 * in[i], 1, color);
	}
	fill(x, y + r, w, h - 2 * r, color);
}

static int text(const struct ui_font *f, int x, int y, const char *s, uint8_t color) {
	return x + ui_text(f, at(x, y), s, color);
}

static void text_right(const struct ui_font *f, int right, int y, const char *s, uint8_t color) {
	text(f, right - ui_text_width(f, s), y, s, color);
}

static void text_center(const struct ui_font *f, int center, int y, const char *s, uint8_t color) {
	text(f, center - ui_text_width(f, s) / 2, y, s, color);
}

// s, shortened with an ellipsis to fit in width pixels (buf: 48 bytes)
static const char *fit(const struct ui_font *f, const char *s, int width, char *buf) {
	size_t n = strlen(s);
	if (ui_text_width(f, s) <= width)
		return s;
	if (n > 46)
		n = 46;
	memcpy(buf, s, n);
	do {
		while (n && buf[n - 1] == ' ')
			n--;
		buf[n] = '\x08';
		buf[n + 1] = 0;
	} while (n-- && ui_text_width(f, buf) > width);
	return buf;
}

static void icon(unsigned int id, int x, int y, uint8_t color) {
	uint8_t *p = at(x, y);
	for (unsigned int r = 0; r < 11; r++, p += FB_W) {
		// a row's two bytes, high one first (8 bit masks: wider shifts are library calls)
		const uint8_t *row = (const uint8_t *)&icon_rows[id][r];
		uint8_t *q = p;
		for (int k = 1; k >= 0; k--)
			for (uint8_t m = 0x80; m; m >>= 1, q++)
				if (row[k] & m)
					*q = color;
	}
}

static void background(void) {
	for (int k = 0; k < 8; k++)
		fill(0, k * 30, FB_W, 30, P_BG + k);
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

// "3 saved states"
static char *put_count(char *p, unsigned int n, const char *one, const char *many) {
	p = put_uint(p, n, 1);
	*p++ = ' ';
	return put_str(p, n == 1 ? one : many);
}

// "Today, 9:41 AM", "Oct 20, 6:12 PM", "Oct 20, 2025" (another year)
static uint8_t today_day, today_month;
static unsigned int today_year;
static uint8_t battery;		// read when a screen opens: reading it takes a while

static char *put_when(char *p, const struct state_header *h) {
	char date[16];
	if (h->day == today_day && h->month == today_month && state_header_year(h) == today_year) {
		p = put_str(p, "Today");
	} else {
		state_date_text(h, date);
		if (state_header_year(h) == today_year)
			date[strlen(date) - 6] = 0;	// without ", 2026"
		p = put_str(p, date);
	}
	p = put_str(p, ", ");
	state_time_text(h, p);
	return p + strlen(p);
}

static void note_today(void) {
	uint8_t day, month;
	uint16_t year;
	battery = boot_GetBatteryStatus();
	boot_GetDate(&day, &month, &year);
	today_day = day;
	today_month = month;
	today_year = year;
}

/* ---------------------------------------------------------------- components */

struct hint {
	const char *key, *what;
};

// the clock and the battery at the top right
__attribute__((noinline)) static void status(void) {
	char buf[8], *p;
	uint8_t s, m, h;
	int bx = FB_W - M - 16;
	unsigned int level = battery;

	fill(bx, 13, 14, 1, P_TEXT2);
	fill(bx, 20, 14, 1, P_TEXT2);
	fill(bx, 14, 1, 6, P_TEXT2);
	fill(bx + 13, 14, 1, 6, P_TEXT2);
	fill(bx + 14, 15, 2, 4, P_TEXT2);
	for (unsigned int i = 0; i < (level * 3 + 3) / 4 && i < 3; i++)
		fill(bx + 2 + i * 4, 15, 3, 4, P_TEXT1);
	boot_GetTime(&s, &m, &h);
	p = put_uint(buf, h % 12 ? h % 12 : 12, 1);
	*p++ = ':';
	put_uint(p, m, 2);
	text_right(BODY, bx - 7, 13, buf, P_TEXT2);
}

// the screen's title (display font), what it is about, a back arrow in the margin
static void draw_header(const char *title, const char *subtitle, bool back) {
	char buf[48];
	if (back)
		icon(ICON_BACK, 2, 11, P_TEXT3);
	text(DISPLAY, M, 10, fit(DISPLAY, title, 220, buf), P_TEXT1);
	if (subtitle)
		text(BODY, M, 26, fit(BODY, subtitle, 220, buf), P_TEXT3);
	status();
}

static void footer(const struct hint *h, unsigned int n) {
	int x = M;
	for (; n; n--, h++) {
		int w = ui_text_width(BODY, h->key) + 8;
		rrect(x, FOOT_Y, w, 13, P_SURFACE_HI, 3);
		text(BODY, x + 4, FOOT_Y + 2, h->key, P_TEXT1);
		x = text(BODY, x + w + 5, FOOT_Y + 2, h->what, P_TEXT2) + 12;
	}
}

// a list row: an icon or a slot number before the label, a value at the right
static void row(int y, bool on, int icon_id, const char *lead, const char *label, const char *value, bool dim) {
	char buf[48];
	int x = M, right = LIST_X + LIST_W - 8;
	if (on)
		rrect(LIST_X, y, LIST_W, ROW - 2, P_SURFACE_HI, 4);
	if (icon_id >= 0) {
		icon(icon_id, x, y + 3, on ? P_ACCENT : P_TEXT3);
		x += 19;
	}
	if (lead) {
		text(BODY, x, y + 4, lead, on ? P_ACCENT : P_TEXT3);
		x += 30;
	}
	if (value) {
		text_right(BODY, right, y + 4, value, on ? P_TEXT2 : P_TEXT3);
		right -= ui_text_width(BODY, value) + 10;
	}
	text(BODY, x, y + 4, fit(BODY, label, right - x, buf), on ? P_TEXT1 : dim ? P_TEXT3 : P_TEXT2);
}

// where a list of count rows is, shown from first
static void scroll_mark(unsigned int first, unsigned int count) {
	int track = LIST_ROWS * ROW - 2 - 16;
	if (count <= LIST_ROWS)
		return;
	rrect(LIST_X + LIST_W + 3, TOP + track * (int)first / (int)(count - LIST_ROWS), 2, 16, P_TEXT3, 1);
}

static void poll_keys(void);

// The pane's picture: a shadow, a hairline, round corners. The background
// takes a little of its color (finish).
static bool pane_shown;
static const void *pane_source, *tinted;	// the picture shown, the one the tint is from
static uint8_t tint_rgb[3];

static void pane_frame(void) {
	rrect(PANE_X + 1, TOP + 2, PANE_W, PANE_H, P_SHADOW, 4);
	rrect(PANE_X - 1, TOP - 1, PANE_W + 2, PANE_H + 2, P_STROKE, 5);
}

static void pane_corners(void) {
	static const uint8_t in[4] = { 4, 2, 1, 1 };
	pane_shown = true;
	for (int i = 0; i < 4; i++) {
		fill(PANE_X, TOP + i, in[i], 1, P_STROKE);
		fill(PANE_X + PANE_W - in[i], TOP + i, in[i], 1, P_STROKE);
		fill(PANE_X, TOP + PANE_H - 1 - i, in[i], 1, P_STROKE);
		fill(PANE_X + PANE_W - in[i], TOP + PANE_H - 1 - i, in[i], 1, P_STROKE);
	}
}

static void pane_picture(const struct picture *pic) {
	pane_source = pic;
	pane_frame();
	pic_draw(pic, at(PANE_X, TOP));
	pane_corners();
}

// a state's stored picture (state_picture), or s on an empty pane
static void pane_state(const uint8_t *pic, const char *s) {
	if (!pic) {
		rrect(PANE_X, TOP, PANE_W, PANE_H, P_SURFACE, 4);
		text_center(BODY, PANE_X + PANE_W / 2, TOP + 51, s, P_TEXT3);
		return;
	}
	pane_source = pic;
	pane_frame();
	pic_draw_stored(pic, at(PANE_X, TOP));
	pane_corners();
}

// tints the background with the pane's picture, then shows the screen
static void finish(void) {
	if (pane_shown && pane_source != tinted)
		pic_average(at(PANE_X + 4, TOP), tint_rgb);
	tinted = pane_shown ? pane_source : NULL;
	tint(pane_shown ? tint_rgb : NULL);
	pane_shown = false;
	poll_keys();
	ui_screen_full(FB);
}

static void pane_empty(const char *s) {
	rrect(PANE_X, TOP, PANE_W, PANE_H, P_SURFACE, 4);
	text_center(BODY, PANE_X + PANE_W / 2, TOP + 51, s, P_TEXT3);
}

// line n of the pane's caption, wrapped at spaces; returns the next line
// (negative lines are above the caption's place: panes without a picture)
static int caption(int n, const char *s, uint8_t color) {
	char buf[48];
	while (*s) {
		size_t len = strlen(s), cut = len;
		if (len > 46)
			len = cut = 46;
		memcpy(buf, s, len);
		buf[len] = 0;
		while (ui_text_width(BODY, buf) > PANE_W && cut) {
			while (cut && buf[cut - 1] != ' ')
				cut--;
			if (!cut)
				break;
			buf[--cut] = 0;
		}
		if (!cut)
			cut = len;
		text(BODY, PANE_X, CAP_Y + n++ * 13, fit(BODY, buf, PANE_W, buf + 0), color);
		s += cut;
		while (*s == ' ')
			s++;
	}
	return n;
}

static void toggle(int right, int y, bool on) {
	rrect(right - 22, y, 22, 12, on ? P_GREEN : P_SURFACE_HI2, 5);
	rrect(right - (on ? 11 : 21), y + 1, 10, 10, P_TEXT1, 4);
}

static void segmented(int x, int y, const char *const *labels, unsigned int n, unsigned int sel) {
	int w = 2;
	for (unsigned int i = 0; i < n; i++)
		w += ui_text_width(BODY, labels[i]) + 14;
	rrect(x, y, w, 16, P_SURFACE, 4);
	x++;
	for (unsigned int i = 0; i < n; i++) {
		int ow = ui_text_width(BODY, labels[i]) + 14;
		if (i == sel)
			rrect(x, y + 1, ow, 14, P_SURFACE_HI2, 4);
		text(BODY, x + 7, y + 4, labels[i], i == sel ? P_TEXT1 : P_TEXT3);
		x += ow;
	}
}

// a key's name in a small cap, right aligned at right
static void key_chip(int right, int y, const char *name, bool on) {
	int w = ui_text_width(BODY, name) + 8;
	rrect(right - w, y + 2, w, 13, on ? P_SURFACE_HI2 : P_SURFACE, 3);
	text(BODY, right - w + 4, y + 4, name, P_TEXT1);
}

// the NES controller, 120x52; lit: 0 none, 1 A, 2 B, 3 Select, 4 Start, 5 the D-pad
__attribute__((noinline)) static void nes_pad(int x, int y, unsigned int lit) {
	// x, y, width, height, color, corner radius, the button it is (lit: white)
	static const uint8_t parts[9][7] = {
		{ 0, 0, 120, 52, P_PAD_EDGE, 4, 0 }, { 1, 1, 118, 50, P_PAD_BODY, 4, 0 },
		{ 6, 10, 108, 34, P_PAD_BAND, 2, 0 },
		{ 20, 17, 7, 20, P_PAD_DARK, 0, 5 }, { 14, 23, 19, 7, P_PAD_DARK, 0, 5 },
		{ 44, 28, 12, 5, P_PAD_KEY, 2, 3 }, { 60, 28, 12, 5, P_PAD_KEY, 2, 4 },
		{ 80, 22, 13, 13, P_PAD_RED, 5, 2 }, { 97, 22, 13, 13, P_PAD_RED, 5, 1 },
	};
	for (unsigned int i = 0; i < 9; i++) {
		const uint8_t *p = parts[i];
		rrect(x + p[0], y + p[1], p[2], p[3], p[6] && p[6] == lit ? P_TEXT1 : p[4], p[5]);
	}
}

/*
	The calculator's keys, 106x95, with the key of group g, bit b (the
	keypad's matrix, keypadc.h) lit. Rows 3-9 follow the matrix: column c
	is group c + 2, row r is bit 9 - r.
*/
__attribute__((noinline)) static void keypad(int x, int y, unsigned int g, unsigned int b) {
	for (unsigned int r = 0; r < 10; r++)
		for (unsigned int c = 0; c < 5; c++) {
			unsigned int kg, kb;
			if (r == 0)
				kg = 1, kb = 4 - c;
			else if (r <= 2 && c >= 3)
				continue;		// the arrow keys are there
			else if (r == 1)
				kg = 1, kb = 5 + c;
			else if (r == 2)
				kg = 2 + c, kb = 7;
			else
				kg = c + 2, kb = 9 - r;
			bool number = r >= 6 && c >= 1 && c <= 3;
			uint8_t color = kg == g && kb == b && !(r == 9 && c == 0) ? P_ACCENT : number ? P_KEY_NUM :
			                P_SURFACE_HI2;
			rrect(x + c * 22, y + r * 9 + (r ? 3 : 0), 18, 6, color, 2);
		}
	// the arrow keys: a diamond in the gap rows 1-2 leave
	static const int8_t arrows[4][2] = { { -5, 6 }, { -17, 0 }, { 7, 0 }, { -5, -6 } };	// down left right up
	for (unsigned int k = 0; k < 4; k++)
		rrect(x + 86 + arrows[k][0], y + 18 + arrows[k][1], 10, 6, g == 7 && b == k ? P_ACCENT : P_SURFACE_HI2, 2);
}

/* ---------------------------------------------------------------- keys */

static uint8_t keys_now[8], keys_last[8], keys_seen[8];

// notes the keys down now (while a screen is drawn: a short press counts)
static void poll_keys(void) {
	for (unsigned int g = 1; g < 8; g++)
		keys_seen[g] |= KEYS[g];
}

static void read_keys(void) {
	poll_keys();
	for (unsigned int g = 1; g < 8; g++) {
		keys_last[g] = keys_now[g];
		keys_now[g] = keys_seen[g];
		keys_seen[g] = 0;
	}
}

// ignore keys that are already down when a screen opens
static void reset_keys(void) {
	read_keys();
	read_keys();
}

static bool any_key_down(void) {
	return keys_now[1] | keys_now[2] | keys_now[3] | keys_now[4] | keys_now[5] | keys_now[6] | keys_now[7];
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

static bool ok_pressed(void) {
	return pressed(K_2ND) || pressed(K_ENTER);
}

static bool back_pressed(void) {
	return pressed(K_CLEAR) || pressed(K_ALPHA) || pressed(K_MODE);
}

// waits for the next frame and reads the keys
static void next_frame(void) {
	lcd_wait_frame();
	read_keys();
}

/* ---------------------------------------------------------------- dialog */

/*
	A question over the screen, which dims (the palette: the dialog has its
	own entries). Returns the button chosen, or -1 ([clear]). The focus
	starts on the last button (the one that does what was asked for);
	destructive: that button is red.
*/
static int dialog(const char *title, const char *line1, const char *line2, const char *const *buttons,
                  unsigned int n, bool destructive) {
	uint16_t *saved = DIALOG_SAVED;
	int lines = 1 + (line2 != NULL), h = 40 + 12 * lines + 30, w = 232;
	int x = (FB_W - w) / 2, y = (FB_H - h) / 2;
	unsigned int sel = n - 1;
	int choice = -1;
	char buf[48];

	spiLock();
	for (unsigned int i = 0; i < P_D_SURFACE; i++) {
		// 3/8 of each channel
		uint16_t c = saved[i] = LCD_PALETTE[i];
		LCD_PALETTE[i] = ((c >> 2) & 0x1CE7) + ((c >> 3) & 0x0C63);
	}
	rrect(x + 2, y + 3, w, h, P_D_SHADOW, 6);
	rrect(x, y, w, h, P_D_SURFACE, 6);
	text(DISPLAY, x + 16, y + 14, fit(DISPLAY, title, w - 32, buf), P_D_TEXT1);
	text(BODY, x + 16, y + 34, fit(BODY, line1, w - 32, buf), P_D_TEXT2);
	if (line2)
		text(BODY, x + 16, y + 46, fit(BODY, line2, w - 32, buf), P_D_TEXT2);
	reset_keys();
	for (;;) {
		int bx = x + w - 16;
		for (int i = n - 1; i >= 0; i--) {
			int bw = ui_text_width(BODY, buttons[i]) + 22;
			bool on = (unsigned int)i == sel, red = destructive && i == (int)n - 1;
			bx -= bw;
			rrect(bx, y + h - 28, bw, 17, on ? (red ? P_D_ACCENT : P_D_TEXT1) : P_D_SURFACE_HI, 4);
			text(BODY, bx + 11, y + h - 24, buttons[i],
			     on ? (red ? P_D_TEXT1 : P_D_INK) : red ? P_D_ACCENT : P_D_TEXT1);
			bx -= 6;
		}
		ui_screen_full(FB);
		do {
			next_frame();
			if (ok_pressed())
				choice = sel;
			else if (pressed(K_CLEAR))
				choice = -2;
		} while (choice == -1 && !arrow(2, K_LEFT) && !arrow(3, K_RIGHT));
		if (choice != -1)
			break;
		sel = (sel + 1) % n;
		spiLock();
	}
	spiLock();
	for (unsigned int i = 0; i < P_D_SURFACE; i++)
		LCD_PALETTE[i] = saved[i];
	reset_keys();
	return choice < 0 ? -1 : choice;
}

static bool confirm(const char *title, const char *line1, const char *line2, const char *action) {
	const char *buttons[2] = { "Cancel", action };
	return dialog(title, line1, line2, buttons, 2, true) == 1;
}

/* ---------------------------------------------------------------- slots */

static uint8_t used[NUM_SLOTS / 8 + 1];	// and AUTO_SLOT
static unsigned int used_count, free_kb;
static int newest_slot;			// the newest dated state, -1: none
static unsigned int cur_slot;		// the slot under a slot list's cursor
static struct slot_info cur;		// what it holds

static bool slot_used(unsigned int slot) {
	return used[slot >> 3] & (1 << (slot & 7));
}

// romname's states: which slots, how many, the newest
__attribute__((noinline)) static void scan_slots(void) {
	uint32_t newest = 0;
	struct slot_info si;

	used_count = scan_states(used);
	free_kb = archive_free_kb();
	newest_slot = -1;
	for (unsigned int s = 0; s <= AUTO_SLOT; s++)
		// the later slot when two have the same minute
		if (slot_used(s) && slot_info(s, &si) && state_header_stamp(&si.header) &&
		    state_header_stamp(&si.header) >= newest) {
			newest = state_header_stamp(&si.header);
			newest_slot = s;
		}
}

// "Slot 012", "Auto save"
static char *put_slot(char *p, unsigned int slot) {
	if (slot == AUTO_SLOT)
		return put_str(p, "Auto save");
	p = put_str(p, "Slot ");
	return put_uint(p, slot, 3);
}

// when a state was saved
static char *put_saved(char *p, const struct slot_info *si) {
	if (si->dated)
		return put_when(p, &si->header);
	return put_str(p, state_has_header(&si->header) ? "Saved" : "Older FamiCalc");
}

/* ---------------------------------------------------------------- pages */

/*
	Save state (all 1000 slots), Load state (the used ones, the auto save
	first), Settings and Controls: in the pause menu, and (Load state as
	"Play from a state", Settings, Controls) in the library. sub_page runs
	one until [clear].
*/
enum { PAGE_SAVE, PAGE_LOAD, PAGE_SETTINGS, PAGE_CONTROLS };

static bool in_game;			// the pause menu, else the library
static bool screen_changed;		// the picture's size, in a game

static unsigned int list_count(int page) {
	return page == PAGE_SAVE ? NUM_SLOTS : used_count;
}

static unsigned int list_slot(int page, unsigned int row) {
	if (page == PAGE_SAVE)
		return row;
	if (slot_used(AUTO_SLOT) && !row--)
		return AUTO_SLOT;
	for (unsigned int s = 0; s < NUM_SLOTS; s++)
		if (slot_used(s) && !row--)
			return s;
	return 0;
}

// the row of a slot (Load: of the first used slot from there on, or the last)
static unsigned int list_row(int page, unsigned int slot) {
	unsigned int row = slot_used(AUTO_SLOT);
	if (page == PAGE_SAVE)
		return slot < NUM_SLOTS ? slot : 0;
	if (slot == AUTO_SLOT)
		return 0;
	for (unsigned int s = 0; s < slot && s < NUM_SLOTS; s++)
		row += slot_used(s);
	return row < used_count ? row : used_count ? used_count - 1 : 0;
}

// the cursor of a list at row: the rows shown follow it
__attribute__((noinline)) static void list_cursor(int page, unsigned int row, unsigned int *sel, unsigned int *top) {
	*sel = row;
	if (row < *top)
		*top = row;
	if (row >= *top + LIST_ROWS)
		*top = row - LIST_ROWS + 1;
	cur_slot = list_slot(page, row);
	slot_info(cur_slot, &cur);
}

__attribute__((noinline)) static void draw_list(int page, unsigned int sel, unsigned int top) {
	char buf[48], lead[5];
	unsigned int count = list_count(page);

	for (unsigned int i = 0; i < LIST_ROWS && top + i < count; i++) {
		unsigned int r = top + i, slot = list_slot(page, r);
		struct slot_info si;
		if (slot == AUTO_SLOT)
			strcpy(lead, "Auto");
		else
			put_uint(lead, slot, 3);
		if (slot_used(slot) && slot_info(slot, &si)) {
			put_saved(buf, &si);
			row(TOP + i * ROW, r == sel, -1, lead, buf, NULL, false);
		} else {
			row(TOP + i * ROW, r == sel, -1, lead, "Empty", NULL, true);
		}
	}
	scroll_mark(top, count);
	if (page == PAGE_LOAD && !used_count) {
		pane_empty("No saved states");
		caption(0, "Save one with Save state, or press sto\x1A while you play", P_TEXT2);
		return;
	}
	put_slot(buf, cur_slot);
	caption(0, buf, P_TEXT3);
	if (!cur.used) {
		pane_empty("Empty slot");
		caption(1, "Empty", P_TEXT1);
		return;
	}
	pane_state(state_picture(romname, cur_slot), "No picture");
	put_saved(buf, &cur);
	caption(1, buf, P_TEXT1);
	if (page == PAGE_SAVE)
		caption(2, "Saving replaces it", P_TEXT2);
	else if ((int)cur_slot == newest_slot)
		caption(2, "Newest", P_TEXT2);
}

enum { SET_PICTURE, SET_FPS, SET_BRIGHTNESS, SET_SAVE_ON_QUIT, SET_PICTURES, SET_CONTROLS, SET_ABOUT, SET_COUNT };
static const uint8_t set_y[SET_COUNT] = {
	TOP, TOP + ROW, TOP + 2 * ROW, TOP + 3 * ROW, TOP + 4 * ROW, TOP + 5 * ROW + 6, TOP + 6 * ROW + 6
};

static bool wide_setting(void) {
	return in_game ? wide_buffer != NULL : wide_wanted;
}

// a key's name (keypad group 1-7, mask)
static const char *key_name(uint8_t group, uint8_t mask) {
	static const char *const names[7][8] = {
		{ "graph", "trace", "zoom", "window", "y=", "2nd", "mode", "del" },
		{ "", "sto\x1A", "ln", "log", "x\x12", "x\x13\x14", "math", "alpha" },
		{ "0", "1", "4", "7", ",", "sin", "apps", "X,T,\x01,n" },
		{ ".", "2", "5", "8", "(", "cos", "prgm", "stat" },
		{ "(-)", "3", "6", "9", ")", "tan", "vars", "" },
		{ "enter", "+", "-", "\x15", "\x16", "^", "clear", "" },
		{ "\x1F", "\x11", "\x10", "\x1E", "", "", "", "" },
	};
	unsigned int bit = 0;
	while (mask > 1)
		mask >>= 1, bit++;
	return group >= 1 && group <= 7 ? names[group - 1][bit] : "";
}

__attribute__((noinline)) static void draw_settings(unsigned int sel) {
	static const char *const pictures[2] = { "Normal", "Wide 4:3" };
	static const char *const labels[SET_COUNT] = {
		"Picture", "Show FPS", "Brightness", "Save on quit", "State pictures", "Controls", "About FamiCalc"
	};
	char buf[48], *p;
	int right = LIST_X + LIST_W - 8;
	bool wide = wide_setting();

	put_uint(buf, brightness, 1);
	for (unsigned int i = 0; i < SET_COUNT; i++)
		row(set_y[i], i == sel, -1, NULL, labels[i], i == SET_PICTURE ? pictures[wide] : i == SET_BRIGHTNESS ? buf :
		    NULL, false);
	toggle(right, set_y[SET_FPS] + 3, show_fps);
	toggle(right, set_y[SET_SAVE_ON_QUIT] + 3, save_on_quit);
	toggle(right, set_y[SET_PICTURES] + 3, state_pictures);

	switch (sel) {
	case SET_PICTURE:
	case SET_FPS: {
		// a screen with the picture's size in it
		int w = wide ? 112 : 90;
		uint8_t *dest = at(PANE_X + (PANE_W - w) / 2, TOP + 3);
		rrect(PANE_X - 1, TOP - 1, PANE_W + 2, 92, P_STROKE, 5);
		rrect(PANE_X, TOP, PANE_W, 90, P_BLACK, 4);
		if (in_game) {
			// the middle of the game's picture, as wide as it is on the screen
			pic_colors(NOW);
			pane_shown = true;
			for (int y = 0; y < 84; y++)
				memcpy(dest + y * FB_W, NOW->pixels + (y + 14) * PIC_W + (PIC_W - w) / 2, w);
		} else {
			fill(PANE_X + (PANE_W - w) / 2, TOP + 3, w, 84, P_SURFACE_HI);
		}
		if (sel == SET_PICTURE) {
			segmented(PANE_X, TOP + 100, pictures, 2, wide);
			caption(1, wide ? "NES pixels 5/4 as wide, like a TV" : "Each NES pixel is one screen pixel", P_TEXT2);
		} else {
			for (int k = 0; k < 2; k++) {
				int x = wide ? PANE_X + 6 + k * 12 : PANE_X + 4, y = wide ? TOP + 1 : TOP + 4 + k * 12;
				fill(x, y, 7, 4, show_fps ? (k ? P_TEXT2 : P_TEXT1) : P_SURFACE_HI2);
				fill(x, y + 6, 7, 2, show_fps ? P_TEXT3 : P_SURFACE_HI2);
			}
			caption(0, "Frame rates beside the picture, never on it", P_TEXT2);
		}
		break;
	}
	case SET_BRIGHTNESS:
		icon(ICON_BRIGHTNESS, PANE_X, TOP + 4, P_TEXT2);
		text(BODY, PANE_X + 18, TOP + 5, "Brightness", P_TEXT2);
		rrect(PANE_X, TOP + 28, PANE_W, 4, P_SURFACE_HI2, 2);
		rrect(PANE_X, TOP + 28, 5 + (PANE_W - 10) * brightness / 10, 4, P_TEXT1, 2);
		rrect(PANE_X + (PANE_W - 10) * brightness / 10, TOP + 24, 10, 12, P_TEXT1, 4);
		p = put_str(buf, "Press ");
		p = put_str(p, key_name(hot_keys[2 * HOT_BRIGHTER], hot_keys[2 * HOT_BRIGHTER + 1]));
		p = put_str(p, " and ");
		p = put_str(p, key_name(hot_keys[2 * HOT_DARKER], hot_keys[2 * HOT_DARKER + 1]));
		put_str(p, " in games too");
		caption(-5, buf, P_TEXT2);
		break;
	case SET_SAVE_ON_QUIT:
		caption(-9, "Leaving a game saves it to the Auto save first. The library continues from it.", P_TEXT2);
		break;
	case SET_PICTURES:
		caption(-9, "Each state keeps a picture of the game (about 4 KB).", P_TEXT2);
		break;
	case SET_CONTROLS:
		caption(-9, "The keys for the NES's buttons and the hotkeys.", P_TEXT2);
		break;
	case SET_ABOUT:
		text(DISPLAY, PANE_X, TOP + 2, "FamiCalc", P_TEXT1);
		p = put_str(buf, "Version ");
		put_str(p, version_string);
		text(BODY, PANE_X, TOP + 22, buf, P_TEXT2);
		text(BODY, PANE_X, TOP + 35, "NES emulator for the", P_TEXT2);
		text(BODY, PANE_X, TOP + 48, "TI-84 Plus CE", P_TEXT2);
		break;
	}
}

static const char *const hot_labels[HOT_COUNT] = {
	"Save state", "Load state", "Load the newest", "Next slot", "Previous slot", "FPS counter", "Brighter", "Darker"
};
static const char *const nes_labels[5] = { "A", "B", "Select", "Start", "D-pad" };

// tab 0: the NES's buttons (the D-pad stays on the arrows), 1: the hotkeys
__attribute__((noinline)) static void draw_controls(unsigned int tab, unsigned int sel, bool asking) {
	static const char *const tabs[2] = { "NES buttons", "Hotkeys" };
	unsigned int n = tab ? HOT_COUNT : 5;
	const uint8_t *keys = tab ? hot_keys : nes_keys;
	int right = LIST_X + LIST_W - 8;
	char buf[48];

	segmented(M, TOP, tabs, 2, tab);
	for (unsigned int i = 0; i < n; i++) {
		int y = TOP + 24 + i * ROW;
		bool on = i == sel, fixed = !tab && i == 4;
		row(y, on, -1, NULL, tab ? hot_labels[i] : nes_labels[i], NULL, fixed);
		if (on && asking)
			text_right(BODY, right, y + 4, "Press a key\x08", P_ACCENT);
		else
			key_chip(right, y, fixed ? "\x09" : key_name(keys[2 * i], keys[2 * i + 1]), on);
	}
	if (!tab) {
		nes_pad(PANE_X, TOP + 4, sel + 1);
		caption(-4, sel == 4 ? "The arrow keys" : "Press 2nd, then the key for it", P_TEXT2);
	} else {
		keypad(PANE_X + 7, TOP + 2, keys[2 * sel], keys[2 * sel + 1]);
		put_str(put_str(put_str(buf, "Press "), key_name(keys[2 * sel], keys[2 * sel + 1])), " in games");
		caption(-1, buf, P_TEXT2);
	}
}

static void show_sub(int page, unsigned int sel, unsigned int top, unsigned int tab, bool asking) {
	static const struct hint save_hints[] = { { "2nd", "Save" }, { "del", "Delete" }, { "clear", "Back" } };
	static const struct hint save_empty[] = { { "2nd", "Save" }, { "clear", "Back" } };
	static const struct hint load_hints[] = { { "2nd", "Load" }, { "del", "Delete" }, { "clear", "Back" } };
	static const struct hint play_hints[] = { { "2nd", "Play" }, { "del", "Delete" }, { "clear", "Back" } };
	static const struct hint set_hints[] = { { "\x11 \x10", "Change" }, { "clear", "Back" } };
	static const struct hint control_hints[] = { { "2nd", "Change" }, { "\x11 \x10", "Tab" }, { "clear", "Back" } };
	char buf[48], *p;

	spiLock();
	background();
	switch (page) {
	case PAGE_SAVE:
	case PAGE_LOAD:
		p = put_count(buf, used_count, "saved state", "saved states");
		if (page == PAGE_SAVE) {
			p = put_str(p, " \x07 ");
			p = put_uint(p, free_kb, 1);
			put_str(p, " KB free");
		}
		draw_header(page == PAGE_SAVE ? "Save state" : in_game ? "Load state" : "Play from a state",
		            in_game || page == PAGE_SAVE ? buf : game_title, true);
		draw_list(page, sel, top);
		if (page == PAGE_SAVE)
			footer(cur.used ? save_hints : save_empty, cur.used ? 3 : 2);
		else if (!used_count)
			footer(load_hints + 2, 1);
		else
			footer(in_game ? load_hints : play_hints, 3);
		break;
	case PAGE_SETTINGS:
		draw_header("Settings", "For all games", true);
		draw_settings(sel);
		footer(set_hints, 2);
		break;
	case PAGE_CONTROLS:
		draw_header("Controls", "For all games", true);
		draw_controls(tab, sel, asking);
		footer(control_hints, 3);
		break;
	}
	finish();
}

// [clear] or [del] cancels; the arrows are the D-pad's
static bool capture_key(uint8_t *key) {
	for (;;) {
		next_frame();
		for (unsigned int g = 1; g < 8; g++) {
			uint8_t hit = keys_now[g] & ~keys_last[g];
			if (!hit)
				continue;
			if ((g == 6 && hit & 1 << 6) || (g == 1 && hit & 1 << 7) || g == 7)
				return false;
			key[0] = g;
			key[1] = hit & -hit;
			return true;
		}
	}
}

// runs a page until [clear]: MENU_RESUME, or MENU_SAVE / MENU_LOAD (a slot was picked)
__attribute__((noinline)) static uint8_t sub_page(int page) {
	unsigned int sel = 0, top = 0, tab = 0, typed = 0, typed_frames = 0;
	char when[48];

	if (page == PAGE_SAVE || page == PAGE_LOAD) {
		unsigned int rows = list_count(page);
		sel = list_row(page, page == PAGE_SAVE || slot_used(save_slot) || newest_slot < 0 ? save_slot : newest_slot);
		// the cursor in the middle when it can be
		top = sel > LIST_ROWS / 2 ? sel - LIST_ROWS / 2 : 0;
		if (rows > LIST_ROWS && top > rows - LIST_ROWS)
			top = rows - LIST_ROWS;
		list_cursor(page, sel, &sel, &top);
	}
	for (bool draw = true;;) {
		unsigned int rows = page == PAGE_SETTINGS ? SET_COUNT : page == PAGE_CONTROLS ? (tab ? HOT_COUNT : 4) :
		                    list_count(page);
		int step, d, dir = 0;
		if (draw)
			show_sub(page, sel, top, tab, false);
		draw = false;
		next_frame();
		if (typed_frames && !--typed_frames)
			typed = 0;
		if (back_pressed())
			return MENU_RESUME;
		draw = true;

		if ((step = arrow(0, K_UP)))
			dir = -step;
		else if ((step = arrow(1, K_DOWN)))
			dir = step;
		if (dir && rows) {
			if (page <= PAGE_LOAD) {
				int r = (int)sel + dir;
				list_cursor(page, r < 0 ? 0 : r >= (int)rows ? rows - 1 : (unsigned int)r, &sel, &top);
			} else {
				sel = (sel + rows + dir) % rows;
			}
			continue;
		}
		dir = arrow(2, K_LEFT) ? -1 : arrow(3, K_RIGHT) ? 1 : 0;

		if (page <= PAGE_LOAD) {
			if (!rows) {
				draw = false;
				continue;
			}
			if (dir) {
				// ten rows at a time
				int r = (int)sel + dir * 10;
				list_cursor(page, r < 0 ? 0 : r >= (int)rows ? rows - 1 : (unsigned int)r, &sel, &top);
				continue;
			}
			// number keys type a slot
			if ((d = digit_pressed()) >= 0) {
				typed = typed >= 100 ? (unsigned int)d : typed * 10 + d;
				typed_frames = 90;
				list_cursor(page, list_row(page, typed), &sel, &top);
				continue;
			}
			if (pressed(K_DEL) && cur.used) {
				char title[24];
				put_str(put_slot(put_str(title, "Delete "), cur_slot), "?");
				title[7] = cur_slot == AUTO_SLOT ? 'a' : 's';	// "Delete slot 012?"
				if (confirm(title, "This cannot be undone.", NULL, "Delete")) {
					delete_state(cur_slot);
					scan_slots();
					rows = list_count(page);
					if (page == PAGE_LOAD && sel >= rows)
						sel = rows ? rows - 1 : 0;
					if (rows <= LIST_ROWS)
						top = 0;
					else if (top > rows - LIST_ROWS)
						top = rows - LIST_ROWS;
					if (rows)
						list_cursor(page, sel, &sel, &top);
				}
				continue;
			}
			if (!ok_pressed()) {
				draw = false;
				continue;
			}
			if (page == PAGE_SAVE && cur.used) {
				char title[24];
				put_saved(put_str(when, "It has a state from "), &cur);
				put_str(put_uint(put_str(title, "Replace slot "), cur_slot, 3), "?");
				if (!confirm(title, when, NULL, "Replace"))
					continue;
			}
			// the auto save loads without becoming the slot [sto] and [ln] use
			if (cur_slot == AUTO_SLOT)
				start_slot = AUTO_SLOT;
			else
				save_slot = cur_slot;
			return page == PAGE_SAVE ? MENU_SAVE : MENU_LOAD;
		}

		if (page == PAGE_CONTROLS) {
			if (dir) {
				tab ^= 1;
				sel = 0;
				continue;
			}
			if (!ok_pressed()) {
				draw = false;
				continue;
			}
			show_sub(page, sel, top, tab, true);
			// let go of [2nd] first
			do
				next_frame();
			while (any_key_down());
			capture_key((tab ? hot_keys : nes_keys) + 2 * sel);
			continue;
		}

		// settings
		if (!dir && !ok_pressed()) {
			draw = false;
			continue;
		}
		switch (sel) {
		case SET_PICTURE:
			if (!in_game) {
				wide_wanted = !wide_wanted;
			} else if (set_wide(!wide_buffer)) {
				screen_changed = !screen_changed;
				wide_wanted = wide_buffer != NULL;
			} else {
				static const char *const ok[1] = { "OK" };
				dialog("Not enough memory", "Wide 4:3 needs 80 KB of free RAM.", "Delete or archive some files.", ok, 1,
				       false);
			}
			break;
		case SET_FPS:
			show_fps = !show_fps;
			break;
		case SET_BRIGHTNESS:
			set_brightness(brightness + (dir ? dir : 1) > 10 && !dir ? 1 : brightness + (dir ? dir : 1));
			break;
		case SET_SAVE_ON_QUIT:
			save_on_quit = !save_on_quit;
			break;
		case SET_PICTURES:
			state_pictures = !state_pictures;
			break;
		case SET_CONTROLS:
			if (!dir || dir > 0)
				sub_page(PAGE_CONTROLS);
			break;
		}
	}
}

/* ---------------------------------------------------------------- the library */

static const char *mapper_name(uint8_t mapper) {
	switch (mapper) {
	case 0: return "NROM";
	case 1: return "MMC1";
	case 2: return "UxROM";
	case 3: return "CNROM";
	case 4: return "MMC3";
	case 7: return "AxROM";
	}
	return "Mapper";
}

// "MMC3 · 384 KB"
static void put_rom_kind(char *p, const struct romheader *h) {
	p = put_str(p, mapper_name(h->mapper));
	p = put_str(p, " \x07 ");
	p = put_uint(p, h->prgsize * 16 + h->chrsize * 8, 1);
	put_str(p, " KB");
}

// the selected game's newest state: its picture and when
static void rom_pane(const struct rom *rom) {
	char buf[48];
	if (!rom->states) {
		pane_empty("Not played yet");
		put_rom_kind(buf, rom->header);
		caption(0, buf, P_TEXT3);
		return;
	}
	pane_state(rom->newest ? state_picture(rom->name, rom->newest_slot) : NULL, "No picture");
	if (rom->newest) {
		struct state_header newest;
		state_stamp_header(rom->newest, &newest);
		caption(0, "Last played", P_TEXT3);
		put_when(buf, &newest);
		caption(1, buf, P_TEXT1);
	} else {
		caption(0, "Saved by an older", P_TEXT3);
		caption(1, "FamiCalc", P_TEXT1);
	}
	put_count(buf, rom->states, "saved state", "saved states");
	caption(2, buf, P_TEXT2);
}

__attribute__((noinline)) static void draw_library(struct rom *roms, unsigned int count, unsigned int sel, unsigned int top,
                         const char *error) {
	static const struct hint hints[] = { { "2nd", "Play" }, { "alpha", "Options" }, { "mode", "Settings" } };
	static const struct hint quit[] = { { "del", "Quit" } };
	static const struct hint continue_hint = { "2nd", "Continue" };
	struct hint h[3] = { hints[0], hints[1], hints[2] };
	char buf[48];

	spiLock();
	background();
	if (!count) {
		draw_header("Library", "No games yet", false);
		nes_pad(100, 58, 0);
		text_center(DISPLAY, 160, 128, "Add your first game", P_TEXT1);
		text_center(BODY, 160, 148, "Convert a .nes file with the FamiCalc", P_TEXT2);
		text_center(BODY, 160, 161, "converter, then send the files to", P_TEXT2);
		text_center(BODY, 160, 174, "your calculator.", P_TEXT2);
		footer(quit, 1);
		finish();
		return;
	}
	put_count(buf, count, "game", "games");
	draw_header("Library", buf, false);
	for (unsigned int i = 0; i < LIST_ROWS && top + i < count; i++) {
		unsigned int g = top + i;
		char key[2] = { (char)('0' + (g + 1) % 10), 0 };
		row(TOP + i * ROW, g == sel, -1, NULL, rom_title(&roms[g]), g < 10 ? key : NULL, false);
	}
	scroll_mark(top, count);
	if (error) {
		pane_empty("Can't start it");
		caption(0, error, P_ACCENT);
	} else {
		rom_pane(&roms[sel]);
	}
	if (roms[sel].newest)
		h[0] = continue_hint;
	footer(h, 3);
	finish();
}

/*
	A game's options: Continue (its newest state), Play from a state,
	Start over (no state), Delete all states. Returns true to play it
	(launch_slot: the state to start from, -1: none).
*/
enum { OPT_CONTINUE, OPT_FROM_STATE, OPT_START_OVER, OPT_DELETE, OPT_COUNT };

__attribute__((noinline)) static bool game_options(struct rom *rom) {
	static const char *const labels[OPT_COUNT] = { "Continue", "Play from a state", "Start over", "Delete all states" };
	static const uint8_t icons[OPT_COUNT] = { ICON_PLAY, ICON_LOAD, ICON_RESET, ICON_TRASH };
	static const struct hint hints[] = { { "2nd", "Select" }, { "clear", "Back" } };
	unsigned int sel = rom->newest ? OPT_CONTINUE : OPT_START_OVER;
	char buf[48];

	strcpy(romname, rom->name);
	game_title = rom_title(rom);
	for (bool draw = true;;) {
		int step;
		if (draw) {
			draw = false;
			spiLock();
			background();
			put_rom_kind(buf, rom->header);
			draw_header(game_title, buf, true);
			for (unsigned int i = 0; i < OPT_COUNT; i++)
				row(TOP + i * ROW + (i == OPT_DELETE ? 6 : 0), i == sel, icons[i], NULL, labels[i], NULL,
				    i != OPT_START_OVER && !rom->states);
			rom_pane(rom);
			footer(hints, 2);
			finish();
		}
		next_frame();
		if (back_pressed())
			return false;
		if ((step = arrow(0, K_UP)) || (step = -arrow(1, K_DOWN))) {
			sel = (sel + OPT_COUNT - (step > 0 ? 1 : -1)) % OPT_COUNT;
			draw = true;
			continue;
		}
		if (!ok_pressed())
			continue;
		draw = true;
		if (sel != OPT_START_OVER && !rom->states)
			continue;
		switch (sel) {
		case OPT_CONTINUE:
			launch_slot = rom->newest ? rom->newest_slot : -1;
			return true;
		case OPT_FROM_STATE:
			scan_slots();
			if (sub_page(PAGE_LOAD) == MENU_LOAD) {
				launch_slot = start_slot >= 0 ? start_slot : (int)save_slot;
				start_slot = -1;
				return true;
			}
			// deleting there changes what the library shows
			rom->states = used_count;
			if (!used_count)
				rom->newest = 0;
			break;
		case OPT_START_OVER:
			launch_slot = -1;
			return true;
		case OPT_DELETE:
			put_count(buf, rom->states, "state", "states");
			if (confirm("Delete all states?", buf, "This cannot be undone.", "Delete")) {
				delete_states(rom->name);
				rom->states = 0;
				rom->newest = 0;
				sel = OPT_START_OVER;
			}
			break;
		}
	}
}

int rom_menu(struct rom *roms, unsigned int count, unsigned int selected, const char *error) {
	unsigned int top = 0;
	bool redraw = true;
	int d, step;

	in_game = false;
	if (selected >= count)
		selected = 0;
	KEY_MODE = 3;		// continuous scanning (the OS's calls may change it)
	ui_stride = FB_W;
	menu_palette();
	note_today();
	reset_keys();
	for (;;) {
		if (selected < top)
			top = selected;
		if (selected >= top + LIST_ROWS)
			top = selected - LIST_ROWS + 1;
		if (redraw)
			draw_library(roms, count, selected, top, error);
		redraw = false;
		next_frame();

		if (pressed(K_DEL) || pressed(K_CLEAR))
			return -1;
		if (pressed(K_MODE)) {
			sub_page(PAGE_SETTINGS);
			redraw = true;
			continue;
		}
		if (!count)
			continue;
		// number keys start (continue) the first ten games
		if ((d = digit_pressed()) >= 0 && (unsigned int)(d + 9) % 10 < count) {
			selected = (d + 9) % 10;
			launch_slot = roms[selected].newest ? roms[selected].newest_slot : -1;
			break;
		}
		if (ok_pressed()) {
			launch_slot = roms[selected].newest ? roms[selected].newest_slot : -1;
			break;
		}
		if (pressed(K_ALPHA)) {
			if (game_options(&roms[selected]))
				break;
			redraw = true;
			continue;
		}
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
	// black, so what the game's window leaves out doesn't keep the list
	fill(0, 0, FB_W, FB_H, P_BLACK);
	lcd_wait_frame();
	lcd_wait_frame();
	return selected;
}

/* ---------------------------------------------------------------- the pause menu */

enum {
	ITEM_RESUME, ITEM_SAVE, ITEM_LOAD, ITEM_SETTINGS, ITEM_CONTROLS, ITEM_RESET, ITEM_GAMES, ITEM_QUIT,
	ITEM_COUNT
};
static const char *const item_labels[ITEM_COUNT] = {
	"Resume", "Save state", "Load state", "Settings", "Controls", "Reset game", "Change game", "Quit FamiCalc"
};
static const uint8_t item_icons[ITEM_COUNT] = {
	ICON_PLAY, ICON_SAVE, ICON_LOAD, ICON_SETTINGS, ICON_CONTROLS, ICON_RESET, ICON_LIBRARY, ICON_POWER
};
static const uint8_t item_page[ITEM_COUNT] = { 0, PAGE_SAVE, PAGE_LOAD, PAGE_SETTINGS, PAGE_CONTROLS, 0, 0, 0 };
// rows in groups, 6 lines apart
static const uint8_t item_y[ITEM_COUNT] = {
	TOP, TOP + ROW, TOP + 2 * ROW, TOP + 3 * ROW + 6, TOP + 4 * ROW + 6, TOP + 5 * ROW + 12,
	TOP + 6 * ROW + 12, TOP + 7 * ROW + 12
};

static uint8_t *const emu_backup = render_cache;

__attribute__((noinline)) static void draw_main(unsigned int sel) {
	static const struct hint hints[] = { { "2nd", "Select" }, { "clear", "Resume" } };
	const uint8_t *newest = sel == ITEM_LOAD && newest_slot >= 0 ? state_picture(romname, newest_slot) : NULL;
	char buf[48];

	spiLock();
	background();
	draw_header("Paused", game_title, false);
	for (unsigned int i = 0; i < ITEM_COUNT; i++)
		row(item_y[i], i == sel, item_icons[i], NULL, item_labels[i], NULL, i == ITEM_LOAD && !used_count);
	if (newest)
		pane_state(newest, NULL);
	else
		pane_picture(NOW);
	switch (sel) {
	case ITEM_RESUME:
		caption(0, "Back to the game", P_TEXT2);
		break;
	case ITEM_SAVE:
		caption(0, "Saves to", P_TEXT3);
		put_slot(buf, save_slot);
		caption(1, buf, P_TEXT1);
		put_count(buf, used_count, "saved state", "saved states");
		caption(2, buf, P_TEXT2);
		break;
	case ITEM_LOAD:
		if (newest_slot < 0) {
			caption(0, used_count ? "Saved by an older FamiCalc" : "No saved states yet", P_TEXT2);
			break;
		}
		caption(0, "Newest state", P_TEXT3);
		slot_info(newest_slot, &cur);
		put_saved(buf, &cur);
		caption(1, buf, P_TEXT1);
		put_slot(buf, newest_slot);
		caption(2, buf, P_TEXT2);
		break;
	case ITEM_SETTINGS:
		caption(0, "Picture", P_TEXT3);
		caption(1, wide_buffer ? "Wide 4:3" : "Normal", P_TEXT1);
		caption(2, show_fps ? "FPS counter on" : "FPS counter off", P_TEXT2);
		break;
	case ITEM_CONTROLS:
		caption(0, "The keys for the NES's buttons and the hotkeys", P_TEXT2);
		break;
	default:
		caption(caption(0, sel == ITEM_RESET ? "Starts over" : sel == ITEM_GAMES ? "Back to the library" :
		                "Back to the calculator", P_TEXT1),
		        save_on_quit && sel != ITEM_RESET ? "Saves the game to the Auto save first" :
		        "Progress since your last save is lost", P_TEXT2);
		break;
	}
	footer(hints, 2);
	finish();
}

// The menu's frame buffer overlaps the emulator's memory: keep a copy. The
// game's picture is taken first.
__attribute__((noinline)) static void open_screen(void) {
	memcpy(emu_backup, EMU_PAGE, EMU_COVERED);
	pic_capture(NOW);
	spiLock();
	ui_stride = FB_W;
	menu_palette();
	note_today();
}

__attribute__((noinline)) static void close_screen(bool saving) {
	// black while the emulator's memory goes back, then the game's window
	spiLock();
	fill(0, 0, FB_W, FB_H, P_BLACK);
	ui_screen_full(FB);
	lcd_wait_frame();
	spiLock();
	memcpy(EMU_PAGE, emu_backup, EMU_COVERED);
	memset(VBUFFER, 0, VBUFFER_SIZE);
	if (wide_buffer)
		memset(wide_buffer, 0, WIDE_SIZE);
	ui_screen_game();
	if (saving) {
		// the state gets the picture from when the menu opened (render_cache
		// is about to hold the state); the panel keeps the black screen
		spiLock();
		memcpy(PICTURE_SPOT, NOW, sizeof(struct picture));
		state_picture_next = PICTURE_SPOT;
	}
}

uint8_t pause_menu(void) {
	unsigned int sel = ITEM_RESUME;
	uint8_t result = MENU_RESUME;

	in_game = true;
	screen_changed = false;
	reset_keys();
	scan_slots();
	open_screen();

	for (bool draw = true;;) {
		int step;
		if (draw)
			draw_main(sel);
		draw = false;
		next_frame();
		if (back_pressed())
			break;
		if ((step = arrow(0, K_UP)) || (step = -arrow(1, K_DOWN))) {
			sel = (sel + ITEM_COUNT - (step > 0 ? 1 : -1)) % ITEM_COUNT;
			draw = true;
			continue;
		}
		if (!ok_pressed() && !(arrow(3, K_RIGHT) && item_page[sel]))
			continue;
		draw = true;
		if (sel == ITEM_RESUME)
			break;
		if (item_page[sel] || sel == ITEM_SAVE) {
			if (sel == ITEM_LOAD && !used_count)
				continue;
			if ((result = sub_page(item_page[sel])) != MENU_RESUME)
				break;
			continue;
		}
		// reset, change game, quit: asks first
		if (!confirm(sel == ITEM_RESET ? "Reset the game?" : sel == ITEM_GAMES ? "Leave this game?" : "Quit FamiCalc?",
		             save_on_quit && sel != ITEM_RESET ? "The Auto save keeps your progress." :
		             "Progress since your last save", save_on_quit && sel != ITEM_RESET ? NULL : "will be lost.",
		             sel == ITEM_RESET ? "Reset" : sel == ITEM_GAMES ? "Leave" : "Quit"))
			continue;
		exit_reason = sel == ITEM_RESET ? EXIT_RESET : sel == ITEM_GAMES ? EXIT_ROM_LIST : EXIT_QUIT;
		quit_saving = save_on_quit && sel != ITEM_RESET;
		result = MENU_LEAVE;
		break;
	}
	// a new picture size moves the JIT's cache: restart the emulator from
	// its state (save and load do that anyway)
	if (screen_changed && result == MENU_RESUME) {
		state_reload_only = 1;
		result = MENU_RELOAD;
	}
	// let go of the keys first, so the game doesn't get them (or reopen the menu)
	do
		next_frame();
	while (any_key_down());
	close_screen(result == MENU_SAVE || quit_saving);
	return result;
}

/* ---------------------------------------------------------------- hotkeys */

// a message at the bottom of the game's picture for a second (io.asm)
#define MESSAGE_LEN (*(volatile uint8_t *)(0xD60F80 - 76))
#define MESSAGE_PTR (*(const char *volatile *)(0xD60F80 - 75))

static void message(const char *s) {
	MESSAGE_PTR = s;
	MESSAGE_LEN = 60;
}

extern void fps_panel_erase(void);

// the hotkeys keys.asm leaves to C: returns what pause.asm does then
uint8_t hotkey(void) {
	static char text[24];
	switch (hotkey_pressed) {
	case HOT_NEWEST:
		scan_slots();
		if (newest_slot < 0) {
			message("No saved states yet");
			break;
		}
		start_slot = newest_slot;
		return MENU_LOAD;
	case HOT_NEXT:
	case HOT_PREVIOUS:
		save_slot = (save_slot + (hotkey_pressed == HOT_NEXT ? 1 : NUM_SLOTS - 1)) % NUM_SLOTS;
		put_slot(text, save_slot);
		message(text);
		break;
	case HOT_FPS:
		show_fps = !show_fps;
		if (!show_fps)
			fps_panel_erase();
		ui_screen_game();	// the counter's place on the panel
		message(show_fps ? "FPS counter on" : "FPS counter off");
		break;
	case HOT_BRIGHTER:
	case HOT_DARKER:
		set_brightness(brightness + (hotkey_pressed == HOT_BRIGHTER ? 1 : -1));
		put_uint(put_str(text, "Brightness "), brightness, 1);
		message(text);
		break;
	}
	return MENU_RESUME;
}
