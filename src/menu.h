#ifndef MENU_H
#define MENU_H

#include <stdbool.h>
#include <stdint.h>

#include "states.h"

struct picture;

struct romheader {
	char magic[8];			// "FAMICALC"
	uint8_t mapper;			// ines mapper
	uint8_t mirroring;		// 0 = Vertical, 1 = Horizontal, 2+ = other
	uint8_t prgsize;		// in 16kb units
	uint8_t chrsize;		// in 8kb units (0 = CHR RAM)
	char description[];		// Text to display on select screen (null terminated).
};

#define MAX_ROMS 32

struct rom {
	char name[9];			// appvar name of the header
	struct romheader *header;	// in RAM or archive
	unsigned int states;		// save states found for it
	uint32_t newest;		// state_header_stamp() of the newest dated one, 0: none
	uint16_t newest_slot;
};

// why startJIT() returned (set by the pause menu, 0 = [del] / quit)
enum { EXIT_QUIT, EXIT_RESET, EXIT_ROM_LIST };

// pause_menu() result, handled in pause.asm
enum { MENU_RESUME, MENU_SAVE, MENU_LOAD, MENU_RELOAD, MENU_LEAVE };

// the hotkeys (keys.asm hot_keys); save and load are done there, the
// others by hotkey() (menu.c)
enum { HOT_SAVE, HOT_LOAD, HOT_NEWEST, HOT_NEXT, HOT_PREVIOUS, HOT_FPS, HOT_BRIGHTER, HOT_DARKER, HOT_COUNT };

// shared with the emulator (asm)
extern uint8_t nes_keys[8 * 2];		// the NES's buttons: keypad group, mask
extern uint8_t hot_keys[HOT_COUNT * 2];
extern uint8_t hot_state, start_load, hotkey_pressed;
extern int start_slot;
extern uint8_t save_on_quit, quit_saving, state_pictures;
extern unsigned int brightness;
extern bool wide_wanted;
extern int launch_slot;
void set_brightness(unsigned int level);
extern unsigned int save_slot;
extern uint8_t exit_reason;
extern bool show_fps;
extern uint8_t *wide_buffer;
extern uint8_t state_reload_only;

// the wide picture: 320x224, 8 bpp. It takes free RAM from the JIT's extra
// cache: at least WIDE_MIN_FREE of it is left (a state being saved may also
// take the wide picture's room: the picture is drawn again after a save)
#define WIDE_SIZE (320 * 224)
#define WIDE_MIN_FREE (8 * 1024)
bool set_wide(bool on);
extern char romname[16];
extern struct romheader header;		// fixed part only (no description)
extern const char *game_title;		// description of the running game

// main.c: save state files
struct slot_info {
	bool used;
	bool dated;			// header with a set clock
	struct state_header header;
	unsigned int size;
};
void state_name(unsigned int slot, char *name);
bool slot_info(unsigned int slot, struct slot_info *info);
bool delete_state(unsigned int slot);
// the picture of a ROM's state (pictures.h), NULL: none
const uint8_t *state_picture(const char *rom, unsigned int slot);
// the picture the next saved state gets (the pause menu's), NULL: the frame on screen
extern const struct picture *state_picture_next;
// marks used slots of the running ROM in a NUM_SLOTS bit array, returns the count
unsigned int scan_states(uint8_t *used);
unsigned int archive_free_kb(void);

// menu.c: fonts (fonts.c) for ui_text (ui.asm); fonts_init sets the offsets
struct ui_font {
	uint8_t rows;
	const uint8_t *widths;		// 128, 0: no glyph
	const uint8_t *bits;		// rows x width bits per glyph, each on a byte
	const uint16_t *offsets;	// of each glyph in bits
};
extern const struct ui_font font_body, font_display, font_hud;
void fonts_init(void);
void menu_palette(void);
// the library: the game to play (launch_slot: the state to start from), -1: quit
int rom_menu(struct rom *roms, unsigned int count, unsigned int selected, const char *error);
uint8_t pause_menu(void);
uint8_t hotkey(void);		// hotkey_pressed in a game
const char *rom_title(const struct rom *rom);
void delete_states(const char *rom);

extern const char version_string[];

#endif
