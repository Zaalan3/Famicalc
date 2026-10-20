#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include <tice.h>
#include <fileioc.h>
#include <keypadc.h>
#include <sys/rtc.h>
#include <sys/timers.h>
#include <ti/vars.h>

#include "menu.h"
#include "states.h"

/*
	ROM Format:
	header: appvar with max 6 character name
	x = hex digit from 0 to F
	NAMEPx - PRG ROM
	NAMECx - CHR ROM
	NAMESV - compressed PRG RAM
	NAMExx - save states (see states.h)
*/

extern void* prg_banks[64];
extern void* chr_banks[512];

extern void startJIT(void);

extern void ui_init(void);
extern void ui_cleanup(void);
extern void ui_restart(void);
extern void ui_screen_game(void);

extern void garbage_collect_preserve(void);
extern void garbage_collect_restore(void);

uintptr_t jit_cache_extend;
uintptr_t jit_cache_extend_end;

void* getFileDataPtr(char prefix,uint8_t id,char* romname);
bool loadROM(const char *name);
void reloadROM(void);

const char version_string[] = "0.3";

size_t free_ram_size;

// shared with the emulator
unsigned int save_slot;
uint8_t state_is_new;		// the state being loaded has a header (current layout)
uint8_t exit_reason;
bool show_fps;
uint8_t *wide_buffer;		// the wide picture's frame buffer, NULL for the normal one
uint8_t state_reload_only;	// the next "save" only restarts the emulator from its state

static struct rom roms[MAX_ROMS];
const char *game_title;

/* Settings: a small appvar in RAM */
#define CONFIG_NAME "FamiCfg"
struct config {
	char magic[2];		// "FC"
	uint8_t version;
	uint8_t show_fps;
	char last_rom[9];
	uint8_t wide;		// added in version 2
};
static struct config config;

static void load_config(void) {
	ti_var_t f = ti_Open(CONFIG_NAME, "r");
	memset(&config, 0, sizeof config);
	if (f) {
		// older versions wrote fewer fields
		if (ti_Read(&config, 1, sizeof config, f) < 3 || memcmp(config.magic, "FC", 2))
			memset(&config, 0, sizeof config);
		ti_Close(f);
	}
	show_fps = config.show_fps;
}

static void save_config(void) {
	struct config old = config;
	ti_var_t f;
	config.magic[0] = 'F';
	config.magic[1] = 'C';
	config.version = 2;
	config.show_fps = show_fps;
	config.wide = wide_buffer != NULL;
	if (!memcmp(&old, &config, sizeof config))
		return;
	if ((f = ti_Open(CONFIG_NAME, "w"))) {
		ti_Write(&config, sizeof config, 1, f);
		ti_Close(f);
	}
}

static void update_free_ram(void) {
	// find free memory for JIT cache
	free_ram_size = os_MemChk((void**)&jit_cache_extend);
	jit_cache_extend_end = free_ram_size + jit_cache_extend - 256;
	// the wide picture's buffer sits at the top of free RAM
	if (wide_buffer) {
		jit_cache_extend_end = (uintptr_t)wide_buffer - 256;
		free_ram_size = (uintptr_t)wide_buffer - jit_cache_extend;
	}
	// If there's too little space left, don't use jit_cache_extend to avoid overwriting VAT data.
	if (jit_cache_extend_end < jit_cache_extend)
		jit_cache_extend_end = jit_cache_extend;
}

// Reserves (or gives back) the wide picture's frame buffer at the top of free
// RAM; false if there isn't room for it plus WIDE_MIN_FREE (for saving states
// and some of the JIT's cache).
bool set_wide(bool on) {
	void *start;
	size_t size;
	wide_buffer = NULL;
	if (on) {
		size = os_MemChk(&start);
		if (size >= WIDE_SIZE + WIDE_MIN_FREE)
			wide_buffer = (uint8_t *)(((uintptr_t)start + size - WIDE_SIZE - 512) & ~(uintptr_t)7);
	}
	update_free_ram();
	return wide_buffer != NULL || !on;
}

// keeps the state named name in rom->newest if it was saved later
static void note_newest(struct rom *rom, const char *name, unsigned int slot) {
	ti_var_t f = ti_Open(name, "r");
	if (!f)
		return;
	if (ti_GetSize(f) >= STATE_HEADER_SIZE && state_has_header(ti_GetDataPtr(f))) {
		struct state_header h;
		memcpy(&h, ti_GetDataPtr(f), sizeof h);
		if (state_header_stamp(&h) > rom->newest) {
			rom->newest = state_header_stamp(&h);
			rom->newest_slot = slot;
		}
	}
	ti_Close(f);
}

// ROM headers on the calculator, how many states each has and its newest
static unsigned int find_roms(void) {
	unsigned int count = 0;
	void *vat_ptr = NULL;
	char *varname;
	uint24_t type, len;
	char name[9];
	void *data, *entry;

	while (count < MAX_ROMS && (varname = ti_Detect(&vat_ptr, "FAMICALC"))) {
		ti_var_t f = ti_Open(varname, "r");
		if (!f)
			continue;
		memset(&roms[count], 0, sizeof roms[count]);
		strcpy(roms[count].name, varname);
		roms[count].header = ti_GetDataPtr(f);
		ti_Close(f);
		count++;
	}

	// count states: appvars named ROMNAME + slot suffix
	entry = os_GetSymTablePtr();
	while ((entry = os_NextSymEntry(entry, &type, &len, name, &data))) {
		if ((type & 0x1F) != OS_TYPE_APPVAR || len < 3 || len > 8)
			continue;
		name[len] = 0;
		for (unsigned int i = 0; i < count; i++) {
			size_t n = strlen(roms[i].name);
			int slot;
			if (n + 2 == len && !memcmp(name, roms[i].name, n) && (slot = suffix_slot(name + n)) >= 0) {
				roms[i].states++;
				note_newest(&roms[i], name, slot);
			}
		}
	}
	return count;
}

int main(void)
{
	unsigned int selected = 0, count;
	const char *error = NULL;

	kb_SetMode(MODE_3_CONTINUOUS);
	ui_init();
	ui_font_init();
	load_config();
	update_free_ram();

	for (;;) {
		int choice;
		count = find_roms();
		for (unsigned int i = 0; i < count; i++)
			if (!strcmp(roms[i].name, config.last_rom))
				selected = i;

		choice = rom_menu(roms, count, selected, error);
		error = NULL;
		if (choice < 0)
			break;
		selected = choice;
		strcpy(config.last_rom, roms[selected].name);
		if (!set_wide(config.wide))
			config.wide = 0;

		header = *roms[selected].header;
		game_title = roms[selected].header->description[0] ? roms[selected].header->description :
		             roms[selected].name;
		if (!loadROM(roms[selected].name)) {
			error = "Some of this game's files are missing.";
			continue;
		}

		do {
			exit_reason = EXIT_QUIT;
			menu_palette();		// the in-game labels use its colors
			ui_screen_game();
			startJIT();
			config.wide = wide_buffer != NULL;	// as chosen in the pause menu
			if (exit_reason != EXIT_QUIT)
				ui_restart();	// start over from a clean screen setup
		} while (exit_reason == EXIT_RESET);

		if (exit_reason == EXIT_QUIT)
			break;
	}

	save_config();
	ui_cleanup();
	return 0;
}

bool loadROM(const char *name) {
	// store name of current ROM
	strcpy(romname,name);

	uint8_t prgsize = header.prgsize;
	uint8_t chrsize = header.chrsize;

	// locate PRG data
	for(unsigned int i = 0;i < 2*prgsize;i++) {
		uint8_t id = i/6;
		int offset = 8192 * (i % 6);

		void* dataptr = getFileDataPtr('P',id,romname);

		if (dataptr)
			prg_banks[i] = dataptr + offset;
		else
			return false;
	}

	// mirror by copying filled section to empty section
	memcpy(&prg_banks[2*prgsize],&prg_banks[0],(64- (2*prgsize))*sizeof(void*));

	if (chrsize == 0) return true;
	// repeat for CHR data
	for(unsigned int i = 0;i < 8*chrsize;i++) {
		uint8_t id = i/(8*7);
		int offset = 1024 * (i % (8*7));

		void* dataptr = getFileDataPtr('C',id,romname);

		if (dataptr)
			chr_banks[i] = dataptr + offset;
		else
			return false;
	}

	memcpy(&chr_banks[8*chrsize],&chr_banks[0],(512-(8*chrsize))*sizeof(void*));

	return true;
}

// for relocating ROM data after a garbage collect
void reloadROM(void) {
	uint8_t prgsize = header.prgsize;
	uint8_t chrsize = header.chrsize;

	// locate PRG data
	for(unsigned int i = 0;i < 2*prgsize;i++) {
		uint8_t id = i/6;
		int offset = 8192 * (i % 6);

		void* dataptr = getFileDataPtr('P',id,romname);

		prg_banks[i] = dataptr + offset;
	}

	// mirror by copying filled section to empty section
	memcpy(&prg_banks[2*prgsize],&prg_banks[0],(64- (2*prgsize))*sizeof(void*));

	// repeat for CHR data
	for(unsigned int i = 0;i < 8*chrsize;i++) {
		uint8_t id = i/(8*7);
		int offset = 1024 * (i % (8*7));

		void* dataptr = getFileDataPtr('C',id,romname);

		chr_banks[i] = dataptr + offset;
	}

	memcpy(&chr_banks[8*chrsize],&chr_banks[0],(512-(8*chrsize))*sizeof(void*));
}

void* getFileDataPtr(char prefix,uint8_t id,char* romname) {
	char filename[16];
	char ext[3];
	ti_var_t f;
	void* dataPtr = NULL;

	ext[0] = prefix;
	ext[1] = id >= 0x0A ? 'A' + (id - 0x0A) : '0' + id;
	ext[2] = 0;
	// construct file name
	strcpy(filename,romname);
	strcat(filename,ext);

	if((f = ti_Open(filename,"r"))) {
		dataPtr = ti_GetDataPtr(f);
		ti_Close(f);
	}

	return dataPtr;
}

void state_name(unsigned int slot, char *name) {
	strcpy(name, romname);
	slot_suffix(slot, name + strlen(name));
}

// writes the state in save_slot; returns where its compressed data now is, or NULL
void* saveToSlot(const void* savedata,uint24_t size) {
	ti_var_t f;
	char filename[16];
	void* dataPtr = NULL;
	struct state_header h;
	uint8_t sec, min, hour, day, month;
	uint16_t year;
	uint24_t total = size + STATE_HEADER_SIZE;

	// if there isn't enough room, don't bother.
	if (free_ram_size < total)
		return NULL;

	os_ArcChk();
	if (os_TempFreeArc < total)
		return NULL;

	boot_GetTime(&sec, &min, &hour);
	boot_GetDate(&day, &month, &year);
	state_header_init(&h, sec, min, hour, day, month, year);
	state_name(save_slot, filename);

	if((f = ti_Open(filename,"w+"))) {
		bool preserve = false;
		ti_Write(&h,sizeof h,1,f);
		ti_Write(savedata,size,1,f);

		if (!ti_ArchiveHasRoomVar(f)) {
			preserve = true;
			garbage_collect_preserve();
		}

		ti_SetArchiveStatus(true,f);
		ti_Close(f);
		f = ti_Open(filename,"r");
		dataPtr = (uint8_t*)ti_GetDataPtr(f) + STATE_HEADER_SIZE;
		ti_Close(f);
		// fetch new extended cache size
		update_free_ram();

		if (preserve) garbage_collect_restore();
	}

	return dataPtr;
}

// the compressed data of the state in save_slot, or NULL
void* getSaveSlot(void) {
	char filename[16];
	ti_var_t f;
	uint8_t *data = NULL;

	state_name(save_slot, filename);
	if ((f = ti_Open(filename, "r"))) {
		data = ti_GetDataPtr(f);
		ti_Close(f);
		state_is_new = state_has_header(data);
		if (state_is_new)
			data += STATE_HEADER_SIZE;
	}
	return data;
}

bool slot_info(unsigned int slot, struct slot_info *info) {
	char filename[16];
	ti_var_t f;

	memset(info, 0, sizeof *info);
	state_name(slot, filename);
	if (!(f = ti_Open(filename, "r")))
		return false;
	info->used = true;
	info->size = ti_GetSize(f);
	if (info->size >= STATE_HEADER_SIZE && state_has_header(ti_GetDataPtr(f))) {
		memcpy(&info->header, ti_GetDataPtr(f), sizeof info->header);
		info->dated = state_header_dated(&info->header);
	}
	ti_Close(f);
	return true;
}

bool delete_state(unsigned int slot) {
	char filename[16];
	state_name(slot, filename);
	return ti_Delete(filename) != 0;
}

unsigned int scan_states(uint8_t *used) {
	unsigned int count = 0;
	size_t n = strlen(romname);
	uint24_t type, len;
	char name[9];
	void *data, *entry = os_GetSymTablePtr();

	memset(used, 0, NUM_SLOTS / 8 + 1);
	while ((entry = os_NextSymEntry(entry, &type, &len, name, &data))) {
		int slot;
		if ((type & 0x1F) != OS_TYPE_APPVAR || len != n + 2 || memcmp(name, romname, n))
			continue;
		name[len] = 0;
		if ((slot = suffix_slot(name + n)) >= 0) {
			used[slot >> 3] |= 1 << (slot & 7);
			count++;
		}
	}
	return count;
}

unsigned int archive_free_kb(void) {
	os_ArcChk();
	return os_TempFreeArc / 1024;
}
