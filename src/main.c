
#include <stdlib.h> 
#include <stdint.h>
#include <stdbool.h>
#include <string.h> 
 
#include <tice.h> 
#include <fileioc.h>
#include <keypadc.h>
#include <sys/timers.h>

/* 
	ROM Format: 
	header: appvar with max 6 character name
	x = hex digit from 0 to F 
	NAMEPx - PRG ROM 
	NAMECx - CHR ROM 
	NAMESV - compressed PRG RAM 
	NAMESx - savestate (not yet implemented)
*/ 

struct romheader {
	char magic[8];			// "FAMICALC"
	uint8_t mapper;			// ines mapper
	uint8_t mirroring;		// 0 = Vertical, 1 = Horizontal, 2+ = other
	uint8_t prgsize;		// in 16kb units 
	uint8_t chrsize;		// in 8kb units (0 = CHR RAM)
	char description[];		// Text to display on select screen (null terminated). 
};


extern struct romheader* header;
extern void* prg_banks[64];
extern void* chr_banks[512];

extern void startJIT(void); 

extern void ui_init(void); 
extern void ui_cleanup(void); 
extern void ui_printString(uint8_t x,uint8_t y,const char* string); 

extern void garbage_collect_preserve(void);
extern void garbage_collect_restore(void);

uintptr_t jit_cache_extend;
uintptr_t jit_cache_extend_end; 

void* getFileDataPtr(char prefix,uint8_t id,char* romname);
bool loadROM(uint8_t index);
struct romheader* roms[16]; 

const char version_string[] = "  *FamiCalc version 0.1*  ";

// buffer for name of current rom
char romname[16];
size_t free_ram_size;

int main(void)
{	
	kb_SetMode(MODE_3_CONTINUOUS);
	ti_SetGCBehavior(&garbage_collect_preserve,&garbage_collect_restore);
	ui_init(); 
	
	// find free memory for JIT cache 
	free_ram_size = os_MemChk((void**)&jit_cache_extend);
	jit_cache_extend_end = free_ram_size + jit_cache_extend - 256;
	
	// UI 
	ui_printString(8,0,version_string);
	ui_printString(8,16,"Press ENTER to select a ROM");
	ui_printString(8,24,"Press DEL to exit.");
	
	// Gather up to 16 ROMs on calc
	int numRoms = 0; 
	
	void* vat_ptr = NULL;
	char* varname;
	uint8_t cury = 40;
	while ((varname = ti_Detect(&vat_ptr,"FAMICALC"))) { 
		ui_printString(16,cury,varname);  
		ti_var_t f = ti_Open(varname,"r");
		roms[numRoms] = ti_GetDataPtr(f); 
		ti_Close(f); 
		numRoms++;
		if (numRoms == 16)
			break; 
		
		cury += 10;
	}
	
	// let user choose a rom 
	uint8_t selection = 0; 
	
	kb_lkey_t last = 0;
	cury = 40; 
	ui_printString(8,cury,">");
	ui_printString(8,208,roms[selection]->description);
	
SELECT:
	do { 
		bool newSelection = false; 
		
		if (kb_IsDown(kb_KeyDel)) { 
			ui_cleanup(); 
			return 0; 
		} else if (kb_IsDown(kb_KeyEnter))
			break; 
		
		if (!kb_IsDown(last)) { 
			last = 0;
			if (kb_IsDown(kb_KeyUp)) { 
				selection = selection == 0 ? 0 : selection-1;
				newSelection = true; 
				last = kb_KeyUp;
			} else if (kb_IsDown(kb_KeyDown)) { 
				selection = selection == numRoms - 1 ? numRoms - 1 : selection+1;
				newSelection = true;
				last = kb_KeyDown; 
			}			
			
			if (newSelection) { 
				ui_printString(8,cury," ");
				cury = 40 + selection*10;
				
				ui_printString(8,cury,">");
				ui_printString(8,208,"                              ");
				ui_printString(8,216,"                              ");
				ui_printString(8,208,roms[selection]->description);
			} 
		} 
		
	} while(1); 
	
	header = roms[selection]; 
	
	if (loadROM(selection)) {  
		startJIT();
	} else { 
		ui_printString(8,208,"                              ");
		ui_printString(8,216,"                              ");
		ui_printString(8,208,"Error opening ROM!");
		goto SELECT; 
	}
	
	ui_cleanup(); 
    return 0;
}



bool loadROM(uint8_t index) { 
	char* varname;
	void* vat_ptr = NULL; 
	
	for(uint8_t i = 0;i < index+1;i++) 
		varname = ti_Detect(&vat_ptr,"FAMICALC");
	
	// store name of current ROM
	strcpy(romname,varname); 
	
	
	uint8_t prgsize = header->prgsize;
	uint8_t chrsize = header->chrsize;
	
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
		int offset = 1024 * i;
		
		void* dataptr = getFileDataPtr('C',id,romname); 
		
		if (dataptr) 
			chr_banks[i] = dataptr + offset; 
		else 
			return false; 
	}
	
	memcpy(&chr_banks[8*chrsize],&chr_banks[0],(512-(8*chrsize))*sizeof(void*)); 
	
	return true; 
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


void* saveToSlot(uint8_t slot,uint24_t size) { 
	const void* savedata = (void*)0xD40000;
	ti_var_t f; 
	char filename[16];
	char ext[3]; 
	void* dataPtr = NULL; 
	
	// if there isn't enough room, don't bother.
	if (free_ram_size < size)
		return NULL;
	
	os_ArcChk(); 
	if (os_TempFreeArc < size)
		return NULL; 
	
	
	
	ext[0] = 'S'; 
	ext[1] = '0' + slot; 
	ext[2] = 0;
	// construct file name
	strcpy(filename,romname);
	strcat(filename,ext);	
	
	if((f = ti_Open(filename,"w+"))) {
		ti_Write(savedata,size,1,f);
		ti_SetArchiveStatus(true,f);
		ti_Close(f); 
		f = ti_Open(filename,"r");
		dataPtr = ti_GetDataPtr(f); 
		ti_Close(f); 
		// fetch new extended cache size
		free_ram_size = os_MemChk((void**)&jit_cache_extend);
		jit_cache_extend_end = free_ram_size + jit_cache_extend - 256;
	} 
	
	return dataPtr;
} 

void* getSaveSlot(uint8_t slot) { 
	return getFileDataPtr('S',slot,romname);
} 
