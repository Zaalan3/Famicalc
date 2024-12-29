/*
 *--------------------------------------
 * Program Name:
 * Author:
 * License:
 * Description:
 *--------------------------------------
*/

#include <stdlib.h> 
#include <stdint.h>
#include <stdbool.h>
#include <string.h> 
 
#include <tice.h> 
#include <graphx.h> 
#include <fileioc.h>
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

uintptr_t jit_cache_extend;
uintptr_t jit_cache_extend_end; 

bool loadROM(void);
void* getFileDataPtr(char prefix,uint8_t id,char* romname);


int main(void)
{	
	// find free memory for JIT cache 
	jit_cache_extend_end = os_MemChk(&jit_cache_extend);
	jit_cache_extend_end = jit_cache_extend_end + jit_cache_extend - 256;
	
	if (loadROM()) {  
		startJIT();
	}
	
    return 0;
}

bool loadROM(void) { 
	// for now, just find first appvar with header magic 
	char romname[16];
	char* varname; 
	void* vat_ptr = NULL;
	
	varname = ti_Detect(&vat_ptr,"FAMICALC"); 
	
	if (!varname)
		return false;
	
	strcpy(romname,varname);
	
	ti_var_t f = ti_Open(varname,"r");
	header = ti_GetDataPtr(f); 
	ti_Close(f); 
	
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

