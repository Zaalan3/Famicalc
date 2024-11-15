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

struct romheader {
	char magic[8]; 		// 'FAMICALC'
	uint8_t prg; 		// # of 16kb PRG ROM pages 
	uint8_t chr; 		// # of 8kb CHR ROM pages (0=CHRRAM)
	uint8_t mapper; 	// iNes mapper
	uint8_t mirror;		// initial mirroring configuration
	char name[54];		// Name on calc. 
};

void* prg_page[64];
void* chr_page[64];
char rom_name[8];

extern void startJIT(void); 
extern void testJIT(void); 

void* getFileDataPtr(char prefix,uint8_t id,char* romname);
void drawNametable(const char* nametable); 


int main(void)
{
	gfx_Begin(); 
	
	gfx_SetTextFGColor(255);
	gfx_SetTextBGColor(0);
	gfx_SetTextTransparentColor(254);
	gfx_ZeroScreen(); 
	startJIT();
	testJIT();
	
	gfx_End(); 
	
    return 0;
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
	} else { 
		gfx_End();
		os_ClrHomeFull();
		strcpy(os_AppErr1,"FileNotFound: ");
		strcat(os_AppErr1,filename);
		os_ThrowError(OS_E_APPERR1); 
	} 
	
	return dataPtr;
} 

void drawNametable(const char* nametable) { 
	timer_Disable(1); 
	
	for(uint8_t y = 0; y < 232; y += 8) { 
		gfx_SetTextXY(32,y); 
		for(uint8_t x = 0; x < 32;x++) { 
			uint8_t c = *(nametable);
			nametable++;
			nametable++;
			gfx_PrintChar(c < 128 ? c : 0); 
		}
	}
	gfx_SetTextXY(1,232);
	gfx_PrintUInt(timer_Get(1),8); 
	timer_Set(1,0);
	timer_Enable(1,TIMER_CPU,TIMER_NOINT,TIMER_UP); 
}