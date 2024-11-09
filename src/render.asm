section .text 

public render_init 
public render_init_cache 
public render_parse



render_init: 

; initialize cache at start of frame
; loads chr rom banks
; fetches initial palette 

; list of banks in cache: 
	; bank address,palette
; bank is list of 16-bit offsets into cache: 64 x 4 x 2 bytes  
; 8kb allocated for banks (up to 64 different bank&palette combos can be in the cache at any time) 
; banks are added dynamically as needed 
	
render_init_cache: 
	; load current chr banks 

; a = slot (0..3) 
; hl = phys address of bank to load
load_chr_slot:

; reads render event list and draw background
render_parse: 
	
	
section .rodata 

public nes_palettes 

; rgb888 palette data
virtual at 0 
	palette_data:: 
	file 'ntscpalette.pal'
end virtual 

macro load_palettes 
	local index,combined,rcomp,gcomp,bcomp
repeat 8*64 
	index = % - 1
	load rcomp:byte from palette_data:(index*3)
	load gcomp:byte from palette_data:(index*3+1)
	load bcomp:byte from palette_data:(index*3+2)
	combined = (bcomp shr 3) + ((gcomp shr 3) shl 5) + ((rcomp shr 3) shl 10)
	store combined:2 at nes_palettes + index*2 
end repeat
end macro 

; NES palettes (all color emphasis versions) in rgb1555  
; 0: normal colors 
; 1: r
; 2: g
; 3: r+g
; 4: b 
; 5: b+r 
; 6: b+g 
; 7: b+g+r
nes_palettes: 
	rb 8*64*2

load_palettes
