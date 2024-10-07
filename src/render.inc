
	; line: 
	; +0 # of lines (0=special cases)
	; +1 y offset 
	; +2 x offset 
	; +3 len1 (len2 = 31 - len1) 
	; +4 nametable1 
	; +6 nametable2 
	; bg disabled lines: 
	; +0 0 
	; +1 0
	; +2 #lines to blank 
	; bankswap: 
	; +0 0 
	; +1 1 
	; +2 bank0..3 
render_init: 
	
	
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
