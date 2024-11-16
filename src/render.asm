section .text 

public render_init 
public render_parse

render_init: 
	; TODO: should tile cache in on the heap?
	ld hl,render_init.free_ptr 
	
	push hl 
	call _os_MemChk 
	pop de 
	ld de,$4000	; if value is less than 16kb, throw an error 
	or a,a 
	sbc hl,de 	
	ld a,1 
	ret c
	add hl,de 
	ld de,$10000 ; if value is greater than 64Kb, set start of cache to $D30000
	jr nc,.l1
	; otherwise, round to nearest page 
	ld hl,0
.free_ptr:=$-3
	inc h 
	ld l,0 
	jr .l2 
.l1: 
	ld hl,$D30000 
.l2: 
	ld (render_cache_start),hl 
	xor a,a
	ret
	

; TODO: 
; list of banks in cache: 
	; bank address,palette
; bank is list of 16-bit offsets into cache: 64 x 4 x 2 bytes  
; 8kb allocated for banks (up to 64 different bank&palette combos can be in the cache at any time) 
; banks are added dynamically as needed 

; For now, a bank swap flushes the associated cache
; 4 sub caches (one for each 1kb bank) of equal size


; a = slot (0..3) 
; hl = phys address of bank to load
load_chr_slot:
	ret

; reads render event list and draw background
render_parse: 
	
	
section .bss 

public render_cache_start

render_cache_start: rb 3 

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

extern _os_MemChk 
