include 'ti84pceg.inc'

;preliminary implementation of the background renderer 

section .text

public render_init
public line_renderer 

public render_src_len

tile_sps_stack_top:= $5700		; last page of vram
tile_sps_stack_bot:= tile_sps_stack_top + 30*2
vram_screen_start := $D42000
tile_cache := $D50000

render_init: 
	ld hl,tile_hashtable 		; set entry hashtable to point to invalid entry
	ld bc,tile_entry_invalid
	ld (hl),bc 
	ld de,tile_hashtable+3 
	ld bc,3*1023 
	ldir 
	
	ld hl,tile_entries
	ld (tile_entry_next),hl 
	ld hl,tile_cache 
	ld (tile_cache_next),hl 
	
	; move routines to low waitstate areas 
	; make sure ports are unlocked!
	ld de,line_renderer
	ld hl,render_data
	ld bc,render_src_len
	ldir 
	
	ld de,compile_line
	ld hl,compile_line_data
	ld c,compile_line_src_len
	ldir 
	
	ret 
	
virtual at $E30800

;iy = line descriptor (+0:x offset,+1:y offset,+2: flags)
;ix = tile line ptr (tile# + palette)
line_renderer: 
	ld (ti.tempSP),sp 
	ld de,vram_screen_start
	ld bc,0
.start:
	ld a,(iy+2) 
	rla 		; PPU turned off?
	jr z,.cont
.blank_line:	; draws a black line
	or a,a 		
	sbc hl,hl 
	ld e,l 
	add hl,de 
	ld (hl),$FE	; universal black
	inc e
	ld c,255
	ldir 
	lea iy,iy+3
	jr .start
.cont:
	rla 		; new tile line? 
	jp nz,compile_line
.draw:
	ld a,8
	ld e,(iy+0) ; load offsets
	ld hl,tile_cache
	ld l,(iy+1) 
	ld sp,hl 
	ld.sis sp,tile_sps_stack_top
repeat 31
	pop.sis hl	; get next tile ptr 
	add hl,sp 	
	ld c,a		; blit 8 pixels
	ldir
end repeat
	lea iy,iy+3 
	inc d 		; d overflows after last line
	jp nz,.start 
	ld sp,(ti.tempSP) 
	ret
	
;called when new tile needs to be added to the cache
; de = hash , c = key
cache_tile: 
	ld sp,ix
	ld a,b 					; preserve count from compile_line
	ex af,af'
	ld a,c
	ld ix,(tile_entry_next)	; create entry 
	lea hl,ix+0 
	ld bc,tile_entry_oob
	or a,a 
	sbc hl,bc 
	jp nc,.invalidate_cache ; out of space 
.cont:
	lea hl,ix+4 
	ld (tile_entry_next),hl 
	ld hl,tile_hashtable 	; find tile bucket  
	add hl,de
	add hl,de
	add hl,de
	ld bc,(hl) 
	ld (ix+0),a 			; place entry on top of bucket
	ld (ix+1),bc 
	ld (hl),ix 
	
	; find tile in CHR memory 
	ld c,a		; c = chr bank config
	ld b,12 
	mlt bc 		; bc = offset of config 
	
	ld a,e 		; nametable tile >> 6
	rla 		
	rla			
	rla 		 
	and a,11b 	; a = bank of tile (0..3) 
	ld l,a 
	ld h,3 
	mlt hl 		
	add hl,bc 	
	ld bc,chr_bank_configs	
	add hl,bc 	 
	ld ix,(hl)  ; ix = ptr to CHR bank 
	
	ld a,e 
	and a,111111b 
	ld c,a 
	ld b,16 
	mlt bc 
	add	ix,bc 	; add offset of tile 

	ld a,d 		; a = palette# 
	add a,a 	; <<2 
	add a,a 
	
	
	ld de,(tile_cache_next)		; get cache location
	push.sis de					; push onto line stack
	ld c,a
	ld b,8
	; transform 2bpp bitplanes into 8bpp
	; ix = tile data 
	; de = out ptr
	; c = palette<<2 
.yloop:  
	ld h,(ix+0) 
	ld l,(ix+8)
repeat 8
	xor a,a
	rl h 
	rla 
	rl l 
	rla 
	or a,c
	ld (de),a 
	inc de
end repeat  
	inc ix
	djnz .yloop
	ld ix,0 
	add ix,sp
	ld (tile_cache_next),de  	; update cache location
	ex af,af' 
	ld b,a
	jp compile_line.end 		
	
.invalidate_cache:
	ld hl,tile_hashtable 		; set entry hashtable to point to invalid entry
	ld bc,tile_entry_invalid
	ld (hl),bc 
	ld de,tile_hashtable+3 
	ld bc,3*4095 
	ldir 
	
	ld hl,tile_entries			; reset next pointers
	ld (tile_entry_next),hl 
	ld hl,tile_cache 
	ld (tile_cache_next),hl 
	
	ld ix,0 
	add ix,sp
	jp compile_line.start		; recompile line
		
	
assert $ < $E30BFC 
load render_src: $ - $$ from $$ 
render_src_len := $ - $$ 
 
end virtual 

render_data: 
	db render_src
	
; only need first byte. 
tile_entry_invalid: 
	db $FF

virtual at $E10010
	; turns tile#'s into cache offsets
compile_line:
	exx 
.start:
	lea ix,ix+31*3  ; start at the end of the line
	ld b,31
	ld.sis sp,tile_sps_stack_bot
.loop:
	lea ix,ix-3
	ld hl,(ix+0) 	; e = bottom 8 bits of tile# , d = palette
	ex.sis de,hl
	ld c,(ix+2)		; 1kb bank config / 4kb bank , used as key 
	ld hl,tile_hashtable 	; find tile bucket
	add hl,de
	add hl,de
	add hl,de
	ld hl,(hl)
	;68cc
.search:  	
	ld a,(hl) 
	cp a,c 
	jr z,.found ; if keys are equal, were done 
	or a,a  
	jp m,cache_tile ; if bit 7 is set, then weve reached the invalid entry, so the tile isnt in the cache
	inc hl 
	ld hl,(hl) 
	jr .search 
	; 44cc per iteration
.found:
	ld de,tile_entries ; cache offset = entry offset*16
	sbc hl,de 
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	push.sis hl		; push cache location
	;30cc
.end: 
	djnz .loop 
	exx 
	jp line_renderer.draw
	
assert $ - $$ < 64	
load compile_line_src: $ - $$ from $$ 
compile_line_src_len := $ - $$ 
end virtual 

compile_line_data: 
	db compile_line_src

;vars 

section .bss 

private tile_entries 
private tile_entry_oob
private tile_entry_next
private tile_cache_next
private tile_hashtable

tile_hashtable: rb 4096*3

tile_entries: rb 4*1024 
tile_entry_next: rb 3 
tile_entry_oob := tile_entries + 4*1024

tile_cache_next: rb 3 


extern _chr_banks
extern chr_bank_configs
