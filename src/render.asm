include 'ti84pceg.inc' 
include 'vars.inc'
section .text 

public render_init 
public render_cleanup
public render_parse

public render_sprites
public high_priority_sprite
public low_priority_sprite

render_init: 
	; TODO: should tile cache in on the heap?
	di 
	xor a,a 
	ld (ti.usbInited),a		; for safety with expanded heap
	
	; clear VRAM
	ld de,$D40000+1  
	ld hl,$D40000
	ld (hl),$FF  
	ld bc,320*480 - 1
	ldir 
	
	ld hl,ti.mpLcdIcr 
	set 3,(hl)
	ld l,ti.lcdRis 
.l1: 	
	bit 3,(hl) 
	jr z,.l1 
	
	; set bpp = 4
	ld a,(ti.mpLcdCtrl) 
	and a,11110001b ; mask out bpp
	or a,ti.lcdBpp8
	ld (ti.mpLcdCtrl),a  
	ld a,(ti.mpLcdCtrl+1)
	and a,00001111b 
	or a,ti.lcdIntFront shr 8 ; set VCOMP at start of front porch
	ld (ti.mpLcdCtrl+1),a
	
	; set vbuffer start
	ld hl,vbuffer 
	ld (ti.mpLcdUpbase),hl
	
	; backup old timing
	ld hl,ti.mpLcdTiming0 
	ld de,lcd_timing_backup
	ld bc,8 
	ldir 
	; set LCD timing 
	ld hl,lcdTiming 
	ld de,ti.mpLcdTiming0 
	ld c,8 
	ldir
	ld hl,1023 	; want as few lines as possible to minimize useless sync signals 
	ld (ti.mpLcdTiming2+2),hl	; clocks per line 
	xor a,a 
	ld (ti.mpLcdTiming2),a		; Clock divisor
	call spiInitVSync
	
	ld hl,ti.mpLcdIcr 
	set 3,(hl)
	ld l,ti.lcdRis 
.l2: 	
	bit 3,(hl) 
	jr z,.l2
	
	ld hl,ti.mpLcdPalette+255*2 
	ld (hl),$FF 
	inc hl 
	ld (hl),$FF 
	
	ld de,vbuffer+1  
	ld hl,vbuffer
	ld (hl),$FF
	ld bc,256*224 - 1  
	ldir 
	
; load render code to cursorImage
	ld hl,render_src
	ld de,render_parse 
	ld bc,render_len 
	ldir
	
	ret

render_cleanup:
	; reload old settings 
	ld hl,$D40000
	ld (ti.mpLcdUpbase),hl
	ld a,(ti.mpLcdCtrl) 
	and a,11110001b ; mask out bpp
	or a,ti.lcdBpp16
	ld (ti.mpLcdCtrl),a  
	
	ld hl,lcd_timing_backup
	ld de,ti.mpLcdTiming0 
	ld bc,8 
	ldir
	ld hl,239
	ld (ti.mpLcdTiming2+2),hl
	call spiEndVSync
	ret 
	
; Timing = (HSW+1+HFP+1+HBP+1+16*(PPL+1)) * (LPP+1+VSW+1+VFP+VBP) * (PCD+2) * 2
;Timing = (1+1+1+(63+1)*16) * (55+1+1+138)*2*2 = 801060 cycles or 59.92 frames/second
; approx 234000 cycles spent sending 
lcdTiming: 
	db	63 shl 2 	; PPL 
	db	0 			; HSW
	db	0 			; HFP 
	db	0 			; HBP 
	dw	55 			; LPP & VSW(0) 
	db	138			; VFP
	db	0 			; VBP
	db 	0 			; 

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


virtual at $E30800 

; reads render event list and draw background
render_parse:
	; wait until front porch to avoid visual errors 
	ld hl,ti.mpLcdRis 
.l1: 	
	bit 3,(hl) 
	jr z,.l1
	ld l,ti.lcdIcr 	; acknowledge interrupt
	set 3,(hl)
	
	call spiLock	; disable DMA to lcd driver
	; clear screen
	ld de,vbuffer+1  
	ld hl,vbuffer
	ld (hl),0
	ld bc,256*224 - 1  
	ldir 
		
	; load tint
	ld ix,jit_scanline_vars 
	ld a,(ppu_mask_backup) 
	bit 0,a 
	jr nz,.grayscale	; bit 0 = grayscale select
	; get tint from top 3 bits
	rla 	; >>5		
	rla 
	rla 
	rla 
	and a,111b 
	ld l,a 
	ld h,64*2 
	mlt hl 
	ld de,nes_palettes
	add hl,de 
	ex de,hl 
	jr .l2
.grayscale: 
	ld de,nes_grayscale_palette 
.l2: 
	; just load regular palette for now 
	ld iy,ppu_palettes 
	ld ix,ti.mpLcdPalette 
	
	; load backdrop color
	or a,a 
	sbc hl,hl 
	ld l,(iy+0) 
	add hl,hl
	add hl,de 
	ld hl,(hl) 
	ld (ix+0),l
	ld (ix+1),h
	lea ix,ix+2
	push hl 
	exx 
	pop hl 
	exx
	inc iy 
	call palette_loop
	ld ix,ti.mpLcdPalette+$81*2
	inc iy 
	call palette_loop 
	; copy background palette to $C0 index range 
	ld hl,ti.mpLcdPalette
	ld de,ti.mpLcdPalette+$C0*2
	ld bc,16*2
	ldir
	call render_sprites
	ld hl,ti.mpLcdRis 
.l3: 	
	bit 3,(hl) 
	jr z,.l3
	ld l,ti.lcdIcr 	; acknowledge interrupt
	set 3,(hl)
	
	call spiUnlock
	ret
	


palette_loop:
	ld c,4
.outer: 
	ld b,3 
.inner: 
	or a,a 	
	sbc hl,hl 
	ld l,(iy+0) 
	add hl,hl 
	add hl,de 
	ld hl,(hl) 
	ld (ix+0),l 
	ld (ix+1),h 
	lea ix,ix+2 
	inc iy 
	djnz .inner 
	dec c 
	ret z 
	exx 
	ld (ix+0),l 
	ld (ix+1),h 
	exx
	lea ix,ix+2 
	inc iy 
	jr .outer
	
;TODO: extend to 8x16 sprites. Test using Galaga or Dig Dug
render_sprites:
	ld ix,jit_scanline_vars
	; find sprite size 
	bit 5,(ppu_ctrl) 
	jp nz,render_big_sprites 
.small: ; 8x8
	; sprite area at $0000 or $1000? 
	or a,a 
	sbc hl,hl 
	bit 3,(ppu_ctrl) 
	jr z,.l1
	ld l,4*3 
.l1: 
	lea de,chr_ptr_backup_0
	add hl,de 
	ld bc,3 
	ld de,(hl)
	ld (s_bank0),de 
	add hl,bc 
	ld de,(hl)
	ld (s_bank1),de 
	add hl,bc 
	ld de,(hl)
	ld (s_bank2),de 
	add hl,bc 
	ld de,(hl)
	ld (s_bank3),de 
.l2: 
	ld iy,ppu_oam
	ld c,64
	exx 
	ld de,vbuffer
	exx
.loop: 
	ld ix,jit_scanline_vars
	ld (s_offset),0
	ld b,8		; initial y length
	; x clipping
	ld a,(iy+3) 
	or a,a 
	jr z,.end 
	cp a,256-8
	jq nc,.end 
	;y clipping
	ld a,(iy+0)
	cp a,231 	; off the bottom?
	jq nc,.end 	; no need for bottom clipping
	cp a,7
	jr nc,.tile 
	; top clipping
	sub a,7 		; find how many lines are offscreen 
	add a,b			
	jq z,.end
	ld b,a 			; adjust length 
	ld a,8			
	sub a,b 
	ld (s_offset),a	; offset = 8 - new length
	ld a,7			; new y start
.tile: 
	exx 
	add a,$20 - 7	; store y offset. Apparently sprites cant be displayed on line 0.
	ld d,a
	exx 
	; find tile bank
	ld a,(iy+1) 
	ld h,4 
	ld l,a 
	mlt hl ; h = bank
	ld l,3 
	lea de,s_bank0 
	mlt hl 
	add hl,de
	ld hl,(hl) 
	; add tile offset
	and a,111111b
	ld d,a  
	ld e,8*2
	mlt de 		; tile# * 16
	add hl,de
	push hl 
	; compute offset
	exx
	ld h,$23	; inc ix 
	ld l,$1C	; inc e
	exx 
	ld de,0
	ld e,(s_offset)
	ld a,(iy+2)
	bit 7,a 	; vertical flip? 
	jr z,.noflip 
	; s_offset = (8-1) - s_offset
	ld hl,7 
	or a,a 
	sbc hl,de
	ex de,hl 
	exx 
	ld h,$2B 	; dec ix 
	exx
.noflip: 
	ld l,(iy+3)
	bit 6,a		; horizontal flip?
	jr z,.noflip2
	; add 8 to x start
	ld h,a 
	ld a,8 
	add a,l 
	ld l,a 
	ld a,h  
	exx 
	inc l		; dec e 
	exx 
.noflip2:
	pop ix		; ix = tile data
	add ix,de
	bit 5,a 
	jq nz,.low_prio 
	call high_priority_sprite
.end: 
	lea iy,iy+4
	dec c 
	jq nz,.loop
	ret
.low_prio: 
	call low_priority_sprite
	jr .end 
	
render_big_sprites: 
	ret
	
; a = sprite flags 
; ix = sprite data
; iy = oam data 
; h' = y op 
; l' = x op
; de' = screen ptr
; l = x start
high_priority_sprite:
	exx 
	; load smc data
	ld e,a 
	ld a,h 
	ld (.smc_y_dir),a 
	ld a,l 
	ld (.smc_x_dir),a
	; initialize palette
	ld a,e 
	and a,11b
	rla
	rla 
	set 7,a 
	ld c,a
	exx 
.outer: 
	exx
	ld h,(ix+8) ; high bit plane 
	ld l,(ix+0) ; low 
	inc d
	exx 
	ret z
	ld a,l 		
	exx
	ld e,a 
	inc ix
.smc_y_dir:=$-1 
	ld b,8
	jr $+3		; skip initial inc/dec
.loop: 
.smc_x_dir: inc e 
	ld a,(de)
	rla 
	jr c,.skip 
	xor a,a
	rl h 		; shift in next bit
	rla  
	rl l 
	adc a,a
	jr z,$+4	; skip transparent pixel 
	or a,c 		; add palette 
	ld (de),a
	djnz .loop
	exx 
	djnz .outer 
	ret 
.skip: 
	add hl,hl 
	djnz .loop 
	exx 
	djnz .outer 
	ret 
	
low_priority_sprite:
	exx 
	; load smc data
	ld e,a 
	ld a,h 
	ld (.smc_y_dir),a 
	ld a,l 
	ld (.smc_x_dir),a
	; initialize palette
	ld a,e 
	and a,11b
	rla
	rla 
	set 7,a 
	ld c,a
	exx 
; low priority sprite
.outer: 
	exx
	ld h,(ix+8) ; high bit plane 
	ld l,(ix+0) ; low 
	inc d
	exx 
	ret z
	ld a,l 		
	exx
	ld e,a 
	inc ix
.smc_y_dir:=$-1 
	ld b,8
	jr $+3		; skip initial inc/dec
.loop: 
.smc_x_dir:	inc e
	ld a,(de) 	
	or a,a 
	jr z,.transparent 	; if pixel is the transparent color, do normal sprite things 
	bit 7,a 
	jr nz,.skip ; skip if already drawn to  
.opaque:
	add hl,hl ; discard this sprite pixel
	or a,11000000b 	; copy of background palette in sprite space  
	ld (de),a 
	inc e 
	djnz .loop
.end: 	
	exx 
	djnz .outer
	ret 
.skip: 
	add hl,hl ; discard this sprite pixel 
	djnz .loop
	exx 
	djnz .outer 
	ret 
.transparent: 
	rl h 		; shift in next bit
	rla  
	rl l 
	adc a,a
	jr z,$+4	; skip transparent pixel 
	or a,c 		; add palette 
	ld (de),a
	djnz .loop
	exx 
	djnz .outer 
	ret 
	
assert $$ < $E30BFF
load render_data:$-$$ from $$ 
render_len := $-$$

end virtual 

render_src: 
	db render_data
	
section .bss 

public lcd_timing_backup

lcd_timing_backup: rb 8


section .rodata 

public nes_palettes 
public nes_grayscale_palette

; rgb888 palette data
virtual at 0 
	palette_data:: 
	file 'ntscpalette.pal'
end virtual 

macro load_palettes 
	local index,combined,average,rcomp,gcomp,bcomp
repeat 8*64 
	index = % - 1
	load rcomp:byte from palette_data:(index*3)
	load gcomp:byte from palette_data:(index*3+1)
	load bcomp:byte from palette_data:(index*3+2)
	combined = (bcomp shr 3) + ((gcomp shr 3) shl 5) + ((rcomp shr 3) shl 10)
	store combined:2 at nes_palettes + index*2 
end repeat
repeat 64 
	index = % - 1
	load rcomp:byte from palette_data:(index*3)
	load gcomp:byte from palette_data:(index*3+1)
	load bcomp:byte from palette_data:(index*3+2)
	average = ( rcomp + bcomp + gcomp ) / 3 
	combined = (average shr 3) + ((average shr 3) shl 5) + ((average shr 3) shl 10)
	store combined:2 at nes_grayscale_palette + index*2 
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
; 8: greyscale
nes_palettes: 
	rb 8*64*2
nes_grayscale_palette: 
	rb 64*2

load_palettes


extern spiInitVSync
extern spiEndVSync
extern spiLock 
extern spiUnlock

extern ppu_chr_ptr