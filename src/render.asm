include 'ti84pceg.inc' 
include 'vars.inc'
section .text 

public render_init 
public render_cleanup
public render_parse

public render_sprites
public high_priority_sprite
public low_priority_sprite

public deb_get_bank 


render_init: 
	; TODO: should tile cache in on the heap?
	di 
	xor a,a 
	ld (ti.usbInited),a		; for safety with expanded heap
	
	; clear VRAM
	ld de,$D40000+1  
	ld hl,$D40000
	ld (hl),0  
	ld bc,320*480 - 1
	ldir 
	
	ld hl,ti.mpLcdPalette 
	ld (hl),0
	ld de,ti.mpLcdPalette+1
	ld bc,511 
	ldir
	
	; Wait a few frames, seems to keep more visually consistent
	ld b,4
.wait: 
	ld hl,ti.mpLcdIcr 
	set 3,(hl)
	ld l,ti.lcdRis 
.l1: 	
	bit 3,(hl) 
	jr z,.l1 
	djnz .wait
	
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
	
; initial debrujin palettes and cache 
	call map_debrujin_sequences
	call generate_debrujin_sequences
	xor a,a 
	ld (debrujin_bank_list_len),a 
	

; load render code to cursorImage
	ld hl,render_src
	ld de,$E30800 
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

map_debrujin_sequences: 
	; debrujin mappings
	; thanks to calc84maniac
    ld hl,debrujin_mapping
    ld b,l
    ld a,$40 ; Initial LFSR state
.loop:
    ; Write the index for the current pixel sequence into the LUT
    ld (hl),b
    ; Shift a 0 into each nibble of the pixel sequence
    sla l
    res 4,l
    ; Shift the LFSR for the high pixel bit
    add a,a
    jr nc,.l1
    xor a,$C3	; $C3 produces a maximal length LFSR for 8 bits (covers all nonzero values) 
    ; Record a 1 in the low nibble
    inc l
.l1:
    ; Shift the LFSR to get the low pixel bit
    add a,a
    jr nc,.l2
    xor a,$C3
    ; Record a 1 in the high nibble
    set 4,l
.l2:
    ; Advance the index
    inc b
    jr nz,.loop
	ret 

generate_debrujin_sequences: 
	; generate palette 0 sequences 
	ld hl,0 
	ld de,render_palettes
	ld a,$40 	; initial LFSR state
.loop: 
	add hl,hl	; shift in new pixel 
	add hl,hl
	ex de,hl 
	ld (hl),d 
	ex de,hl 
	ld h,0 
	add a,a
	jr nc,.l1
    xor a,$C3
    inc l
.l1:
    add a,a
    jr nc,.l2
    xor a,$C3
    set 1,l
.l2:
	inc e
	jr nz,.loop 

modify_bg: 
; modify for other palettes 
	ld de,render_palettes+$100 
	ld c,0  
	ld b,3 
.outer: 
	ld a,c 
	add a,3
	ld c,a 
	ld hl,render_palettes 
.loop: 
	ld a,(hl) 
	or a,a 
	jr z,.skip	; 0 pixels stay the same 
	add a,c 
.skip: 
	ld (de),a 
	inc de 
	inc l 
	jr nz,.loop 
	djnz .outer 
	
modify_sprite: 
	ld c,$0F 
	ld b,4 
.outer: 
	ld hl,render_palettes 
.loop: 
	ld a,(hl) 
	or a,a 
	jr z,.skip	; 0 pixels stay the same 
	rla			; shift to top nibble
	rla
	rla
	rla
	add a,c 
.skip: 
	ld (de),a 
	inc de 
	inc l 
	jr nz,.loop 
	ld a,c 
	add a,3 shl 4 
	ld c,a 
	djnz .outer 
	ret 

; returns a ptr to a debrujin translated bank, given a ptr to an untranslated one.
; in: de = bank ptr 
; out: de = translated bank ptr
deb_get_bank: 
	ld a,(debrujin_bank_list_len) 
	or a,a 
	jr z,.add_bank 
	ld iy,debrujin_bank_list
	ld b,a 
.find: 
	ld hl,(iy+0) 
	or a,a 
	sbc hl,de 
	jr z,.found 
	lea iy,iy+3
	djnz .find 
	jr .add_bank
.found: 
	ld a,(debrujin_bank_list_len) 
	sub a,b 		; find index 
	ld l,a 			
	ld h,128 		; offset = 1024*index = 128*8*index
	mlt hl 
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,debrujin_cache
	add hl,de 
	ex de,hl 
	ret 
	
.add_bank: 
	ld a,(debrujin_bank_list_len) 
	inc a 
	cp a,debrujin_bank_list_max 	; reset cache if we've reached max # banks 
	jr nz,$+4 
	ld a,1  
	ld (debrujin_bank_list_len),a 
	dec a 
	push de 
	pop iy 	; iy = bank ptr 
	; add bank to list 
	ld hl,debrujin_bank_list
	ld d,a 
	ld e,3 
	mlt de 
	add hl,de 
	ld (hl),iy 
	; find offset into cache
	ld l,a 			
	ld h,128 		; offset = 1024*index = 128*8*index
	mlt hl 
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,debrujin_cache
	add hl,de 
	ex de,hl 
	push de 
; bitplane to debuijin tile 
; de = output ptr , iy = bank ptr
.convert:
	exx 
	ld d,$F0 
	ld e,$0F 
	exx 
	ld hl,debrujin_mapping
	ld c,64 
.outer: 
	ld b,8 
.loop:
	exx 
	ld h,(iy+8) 
	ld l,(iy+0) 
	; combine bitplane for high 4 pixels 
	ld a,h 
	and a,d	;$F0 
	ld c,a 
	ld a,l 
	and a,d
	rra
	rra
	rra 
	rra 
	or a,c 
	exx 
	ld l,a 	
	ld a,(hl) 
	ld (de),a 
	inc de 
	exx 
	; again for low 4 pixels 
	ld a,h
	and a,e	;$0F
	rla 
	rla 
	rla 
	rla
	ld c,a 
	ld a,l
	and a,e
	or a,c
	exx 
	ld l,a 
	ld a,(hl) 
	ld (de),a 
	inc de 
	inc iy 
	djnz .loop 
	lea iy,iy+8
	dec c 
	jr nz,.outer 
	pop de
	ret 
	
	

; reads render event list and draw background
render_parse:
	; wait until front porch to avoid visual errors 
	ld hl,ti.mpLcdRis 
.l1: 	
	bit 3,(hl) 
	jr z,.l1
	ld l,ti.lcdIcr 	; acknowledge interrupt
	set 3,(hl)
	
	call spiLock	; disable DMA to lcd driver; lets us mess with framebuffer
	
	;clear screen
	; TODO: remove once render_background is functional
	ld de,vbuffer+1  
	ld hl,vbuffer
	ld (hl),0
	ld bc,256*224 - 1  
	ldir 
	
	; do the actual drawing 
	call render_background 
	call render_sprites
	
	
	; palette stuff
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
	; DE = palette tint ptr
	; Fetch palette for rendering
	ld iy,ppu_palettes 
	ld ix,ti.mpLcdPalette 
	
	; load backdrop color
	or a,a 
	sbc hl,hl 
	ld l,(iy+0) 
	add hl,hl
	add hl,de 
	ld hl,(hl) 
	ld (ix+0),hl
	lea ix,ix+2
	; fetch BG colors 
	inc iy 
fetch_bg_palette: 
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
	inc iy 	; skip $04,$08,$0C
	dec c 
	jr nz,.outer 
	
	; fetch sprite colors 
	; sprite colors are all stored at $xF (transparent pixels are 0) 
	ld ix,ti.mpLcdPalette+$1F*2
	ld iy,ppu_palettes+$11
fetch_spr_palette: 
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
	lea ix,ix+$10*2
	inc iy 
	djnz .inner
	inc iy 	; skip $04,$08,$0C
	dec c 
	jr nz,.outer 
	
	; copy background palette to $F0 index range 
	ld hl,ti.mpLcdPalette
	ld de,ti.mpLcdPalette+$F0*2
	ld bc,13*2
	ldir
	
	
	; wait until next front porch to reenable DMA 
	ld hl,ti.mpLcdRis 
.l3: 	
	bit 3,(hl) 
	jr z,.l3
	ld l,ti.lcdIcr 	; acknowledge interrupt
	set 3,(hl)
	
	call spiUnlock
	ret
	
;TODO: extend to 8x16 sprites. Test using Galaga or Dig Dug
render_sprites:
	; upload sprite renderer 
	ld hl,spr_src
	ld de,$E10010 
	ld bc,spr_len 
	ldir
	
	ld ix,jit_scanline_vars
	bit 4,(ppu_mask) 
	ret z 
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
; fetch banks
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

	ld de,(s_bank0) 
	call deb_get_bank
	ld (s_bank0),de
	
	ld de,(s_bank1) 
	call deb_get_bank
	ld (s_bank1),de
	
	ld de,(s_bank2) 
	call deb_get_bank
	ld (s_bank2),de
	
	ld de,(s_bank3) 
	call deb_get_bank
	ld (s_bank3),de
		
	ld iy,ppu_oam
	ld c,64
	exx 
	ld de,vbuffer
	exx
	jp render_sprites_loop

render_big_sprites: 
; fetch banks
	ld de,(chr_ptr_backup_0) 
	call deb_get_bank
	ld (s_bank0),de
	
	ld de,(chr_ptr_backup_1) 
	call deb_get_bank
	ld (s_bank1),de
	
	ld de,(chr_ptr_backup_2) 
	call deb_get_bank
	ld (s_bank2),de
	
	ld de,(chr_ptr_backup_3) 
	call deb_get_bank
	ld (s_bank3),de
	
	ld de,(chr_ptr_backup_4) 
	call deb_get_bank
	ld (s_bank4),de
	
	ld de,(chr_ptr_backup_5) 
	call deb_get_bank
	ld (s_bank5),de
	
	ld de,(chr_ptr_backup_6) 
	call deb_get_bank
	ld (s_bank6),de
	
	ld de,(chr_ptr_backup_7) 
	call deb_get_bank
	ld (s_bank7),de
	
	ld iy,ppu_oam
	ld c,64
	exx 
	ld de,vbuffer
	exx
	jp render_big_sprites_loop
	
virtual at $E30800 

render_background: 
	ret 

render_sprites_loop:
.loop: 
	ld ix,jit_scanline_vars
	ld (s_offset),0
	ld b,8		; initial y length
	
	; x clipping
	ld a,(iy+3) 
	or a,a 
	jq z,.end 
	cp a,256-8
	jq nc,.end 
	
	;y clipping
	ld a,(iy+0)
	cp a,231 	; off the bottom?
	jq nc,.end 	
	cp a,231-7	; partial? 
	jr c,.nobotclip
	ld e,a 
	sub a,231	; new length = 231 - y start  
	neg 
	ld b,a 
	ld a,e 
	jr .tile 
.nobotclip: 
	cp a,7			; partially off the top? 
	jr nc,.tile 	
	; top clipping
	sub a,7 		; find how many lines are offscreen 	
	add a,b
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
	ld h,2		; y dir
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
	ld h,-2
	exx
.noflip: 
	ld l,(iy+3)
	bit 6,a		; horizontal flip?
	jr z,.noflip2
	; add 7 to x start
	ld h,a 
	ld a,7
	add a,l 
	ld l,a 
	ld a,h  
	exx 
	inc l		; dec e 
	exx 
.noflip2:
	pop ix		; ix = tile data
	add ix,de	; + 2*s_offset
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

render_big_sprites_loop: 
.loop:
	ld ix,jit_scanline_vars
	ld (s_offset),0
	ld b,16		; initial y length
	
	; x clipping
	ld a,(iy+3) 
	or a,a 
	jq z,.end 
	cp a,256-8
	jq nc,.end 
	
	;y clipping
	ld a,(iy+0)
	cp a,231 	; off the bottom?
	jq nc,.end 	
	cp a,231-15	; partial? 
	jr c,.nobotclip
	ld e,a 
	sub a,231	; new length = 231 - y start  
	neg 
	ld b,a 
	ld a,e 
	jr .tile 
.nobotclip: 
	cp a,7			; partially off the top? 
	jr nc,.tile 	
	; top clipping
	sub a,7 		; find how many lines are offscreen 	
	add a,b
	ld b,a 			; adjust length 
	ld a,16			
	sub a,b 
	ld (s_offset),a	; offset = 16 - new length
	ld a,7			; new y start
.tile: 
	exx 
	add a,$20 - 7	; store y offset. Apparently sprites cant be displayed on line 0.
	ld d,a
	exx 
	; find tile bank
	ld a,(iy+1)
	rrca	; bit 0 selects whether the tile is in $0000 or $1000 
	ld h,8
	ld l,a 
	mlt hl ; h = bank
	ld l,3 
	lea de,s_bank0 
	mlt hl 
	add hl,de
	ld hl,(hl) 
	; add tile offset
	and a,11111b
	ld d,a  
	ld e,16*2
	mlt de 		; tile# * 32
	add hl,de
	push hl 
	; compute offset
	exx
	ld h,2		; y dir
	ld l,$1C	; inc e
	exx 
	ld de,0
	ld e,(s_offset)
	ld a,(iy+2)
	bit 7,a 	; vertical flip? 
	jr z,.noflip 
	; s_offset = (16-1) - s_offset
	ld hl,15 
	or a,a 
	sbc hl,de
	ex de,hl 
	exx 
	ld h,-2
	exx
.noflip: 
	ld l,(iy+3)
	bit 6,a		; horizontal flip?
	jr z,.noflip2
	; add 7 to x start
	ld h,a 
	ld a,7
	add a,l 
	ld l,a 
	ld a,h  
	exx 
	inc l		; dec e 
	exx 
.noflip2:
	pop ix		; ix = tile data
	add ix,de	; + 2*s_offset
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

	
; a = sprite flags 
; ix = sprite data
; iy = oam data 
; h' = y dir
; l' = x op
; de' = screen ptr
; l = x start
high_priority_sprite:
	exx 
	; load smc data
	ld e,a 
	ld a,l 
	ld (sprite_outer.smc_x_dir),a
	ld (sprite_outer.smc_x_dir2),a
	; initialize palette
	ld a,e 
	and a,11b
	add a, 4 + ((render_palettes shr 8 ) and $FF)
	ld (sprite_outer.smc_palette),a 
	ld a,h 
	ld (sprite_outer.smc_y_dir),a
	ld hl,render_palettes
	exx 
	ld a,l 
	ld (sprite_outer.smc_x_start),a 
	jp sprite_outer

	
low_priority_sprite:
	exx 
	; load smc data
	ld e,a 
	ld a,l 
	ld (.smc_x_dir),a
	; initialize palette
	ld a,e 
	and a,11b
	add a, 4 + ((render_palettes shr 8 ) and $FF)
	ld (.smc_palette),a 
	ld a,h 
	ld (.smc_y_dir),a
	ld hl,render_palettes
	exx 
	ld a,l 
	ld (.smc_x_start),a 
.outer: 
	exx
	inc d				; y += 1 
	ld c,2
	ld l,(ix+0) 
	ld e,0
.smc_x_start:=$-1
.fetch:
	ld h,0 
.smc_palette:= $-1 
	ld b,4
.loop:
	bit 0,(hl) 
	jr z,.skip
	ld a,(de)
	cp a,$10 
	jr nc,.skip
	or a,a 
	jr z,.backdrop
	or a,$F0 
	jr $+3 
.backdrop: 
	or a,(hl) 
	ld (de),a
.skip: 
	inc hl 
.smc_x_dir:	inc e
	djnz .loop
	ld l,(ix+1)
	dec c 
	jr nz, .fetch 
	exx 
	lea ix,ix+2
.smc_y_dir:= $-1
	djnz .outer
	ret	
	

assert $$ < $E30BFF
load render_data:$-$$ from $$ 
render_len := $-$$

end virtual 

render_src: 
	db render_data
	
	
virtual at $E10010 

sprite_outer:
.outer: 
	exx
	inc d				; y += 1 
	ld c,2
	ld l,(ix+0) 
	ld e,0
.smc_x_start:=$-1
.fetch:
	ld h,0 
.smc_palette:= $-1 
	ld b,2
.loop: 
	ld a,(de)
	cp a,$10 
	jr nc,.skip
	or a,(hl)
	ld (de),a
.skip: 
	inc hl 
.smc_x_dir:	inc e
	ld a,(de)
	cp a,$10 
	jr nc,.skip2
	or a,(hl)
	ld (de),a
.skip2: 
	inc hl 
.smc_x_dir2:inc e
	djnz .loop
	ld l,(ix+1)
	dec c 
	jr nz,.fetch 
	exx 
	lea ix,ix+2
.smc_y_dir:= $-1
	djnz .outer
	ret
	
assert $$-$ <= 64
load spr_data:$-$$ from $$ 
spr_len := $-$$
end virtual

spr_src: 
	db spr_data
	
section .bss 

public lcd_timing_backup

public debrujin_bank_list_len
public debrujin_bank_list_max
public debrujin_bank_list 

lcd_timing_backup: rb 8

debrujin_bank_list_max := 32 

debrujin_bank_list_len: rb 1 	; list of banks currently in cache (ez80 address)  
debrujin_bank_list: rb 3*debrujin_bank_list_max 

section .rodata
public debrujin_cache

debrujin_cache:			; user ram reserved for cached chr banks
	db debrujin_bank_list_max*1024 dup 0
	
section .data

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

