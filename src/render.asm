include 'ti84pceg.inc' 
include 'vars.inc'
section .text 

public render_init 
public render_cleanup
public render_draw

public render_background
public render_sprites
public high_priority_sprite
public low_priority_sprite

public debrujin_translate_tile
public update_chr_ram

public render_background.nextevent
public fetch_tile.loop
public fetch_tile

public cache_add_bank
public flush_bank_cache

temp_stack := $D02400

render_init:
	di 
	call spiSetup
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
	
	; Wait a few frames
	ld b,4
.wait: 
	ld hl,ti.mpLcdIcr 
	set 3,(hl)
	ld l,ti.lcdRis 
.l1: 	
	bit 3,(hl) 
	jr z,.l1 
	djnz .wait
	
	; set bpp = 8
	ld a,(ti.mpLcdCtrl) 
	and a,11110001b ; mask out bpp
	or a,ti.lcdBpp8
	ld (ti.mpLcdCtrl),a  
	ld a,(ti.mpLcdCtrl+1)
	and a,00001111b 
	or a,ti.lcdIntFront shr 8 ; set VCOMP interrupt at start of front porch.
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
	call generate_interleave_lut
	call map_debrujin_sequences
	call generate_debrujin_sequences
	call flush_bank_cache

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
	
	
;------------------------------------------------
; Utility functions 

; interleaves zero bits between bits of 8-bit value
generate_interleave_lut: 
	ld b,0 
	ld de,interleave_lut 
.loop: 
	ld a,b
	or a,a 
	sbc hl,hl
repeat 8 
	add hl,hl 
	rla 
	adc hl,hl 
end repeat 
	ex de,hl 
	ld (hl),e 
	inc h 
	ld (hl),d 
	dec h 
	ex de,hl 
	inc e
	inc b 
	jr nz,.loop 
	ret 

map_debrujin_sequences: 
	; debrujin mappings
	; thanks to calc84maniac
    ld hl,debrujin_mapping
    ld b,l
    ld a,$40 ; Initial LFSR state
.loop:
    ; Write the index for the current pixel sequence into the LUT
    ld (hl),b
    ; Shift a 0 into the pixel sequence
    sla l
    sla l
    ; Shift the LFSR for the high pixel bit
    add a,a
    jr nc,.l1
    xor a,$C3	; $C3 produces a maximal length LFSR for 8 bits (covers all nonzero values) 
    ; Record a 1 in the low bit
    inc l
.l1:
    ; Shift the LFSR to get the low pixel bit
    add a,a
    jr nc,.l2
    xor a,$C3
    ; Record a 1 in the high bit
    set 1,l
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
	
; set bit 4 of all non-zero entries to (colors in range $1x) 
	ld b,0 
	ld c,4
	ld hl,render_palettes
.loop2: 
	ld a,(hl) 
	or a,a 
	jr z,$+5
	set 4,a 
	ld (hl),a 
	inc hl 
	djnz .loop2 
	dec c 
	jr nz,.loop2 
	ret 

	
; bitplane to debuijin tile 
; de = output ptr , iy = bank ptr
debrujin_translate_tile: 
	push de 
	exx 
	pop de 
.loop: 
repeat 8
	ld hl,interleave_lut
	ld l,(iy+%-1) 
	ld c,(hl) 
	inc h 
	ld b,(hl) 
	ld l,(iy+%+7) 
	ld a,(hl)
	dec h 
	ld l,(hl)
	ld h,a 
	add hl,hl
	add hl,bc
	ld a,h 
	ld c,l 
	ld hl,debrujin_mapping
	ld l,a 
	ld a,(hl) 
	ld (de),a 
	inc de  
	ld l,c
	ldi 
end repeat
	push de 
	exx
	pop de
	ret 
	
; a = bank slot 
; de = bank pointer 
cache_add_bank:
	push iy
	ld ix,jit_scanline_vars
	; find current bank 
	ld h,a 
	ld l,6
	mlt hl
	lea bc,t_bank0 
	add hl,bc
	push hl
	pop iy 
	; return if new == current 
	ld hl,(iy) 
	or a,a 
	sbc hl,de 
	jr z,.end 
	; find bank slot 
	or a,a 
	sbc hl,hl 
	ld h,a 
	add hl,hl
	ld bc,render_tile_set
	add hl,bc 
	ld (bank_slot),hl
	; if current bank in slot is null, skip store 
	cp a,(iy+5) 
	jr nc,.skip 
	; store current bank tile pointers
	push de 
	ld de,(iy+3) 
	ld bc,512
	ldir
	pop de 
.skip: 
	; is the new bank in the cache? 
	push iy 
	ld (iy),de
	ld a,(render_banks_len) 
	or a,a 
	jr z,.not_in_cache 
	bit 0,(chr_ram_enable) 
	jr nz,.not_in_cache
	ld b,a
	ld iy,render_banks_list
.loop: 
	ld hl,(iy) 
	or a,a 
	sbc hl,de 
	jr z,.in_cache 
	lea iy,iy+3
	djnz .loop 
	jr .not_in_cache
.in_cache: 
	; find index 
	ld a,(render_banks_len) 
	sub a,b
	; find bank pointers
	; + index*512
	sbc hl,hl 
	ld h,a 
	add hl,hl 
	ld bc,render_banks 
	add hl,bc 
	pop iy 
	; copy pointers in cache to slot
	ld (iy+3),hl
	ld de,(bank_slot) 
	ld bc,512 
	ldir 
.end:
	pop iy 
	ret 
.not_in_cache: 
	ld a,(render_banks_len) 
	cp a,render_banks_len_max
	jr nz,.noflush 
	call flush_bank_cache 
	xor a,a 
.noflush: 
	pop iy 	
	; add bank to list 
	ld h,a 
	ld l,3 
	mlt hl 
	ld bc,render_banks_list
	add hl,bc 
	ld (hl),de
	; offset = 512*index 
	or a,a 
	sbc hl,hl 
	ld h,a 
	add hl,hl 
	ld bc,render_banks
	add hl,bc
	ld (iy+3),hl 
	; 
	inc a 
	ld (render_banks_len),a 
	; reset slot pointers 
	ld a,1 
	ld hl,(bank_slot)
	ld b,0
.clear: 
	ld (hl),a 
	inc hl 
	inc hl
	djnz .clear 
	pop iy 
	ret 
	
	 
flush_bank_cache:
	push ix 
	ld ix,jit_scanline_vars
	xor a,a 
	sbc hl,hl
	ld (render_tile_next),hl
	ld (render_banks_len),a 
	
	ld (t_next0),hl
	ld (t_next1),hl
	ld (t_next2),hl
	ld (t_next3),hl
	
	ld hl,render_tile_set
	ld de,render_tile_set+1
	ld (hl),1 
	ld bc,512*4 - 1
	ldir 
	
	pop ix
	ret
	
	
; iterates through chr ram update flags to retranslate tiles 
update_chr_ram:
	ld hl,render_chrram_flags
	bit 4,(ppu_ctrl_backup) 
	jr z,$+3
	inc h 
.loop: 
	bit 0,(hl) 
	jr nz,.update_tile 
.return:
	inc l 
	jr nz,.loop 
	ret 
.update_tile: 
	res 0,(hl)
	push hl 
	; get tile #
	ld h,0  
	add.sis hl,hl
	ld de,render_tile_set 
	add hl,de 
	
	; invalidate tiles in bg cache
	ld (hl),1
	inc h 
	inc h
	ld (hl),1
	inc h
	inc h
	ld (hl),1
	inc h
	inc h
	ld (hl),1 
	pop hl
	jq .return 
	

set_frameskip: 
	; disable timer 1 
	ld bc,0 
	ld (ti.mpTmrCtrl),bc 
	
	; divide by frameskip to find average cycles per frame 
	ld hl,(ti.mpTmr1Counter)
	ld c,(frameskip)
	call __idvrmu	; de = hl/bc 
	; get new frameskip value  
	ld hl,100000
	or a,a 
	sbc hl,de
	jr nc,.waste_time
	ld hl,400000
	ld a,2 			; minimum value = 2 , to account for render time 
	or a,a 
	sbc hl,de
	ret nc 
	inc a 			; 3 
	or a,a 
	ld hl,534000
	sbc hl,de 
	ret nc 
	inc a 			; 4 
	or a,a
	ld hl,601000
	sbc hl,de
	ret nc 
	inc a			; 5
	or a,a
	ld hl,640000
	sbc hl,de
	ret nc 
	inc a			; max 6 
	ret 
.waste_time: 
	; TODO: waste 100,000 cycles to keep rendering from breaking.
	; This is bad. 
	ld b,0 
	ld c,40
.loop: 
	djnz .loop 
	dec c 
	jr nz,.loop 
	ld a,2 
	ret 
	
start_frame_timer:
	; Starts timer 1 counting up at 48Mhz
	xor a,a 
	sbc hl,hl 
	ld (ti.mpTmr1Counter),hl
	ld (ti.mpTmr1Counter+3),a 
	ld hl,ti.tmr1Enable + ti.tmr1CountUp
	ld (ti.mpTmrCtrl),hl 
	ret 
	
;------------------------------------------------------------------
; draw functions 


; reads render event list and draw background
render_draw:
	; update frameskip 
	ld ix,jit_scanline_vars 
	call set_frameskip 
	ld (frameskip),a
	
	; update nametables 
	call attribute_update 
	
	; update chr ram 
	ld ix,jit_scanline_vars
	ld a,(chr_ram_enable) 
	or a,a 
	call nz,update_chr_ram
	
	; wait until front porch to ensure last buffer got sent  
	ld hl,ti.mpLcdRis
.l1: 	
	bit 3,(hl)  
	jr z,.l1
	
	call spiLock	; disable DMA to lcd driver; lets us mess with framebuffer
	
	; do the actual drawing 
	ld (s_topclip),8-1
	ld (s_botclip),232-1
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
	; fetch BG colors (at $1x) 
	lea ix,ix+($11*2)
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
	; sprite colors are all stored at $8x (transparent pixels are 0) 
	ld ix,ti.mpLcdPalette+$80*2
	ld iy,ppu_palettes+$10
fetch_spr_palette: 
	ld b,4*4
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
	
	; copy background palette to $91 index range 
	ld hl,ti.mpLcdPalette+$11*2
	ld de,ti.mpLcdPalette+$91*2
	ld bc,12*2
	ldir

	; reenable DMA if already in front porch
	ld a,(ti.mpLcdUpcurr+2)
	cp a,$D5 
	call z,spiUnlock
	
	jp start_frame_timer
	
render_sprites:
	ld ix,jit_scanline_vars
	ld a,(s_botclip) 
	cp a,8 
	jr c,.early_exit
	ld a,(s_topclip) 
	cp a,(s_botclip) 
	jr z,.early_exit
	cp a,7 
	jr nc,.adjust 
	ld (s_topclip),7 
.adjust: 
	bit 4,(ppu_mask_backup) 
	jr nz,.start  
.early_exit: 
	ld a,(s_botclip) 
	ld (s_topclip),a 
	ld (s_botclip),232-1 
	ret
.start: 	
	; upload sprite renderer 
	ld hl,spr_src
	ld de,$E10010 
	ld bc,spr_len 
	ldir
	
	; find sprite size 
	bit 5,(ppu_ctrl_backup) 
	jr nz,.big 
.small: ; 8x8
	ld (s_size),8
	; sprite area at $0000 or $1000? 
	or a,a 
	sbc hl,hl 
	bit 3,(ppu_ctrl_backup) 
	jr z,.l1
	ld l,4*3 
.l1: 
	; fetch banks
	lea de,chr_ptr_backup_0
	add hl,de 
	lea de,s_bank0 
	ld bc,3*4
	ldir
.goto: 
	ld iy,ppu_oam
	ld c,64
	exx 
	ld hl,vbuffer
	exx
	ld a,(s_topclip)
	add a,$20 - 7 
	ld (sprite_outer.smc_top),a
	ld (low_priority_sprite.smc_top),a
	jp render_sprites_loop

.big: 
	ld (s_size),16
	
	lea hl,chr_ptr_backup_0
	lea de,s_bank0 
	ld bc,3*8
	ldir
	
	jr .goto
	
render_background:
	; load drawtile function
	ld hl,drawtile_src
	ld de,$E10010 
	ld bc,drawtile_len 
	ldir
	
	; initialize caches 
	ld ix,jit_scanline_vars
	lea iy,chr_ptr_backup_0
	ld de,0
	bit 4,(ppu_ctrl_backup)		; do tiles start at $0000 or $1000? 
	jr z,$+4 
	ld e,3*4 
	add iy,de 
	
	ld a,0 
	ld de,(iy+0) 
	call cache_add_bank
	ld a,1 
	ld de,(iy+3) 
	call cache_add_bank
	ld a,2 
	ld de,(iy+6) 
	call cache_add_bank
	ld a,3 
	ld de,(iy+9) 
	call cache_add_bank
	
	ld ix,jit_scanline_vars
	; load mirroring 
	lea hl,nametable_backup
	lea de,t_nametable_0
	ld bc,3*4 
	ldir 
	
	ld (.smc_sp),sp 
	ld a,$D6
	ld mb,a 
	ld hl,render_event_list 
	ld i,hl 
	ld iy,$D41800
	
	ld a,(ppu_mask_backup) 
	ld (mask),a
	; compute initial scroll 
	ld a,(ppu_ctrl_backup) 
	ld (ctrl),a
	and a,11b 
	ld (nametable_select),a 
	; x 
	ld a,(ppu_x_backup)
	ld b,a 
	and a,111b 
	ld (x_fine),a 
	ld a,b 
	rra 
	rra
	rra 
	and a,11111b 
	inc a
	ld (x_course),a 
	ld (x_new),1
	; y 
	ld a,(ppu_y_backup)
	ld b,a 
	and a,111b 
	ld (y_fine),a 
	ld a,b 
	rra 
	rra
	rra 
	and a,11111b 
	ld (y_course),a 

.fetch:
	ld hl,i 	; fetch next event scanline
	ld a,(hl)
	cp a,231 	; off the bottom? 
	jr c,.split 
	ld (end_y),$FF 
	jr .scroll 
.split: 
	add a,$20-8
	ld (end_y),a	
.scroll: 
	ld a,(x_new)
	ld (x_new),0
	or a,a 
	jp z,render_background_loop
	; now compute derivitive x values
	ld a,32 
	sub a,(x_course) 
	ld (x_len1),a 
	sub a,31 
	neg 
	ld (x_len2),a 
.xfine:
	ld a,8 
	sub a,(x_fine) 
	ld (x_start),a  
	cp a,8 
	jr nz,.start 
	; don't draw rightmost tile if its entirely offscren 
	ld a,(x_len2) 
	or a,a 
	jr z,.noright 
	dec (x_len2) 
	jr .start 
.noright:
	dec (x_len1) 
.start: 
	jp render_background_loop 

.exit:
	; flush for final sprite call
	ld a,(mask)
	ld (ppu_mask_backup),a
	ld a,(ctrl) 
	ld (ppu_ctrl_backup),a 
	ld a,$D5 
	ld mb,a 
	ld.sis sp,(jit_event_stack_top + 241*2) and $FFFF
	ld sp,0 
.smc_sp:=$-3
	ret
	
.nextevent: 
	ld sp,temp_stack
	lea hl,chr_ptr_backup_0
	ld de,chr_ptr_update_buffer 
	ld bc,3*8 
	ldir 
	
	ld hl,i 
	ex de,hl 
	ld a,(de) 
	ld (end_y),a 
	ld (s_update),0
	ld (t_update),0
	; compute all changes on this scanline
.l2: 
	ld a,(de) 
	cp a,(end_y) 
	jr nz,.end 
	inc de 
	ld a,(de)
	inc de 
	ld hl,.functable
	ld b,3
	ld c,a 
	mlt bc 
	add hl,bc
	ld hl,(hl) 
	call .callhl 
	jr .l2
.end: 
	ex de,hl 
	ld i,hl

	bit 0,(s_update)
	jq z,.t_update 
	push iy
	call render_sprites
	pop iy
	ld hl,drawtile_src
	ld de,$E10010 
	ld bc,drawtile_len 
	ldir
	
.t_update:
	bit 0,(t_update) 
	jq z,.fetch
	; update tile caches
	push iy
	ld iy,chr_ptr_update_buffer
	ld de,0
	bit 4,(ctrl)		; do tiles start at $0000 or $1000? 
	jr z,$+4 
	ld e,3*4 
	add iy,de 
	
	ld a,0 
	ld de,(iy+0) 
	call cache_add_bank
	ld a,1 
	ld de,(iy+3) 
	call cache_add_bank
	ld a,2 
	ld de,(iy+6) 
	call cache_add_bank
	ld a,3 
	ld de,(iy+9) 
	call cache_add_bank
	
	pop iy
	lea de,chr_ptr_backup_0
	ld hl,chr_ptr_update_buffer
	ld bc,3*8 
	ldir 
	jq .fetch
	
.callhl: 
	jp (hl) 
.functable: 
	emit 3: .ppu_ctrl 
	emit 3:	.data_read 
	emit 3:	.x_scroll 
	emit 3:	.ppu_address 
	emit 3:	.ppu_mask 
	emit 3:	.chr_bank 
	emit 3:	.mirroring 
	
.mirroring:
	; copy nametable config 
	ex de,hl
	lea de,t_nametable_0
	ld bc,12
	ldir 
	ex de,hl 
	ret 
.ppu_ctrl: 
	ld a,(ctrl) 
	ld (ppu_ctrl_backup),a 
	ld a,(de) 
	inc de 
	ld (ctrl),a 
	; update bottom bit of nametable select 
	and a,1 
	ld b,a 
	ld a,(nametable_select) 
	and a,10b 
	or a,b 
	ld (nametable_select),a
	; update pattern table ptrs
	ld (t_update),1
	; did the sprite size or sprite pattern address change?
	ld a,(ppu_ctrl_backup)
	xor a,(ctrl)
	and a,00101000b
	jr z,.ppu_ctrl.nosprite
	ld (s_update),1
	ld a,(end_y)
	dec a
	ld (s_botclip),a 
.ppu_ctrl.nosprite:
	ret 
.data_read:
	; TODO: 
	ret 
.x_scroll: 
	ld a,(de) 
	inc de 
	ld b,a 
	and a,111b 
	ld (x_fine),a 
	ld a,b 
	rra 
	rra
	rra 
	and a,11111b 
	inc a
	ld (x_course),a
	ld (x_new),1
	ret 
.ppu_mask:
	ld a,(mask) 
	ld (ppu_mask_backup),a 
	ld a,(de) 
	inc de 
	ld (mask),a
	xor a,(ppu_mask_backup) 
	; sprite rendering changed? 
	bit 4,a 
	jr z,.ppu_mask.skip
	ld (s_update),1
	ld a,(end_y) 
	dec a
	ld (s_botclip),a 	
.ppu_mask.skip:
	ret 
.ppu_address:
	; split address into scroll components
	ld a,(de) 
	and a,11b 
	ld (nametable_select),a 
	inc de 
	ld a,(de) 
	ld b,a 
	and a,111b 
	ld (y_fine),a 
	ld a,b 
	rra 
	rra 
	rra 
	and a,11111b 
	ld (y_course),a 
	inc de
	ld a,(de)
	ld b,a 
	and a,111b 
	ld (x_fine),a 
	ld a,b 
	rra 
	rra
	rra 
	and a,11111b 
	inc a
	ld (x_course),a 
	ld (x_new),1
	inc de
	ret 
.chr_bank:
	ld a,(de)
	inc de 
	; find bank to swap in
	or a,a 
	sbc hl,hl 
	ex de,hl 
	ld e,(hl)
	inc hl 
	ld d,(hl)
	inc hl 
	push hl 
	;*3
	push de 
	pop hl
	add hl,hl 
	add hl,de 
	ld de,_chr_banks
	add hl,de 
	ld hl,(hl)
	ex de,hl 
	; find chr ptr to update
	ld c,a 
	ld b,3 
	mlt bc 
	ld hl,chr_ptr_update_buffer
	add hl,bc 
	ld (hl),de 
	pop de
	; update tile caches and sprites
	ld (t_update),1
	ld (s_update),1
	ld a,(end_y) 
	dec a
	ld (s_botclip),a 
	ret 

chr_ptr_update_buffer: 
	rb 24
	
virtual at $E30800 

render_background_loop:
	ld bc,0 
	exx 
.drawloop: 
	; compute how many lines to draw 
	ld a,8 
	sub a,(y_fine) 
	ld b,a 
	add a,iyh
	jr c,.bottom_clip
	cp a,(end_y)
	jr c,.noclip
.clip: 
	sub a,b
	sub a,(end_y)
	neg
	jp z,render_background.nextevent 
	ld b,a 
	jr .noclip 
.bottom_clip:
	; clip to bottom of screen
	ld a,iyh  
	neg
	ld b,a
.noclip:
	ld (y_len),b 
	ld a,iyh 
	cp a,$20 	
	jq nc,.draw 
	add a,b 
	cp a,$20+1	; is the end of this tile on screen? 
	jq c,.nodraw
	; clip to top of screen
	sub a,$20
	ld (y_len),a
	ld b,a
	ld a,$20 
	sub a,iyh  
	add a,(y_fine) 
	ld (y_fine),a 
	ld iyh,$20
.draw: 
	; is rendering on? 
	bit 3,(mask) 
	jr nz,.noblank
.blank: 
	; fill lines with background color  
	ld a,b 
	add a,iyh 
	jr nc,$+3
	xor a,a 
	sub a,iyh 
	ld bc,0 
	ld b,a 
	dec bc
	dec bc
	lea de,iy+0 
	ld e,1 
	lea hl,iy+0
	ld l,0 
	ld (hl),0
	ldir 
	jq .nodraw
.noblank:
	; compute offset into unrolled draw loop
	ld a,8 
	sub a,b 
	and a,11b 
	ld b,a
	ld c,6 
	mlt bc
	ld a,3 	; skip initial `exx \ ld e,iyl`
	add a,c 
	ld (draw_tile.smc_offset),a
	
	; compute y offset 
	ld hl,render_cache 
	ld e,(y_fine)
	ld d,8 
	mlt de 
	add hl,de
	ld sp,hl
	exx 
	; set to start of line
	lea de,iy+0
	ld e,(x_start) 
	exx 
	ld a,(x_len1) 
	or a,a 
	jr z,.right
	; find left nametable
	lea hl,t_nametable_0 
	ld b,(nametable_select) 
	ld c,3 
	mlt bc 
	add hl,bc 
	ld hl,(hl) 
	; + 2*(x_course+1) 
	ld b,(x_course)
	ld c,2
	mlt bc 
	add hl,bc 
	; + 64*y_course
	ld e,64 
	ld d,(y_course) 
	mlt de 
	add hl,de 
	ld.sis sp,hl 
	
	; loop if length > 4 
	ld b,1 
	ld a,(y_len) 
	cp a,5 
	jr c,$+3
	inc b 
	ld e,b
	ld c,(x_len1) 
	ld hl,.right 
	ld a,8
	jp draw_tile
.right: 
	; only draw right nametable if xlen2 > 0
	ld a,(x_len2) 
	or a,a 
	jr z,.nodraw 
	lea hl,t_nametable_0 
	ld a,(nametable_select)
	xor a,1 		; toggle nametable 
	ld c,a
	ld b,3 
	mlt bc 
	add hl,bc 
	ld hl,(hl)
	ld e,64
	ld d,(y_course) 
	mlt de
	add hl,de 		; de = 64*y_course
	ld.sis sp,hl
	
	ld b,1 
	ld a,(y_len) 
	cp a,5 
	jr c,$+3
	inc b 
	ld e,b
	ld c,(x_len2) 
	ld hl,.nodraw 
	ld a,8 
	jp draw_tile
.nodraw: 
	; compute y scroll for next line 
	ld a,(y_fine) 
	add a,(y_len) 
	ld (y_fine),a 
	cp a,8 
	jr nz,.noincrement 
.increment: 
	ld (y_fine),0 
	ld a,(y_course) 
	inc a 
	cp a,30 		; 30 resets to 0, but 31 doesnt 
	jr nz,$+4 
	inc a 
	inc a 
	cp a,32
	jr nz,.nonameswap
.nameswap:  
	ld a,(nametable_select) 
	xor a,10b 		; swap selected y nametable 
	ld (nametable_select),a 
	xor a,a 
.nonameswap: 
	ld (y_course),a 
.noincrement: 
	ld a,iyh 
	add a,(y_len) 
	jp c,render_background.exit
	ld iyh,a 
	jq .drawloop

	
fetch_tile: 
.translate_tile:
	ld (.smc_sp),sp 
	ld sp,temp_stack
	push iy
	dec.sis sp
	dec.sis sp
	pop.sis hl 
	push hl
	ld a,l 
	; find chr data 
	ld h,4 		; high 2 bits for bank
	mlt hl 
	ld c,h
	ld l,6
	mlt hl 
	lea de,t_bank0 
	add hl,de 
	ld iy,(hl)
	and a,111111b ; low 6 bits for offset 
	ld d,a 
	ld e,16 
	mlt de 
	add iy,de	; iy = chr data ptr 
	
	ld de,(render_tile_next) 
	ld hl,render_cache_end - render_cache 
	or a,a 
	sbc hl,de
	jr nz,.valid 
	call flush_bank_cache
	ld de,0 
.valid:
	ld hl,64 
	add hl,de 
	ld (render_tile_next),hl 
	
	pop hl
	ld a,(render_palettes shr 8) and $FF
	add a,h 
	ld ix,render_palettes
	ld ixh,a 
	add hl,hl 	; set tile ptr
	ld.sis (hl),e 
	inc hl 
	ld.sis (hl),d 
	ld hl,render_cache
	add hl,de
	ex de,hl	; de = cache ptr
	push de
	ld ixl,8
	ld bc,0
.loop: 
	; find debruijin entries for each set of 4 pixels
	ld hl,interleave_lut
	ld l,(iy+0) 
	ld c,(hl) 
	inc h 
	ld b,(hl) 
	ld l,(iy+8) 
	ld a,(hl)
	dec h 
	ld l,(hl)
	ld h,a 
	add hl,hl
	add hl,bc
	ld b,h
	ld c,l 
	ld hl,debrujin_mapping
	ld l,b
	ld b,(hl)
	ld l,c 
	ld a,(hl) 
	; copy from entries to cache
	lea hl,ix
	ld l,b 
	ld bc,4 
	ldir 
	lea hl,ix
	ld l,a 
	ld c,4 
	ldir 
	inc iy
	dec ixl
	jr nz,.loop 
	
	pop hl
	pop iy 
	ld ix,jit_scanline_vars
	ld sp,0
.smc_sp := $-3
	ld bc,0 
	lea de,iy+0
	ld a,8
	jp draw_tile.return

	
render_sprites_loop:
	call .loop 
	lea iy,iy+4
	ld a,iyl 
	or a,a
	jr nz,render_sprites_loop
	ld ix,jit_scanline_vars
	ld a,(s_botclip) 
	ld (s_topclip),a
	ld (s_botclip),232-1
	ret
.loop: 
	ld ix,jit_scanline_vars
	
	; x clipping
	ld a,(iy+3) 
	or a,a 
	ret z
	cp a,256-8
	ret nc
	
	ld b,(s_size)	; initial y length
	;y clipping
	; is y >= bottom y ? 
	ld e,(iy+0)
	ld a,(s_botclip) 
	sub a,e 
	ret c 
	ret z
.top: 
	ld a,e
	; y < top y ? 
	cp a,(s_topclip)
	jr nc,.tile 
.top_clip:
	sub a,(s_topclip)	; find how many lines are offscreen
	neg 
	cp a,b				; if >= sprite size,skip 
	ret nc 
.tile: 
	ld a,(iy+0)
	exx 
	add a,$20 - 8	; store y offset. Apparently sprites cant be displayed on line 0.
	ld h,a
	exx 
	; find tile bank
	ld a,(iy+1)
	bit 4,(s_size)	
	jr nz,.big
.small:
	ld h,4 
	ld l,a 
	jr .shared
.big: 
	rrca	; bit 0 selects whether the tile is in $0000 or $1000 
	ld l,a 
	ld h,8 
	; h = bank
.shared: 
	mlt hl 	; h = bank , l = shifted tile#
	ld a,l 
	ld l,3 
	mlt hl 
	lea de,s_bank0
	add hl,de 
	ld de,(hl) 
	; add tile offset 
	sbc hl,hl 
	ld l,a 
	add hl,hl 
	add hl,hl 
	add hl,de 
	push hl
.flip:
	ld a,(iy+2)
	exx
	ld d,$24	; inc h
	bit 7,a 	; vertical flip? 
	jr z,.noflip 
	
	inc d 		; dec h 
	; add sprite size - 1 to starting y
	exx 
	ld a,b
	dec a
	exx 
	add a,h 
	ld h,a 
	ld a,(iy+2) 
	
.noflip: 
	ld l,(iy+3)
	bit 6,a		; horizontal flip?
	ld e,$2C	; inc l
	jr z,.noflip2

	inc e		; dec l
	; add 7 to x start
	ld a,l 
	add a,7 
	ld l,a 
	ld a,(iy+2)
.noflip2: 
	exx 
	pop ix		; ix = tile data
	bit 5,a 
	jq nz,low_priority_sprite
	
; a = sprite flags 
; ix = sprite data
; iy = oam data 
; d' = y op
; e' = x op
; hl' = screen ptr
high_priority_sprite:
	; big sprites? 
	ld c,1 
	bit 4,b 
	jr z,$+3
	inc c 
	exx 
	; c = palette
	and a,11b
	;*4 
	add a,a
	add a,a
	; sprite colors all at $8x
	add a,$80
	ld c,a 
	; load smc data
	ld a,l 
	ld (sprite_outer.smc_x_start),a 
	ld a,e 
	ld (sprite_outer.smc_x_dir),a
	ld (sprite_outer.smc_x_dir2),a
	ld a,d 
	ld (sprite_outer.smc_y_dir),a
	; initialize palette
	exx 
	call sprite_outer
	lea ix,ix+8
	dec c
	call nz,sprite_outer
	ret 

	
low_priority_sprite:
	; big sprites? 
	ld c,1 
	bit 4,b 
	jr z,$+3
	inc c 
	exx 
	; c = palette
	and a,11b
	;*4 
	add a,a
	add a,a
	; sprite colors all at $8x
	add a,$80
	ld c,a 
	; load smc data
	ld a,l 
	ld (.smc_x_start),a 
	ld a,e 
	ld (.smc_x_dir),a
	ld (.smc_x_dir2),a
	ld a,d 
	ld (.smc_y_dir),a
	; initialize palette
	exx
	ld b,8
.outer: 
	exx 
	ld a,h 
	cp a,0 
.smc_top:=$-1	
	jr c,.next 
	cp a,$20 
	jr c,.next
	ld d,(ix+8) 
	ld e,(ix) 
	ld b,4 
	ld l,0 
.smc_x_start:=$-1
.loop: 
	xor a,a 
	rl d
	adc a,a  
	rl e
	adc a,a 
	jr z,.skip
	bit 7,(hl) 
	jr nz,.skip
	bit 4,(hl) 
	jr z,.zero
	set 7,(hl)
	jr .skip
.zero:
	add a,c 
	ld (hl),a 
.skip:
.smc_x_dir: 
	inc l 
	
	xor a,a 
	rl d
	adc a,a  
	rl e 
	adc a,a 
	jr z,.skip2
	bit 7,(hl) 
	jr nz,.skip2
	bit 4,(hl) 
	jr z,.zero2
	set 7,(hl)
	jr .skip
.zero2:
	add a,c
	ld (hl),a 
.skip2:
.smc_x_dir2: 
	inc l 
	
	djnz .loop 
.next: 
	inc ix
.smc_y_dir:
	inc h
	exx 
	djnz .outer
	lea ix,ix+8
	ld b,8
	dec c
	jr nz,.outer
	ret 
	

assert $$ < $E30BC0
load render_data:$-$$ from $$ 
render_len := $-$$

end virtual 

render_src: 
	db render_data
	
	
virtual at $E10010 

sprite_outer:
	ld b,8 
.outer: 
	exx 
	ld a,h 
	cp a,0 
.smc_top:=$-1	
	jr c,.next
	cp a,$20
	jr c,.next
	ld d,(ix+8) 
	ld e,(ix) 
	ld b,4 
	ld l,0 
.smc_x_start:=$-1
.loop: 
	xor a,a 
	rl d
	adc a,a  
	rl e 
	adc a,a
	jr z,.skip 
	bit 7,(hl) 
	jr nz,.skip 
	add a,c 
	ld (hl),a 
.skip: 
.smc_x_dir:
	inc l 
	
	xor a,a 
	rl d
	adc a,a  
	rl e 
	adc a,a
	jr z,.skip2
	bit 7,(hl) 
	jr nz,.skip2
	add a,c 
	ld (hl),a 
.skip2:
.smc_x_dir2: 
	inc l 
	djnz .loop 
.next: 
	inc ix
.smc_y_dir:
	inc h
	exx 
	djnz .outer
	ret 
	
assert $-$$ <= 64
load spr_data:$-$$ from $$ 
spr_len := $-$$
end virtual

spr_src: 
	db spr_data
	
	
virtual at $E10010 
	; 8 bpp tile drawing 
draw_tile:
.outer:
	exx 
	ld d,iyh 	; load y start 
	ld iyl,e	; x start += 8
	pop.sis	hl 	; load tile ptr
	add hl,hl
	ld.sis hl,(hl)
	bit 0,l 
	jr nz,.translate_tile 
	add hl,sp
.return:
	jr $
.smc_offset:= $-1
.loop:
	exx
repeat 4
	ld e,iyl 
	ld c,a 
	ldir 
	inc d
end repeat 
	exx 
	djnz .loop
	ld b,e
	dec c 
	jr nz,.outer
	jp (hl) 	; 622 cc / tile
.translate_tile: 
	jp fetch_tile
	
assert $-$$ <= 64
load drawtile_data:$-$$ from $$ 
drawtile_len := $-$$
end virtual

drawtile_src: 
	db drawtile_data	

	
section .bss 

public lcd_timing_backup

public render_cache
public render_cache_end
public render_banks
public render_banks_len
public render_banks_list
public render_tile_next

lcd_timing_backup: rb 8

render_banks_len_max := 16

render_cache: rb 52*1024

render_cache_end:
render_banks: rb render_banks_len_max*512

render_banks_len: rb 1
render_banks_list: rb 3*render_banks_len_max
render_tile_next: rb 3

section .data

public nes_palettes 
public nes_grayscale_palette
public lcdTiming

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

; Timing = (HSW+1+HFP+1+HBP+1+16*(PPL+1)) * (LPP+1+VSW+1+VFP+VBP) * (PCD+2) * 2
;Timing = (1+1+1+(63+1)*16) * (55+1+1+138)*2*2 = 801060 cycles or 59.92 frames/second
; approx 234000 cycles spent sending 
lcdTiming: 
	db	63 shl 2 	; PPL 
	db	0 			; HSW
	db	0 			; HFP 
	db	0 			; HBP 
	dw	55 			; LPP & VSW(0) 
	db	138			; VFP. =138 for 60 fps, =176 for 50 fps 
	db	0 			; VBP
	db 	0 			; 

extern spiSetup
extern spiInitVSync
extern spiEndVSync
extern spiLock 
extern spiUnlock

extern ppu_chr_ptr
extern ppu_nametable_ptr
extern attribute_update
extern _chr_banks

extern __idvrmu
