include 'ti84pceg.inc' 

section .text

public io_init 

public io_get_read_function
public io_get_read_function.io_lut
public io_get_read_function.ppu_lut
public io_get_write_function
public io_get_write_function.io_lut
public io_get_write_function.ppu_lut

public io_read_status
public io_read_joy1
public io_read_joy2

public write_dmc_freq
public write_dmc_start
public write_dmc_length
public write_oam_dma
public write_apu_enable
public write_joy_strobe
public write_apu_frame

public read_ppu_io_bus
public read_ppu_status
public read_oam_data
public read_ppu_data

public write_ppu_control
public write_ppu_mask
public write_ppu_io_bus
public write_oam_address
public write_oam_data
public write_ppu_scroll
public write_ppu_address
public write_ppu_data

public ppu_video_start
public ppu_video_end

public io_frame_irq 

public set_mirroring
public attribute_update

include 'vars.inc'

;TODO:
io_init: 
	; blank area
	ld ix,jit_scanline_vars	
	lea hl,ix+0 
	lea de,ix+1
	ld (hl),0 
	ld bc,127 
	ldir 
	ld hl,jit_event_stack_top+2*264		; set to dummy line thats never reached
	ld (frame_counter_irq_line),hl  
	ld (ppu_address_increment),1 
	
	ld hl,render_event_list
	ld (ppu_event_list),hl 
	
	ld (frameskip),60		; 1 second of setup
	ret 

set_mirroring:
	bit 0,a 
	jr nz,.horizontal
.vertical: 
	ld hl,ppu_nametables
	ld (ppu_nametable_ptr),hl
	ld (ppu_nametable_ptr+3),hl
	ld hl,ppu_nametables+2048
	ld (ppu_nametable_ptr+6),hl
	ld (ppu_nametable_ptr+9),hl
	ret 
.horizontal: 
	ld hl,ppu_nametables
	ld (ppu_nametable_ptr),hl
	ld (ppu_nametable_ptr+6),hl
	ld hl,ppu_nametables+2048
	ld (ppu_nametable_ptr+3),hl
	ld (ppu_nametable_ptr+9),hl
	ret 
	
	
ppu_video_start:
	ld ix,jit_scanline_vars
	; 
	res 6,(ppu_status)		; clear sprite zero flag
	res 7,(ppu_status)		; clear vblank flag, if not already
	res 0,(in_vblank)
	inc (current_frame)
	ld hl,jit_event_stack_top
.smc_spz_line:= $-3 
	res scan_event_sprite_zero,(hl) 
	; handle keys 
	push af
	push bc
	ld a,(ti.mpKeyData+2) 	; return if DEL down 
	and a,$80 
	jp nz,_startJIT.return
	call get_keys
	ld (joypad1_input),a
	
	; compute sprite zero line
	ld de,ppu_oam
	ld a,(de) 
	cp a,239 
	jq nc,.skip 	; offscreen 
	inc de 
	; get pointer to sprite data
	ld a,(de)
	dec de
	; find bank 	
	bit 5,(ppu_ctrl) ; 8x8 or 8x16 ? 
	jr z,.x8
.x16:
	rrca 
	ld l,a 
	ld h,8 
	mlt hl 	
	jr .l1 
.x8:
	ld h,4
	ld l,a 
	mlt hl 
	bit 3,(ppu_ctrl) ; sprite data at $0000 or $1000?
	jr z,.l1
	set 2,h			; +4 
.l1: 
	ld l,3 
	mlt hl 
	ld bc,ppu_chr_ptr 
	add hl,bc 
	ld hl,(hl)
	; add offset
	and a,111111b   
	ld b,16 
	bit 5,(ppu_ctrl) ; 8x16 means 32 byte sprites  
	jr z,.l2 
	and a,11111b
	ld b,32 
.l2: 
	ld c,a
	ld a,b 
	mlt bc 
	add hl,bc 
	push hl 
	pop iy
	; find first opaque line 
	srl a 
	ld b,a 
	ld c,a
.l3: 
	ld a,(iy) 
	inc hl 
	or a,(iy+8)
	jr nz,.l4
	inc iy 
	djnz .l3 
.l4:
	ld iy,jit_nes_iwram+$80
	ld a,b 
	or a,a 	; if a=0 then there is no hit 
	jr z,.skip 
	ld a,c 
	sub a,b 
	ex de,hl 
	add a,(hl)	
	cp a,239 	; offscreen? 
	jr nc,.skip 
	inc a		; rendered at +1
	ld l,a 
	ld h,2 
	mlt hl 
	ld de,jit_event_stack_top 
	add hl,de 
	set scan_event_sprite_zero,(hl)
	ld (.smc_spz_line),hl
.skip: 
	
	; store initial ppu config 
	ld a,(ppu_ctrl) 
	ld (ppu_ctrl_backup),a
	ld a,(ppu_mask) 
	ld (ppu_mask_backup),a
	ld a,(ppu_x_scroll) 
	ld (ppu_x_backup),a
	ld a,(ppu_y_scroll) 
	ld (ppu_y_backup),a
	
	ld hl,ppu_chr_ptr
	lea de,chr_ptr_backup_0
	ld bc,3*8 
	ldir 
	
	ld hl,ppu_nametable_ptr
	lea de,nametable_backup 
	ld bc,3*4 
	ldir 

	;reset event list 
	ld hl,render_event_list
	ld (ppu_event_list),hl 
	; enable rendering if on a render frame
	ld a,(frameskip)
	cp a,(current_frame) 
	jr nz,.norender
	ld (current_frame),0
	ld a,$80 
.norender: 
	ld r,a 
	pop bc 
	pop af 
	ret 
	
ppu_video_end:
	ld ix,jit_scanline_vars
	set 7,(ppu_status)		; set vblank flag
	set 0,(in_vblank)
	push af 
	push bc 
	push hl 
	exx 
	push hl 
	push bc 
	ld hl,i 
	push hl 
	ld a,r 	; is rendering enabled? 
	rla 
	jr nc,.norender 
	; mark end of event list
	ld hl,(ppu_event_list) 
	; reset list if trailed passed the end 
	ld de,render_event_list_end
	or a,a 
	sbc hl,de
	add hl,de
	jr c,$+6 
	ld hl,render_event_list 
	ld (hl),240
	call render_draw 
	call load_jit_search ; reset SHA area
.norender: 
	; clear any pending VCOMP interrupts 
	ld hl,ti.mpLcdIcr
	set 3,(hl)
	; disable rendering 
	xor a,a 
	ld r,a 
	pop hl 
	ld i,hl
	pop bc 
	pop hl 
	exx 
	pop hl 
	pop bc 
	pop af
	ld iy,jit_nes_iwram+$80
	ld ix,jit_scanline_vars
	bit 7,(ppu_ctrl)
	ret


; a = NES key result
get_keys:
	ld hl,ti.mpKeyData
	ld de,key_list 
	ld b,8 
	ld c,0 
.loop: 
	ld a,(de) 	; read row 
	inc de
	add a,a 
	add a,$10 
	ld l,a 
	ld a,(de) 	; read column 
	inc de 
	and a,(hl)
	jr z,.off 	; shift in bit
.on: 	
	scf 
.off:
	rl c 
	djnz .loop 	
	ld a,c 
	ld de,0
	ret

key_list: 
	db 1,1 shl 5 		; A
	db 2,1 shl 7 		; B
	db 3,1 shl 7 		; Select
	db 1,1 shl 6 		; Start
	db 7,1 shl 3 		; Up
	db 7,1 shl 0 		; Down
	db 7,1 shl 1 		; Left
	db 7,1 shl 2 		; Right
	
	
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

io_frame_irq: 
	ld ix,jit_scanline_vars
	set 6,(apu_status) 	
	ret 

; hl = address
; returns: hl = function, a = inline(true/false)
io_get_read_function: 
	ld a,h 
	cp a,$40 
	jr c,.ppu 
	ld a,l
	cp a,$15 ; <$4015 all open bus
	jr c,.openbus
	cp a,$18
	jr nc,.openbus 
	sub a,$15 
	ld c,a 
	ld b,3 
	mlt bc
	ld hl,.io_lut 
	add hl,bc
	ld hl,(hl)
	xor a,a
	ret 
.ppu: 
	ld a,l 
	and a,111b 
	ld c,a  
	ld b,3 
	mlt bc
	ld hl,.ppu_lut
	add hl,bc
	ld hl,(hl)
	xor a,a 
	ret
.openbus: 
	ld hl,io_open_bus 
	xor a,a
	ret 
	
.io_lut: 
	emit 3: io_read_status,io_read_joy1,io_read_joy2
.ppu_lut: 
	emit 3: read_ppu_io_bus, read_ppu_io_bus, read_ppu_status, read_ppu_io_bus
	emit 3: read_oam_data, read_ppu_io_bus, read_ppu_io_bus, read_ppu_data
	
	
io_get_write_function:
	ld a,h 
	cp a,$40 
	jr c,.ppu 
	ld a,l
	cp a,$14 ; <$ignore writes before $4014
	jr c,.openbus
	cp a,$18 ; and >$4017
	jr nc,.openbus 
	sub a,$14
	ld c,a 
	ld b,3 
	mlt bc 
	ld hl,.io_lut 
	add hl,bc
	ld hl,(hl)
	xor a,a 
	ret
	
.ppu: 
	ld a,l 
	and a,111b 
	ld c,a  
	ld b,3 
	mlt bc
	ld hl,.ppu_lut
	add hl,bc
	ld hl,(hl)
	xor a,a 
	ret
.openbus: 
	ld a,1 
	ret 

.io_lut: 
	;emit 3: write_dmc_freq , 0, write_dmc_start, write_dmc_length	
	emit 3: write_oam_dma, write_apu_enable, write_joy_strobe, write_apu_frame

.ppu_lut: 
	emit 3: write_ppu_control, write_ppu_mask, write_ppu_io_bus, write_oam_address
	emit 3: write_oam_data, write_ppu_scroll, write_ppu_address, write_ppu_data
	

io_open_bus: 
	ld e,h
	ret 
	
io_read_status:
	ld ix,jit_scanline_vars
	ld e,(apu_status) 
	res 6,(apu_status) 	; reading resets APU frame interrupt
	ret 
	
io_read_joy1:
	ld ix,jit_scanline_vars
	ld l,a 
	ld a,(joypad1_shift) 
	scf 	; shift 1 into register
	rla 
	ld (joypad1_shift),a 
	rla 
	and a,1
	or a,$40 	; open bus on top bits
	ld e,a 
	ld a,l
	ret 

io_read_joy2:
	ld ix,jit_scanline_vars
	ld l,a 
	ld a,(joypad2_shift) 
	scf 	; shift 1 into register
	rla 
	ld (joypad2_shift),a 
	rla 
	and a,1
	or a,$40 	; open bus on top bits
	ld e,a 
	ld a,l
	ret 

; reloads shift registers
write_joy_strobe:
	push af
	ld ix,jit_scanline_vars
	ld a,(joypad1_input) 
	ld (joypad1_shift),a 
	ld a,(joypad2_input) 
	ld (joypad2_shift),a 
	pop af
	ret 
	
; TODO: DMC irq support
write_dmc_freq:
write_dmc_start:
write_dmc_length:
write_apu_enable:
	ret 
	
; if top two bits are both reset, enables irq next frame
write_apu_frame:
	ld ix,jit_scanline_vars
	bit 7,e 
	ret nz 
	bit 6,e 
	ret nz 
	; reschedule previous irq
	ld hl,(frame_counter_irq_line)
	res.sis scan_event_apu_irq,(hl)
	; set flag for previous line(on next frame)
	or a,a
	sbc hl,hl 
	add.sis hl,sp 
	dec hl
	dec hl
	set.sis scan_event_apu_irq,(hl)  
	ld (frame_counter_irq_line),hl 
	ret 
	
read_ppu_io_bus:
	ld e,0 
	ret 
	
read_ppu_status:
	ld ix,jit_scanline_vars
	ld (ppu_write_latch),0
	ld e,(ppu_status) 
	res 7,(ppu_status) 	; free to clear bit if past preline
	ret 

; copies from specified page to oam
write_oam_dma:
	push bc 
	push de 
	ld d,3 		; fetch page address
	mlt de
	ld hl,jit_translation_buffer
	add hl,de
	ld hl,(hl) 
	ld de,-128 
	add hl,de 
	ld de,ppu_oam
	ld bc,256 
	ldir 
	pop de 
	pop bc 
	; discard next 4 scanlines 
	; TODO: carry over events that occur in this time frame, also add more granularity (512 cycles exactly)
	pop.sis hl
	pop.sis hl
	pop.sis hl
	pop.sis hl
	ex af,af' 
	sub a,512 - scanline_cycle_count*4
	jr nc,$+3
	xor a,a 
	ex af,af' 
	ret 
	
	
read_oam_data:
	ld ix,jit_scanline_vars
	ld hl,ppu_oam 
	ld l,(oam_address) 
	ld e,a 
	ld a,(oam_address) 
	and a,11100011b 	; mask out unimplemented bits
	ld l,a 
	ld a,e 
	ld e,l
	ret

write_oam_address:
	ld ix,jit_scanline_vars
	ld (oam_address),e 
	ret 
	
write_oam_data:
	ld ix,jit_scanline_vars
	ld hl,ppu_oam 
	ld l,(oam_address) 
	ld (hl),e
	inc (oam_address)
	ret

write_ppu_control:
	ld ix,jit_scanline_vars
	ld (ppu_ctrl),e
	ld l,1 
	bit 2,e		; get address increment amount 
	jr z,$+4 
	ld l,32
	ld (ppu_address_increment),l 
.skip: 
	ld l,a 
	ld a,r 
	ld a,l
	ret p 
.active_render:
	ld hl,(ppu_event_list) 
	pop.sis de 
	push.sis de 
	ld (hl),d
	inc hl 
	ld (hl),ppu_event_ctrl 
	inc hl 
	ld e,(ppu_ctrl)
	ld (hl),e 
	inc hl
	ld (ppu_event_list),hl 
	ld d,0
	ret 
	
write_ppu_mask:
	ld ix,jit_scanline_vars
	ld (ppu_mask),e
	ld e,a 
	ld a,r 
	ld a,e 
	ret p
	ld hl,(ppu_event_list) 
	pop.sis de 
	push.sis de 
	ld (hl),d
	inc hl 
	ld (hl),ppu_event_mask 
	inc hl 
	ld e,(ppu_mask)
	ld (hl),e 
	inc hl
	ld (ppu_event_list),hl 
	ld d,0
	ret
	
write_ppu_io_bus: 
	ret 
	
write_ppu_scroll:
	ld ix,jit_scanline_vars
	bit 0,(ppu_write_latch) 
	jq nz,yscroll 
xscroll: 
	ld (ppu_write_latch),1
	ld l,a 
	ld (ppu_x_scroll),e 
	ld a,r 
	ld a,l
	ret p 
.x_scroll_event: 
	ld hl,(ppu_event_list) 
	pop.sis de 
	push.sis de 
	ld (hl),d
	inc hl 
	ld (hl),ppu_event_scroll_x 
	inc hl 
	ld e,(ppu_x_scroll) 
	ld (hl),e 
	inc hl
	ld (ppu_event_list),hl 
	ld d,0 
	ret 
	
yscroll: 
	; writes to y scroll don't effect rendering unless T is flushed
	ld (ppu_write_latch),0
	ld (ppu_y_scroll),e 
	ld d,0
	ret 

write_ppu_address:
	ld ix,jit_scanline_vars
	bit 0,(ppu_write_latch) 
	jq nz,.write2
.write1: 
	ld (ppu_address_new_high),e 
	ld (ppu_write_latch),1 
	; writes to address effect scroll
	ld l,a 
	; nametable select  
	ld a,(ppu_ctrl) 
	and a,11111100b 
	ld h,a 
	ld a,e 
	rra 
	rra 
	and a,11b 
	or a,h 
	ld (ppu_ctrl),a 
	; y fine and top 2 of y course 
	ld a,e 
	rla
	rla
	rla
	rla
	and a,011b 	; topmost fine bit is cleared
	ld h,a 
	ld a,(ppu_y_scroll) 
	and a,00111000b 
	or a,h 
	ld h,a 
	ld a,e 
	rra 
	rra 
	rra
	and a,11000000b 
	or a,h 
	ld (ppu_y_scroll),a 
	ld a,l 
	ret 
.write2:
	ld l,a 	
	ld (ppu_address),e
	ld a,(ppu_address_new_high)
	and a,$3F	; mask out top bits
	ld (ppu_address+1),a 
	ld (ppu_write_latch),0
	; x course 
	ld a,(ppu_x_scroll) 
	and a,111b 
	ld h,a 
	ld a,e 
	rla 
	rla
	rla 
	and a,11111000b 
	or a,h 
	ld (ppu_x_scroll),a 
	; low 3 bits of y course 
	ld a,(ppu_y_scroll) 
	and a,11000111b 
	ld h,a 
	ld a,e 
	rra 
	rra 
	and a,00111000b 
	or a,h 
	ld (ppu_y_scroll),a 
	; during render?
	ld a,r 
	ld a,l 
	ret p
	ld hl,(ppu_event_list) 
	pop.sis de 
	push.sis de 
	ld (hl),d 
	inc hl 
	ld (hl),ppu_event_address
	inc hl 
	; write new x,y,and nametable select
	ld e,(ppu_ctrl) 
	ld (hl),e 
	inc hl 
	ld e,(ppu_y_scroll) 
	ld (hl),e 
	inc hl 
	ld e,(ppu_x_scroll) 
	ld (hl),e 
	inc hl 
	ld (ppu_event_list),hl
	ld d,0 
	ret 
	
; TODO: this could be faster 
read_ppu_data: 
	ld ix,jit_scanline_vars
	push af
	ld a,r 
	rla 
	jr c,.ppu_read_during_render 
.get_byte:
	ld hl,ppu_read_lut 
	ld a,(ppu_address+1)
	and a,$3F 
	ld (ppu_address+1),a
	ld l,a
	ld l,(hl) 
	jp (hl) 
; reading from PPUDATA during rendering causes increment to Y scroll and X course scroll
.ppu_read_during_render:
	ld hl,(ppu_event_list) 
	pop.sis de 
	push.sis de
	ld (hl),d 
	inc hl 
	ld (hl),ppu_event_read
	inc hl 
	ld (ppu_event_list),hl 
	ld d,0
	jr .get_byte

write_ppu_data: 
	ld ix,jit_scanline_vars
	push af
	ld hl,ppu_write_lut 
	ld a,(ppu_address+1) 
	and a,$3F 
	ld (ppu_address+1),a
	ld l,a
	ld l,(hl) 
	jp (hl) 
	
; align to page boundary 
rb $100 - ($ and $FF) 

ppu_read_lut: 
	db 4 dup (read_chr_0 and $FF)
	db 4 dup (read_chr_1 and $FF)
	db 4 dup (read_chr_2 and $FF)
	db 4 dup (read_chr_3 and $FF)
	db 4 dup (read_chr_4 and $FF)
	db 4 dup (read_chr_5 and $FF)
	db 4 dup (read_chr_6 and $FF)
	db 4 dup (read_chr_7 and $FF)
	db 4 dup (read_nametable_0 and $FF)
	db 4 dup (read_nametable_1 and $FF)
	db 4 dup (read_nametable_2 and $FF)
	db 4 dup (read_nametable_3 and $FF)
	db 4 dup (read_nametable_0 and $FF)
	db 4 dup (read_nametable_1 and $FF)
	db 4 dup (read_nametable_2 and $FF)
	db 3 dup (read_nametable_3 and $FF)
	db read_palette and $FF
	
	
read_palette: 
	ld hl,ppu_palettes
	ld de,(ppu_address) 
	ld a,e
	and a,11111b	; mirroring
	tst a,0011b	; set to zero if multiple of 4 
	jr nz,.skip 
	xor a,a 
.skip:
	ld l,a
	jr read_generic.skip
	
; need 4 since the mirroring is variable
read_nametable_0: 
	ld hl,(ppu_nametable_ptr) 
	jr read_nametable_generic 
read_nametable_1: 
	ld hl,(ppu_nametable_ptr+3) 
	jr read_nametable_generic 
read_nametable_2: 
	ld hl,(ppu_nametable_ptr+3*2) 
	jr read_nametable_generic 
read_nametable_3: 
	ld hl,(ppu_nametable_ptr+3*3) 
	jr read_nametable_generic
	
read_nametable_generic: 
	ld de,(ppu_address) 
	ld a,d 
	and a,11b
	ld d,a 
	add hl,de
	add hl,de
	ld d,(ppu_address+1) 
	jr read_generic.skip 
	
read_chr_0: 
	ld hl,(ppu_chr_ptr) 
	jr read_generic
read_chr_1: 
	ld hl,(ppu_chr_ptr+3) 
	jr read_generic 
read_chr_2: 
	ld hl,(ppu_chr_ptr+3*2) 
	jr read_generic 
read_chr_3: 
	ld hl,(ppu_chr_ptr+3*3) 
	jr read_generic 	
read_chr_4: 
	ld hl,(ppu_chr_ptr+3*4) 
	jr read_generic 	
read_chr_5: 
	ld hl,(ppu_chr_ptr+3*5) 
	jr read_generic 
read_chr_6: 
	ld hl,(ppu_chr_ptr+3*6) 
	jr read_generic 
read_chr_7: 
	ld hl,(ppu_chr_ptr+3*7) 

read_generic:
	ld de,(ppu_address) 
	ld a,d 
	and a,11b
	ld d,a 
	add hl,de
	ld d,(ppu_address+1)
.skip:	
	ld a,(hl)
	or a,a 
	sbc hl,hl
	ld l,(ppu_address_increment) 
	ex de,hl 
	add hl,de 
	ld (ppu_address),hl 
	ld e,(ppu_read_buffer) 
	ld (ppu_read_buffer),a
	pop af
	ret 
	
assert $ - ppu_read_lut <= 256

; align to page boundary 
rb $100 - ($ and $FF)

ppu_write_lut: 
	db 32 dup (write_chr and $FF)
	db 4 dup (write_nametable_0 and $FF)
	db 4 dup (write_nametable_1 and $FF)
	db 4 dup (write_nametable_2 and $FF)
	db 4 dup (write_nametable_3 and $FF)
	db 4 dup (write_nametable_0 and $FF)
	db 4 dup (write_nametable_1 and $FF)
	db 4 dup (write_nametable_2 and $FF)
	db 3 dup (write_nametable_3 and $FF)
	db write_palette and $FF
	
	
write_palette: 
	ld hl,ppu_palettes 
	ld a,(ppu_address) 
	and a,11111b	; mirroring
	cp a,$10	; set to zero if = $10
	jr nz,.skip 
	xor a,a 
.skip:
	ld l,a
	ld a,e
	ld de,(ppu_address)
	jr write_generic.skip2
	
; need 4 since the mirroring is variable
write_nametable_0: 
	ld hl,(ppu_nametable_ptr) 
	jr write_nametable_generic 
write_nametable_1: 
	ld hl,(ppu_nametable_ptr+3) 
	jr write_nametable_generic 
write_nametable_2: 
	ld hl,(ppu_nametable_ptr+3*2) 
	jr write_nametable_generic 
write_nametable_3: 
	ld hl,(ppu_nametable_ptr+3*3) 
	
write_nametable_generic:
	push hl 
	ld hl,(ppu_address)
	ld a,h 
	and a,11b 	; mask out high bits to get offset
	ld h,a 
	add hl,hl
	ld a,e
	pop de
	add hl,de 
	inc l 
	res 7,(hl)	; mark as being written to.
	dec l 
	ld de,(ppu_address) 
	jr write_generic.skip2
	
	
write_chr:
	ld a,e
	ld hl,(ppu_address) ; find update flag
	repeat 4 	; >> 4 
	srl h 
	rr l
	end repeat
	ld de,render_chrram_flags
	add hl,de 
	ld (hl),1 
	ld hl,ppu_chr_ram 
	jr write_generic.skip 
	
write_generic:
	ld a,e 
.skip:
	ld de,(ppu_address) 
	add hl,de
.skip2:
	ld (hl),a 
	or a,a 
	sbc hl,hl 
	ld l,(ppu_address_increment) 
	ex de,hl
	add hl,de 
	ld (ppu_address),hl
	ld e,a
	pop af
	ret 
	
assert $ - ppu_write_lut <= 256


; hl = address 
attribute_update: 
	ld iy,ppu_nametables + 1
	ld hl,ppu_nametables + 960*2 + 1
	call update_nametable 
	ld iy,ppu_nametables + 2048 + 1
	ld hl,ppu_nametables + 2048 + 960*2 + 1
	call update_nametable 
	ld iy,ppu_nametables + 2048*2 + 1
	ld hl,ppu_nametables + 2048*2 + 960*2 + 1
	call update_nametable 
	ld iy,ppu_nametables + 2048*3 + 1 
	ld hl,ppu_nametables + 2048*3 + 960*2 + 1
	call update_nametable 
	ret

update_nametable:  
	exx 
	ld c,11b 
	exx
	ld c,7
.start: 
	ld b,8 
.loop: 
	bit 7,(hl) 	; if bit 7 is 0, tile needs updating
	jr z,.update 
.return: 
	set 7,(hl)
	inc hl 
	inc hl
	lea iy,iy+8 
	djnz .loop 
	inc iyh 
	ld iyl,1
	dec c 		
	jr nz,.start 
	ld b,8 
.l2: 			; avoid corrupting attributes
	bit 7,(hl) 
	jq z,.update2 
.return2:	
	set 7,(hl)
	inc hl 
	inc hl
	lea iy,iy+8 
	djnz .l2 
	ret
	
.update: 
	dec hl 
	ld a,(hl) 
	inc hl
	exx
	ld b,a 		; fetch pattern 
	
	and a,c 	; mask out top six bits
	rr b 
	rr b 
	
	ld (iy+0),a 
	ld (iy+2),a 
	ld (iy+64),a 
	ld (iy+64+2),a 
	
	ld a,b 
	and a,c 
	rr b 
	rr b 
	
	ld (iy+4),a
	ld (iy+6),a
	ld (iy+64+4),a
	ld (iy+64+6),a
	
	lea iy,iy+127 
	inc iy 
	
	ld a,b 
	and a,c 
	rr b 
	rr b 
	
	ld (iy+0),a 
	ld (iy+2),a 
	ld (iy+64),a 
	ld (iy+64+2),a 
	
	ld a,b 
	and a,c 
	
	ld (iy+4),a
	ld (iy+6),a
	ld (iy+64+4),a
	ld (iy+64+6),a
	
	lea iy,iy-128
	
	exx 
	jq .return 
	
.update2: 
	dec hl 
	ld a,(hl) 
	inc hl
	exx
	ld b,a 		; fetch pattern 
	
	and a,c 	; mask out top six bits
	rr b 
	rr b 
	
	ld (iy+0),a 
	ld (iy+2),a 
	ld (iy+64),a 
	ld (iy+64+2),a 
	
	ld a,b 
	and a,c 
	rr b 
	rr b 
	
	ld (iy+4),a
	ld (iy+6),a
	ld (iy+64+4),a
	ld (iy+64+6),a
		
	exx 
	jq .return2


section .bss

public ppu_nametable_ptr
public ppu_chr_ptr

ppu_nametable_ptr: rb 3*4			; ptr's to current vram configuration. 
ppu_chr_ptr: rb 3*8 


extern jit_translation_buffer
extern load_jit_search

extern _startJIT
extern _startJIT.return

extern render_draw
extern scanline_cycle_count

