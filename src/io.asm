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
public ppu_sprite_zero_hit

public io_frame_irq 


include 'vars.inc'

scanline_counter := ix+0

apu_status := ix+1
joypad1_input := ix+2
joypad1_shift := ix+3
joypad2_input := ix+4
joypad2_shift := ix+5
frame_counter_irq_line := ix+7

ppu_status := ix+10
ppu_write_latch := ix+11
in_vblank := ix+12 
oam_address := ix+13 
ppu_ctrl := ix+14 
ppu_mask := ix+15 
ppu_address := ix+16
ppu_address_increment := ix+19 
ppu_x_scroll := ix+20 
ppu_y_scroll := ix+21
ppu_address_new_high := ix+22 
ppu_read_buffer := ix+23
ppu_mirroring := ix+24 
ppu_event_list := ix+25



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
	
	ld a,3								; continuous keyboard scanning
	ld (ti.mpKeyMode),a 
	
	; default horizontal mirroring
	ld hl,ppu_nametables-$2000
	ld (ppu_nametable_ptr),hl
	ld hl,ppu_nametables-$2400
	ld (ppu_nametable_ptr+3),hl
	ld hl,ppu_nametables+$400-$2800
	ld (ppu_nametable_ptr+6),hl
	ld hl,ppu_nametables+$400-$2C00
	ld (ppu_nametable_ptr+9),hl
	
	ld de,ppu_nametables+1 
	ld hl,ppu_nametables
	ld bc,4095 
	ld (hl),0 
	ldir 
	
	ret 
	
ppu_sprite_zero_hit:
	ret
ppu_video_start:
	ld ix,jit_scanline_vars
	res 6,(ppu_status)		; clear sprite zero flag
	res 7,(ppu_status)		; clear vblank flag, if not already
	res 0,(in_vblank)
	ld (scanline_counter),0
	push af
	push bc
	ld a,(ti.mpKeyData+2) 	; return if DEL down 
	and a,$80 
	jp nz,_testJIT.return
	call io_get_keys
	ld (joypad1_input),a 
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
	ld hl,ppu_nametables 
	push hl 
	call _drawNametable
	pop hl 
	pop hl 
	pop bc 
	pop af 
	ld iy,jit_nes_iwram+$80
	bit 7,(ppu_ctrl) 
	ret


; a = NES key result
io_get_keys:
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
	ret 
	
	
read_oam_data:
	ld ix,jit_scanline_vars
	ld hl,ppu_oam 
	ld l,(oam_address) 
	ld e,(hl) 
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
	inc l 
	ld (oam_address),l
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
	ld e,(scanline_counter) 
	ld (hl),e 
	inc hl 
	ld (hl),ppu_event_ctrl 
	inc hl 
	ld e,(ppu_ctrl)
	ld (hl),e 
	inc hl
	ld (ppu_event_list),hl 
	ret 
	
write_ppu_mask:
	ld ix,jit_scanline_vars
	ld (ppu_mask),e
	ld e,a 
	ld a,r 
	ld a,e 
	ret p 
.active_render: 
	ld hl,(ppu_event_list) 
	ld e,(scanline_counter) 
	ld (hl),e 
	inc hl 
	ld (hl),ppu_event_mask
	inc hl 
	ld e,(ppu_mask) 
	ld (hl),e 
	inc hl
	ld (ppu_event_list),hl 
	ret 
	
write_ppu_io_bus: 
	ret 
	
	; TODO: 
write_ppu_scroll:
	ld ix,jit_scanline_vars
	bit 0,(ppu_write_latch) 
	jq nz,yscroll 
xscroll: 
	ld l,a 
	ld (ppu_x_scroll),e 
	ld a,r 
	rla 
	ld a,l
	jr c,.x_scroll_event 
	ld (ppu_write_latch),1 
	ret 
.x_scroll_event: 
	ld d,ppu_event_scroll_x
	jr scroll_event 
	
yscroll: 
	ld l,a 
	ld (ppu_y_scroll),e 
	ld a,r 
	rla 
	ld a,l
	jr c,.y_scroll_event  
	ld (ppu_write_latch),0
	ret 
.y_scroll_event: 
	ld d,ppu_event_scroll_y

scroll_event:
	ld hl,(ppu_event_list) 
	ld e,(scanline_counter) 
	ld (hl),e 
	inc hl 
	ld (hl),d 
	inc hl 
	ld d,0 
	ld (ppu_event_list),hl 
	ret 

write_ppu_address:
	ld ix,jit_scanline_vars
	bit 0,(ppu_write_latch) 
	jq nz,writehigh 
writelow: 
	ld (ppu_address_new_high),e 
	ld (ppu_write_latch),1 
	ret 
writehigh:
	ld l,a 	
	ld (ppu_address),e
	ld a,(ppu_address_new_high)
	and a,$3F	; mask out top bits
	ld (ppu_address+1),a 
	ld (ppu_write_latch),0
	ld a,r 
	rla 
	ld a,l 
	ret nc 
	push de 
	ld hl,(ppu_event_list) 
	ld d,(scanline_counter) 
	ld (hl),d 
	inc hl 
	ld (hl),ppu_event_address
	inc hl 
	ld d,(ppu_address+1) 
	ld (hl),de 
	inc hl 
	inc hl 
	pop de 
	ret 
	
; TODO: this could be faster 
read_ppu_data: 
	ld ix,jit_scanline_vars
	push af
	ld a,r 
	rla 
	jr c,.ppu_read_during_render 
	ld hl,ppu_read_lut 
	ld a,(ppu_address+1) 
	and a,$3F 
	ld l,a
	ld l,(hl) 
	jp (hl) 
; reading from PPUDATA during rendering causes increment to Y scroll and X course scroll
.ppu_read_during_render:
	pop af
	ld hl,(ppu_event_list) 
	ld e,(scanline_counter) 
	ld (hl),e 
	inc hl 
	ld (hl),ppu_event_read
	inc hl 
	ld (ppu_event_list),hl 
	ret 

write_ppu_data: 
	ld ix,jit_scanline_vars
	push af
	ld hl,ppu_write_lut 
	ld a,(ppu_address+1) 
	and a,$3F 
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
	and a,1111b	; mirroring
	tst a,0011b	; set to zero if multiple of 4 
	jr nz,.skip 
	xor a,a 
.skip:
	ld l,a
	jr read_generic.skip
	
; need 4 since the mirroring is variable
read_nametable_0: 
	ld hl,(ppu_nametable_ptr) 
	jr read_generic 

read_nametable_1: 
	ld hl,(ppu_nametable_ptr+3) 
	jr read_generic 
	
read_nametable_2: 
	ld hl,(ppu_nametable_ptr+3*2) 
	jr read_generic 
	
read_nametable_3: 
	ld hl,(ppu_nametable_ptr+3*3) 
	jr read_generic 

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
	add hl,de
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
	and a,1111b	; mirroring
	tst a,0011b	; set to zero if multiple of 4 
	jr nz,.skip 
	xor a,a 
.skip:
	ld l,a
	jr write_generic.skip2
	
; need 4 since the mirroring is variable
write_nametable_0: 
	ld hl,(ppu_nametable_ptr) 
	jr write_generic 

write_nametable_1: 
	ld hl,(ppu_nametable_ptr+3) 
	jr write_generic 
	
write_nametable_2: 
	ld hl,(ppu_nametable_ptr+3*2) 
	jr write_generic 
	
write_nametable_3: 
	ld hl,(ppu_nametable_ptr+3*3) 
	jr write_generic 

write_chr:
	ld a,e
	ld hl,(ppu_address) ; find update flag
	add hl,hl	; >> 5 
	add hl,hl
	add hl,hl	; 1 flag for every two tiles 
	ld l,h 
	ld h,0
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


extern jit_translation_buffer

extern ppu_nametable_ptr
extern ppu_chr_ptr
extern _testJIT.return

extern _drawNametable
