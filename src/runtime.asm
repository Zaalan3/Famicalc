; JIT functions called during runtime

section .text 

public izx_read 
public izx_write 
public izy_read 
public izy_write

public read_byte
public write_byte

public jit_scanline
public jit_scanline_skip
public jit_scanline_skip.nopush

public jit_jump_indirect
public jit_push_flags
public jit_pop_flags
public jit_return
public jit_return_int
public jit_break
public jit_call
public jit_call_local
public jit_branch_local 
public jit_branch_global

public io_read_register_ind
public ppu_read_register_ind
public io_write_register_ind
public ppu_write_register_ind

public jit_reset

public scanline_cycle_count


macro align_to_page
	rb $100 - ($ and $FF) 
end macro 

include 'vars.inc'
include 'ti84pceg.inc' 

; The opcode table doesn't take into account variability from branches and page crossing, 
; so there's some room for error here. Hard to find a good amount. 
; *Consider making variable for speed hacks
scanline_cycle_count := 114

; hl = NES address of caller
; d = scanline # 
; e = event flags
jit_scanline:
	push af
	; test to see if enough time has passed to unlock screen
	ld a,(ti.mpLcdUpcurr+2)
	cp a,$D5 
	call z,spiUnlock
	ex de,hl
	ld a,e
.event_handler:
	rra 
	jq c,.bankswap
	rra 
	jq c,.videostart 
	rra 
	jq c,.videoend 
	rra 
	jq c,.sprite_zero 
	rra
	jq c,.apu_irq
	rra 
	jq c,.dmc_irq	
	pop af 
	jp mapper_event
.bankswap:
	pop af
	pop af		; we'll not be returning
	dec.sis sp
	dec.sis sp
	pop.sis de
	res scan_event_bank_swap,e 
	push.sis de 
	ld de,0
	ld ix,jit_scanline_vars
	ld a,(cycle_backup) 
	ld (bankswap_ack),0
	ex af,af'
	call jit_search 
	jp (ix) 
.videostart:
	pop af 
	call ppu_video_start 	; reset sprite zero flag, do video timing 
	ld.sis sp,jit_event_stack_top and $FFFF ; reset event stack 
	call set_save_slot
	ld de,0
	ret 
.videoend: 
	pop af 
	call ppu_video_end 
	ld de,0
	jp nz,jit_nmi
	ret 
.sprite_zero: 
	pop af
	ld ix,jit_scanline_vars 
	bit 3,(ppu_mask) 
	jr z,.sprite_zero_skip
	bit 4,(ppu_mask) 
	jr z,.sprite_zero_skip
	set 6,(ppu_status)	; set sprite zero hit flag
.sprite_zero_skip:
	pop hl 
	push hl 
	call profile_block
	ret 
	
.apu_irq:
	pop af 
	ld ix,jit_scanline_vars
	ld de,0
	set 6,(apu_status) 
	set 0,(irq_sources)	
	jp jit_irq
	
.dmc_irq:
	pop af
	push hl
	ld ix,jit_scanline_vars 
	ld hl,(dmc_irq_line) 
	res scan_event_dmc_irq,(hl) 
	ld hl,(dmc_scanlines_remaining) 
	ld de,262
	or a,a 
	sbc hl,de 
	jr c,.dmc_end 
	jr z,.dmc_end
.dmc_continue: 
	ld (dmc_scanlines_remaining),hl
	ld hl,(dmc_irq_line) 
	set scan_event_dmc_irq,(hl)
	ld de,0 
	pop hl
	ret 
.dmc_wraparound:
	; find next testing point 
	add hl,hl
	add.sis hl,sp 
	; wraparound test 
	res 3,h 
	ld de,261*2
	sbc hl,de
	jr nc,$+3 
	add hl,de 
	set 3,h
	set.sis scan_event_dmc_irq,(hl)  
	ld (dmc_irq_line),l 
	ld (dmc_irq_line+1),h 
	ld de,0
	pop hl
	ret
.dmc_end: 
	add hl,de 
	ld de,0 
	ld (dmc_scanlines_remaining),de 
	or a,a 
	sbc hl,de 
	jr nz,.dmc_wraparound
.dmc_trigger: 
	res 4,(apu_status)
	pop hl
	; loop? 
	bit 6,(dmc_rate) 
	jp nz,write_apu_enable.start_sample
	; irq enabled? 
	bit 7,(dmc_rate)
	ret z 
	set 7,(apu_status) 
	set 1,(irq_sources)
	jp jit_irq
	
	
jit_scanline_skip:
	push hl
.nopush:
	ex af,af'
	ld l,$FF 
.loop: ; iterate until we find an event scanline 
	pop.sis de 
	ld a,e 
	inc l
	or a,a 
	jr z,.loop 
	push.sis de
	xor a,a 
	ld d,a
	ex af,af'
	ret 
	
jit_reset:
	ld.sis sp,jit_event_stack_top and $FFFF
	ld sp,jit_call_stack_bot 
	ld hl,$4000 
	push hl
	push hl 
	push hl 
	ld bc,0
	ld de,0 
	ld a,1 
	ld i,a
	ld r,a
	xor a,a
	exx 
	ld hl,jit_nes_iwram+$1FD 
	ld b,1 shr 2 	; I flag set. 
	ld c,a			; carry = 0
	exx
	ex af,af' 
	ld a,scanline_cycle_count
	ex af,af' 
	ld iy,jit_nes_iwram+$80
	ld hl,$FFFC 
	jp jit_jump_indirect
	
jit_nmi: 
	ld a,scanline_cycle_count-7	; takes seven cycles to trigger
	ex af,af'
	push hl 
	exx 
	pop de 
	ld (hl),d 
	dec l 
	ld (hl),e 
	dec l 
	res 4,b 	; reset B flag 
	call jit_push_flags
	exx
	set 2,b		; set I flag 
	exx 
	pop hl		; remove previous return address
	call profile_block
	ld hl,$FFFA ; get NMI vector 
	jp jit_jump_indirect 
	
jit_irq:
	ex af,af'
	exx 
	bit 2,b		; if I enabled, IRQs are suppressed 
	exx
	ret nz 
	push hl 
	exx 
	pop de
	ld (hl),d 
	dec l 
	ld (hl),e 
	dec l 
	res 4,b 	; reset B flag 
	call jit_push_flags
	exx
	set 2,b		; set I flag 
	exx 
	pop hl		; remove previous return address
	;call profile_block
	ld hl,$FFFE ; get IRQ vector 
	jp jit_jump_indirect 
	ret 
	
; replaces block cycles to consume a scanline if frequently called during a scanline event. 
; hl = pointer to start of `jr $` after call `jit_scanline` in block header 
; clobbers hl & de 
profile_block:
	; increment counter 
	ld de,5 
	or a,a 
	sbc hl,de 
	inc (hl) 
	inc (hl) 
	inc (hl) 
	inc (hl) 
	; if we've been here 64 times before, make this block consume more cycles
	ret nz 
	ld e,1+4+2+1+1
	add hl,de 
	ld (hl), (scanline_cycle_count / 2) + 1 	; sub a,count 
	ret 

jit_branch_global:
	pop ix
	call jit_search
	jp (ix) 
	
; inlines branch location
jit_branch_local:
	; if a bankswap is occuring, replacing the branch may cause errors
	ld ix,jit_scanline_vars
	bit 0,(bankswap_ack) 
	jq nz,jit_branch_global
	; if this call is at the end of cache, reclaim the last 4 bytes 
	pop de 
	push hl 
	ld hl,(jit_cache_free)
	or a,a 
	sbc hl,de 
	jr nz,.cont 
	ld hl,-4 
	add hl,de 
	ld (jit_cache_free),hl 
.cont: 
	pop hl 
	push de
	ld de,0
	call jit_search	 
	; detect a cache flush if nothing is on the stack
	ld hl,jit_call_stack_bot-9
	or a,a 
	sbc hl,sp 
	jq z,.nowrite
	; overwrite branch
	pop hl
	ld de,6
	or a,a 
	sbc hl,de	
	ex de,hl 
	; jr or jp? 
	lea hl,ix+0 
	sbc hl,de
	push de
	jr nc,.positive 
.negative: 
	ld de,-128 
	or a,a
	sbc hl,de 
	jr nc,.relative
	jr .jmp 
.positive: 
	ld de,127 
	sbc hl,de 
	jr c,.relative
	jr z,.relative
	jr .jmp
.relative: 
	add hl,de 
	pop de
	ex de,hl
	dec hl
	dec hl 
	ld (hl),$18	; jr d
	inc hl 
	ld (hl),e 
	ld de,0 
	jp (ix)
.jmp:
	pop hl  
	dec hl 
	dec hl
	ld (hl),$C3	; jp mmnn
	inc hl 
	ld (hl),ix 	
.nowrite:	
	ld de,0 
	jp (ix) 


; reads two bytes and jumps there
; hl = address
jit_jump_indirect:
	push af 
	ex de,hl 
	push de
	ld l,d 
	ld h,3 
	mlt hl 
	ld a,e 
	ld de,jit_translation_buffer
	add hl,de 
	ld hl,(hl) 
	ld de,128
	or a,a 
	sbc hl,de 
	ld e,a 
	add hl,de 
	ld a,(hl)
	pop de 
	inc e 
	push af 
	ld l,d 
	ld h,3 
	mlt hl 
	ld a,e 
	ld de,jit_translation_buffer
	add hl,de 
	ld hl,(hl) 
	ld de,128
	or a,a 
	sbc hl,de 
	ld e,a 
	add hl,de 
	ld a,(hl)
	ld h,a 
	pop af
	ld l,a 
	pop af
	call jit_search 
	jp (ix)

	
jit_push_flags:
	exx
	ld e,a 
	exx 
	ex de,hl 
	ld hl,i 
	ld a,i 
	res 1,b		; Z flag 
	jr nz,.l1 	
	set 1,b 
.l1: 
	res 7,b		; N flag 
	ld a,h
	rla 
	jr nc,.l2 
	set 7,b 
.l2: 
	res 0,b
	bit 0,c		; C flag
	jr z,.l3 
	set 0,b 
.l3: 
	set 5,b		; always set when pushed
	ex de,hl 
	ld (hl),b 
	dec l
	exx 
	ld a,e 
	ret 
	
jit_pop_flags:
	ld e,a 
	exx 
	inc l 
	ld b,(hl) 
	ld c,b 	 ; C flag
	ld d,b 	 ; N flag  
	xor a,a	 
	bit 1,b  ; Z flag
	jr nz,.l1 
	inc a 
.l1: 
	ld e,a 
	ex de,hl 
	ld i,hl 
	ex de,hl
	exx 
	ld a,e 
	ret


; hl = address to call 
; de' = return address
jit_call:
	ld (hl),d 
	dec l 
	ld (hl),e 
	dec l
	inc de
	; dont push onto call stack if address is in RAM 
	bit 7,d 
	jr z,.skip 
	push ix		; push page & bank
	push de 	; push NES address onto call stack
	exx 
	call jit_search
	ld hl,$D64E00 
	or a,a 
	sbc hl,sp 
	jr c,$+6
	ld sp,jit_call_stack_bot-9
	jp (ix)
.skip:
	exx 
	call jit_search
	pop hl 
	jp (ix) 
	
jit_call_local: 
	push ix
	ld (hl),d 
	dec l 
	ld (hl),e 
	dec l 
	inc de
	exx 
	call jit_search 
	ld hl,jit_call_stack_bot-9
	or a,a 
	sbc hl,sp 
	jr z,.flush  ;verify there wasnt a cache flush
	pop hl
	pop hl
	ld de,3		; replace call target 
	or a,a 
	sbc hl,de 
	ld de,jit_call_local_inline 
	ld (hl),de
	ld de,14 		; replace value of ld hl,mmnn 
	sbc hl,de 
	ld (hl),ix 
	exx 
repeat 6
	dec sp
end repeat
	push de
	exx
	jp (ix) 
.flush:
	ex de,hl 
	pop hl
	pop hl
	jp (ix)

jit_call_local_inline:
	ld (hl),d 
	dec l 
	ld (hl),e 
	dec l 
	inc de 
	push ix
	push de ; push onto call stack 
	exx 
	push hl 
	pop ix
	ld hl,$D64E00 
	or a,a 
	sbc hl,sp 
	jr c,$+6
	ld sp,jit_call_stack_bot-9
	jp (ix) 
	
	
jit_return_int:	
	call jit_pop_flags
	exx 
	ld de,0 
	inc l 
	ld e,(hl)	; fetch return address 
	inc l
	ld d,(hl)
	push de 
	exx 
	pop hl
	call jit_search
	jp (ix) 
	
jit_return:
	exx 
	ld de,0
	inc l 
	ld e,(hl)	; fetch return address 
	inc l
	ld d,(hl)
	inc de		; 
	push de
	ld e,a
	exx 
	pop de
	or a,a 
	sbc hl,hl 
	add hl,sp 
	bit 1,h 	; if stack <$D64E00, reset 
	jr nz,.noreset
	ld sp,jit_call_stack_bot-9
	jr .skip2
.noreset: 
	; compare stored address with fetched address 
	pop hl
	sbc.sis hl,de 
	jr nz,.mismatch2 
	; insure page is set to correct bank 
	pop hl
	ld a,h 
	ld h,(prg_page_bank and $FF00) shr 8
	sub a,(hl) 
	jr nz,.mismatch
	ld d,a 
	exx 
	ld a,e 
	exx
	pop hl
	jp (hl) 
.mismatch:
	dec sp
	dec sp
	dec sp
.mismatch2:	
	dec sp
	dec sp
	dec sp
.skip: 
	exx 
	ld a,e 
	exx 
.skip2:
	ex.sis de,hl
	call jit_search
	jp (ix) 

jit_break:
	ld (hl),d 
	dec l 
	ld (hl),e 
	dec l 
	set 4,b 	; set B flag 
	call jit_push_flags
	exx
	set 2,b		; set I flag 
	exx 
	ld hl,$FFFE ; get IRQ vector 
	jp jit_jump_indirect
	
;-----------------------------------------------------
; Indirect Memory access functions

read_byte: 
	ex de,hl 
	ld hl,jit_read_page_lut
	ld l,d 
	ld l,(hl)
	inc h 
	jp (hl) 

write_byte:
	ld ixl,e 
.skip_save:
	ex de,hl 
	ld hl,jit_write_page_lut
	ld l,d 
	ld l,(hl)
	inc h 
	jp (hl) 

izy_read: 
	ld h,d 
	ld.sis e,(hl) 
	inc l 
	ld.sis d,(hl)
	ld l,c 
	add.sis hl,de 
	ex de,hl 
	ld hl,jit_read_page_lut
	ld l,d 
	ld l,(hl)
	inc h 
	jp (hl) 

izy_write: 
	ld ixl,e
	ld h,d 
	ld.sis e,(hl) 
	inc l 
	ld.sis d,(hl)
	ld l,c 
	add.sis hl,de 
	ex de,hl 
	ld hl,jit_write_page_lut
	ld l,d 
	ld l,(hl)
	inc h 
	jp (hl)  

; l = zero page address to read from 
izx_read: 
	ld e,b 
	add hl,de
	ld h,d
	ld.sis e,(hl) 
	inc l 
	ld.sis h,(hl)
	ld l,e 
	ex.sis de,hl 
	ld hl,jit_read_page_lut
	ld l,d 
	ld l,(hl)
	inc h 
	jp (hl) 

izx_write: 
	ld ixl,e
	ld e,b 
	add hl,de
	ld h,d
	ld.sis e,(hl) 
	inc l 
	ld.sis h,(hl)
	ld l,e 
	ex.sis de,hl 
	ld hl,jit_write_page_lut
	ld l,d 
	ld l,(hl)
	inc h 
	jp (hl) 


;LUTs + functions aligned to page boundaries for easy branching
align_to_page


jit_read_page_lut: 
	db 8 dup (ram_read_byte and $FF)	 	
	db 24 dup (ram_mirror_read_byte and $FF)	 	
	db 32 dup (ppu_read_register and $FF) 
	db (io_read_register and $FF) 	
	db 7 dup open_bus_read and $FF
	db 56 dup (mapper_read_branch and $FF) ;48-7F 
	db 32 dup (mapper_read_bank0 and $FF)
	db 32 dup (mapper_read_bank1 and $FF)
	db 32 dup (mapper_read_bank2 and $FF)
	db 32 dup (mapper_read_bank3 and $FF)

; in: de = address 
; out: e = result
ram_read_byte:
	or a,a 
	sbc hl,hl
	ex de,hl 
	ld.sis e,(hl)
	ret 
	
ram_mirror_read_byte: 
	or a,a 
	sbc hl,hl
	ex de,hl 
	res 3,h 
	res 4,h 
	ld.sis e,(hl)
	ret 
	
ppu_read_register_ind:
	ld d,a  
	ld a,l
	jr $+4 
ppu_read_register:
	ld d,a 
	ld a,e 
	and a,111b 
	ld e,a 
	ld a,d 
	ld d,3 
	mlt de 
	ld hl,io_get_read_function.ppu_lut
	add hl,de 
	ld hl,(hl)
	ld d,0 
	jp (hl) 

io_read_register_ind:
	push af 
	ld a,l 
	jr $+4 
io_read_register: 
	push af 
	ld a,e 
	cp a,$15 ; <$ignore writes before $4014
	jr c,.openbus
	cp a,$18
	jr nc,.openbus 
	sub a,$15 
	ld e,a 
	ld d,3 
	mlt de 
	ld hl,io_get_read_function.io_lut 
	add hl,de
	ld hl,(hl)
	ld d,0 
	pop af 
	jp (hl)  
.openbus: 
	pop af
	ld e,d
	ld d,0 
	ret 
	
open_bus_read: 
	ex de,hl 
	ld e,h 
	ld d,0 
	ret 

mapper_read_branch: 
	ld ixl,a 
	ld l,d 
	ld h,3 
	mlt hl 
	ld a,e 
	ld de,jit_translation_buffer
	add hl,de 
	ld hl,(hl)
	ld de,128 
	or a,a 
	sbc hl,de 
	ld e,a 
	add hl,de 
	ld e,(hl) 
	ld a,ixl 
	ret 

mapper_read_bank0: 
	ld hl,(jit_translation_buffer + $80*3)
	res 7,d
	res 6,d
	res 5,d
	add hl,de
	ld d,0 
	ld e,128
	or a,a
	sbc hl,de 
	ld e,(hl) 
	ret 
mapper_read_bank1: 
	ld hl,(jit_translation_buffer + $A0*3)
	res 7,d
	res 6,d
	res 5,d
	add hl,de
	ld d,0 
	ld e,128 
	or a,a
	sbc hl,de 
	ld e,(hl) 
	ret 
mapper_read_bank2:
	ld hl,(jit_translation_buffer + $C0*3)
	res 7,d
	res 6,d
	res 5,d
	add hl,de
	ld d,0 
	ld e,128
	or a,a
	sbc hl,de 
	ld e,(hl) 
	ret 
mapper_read_bank3: 
	ld hl,(jit_translation_buffer + $E0*3)
	; somehow faster than the alternative
	res 7,d
	res 6,d
	res 5,d
	add hl,de
	ld d,0 
	ld e,128
	or a,a
	sbc hl,de 
	ld e,(hl) 
	ret 


	
assert $ - ram_read_byte < $100
align_to_page

; in: de = address , ixl = byte to write
jit_write_page_lut:
	db 8 dup (ram_write_byte and $FF)
	db 24 dup (ram_mirror_write_byte and $FF)	 	
	db 32 dup (ppu_write_register and $FF) 
	db (io_write_register and $FF)
	db 7 dup (open_bus_write and $FF) 	
	db 184 dup (mapper_write_branch and $FF)  
	
ram_write_byte: 
	ex.sis de,hl 
	ld e,ixl
	ld.sis (hl),e
	ld d,0
	ret 

ram_mirror_write_byte: 
	ex.sis de,hl 
	res 3,h 
	res 4,h 
	ld e,ixl
	ld.sis (hl),e
	ld d,0
	ret 
	
ppu_write_register_ind: 
	ld a,l 
	ld d,a 
	jr $+4 
ppu_write_register: 
	ld d,a 
	ld a,e 
	and a,111b 
	ld e,a 
	ld a,d 
	ld d,3 
	mlt de 
	ld hl,io_get_write_function.ppu_lut
	add hl,de 
	ld hl,(hl)
	ld d,0 
	ld e,ixl
	jp (hl)
	
io_write_register_ind: 
	push af 
	ld a,l 
	jr $+4
io_write_register: 
	push af 
	ld a,e 
	cp a,$10 ; <$ignore writes before $4010
	jr c,.skip
	cp a,$18 ; and after $4017
	jr nc,.skip 
	sub a,$10
	ld e,a 
	ld d,3 
	mlt de 
	ld hl,io_get_write_function.io_lut 
	add hl,de
	ld hl,(hl)
	ld d,0
	ld e,ixl
	pop af 
	jp (hl)  
.skip: 
	pop af
	ld d,0 
	ld e,ixl
	ret 
	
open_bus_write: 
	ld de,0
	ld e,ixl
	ret 

mapper_write_branch: 
	jp mapper_write 

assert $ - ram_write_byte < $100


extern io_get_read_function
extern io_get_read_function.io_lut
extern io_get_read_function.ppu_lut
extern io_get_write_function
extern io_get_write_function.io_lut
extern io_get_write_function.ppu_lut

extern mapper_write
extern mapper_event

extern jit_search
extern jit_translation_buffer
extern jit_call_stack_ptr

extern ppu_video_start
extern ppu_video_end
extern io_frame_irq
extern write_apu_enable.start_sample

extern jit_cache_free
extern spiUnlock
extern set_save_slot