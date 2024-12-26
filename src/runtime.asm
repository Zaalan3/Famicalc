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

; The opcode table doesn't take into account variability from branches and page crossing, 
; so there's some room for error here. Hard to find a good amount. 
scanline_cycle_count := 114

; hl = NES address of caller
; d = scanline # 
; e = event flags
jit_scanline:
	add a,scanline_cycle_count
	pop.sis de 
	dec e 
	ld d,0
	ret m 
	push af
	ld a,e 
	inc a
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
	push.sis de 
	pop af 
	ex af,af'
	pop ix 	; we'll not be returning
	call jit_search 
	jp (ix) 
.videostart:
	pop af 
	call ppu_video_start 	; reset sprite zero flag, do video timing 
	ld.sis sp,jit_event_stack_top and $FFFF ; reset event stack 
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
	ld de,0
	ret 
.apu_irq:
	pop af 
	call io_frame_irq
	jp jit_irq 
.dmc_irq:
	pop af
	ret 
	
	
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
	ld hl,$FFFA ; get NMI vector 
	jp jit_jump_indirect 
	
jit_irq:
	ld a,scanline_cycle_count-7
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
	ld hl,$FFFE ; get IRQ vector 
	jp jit_jump_indirect 
	ret 

; inlines branch location
jit_branch_local:
	call jit_search
	ld hl,jit_call_stack_bot-6
	or a,a 
	sbc hl,sp 
	jr z,.nowrite
	pop hl
	ld de,8 	; replace `LD HL,MMNN` with jump to cached block
	or a,a 
	sbc hl,de 
	ld (hl),$C3	; jp mmnn
	inc hl 
	ld (hl),ix 	
.nowrite:	
	ld de,0 
	jp (ix) 
	
; TODO: inline somehow
jit_branch_global:
	pop ix
	call jit_search
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
	ld a,l
	or a,h 
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
; de = return address
jit_call:
	ld (hl),d 
	dec l 
	ld (hl),e 
	dec l
	inc de
	push de 	; push onto call stack
	exx 
	call jit_search
	jp (ix)
	
jit_call_local: 
	ld (hl),d 
	dec l 
	ld (hl),e 
	dec l 
	inc de
	exx 
	call jit_search 
	ld hl,jit_call_stack_bot-6
	or a,a 
	sbc hl,sp 
	jr z,.flush  ;verify there wasnt a cache flush
	pop hl 
	ld de,3		; replace call target 
	or a,a 
	sbc hl,de 
	ld de,jit_call_local_inline 
	ld (hl),de
	ld de,9 		; replace value of ld hl,mmnn 
	sbc hl,de 
	ld (hl),ix 
	exx 
	dec sp
	dec sp 
	dec sp 
	push de
	exx
	jp (ix) 
.flush: 
	ex de,hl 
	jp (ix)

jit_call_local_inline: 
	ld (hl),d 
	dec l 
	ld (hl),e 
	dec l 
	inc de 
	push de ; push onto call stack 
	exx 
	jp (hl) 
	
	
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
	exx 
	or a,a 
	sbc hl,hl 
	add hl,sp 
	ld de,$D50E00
	sbc hl,de 
	jr c,.mismatch
	pop de
	pop hl 
	or a,a 
	sbc.sis hl,de 
	jr nz,.mismatch 
	ld d,h 
	pop hl
	jp (hl) 
.mismatch:
	; TODO: add special case handling
	ld sp,jit_call_stack_bot-6 ;reset stack
	or a,a 
	sbc hl,hl
	ex de,hl
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
	cp a,$14 ; <$ignore writes before $4014
	jr c,.skip
	cp a,$18 ; and after $4017
	jr nc,.skip 
	sub a,$14
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
