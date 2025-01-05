

section .text 

public mapper_init
public mapper_get_read_region_function

public mapper_write
public mapper_get_write_function
public mapper_get_write_region_function
public mapper_test_bankswap
public mapper_test_long_branch
public mapper_test_bank_fixed
public mapper_test_bank_cross
public mapper_rmw_response
public mapper_event

include 'vars.inc'


mapper_len := 10*4 + 1 
mapper_def_len := 10*4
macro mapper_def name,id 
	db id 
	jp name.mapper_reset
	jp name.mapper_write
	jp name.mapper_get_write_function
	jp name.mapper_get_write_region_function
	jp name.mapper_test_bankswap
	jp name.mapper_test_long_branch
	jp name.mapper_test_bank_fixed 
	jp name.mapper_test_bank_cross 
	jp name.mapper_rmw_response
	jp name.mapper_event
end macro 

; loads mapper branches given id
; a = mapper id to load 
; iy = rom header ptr
mapper_init: 
	ld ix,mapper_list 
.find:
	cp a,(ix+0) 
	jr z,.fill 
	lea ix,ix+mapper_len 
	jr .find  
.fill: 
	ld bc,mapper_def_len
	ld de,mapper_reset 
	lea hl,ix+1
	ldir 
	jp mapper_reset
	ret


; hl = address to get function for
; returns: hl = function, de = base address for region_code 
mapper_get_read_region_function:
	call mapper_test_bank_cross
	jr c,.bank_boundary
	call mapper_test_bank_fixed
	jr c,.bank_fixed 
.bank_variable: 
	ld a,l 
	; find translation buffer entry 
	ld l,3
	mlt hl
	ld de,jit_translation_buffer
	add hl,de 
	ld (bank_variable.smc_tlb),hl 
	or a,a 
	sbc hl,hl
	ld l,a	; only low byte of address for offset into TLB
	ld de,128
	sbc hl,de
	ex de,hl
	ld hl,bank_variable
	ret  
.bank_boundary: 
	ex de,hl 	; de = full address	
	ld hl,bank_boundary
	ret 
.bank_fixed:
	; fetch host address of bank
	ld a,l 
	ld l,3 
	mlt hl 
	ld de,jit_translation_buffer
	add hl,de 
	ld hl,(hl) 
	ld de,128 
	or a,a 
	sbc hl,de
	ld e,a 
	add hl,de 
	ex de,hl 	; de = host address for base 
	ld hl,bank_fixed
	ret 
	
bank_variable: 
	db .end - .start
.start:
	ld de,(0)
.smc_tlb := $ - 3 	
	add hl,de 
	ld de,0 
	ld e,(hl)
.end: 

bank_boundary: 
	db .end - .start
.start: 
	ld e,l 
	ld l,3  
	mlt hl 
	ex de,hl 
	ld ix,jit_translation_buffer
	add ix,de  
	ld ix,(ix) 
	ex de,hl 
	add ix,de
	ld e,(ix-128)
.end: 
	


bank_fixed:
	db .end - .start
.start:
	ld e,(hl) 
.end:

acknowledge_bankswap: 
	pop.sis hl
	set scan_event_bank_swap,hl
	push.sis hl 
	pop hl 
	ld sp,jit_call_stack_bot-6
	ex af,af'
	ld ix,jit_scanline_vars
	ld (cycle_backup),a  
	xor a,a 
	ex af,af'
	jp (hl) 
	
mapper_reset: 
	jp 0
mapper_write: 
	jp 0
mapper_get_write_function:
	jp 0
mapper_get_write_region_function: 
	jp 0
mapper_test_bankswap: 
	jp 0
mapper_test_long_branch:
	jp 0
mapper_test_bank_fixed: 
	jp 0
mapper_test_bank_cross: 
	jp 0
mapper_rmw_response:
	jp 0
mapper_event: 
	jp 0


include "mappers/nrom.inc" 
include "mappers/uxrom.inc" 

mapper_list: 
	mapper_def NROM,0
	mapper_def UXROM,2
	db $FF 
	
	
extern jit_translation_buffer
extern jit_nes_ewram
extern prg_load_wram
extern prg_bank_swap
extern chr_bank_swap 
extern set_mirroring
