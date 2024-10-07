;use this as an example for writing mappers going forward 
; any mapper specific variables need to go to ix+64 and up of jit_scanline_vars

; load default mapper configuration and reset any variables
;
NROM.mapper_reset: 
	ld a,0 
	call prg_load_wram
	ld a,0 		; a = virtual address bank (80,A0,C0,E0)
	ld e,0 		; e = 8kb bank 
	call prg_bank_swap 
	ld a,1 
	ld e,1	
	call prg_bank_swap
	ld a,2 
	ld e,2	
	call prg_bank_swap
	ld a,3 
	ld e,3	
	call prg_bank_swap
	ret 
	
; as a rule, don't use the exx register set for these. 
; ixl = byte to write. 
; de = address
; preserve af,bc,d(0),iy
NROM.mapper_write:
	push af 
	ld a,d
	cp a,$60
	jr c,.nowrite 
	cp a,$80 
	jr c,.nowrite
	ex.sis hl,de 
	ld de,jit_nes_ewram-$6000
	add hl,de 
	ld (hl),e 
	ld de,0
.nowrite: 
	pop af
	ret 

; hl = address
; a != 0 if no valid write
; hl = function to inline
NROM.mapper_get_write_function:
	ld a,h 
	cp a,$60 
	jr c,.nofunc
	cp a,$80 
	jr nc,.nofunc 
	ex.sis de,hl
	ld hl,jit_nes_ewram-$6000 
	add hl,de 
	ld (NROM.write_wram.smc_address),hl 
	ld hl,NROM.write_wram
	xor a,a 
	ret
.nofunc: 
	ld a,1 
	ret 
	
; hl = address
; a != 0 if no valid write
; hl = function to inline
; de = address to smc into region_code
; make sure the address is the same register as would be used in mapper_get_read_function
NROM.mapper_get_write_region_function:
	ld a,h 
	cp a,$60 
	jr c,.nofunc
	cp a,$80 
	jr nc,.nofunc 
	ex.sis de,hl
	ld hl,jit_nes_ewram-$6000 
	add hl,de 
	ex de,hl 
	ld hl,NROM.write_wram_region
	xor a,a 
	ret
.nofunc:
	ld a,1 
	ret 

NROM.write_wram: 
	db .end - .start 
.start: 
	ld hl,0
.smc_address:=$-3
	ld (hl),e
.end: 

NROM.write_wram_region: 
	db .end - .start 
.start: 
	ld (hl),e
.end: 
	

; set bit 7 of a if write could cause bankswap
;	abs 		:= 1 shl 3		; write absolute addressing 
;	abs_ind 	:= 1 shl 4		; indexed addressing
; preserve de,iy, and a.
; (iy+1) = address to test
NROM.mapper_test_bankswap:
	ret 

; a=0 if branch is inside of current memory page, otherwise a != 0 
; hl = new memory address
; bc = old memory address
; only values of $8000+ will be given
; preserve de. 
NROM.mapper_test_long_branch:
	xor a,a 
	ret 
	
; hl = address 
; carry = true/false
; is this bank fixed to this address? 
NROM.mapper_test_bank_fixed:
	scf
	ret 
; is this address within 256 bytes of a bank cross?
NROM.mapper_test_bank_cross: 
	ld a,h 
	cp a,$BF 
	jr nz,.next 	; border of first and second 16kb bank
	scf 	
	ret 
.next:
	sub a,$FF 		; page $FFxx
	or a,a 
	ret nz 
	scf 
	ret 

; for any timing sensitive mapper events
NROM.mapper_event: 
	ret 