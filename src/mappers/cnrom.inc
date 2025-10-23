
; load default mapper configuration and reset any variables
; iy = rom header ptr
CNROM.mapper_reset: 
	; fixed PRG ROM with readdressable CHR ROM 
repeat 4 
	index = %-1 
	ld a,index 		; a = virtual address bank (80,A0,C0,E0)
	ld e,a			; e = 8kb bank
	call prg_bank_swap
end repeat 
repeat 8 
	index = %-1 
	ld a,index 
	ld de,index 
	call chr_bank_swap 
end repeat 
	ld a,(iy+9)
	jp set_mirroring
	
; as a rule, don't use the exx register set for these. 
; ixl = byte to write. 
; de = address
; preserve af,bc,d(0),iy
CNROM.mapper_write:
	; no prg ram capability 
	bit 7,d
	jr z,.nowrite
	ld de,0
	ld e,ixl 
	jp CNROM.bank_select
.nowrite: 
	ld de,0 
	ret 

; hl = address
; a != 0 if no valid write
; hl = function to inline
CNROM.mapper_get_write_function:
	bit 7,h
	jr z,.nofunc
	; read value at address 
	ld a,l 
	ld l,3 
	mlt hl 
	ld de,jit_translation_buffer
	add hl,de 
	ld hl,(hl) 
	ld de,0
	ld e,a 
	add hl,de 
	ld e,128 
	sbc hl,de 
	ld a,(hl)
	ld (CNROM.write_bank_select.and_value),a 
	
	ld hl,CNROM.write_bank_select
	xor a,a 
	ret
.nofunc: 
	ld a,1 
	ret 
	
CNROM.write_bank_select: 
	db .end - .start 
.start:
	ld d,0 
.and_value:=$-1
	call CNROM.bank_select
.end: 

CNROM.write_bank_select_region: 
	db .end - .start 
.start: 
	call CNROM.bank_select_region
.end: 

; hl = address
; a = FF if no valid write , 1 if region ignores first write(RMW),2 if region ignores second write(RMW), 0 otherwise
; hl = function to inline
; de = address to smc into region_code
; make sure the address is the same register as would be used in mapper_get_read_function
CNROM.mapper_get_write_region_function:
	ld a,h
	cp a,$7F 
	jr c,.nofunc
	ld de,(iy+1)
	ld hl,CNROM.write_bank_select_region
	ld a,1 
	ret
.nofunc:
	ld a,$FF 
	ret  
	
; returns a = 1 if mapper responds to writeback of RMW instructions, a=2 if it instead responds to modified value
CNROM.mapper_rmw_response: 
	ld a,2 
	ret
; set bit 7 of a if write could cause bankswap
;	abs 		:= 1 shl 3		; write absolute addressing 
;	abs_ind 	:= 1 shl 4		; indexed addressing
; preserve de,iy, and a.
; (iy+1) = address to test
CNROM.mapper_test_bankswap:
	ret 

; a=0 if branch is inside of current memory page, otherwise a != 0 
; hl = new memory address
; bc = old memory address
; only values of $8000+ will be given
; preserve de. 
CNROM.mapper_test_long_branch:
	xor a,a 
	ret 
	
; hl = address 
; carry = true/false
; is this bank fixed to this address? 
CNROM.mapper_test_bank_fixed:
	scf
	ret 
; is this address within 256 bytes of a bank cross?
CNROM.mapper_test_bank_cross: 
	ld a,h 
	cp a,$7F 
	jr nz,.next 	
	scf 	
	ret 
.next:
	sub a,$FF 		; page $FFxx
	or a,a 
	ret nz 
	scf 
	ret 

; for any timing sensitive mapper events
CNROM.mapper_event: 
	ret 


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
CNROM.bank_select_region:
	bit 7,h 
	ret z
	push af 
	push bc 
	; read value at address 
	ld a,l 
	ld l,3 
	mlt hl 
	ld bc,jit_translation_buffer
	add hl,bc 
	ld hl,(hl) 
	ld bc,0
	ld c,a 
	add hl,bc 
	ld c,128 
	sbc hl,bc 
	ld a,(hl)
	ld d,a 
	jr $+4
CNROM.bank_select: 
	; d AND e = 8kb CHR bank to swap in
	push af
	push bc
	
	ld a,d 
	and a,e 
	and a,$0F
	;*8
	sbc hl,hl
	ld l,a
	add hl,hl
	add hl,hl
	add hl,hl
	ex de,hl
	; swap each chr bank
	ld b,8
	xor a,a 
.loop: 
	push bc 
	push de 
	push af 
	call chr_bank_swap_render
	pop af 
	pop de 
	pop bc 
	inc a 
	inc de 
	djnz .loop 
	ld de,0 
	pop bc 
	pop af 
	ret 