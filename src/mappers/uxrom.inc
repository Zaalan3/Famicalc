;use this as an example for writing mappers going forward 
; any mapper specific variables need to go to mapper_area (max. 1.5 kb)

; load default mapper configuration and reset any variables
; iy = rom header ptr
UXROM.mapper_reset: 
repeat 2 
	index = %-1 
	ld a,index 		; a = virtual address bank (80,A0,C0,E0)
	ld e,a			; e = 8kb bank
	call prg_bank_swap
end repeat 
	; $C000 - $FFFF fixed to last bank
	ld a,2 
	ld de,62
	call prg_bank_swap
	ld a,3 
	ld de,63 
	call prg_bank_swap
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
UXROM.mapper_write:
	; no prg ram capability 
	bit 7,d 
	ld de,0
	ld e,ixl 
	jp nz,UXROM.bank_select
	ret 

; hl = address
; a != 0 if no valid write
; hl = function to inline
UXROM.mapper_get_write_function:
	ld a,h 
	cp a,$80 
	jr c,.nofunc
	ld hl,UXROM.write_bank_select
	xor a,a 
	ret
.nofunc: 
	ld a,1 
	ret 

UXROM.write_bank_select: 
	db .end - .start 
.start: 
	call UXROM.bank_select
.end: 

UXROM.write_bank_select_region: 
	db .end - .start 
.start: 
	call UXROM.bank_select_region
.end: 
	
; hl = address
; a = FF if no valid write , 1 if region ignores first write(RMW),2 if region ignores second write(RMW), 0 otherwise
; hl = function to inline
; de = address to smc into region_code
; iy = code ptr
; make sure the address is the same register as would be used in mapper_get_read_function
UXROM.mapper_get_write_region_function:
	ld a,h 
	cp a,$7F 
	jr c,.nofunc
	ld de,(iy+1)
	ld hl,UXROM.write_bank_select_region	; 
	ld a,1 
	ret
.nofunc:
	ld a,$FF 
	ret 
	
; returns a = 1 if mapper responds to writeback of RMW instructions, a=2 if it instead responds to modified value
; hl = address
UXROM.mapper_rmw_response: 
	ld a,2 
	ret

; set bit 7 of a if write could cause bankswap
;	abs 		:= 1 shl 3		; write absolute addressing 
;	abs_ind 	:= 1 shl 4		; indexed addressing
; preserve de,iy, and a.
; (iy-1) = address to test
UXROM.mapper_test_bankswap:
	ld h,$80
	bit 3,a 
	jr nz,.test
	dec h 
	bit 4,a
	ret z
.test: 
	ld l,a 
	ld a,(iy-1) 
	cp a,h  
	ld a,l 
	ret c 
	set 7,a 
	ret  

; a=0 if branch is inside of current memory page, otherwise a != 0 
; hl = new memory address
; bc = old memory address
; only values of $8000+ will be given
; preserve de. 
UXROM.mapper_test_long_branch:
	ld a,h 
	and a,$C0 
	ld h,a 
	ld a,b 
	and a,$C0 
	sub a,h  
	ret 
	
; hl = address 
; carry = true/false
; is this bank fixed to this address? 
UXROM.mapper_test_bank_fixed:
	ld a,h 
	cp a,$C0 	; $C000 - FFFF fixed
	ccf 
	ret 

; is this address within 256 bytes of a bank cross?
UXROM.mapper_test_bank_cross: 
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
UXROM.mapper_event: 
	ret 

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
UXROM.bank_select_region:
	ld l,a 
	ld a,h 
	cp a,$80 
	ld a,l 
	ret c 
UXROM.bank_select: 
	; e = 16kb bank to swap $8000-$BFFF 
	push af
	push bc 
	sla e 
	push de 
	ld a,0 
	call prg_bank_swap
	pop de 
	ld a,1 
	inc e 
	call prg_bank_swap 
	ld de,0 
	pop bc 
	pop af 
	jp acknowledge_bankswap 
	