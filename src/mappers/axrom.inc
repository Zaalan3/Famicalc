
; load default mapper configuration and reset any variables
; iy = rom header ptr
AXROM.mapper_reset: 
	; $8000-$FFFF swappable 32kb bank
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
	; set one screen mirroring 
	ld hl,ppu_nametables
	ld (ppu_nametable_ptr),hl
	ld (ppu_nametable_ptr+3),hl
	ld (ppu_nametable_ptr+3*2),hl
	ld (ppu_nametable_ptr+3*3),hl
	ret 
	
; as a rule, don't use the exx register set for these. 
; ixl = byte to write. 
; de = address
; preserve af,bc,d(0),iy
AXROM.mapper_write:
	; no prg ram capability 
	bit 7,d
	jr z,.nowrite
	ld de,0
	ld e,ixl 
	jp AXROM.bank_select
.nowrite: 
	ld de,0 
	ret 

; hl = address
; a != 0 if no valid write
; hl = function to inline
AXROM.mapper_get_write_function:
	ld a,h 
	cp a,$80 
	jr c,.nofunc
	ld hl,AXROM.write_bank_select
	xor a,a 
	ret
.nofunc: 
	ld a,1 
	ret 

AXROM.write_bank_select: 
	db .end - .start 
.start: 
	call AXROM.bank_select
.end: 

AXROM.write_bank_select_region: 
	db .end - .start 
.start: 
	call AXROM.bank_select_region
.end: 
	
; hl = address
; a = FF if no valid write , 1 if region ignores first write(RMW),2 if region ignores second write(RMW), 0 otherwise
; hl = function to inline
; de = address to smc into region_code
; iy = code ptr
; make sure the address is the same register as would be used in mapper_get_read_function
AXROM.mapper_get_write_region_function:
	ld a,h 
	cp a,$7F 
	jr c,.nofunc
	ld de,(iy+1)
	ld hl,AXROM.write_bank_select_region	; 
	ld a,1 
	ret
.nofunc:
	ld a,$FF 
	ret 
	
; returns a = 1 if mapper responds to writeback of RMW instructions, a=2 if it instead responds to modified value
; hl = address
AXROM.mapper_rmw_response: 
	ld a,2 
	ret

; set bit 7 of a if write could cause bankswap
;	abs 		:= 1 shl 3		; write absolute addressing 
;	abs_ind 	:= 1 shl 4		; indexed addressing
; preserve de,iy, and a.
; (iy-1) = address to test
AXROM.mapper_test_bankswap:
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
; preserve hl,de,bc. 
AXROM.mapper_test_long_branch:
	xor a,a 
	ret 
	
; hl = address 
; carry = true/false
; is this bank fixed to this address? 
AXROM.mapper_test_bank_fixed:
	; whole area readdressable 
	or a,a 
	ret 

; is this address within 256 bytes of a bank cross?
AXROM.mapper_test_bank_cross: 
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
AXROM.mapper_event: 
	ret 

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
AXROM.bank_select_region:
	ld l,a 
	ld a,h 
	cp a,$80 
	ld a,l 
	jr nz,AXROM.bank_select 
	ex de,hl 
	jp write_byte
AXROM.bank_select: 
	; e = xxxM xPPP
	; P = 32kb PRG ROM bank 
	; M = which nametable to display
	push af
	push bc 
	ld ix,jit_scanline_vars 
	; setup nametable 
	ld bc,ppu_nametables
	bit 4,e 
	jr z,.l1 
	ld b,$28
.l1: 
	ld (ppu_nametable_ptr),bc
	ld (ppu_nametable_ptr+3),bc
	ld (ppu_nametable_ptr+3*2),bc
	ld (ppu_nametable_ptr+3*3),bc
	; is this a render frame? 
	ld a,r 
	rla 
	jr nc,.l3
	; create nametable change event 
	; fetch scanline #
	pop.sis hl 
	ld a,h 
	push.sis hl 
	ld hl,(ppu_event_list) 
	ld (hl),a 
	inc hl 
	ld (hl),ppu_event_mirroring
	inc hl 
	; copy nametable ptrs 
	ld a,4 
.l2: 
	ld (hl),bc 
	inc hl
	inc hl
	inc hl
	dec a
	jr nz,.l2 
	ld (ppu_event_list),hl 
.l3: 
	; change PRG bank
	ld a,e 
	and a,0111b
	rla 
	rla 	; 4 8kb banks per 32kb bank
	ld e,a
	push de 
	ld a,0 
	call prg_bank_swap
	ld a,1 
	pop de 
	inc e 
	push de 
	call prg_bank_swap
	ld a,2 
	pop de 
	inc e 
	push de 
	call prg_bank_swap
	ld a,3
	pop de
	inc de 
	call prg_bank_swap
	ld de,0 
	pop bc 
	pop af 
	jp acknowledge_bankswap 