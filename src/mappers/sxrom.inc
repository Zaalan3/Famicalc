
SXROM.shift_count := mapper_area
SXROM.shift_value := mapper_area + 1
SXROM.prg_mode := mapper_area + 2
SXROM.chr_mode := mapper_area + 3
SXROM.prg_outer := mapper_area + 4
 
; load default mapper configuration and reset any variables
; iy = rom header ptr
SXROM.mapper_reset: 
	xor a,a
	call prg_load_wram
repeat 4 
	index = %-1 
	ld a,index 			; a = virtual address bank (80,A0,C0,E0)
	ld e,$3C + index	; e = 8kb bank
	call prg_bank_swap
end repeat 
repeat 8 
	index = %-1 
	ld a,index 
	ld de,index 
	call chr_bank_swap 
end repeat
	; default register state
	ld a,5 
	ld (SXROM.shift_count),a
	xor a,a 
	ld (SXROM.shift_value),a
	ld (SXROM.chr_mode),a
	ld (SXROM.prg_outer),a
	ld a,3 
	ld (SXROM.prg_mode),a
	
	ld a,(iy+9)
	jp set_mirroring 
	
; as a rule, don't use the exx register set for these. 
; ixl = byte to write. 
; de = address
; preserve af,bc,d(0),iy
SXROM.mapper_write:
	push af 
	ld a,d 
	cp a,$60 
	jr c,.nowrite 
	cp a,$80
	jr c,.wram
	pop af
	ld e,ixl 
	jp SXROM.shift
.wram: 
	ex.sis hl,de 
	ld de,jit_nes_ewram-$6000
	add hl,de 
	ld e,ixl
	ld (hl),e 
.nowrite: 
	pop af 
	ld de,0 
	ret 

; hl = address
; a != 0 if no valid write
; hl = function to inline
SXROM.mapper_get_write_function:
	ld a,h 
	cp a,$60	
	jr c,.nofunc
	cp a,$80 
	jr c,.wram 
	ld (SXROM.write_shift.smc_page),a 
	ld hl,SXROM.write_shift
	xor a,a 
	ret
.wram: 
	ex.sis de,hl
	ld hl,jit_nes_ewram-$6000 
	add hl,de 
	ld (SXROM.write_wram.smc_address),hl 
	ld hl,SXROM.write_wram
	xor a,a  
	ret
.nofunc: 
	ld a,1 
	ret 

SXROM.write_shift: 
	db .end - .start 
.start: 
	ld d,0 
.smc_page:=$-1 
	call SXROM.shift
.end: 

SXROM.write_wram:
	db .end - .start 
.start: 
	ld hl,0
.smc_address:=$-3
	ld (hl),e
.end: 

SXROM.write_region: 
	db .end - .start 
.start:
	ld ixl,e
	ex de,hl
	call SXROM.mapper_write
.end: 
	
; hl = address
; a = FF if no valid write , 1 if region ignores first write(RMW),2 if region ignores second write(RMW), 0 otherwise
; hl = function to inline
; de = address to smc into region_code
; iy = code ptr
; make sure the address is the same register as would be used in mapper_get_read_function
SXROM.mapper_get_write_region_function:
	ld a,h 
	cp a,$60 
	jr c,.nofunc
	ld de,(iy+1)
	ld hl,SXROM.write_region
	ld a,1 
	ret
.nofunc:
	ld a,$FF 
	ret 
	
; returns a = 1 if mapper responds to writeback of RMW instructions, a=2 if it instead responds to modified value
; hl = address
SXROM.mapper_rmw_response: 
	ld a,1 
	ret

; set bit 7 of a if write could cause bankswap
;	abs 		:= 1 shl 3		; write absolute addressing 
;	abs_ind 	:= 1 shl 4		; indexed addressing
; preserve de,iy, and a.
; (iy-1) = address to test
SXROM.mapper_test_bankswap:
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
SXROM.mapper_test_long_branch:
	xor a,a 
	ret 
	
; hl = address 
; carry = true/false
; is this bank fixed to this address? 
SXROM.mapper_test_bank_fixed:
	; whole area readdressable 
	or a,a 
	ret 

; is this address within 256 bytes of a bank cross?
SXROM.mapper_test_bank_cross: 
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
SXROM.mapper_event: 
	ret 

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
; d = page 
; e = byte to write 
SXROM.shift:
	push af 
	bit 7,e 
	jq nz,.reset 
	; shift in new bit (LSB is first shifted in)
	ld hl,SXROM.shift_value  
	ld a,(hl) 
	rr e 
	rra
	ld (hl),a 
	; (hl) = shift count
	dec hl 
	; if 5 writes in a row, change value of register 
	dec (hl) 
	jr z,.register_write 
	pop af 
	ld de,0 
	ret 
.register_write: 
	; reset shift register
	ld (hl),5 
	inc hl 
	ld (hl),0 
	; shift value down to LSB 
	rra 
	rra 
	rra 
	and a,$1F 
	ld l,a 
	; find register to write to
	ld a,d
	cp a,$A0 
	jq c,SXROM.write_control
	cp a,$C0 
	jq c,SXROM.write_chr_0 
	cp a,$E0 
	jq c,SXROM.write_chr_1 
	jq SXROM.write_prg
.reset: 
	; reset shift register 
	ld hl,SXROM.shift_count 
	ld (hl),5 
	inc hl 
	ld (hl),0 
	; set PRG mode to 3 
	inc hl 
	ld (hl),3
	push bc 
	; set last two pages to last banks
	
	ld de,$1E
	; are we in first or second 256KB? 
	ld hl,SXROM.prg_outer
	bit 0,(hl) 
	jr z,$+4 
	ld e,$3E 
	ld a,2 
	push de 
	call prg_bank_swap
	pop de 
	inc de
	ld a,3 
	call prg_bank_swap
	ld de,0
	pop bc 
	pop af 
	jp acknowledge_bankswap

; l = value to write 
SXROM.write_control:
	; CHR mode 
	; 0 = 8KB , 1 = 4KB 
	push bc
	xor a,a 
	bit 4,l 
	jr z,$+3 
	inc a 
	ld (SXROM.chr_mode),a 
	; copy new nametable arrangement 
	ld a,l
	and a,11b 
	ld bc,12 
	ld e,a 
	ld d,c  
	mlt de
	ld a,l 
	ld hl,.nametable_arrangements
	add hl,de 
	ld de,ppu_nametable_ptr 
	ldir 
	; was this during rendering? 
	ld c,a 
	ld a,r 
	rla 
	ld a,c 
	jr nc,.prg_mode 
	; create nametable change event 
	ld ix,jit_scanline_vars
	pop.sis hl 
	ld c,h 
	push.sis hl 
	ld hl,(ppu_event_list) 
	ld (hl),c 
	inc hl 
	ld (hl),ppu_event_mirroring
	inc hl 
	ex de,hl 
	ld hl,ppu_nametable_ptr
	ld c,12 
	ldir 
	ld (ppu_event_list),de 
.prg_mode: 
	; get PRG mode 
	rra 
	rra
	and a,11b 
	ld (SXROM.prg_mode),a
	cp a,2 
	jr nc,.mode01 
	jr z,.mode2 
.mode3: 
	; fix last bank at $C000
	ld a,2 
	ld de,$1E
	jr .bankswap 
.mode2: 
	; fix first bank at $8000 
	ld a,0 
	ld de,0 
	jr .bankswap 
.mode01: 
	; switch 32KB at a time 
	; no work needed 
	ld de,0
	pop bc
	pop af 
	ret
.bankswap: 
	; are we in first or second 256KB? 
	ld hl,SXROM.prg_outer
	bit 0,(hl) 
	jr z,$+4 
	set 5,e
	push af 
	push de 
	call prg_bank_swap
	pop de 
	pop af 
	inc a 
	inc de 
	call prg_bank_swap
	ld de,0
	pop bc 
	pop af 
	jp acknowledge_bankswap
	
	
.nametable_arrangements: 
	emit 3: ppu_nametables,ppu_nametables,ppu_nametables,ppu_nametables
	emit 3: ppu_nametables+2048,ppu_nametables+2048,ppu_nametables+2048,ppu_nametables+2048
	emit 3: ppu_nametables,ppu_nametables+2048,ppu_nametables,ppu_nametables+2048
	emit 3: ppu_nametables,ppu_nametables,ppu_nametables+2048,ppu_nametables+2048
	
; l = value to write
SXROM.write_chr_0:
	push bc 
	call SXROM.write_chr_helper
	; swap CHR banks 
	ld a,l
	; x4 to get 1kb bank 
	add a,a 
	add a,a 
	ld de,0 
	ld e,a
	xor a,a
	ld hl,SXROM.chr_mode
	bit 0,(hl) 
	jq nz,.low_only 
	; mask out low bit in 8KB mode 
	res 2,e 
	ld b,8 
	jr .loop 
.low_only: 
	ld b,4
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
	jp acknowledge_bankswap
	
; l = value to write	
SXROM.write_chr_1:
	push bc 
	; ignore write if in 8KB mode
	ld a,l
	ld hl,SXROM.chr_mode
	bit 0,(hl) 
	jr z,.end 
	ld l,a 
	call SXROM.write_chr_helper 
	; swap CHR banks 
	ld a,l
	; x4 to get 1kb bank 
	add a,a 
	add a,a 
	ld de,0 
	ld e,a
	ld a,4 
	ld b,a
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
.end: 
	ld de,0
	pop bc
	pop af 
	jp acknowledge_bankswap 
	
; code shared between both functions
SXROM.write_chr_helper:
	push hl
	
	xor a,a 
	bit 4,l 
	jr z,$+3 
	inc a 
	ld (SXROM.prg_outer),a 
	; swap PRG bank 
	ld a,l 
	rra 
	rra 
	and a,11b 
	call prg_load_wram
	
	; swap PRG outer bank based on bit 4 
	pop hl 
	push hl 
	ld a,$AB	; res 5,e 
	bit 4,l 
	jr z,$+4
	ld a,$EB ; set 5,e 
	ld (.smc_bit5),a 
	ld hl,prg_page_bank
	xor a,a
.loop: 
	ld de,0 
	ld e,(hl) 
	res 5,e 
.smc_bit5:=$-1
	push hl
	push af 
	call prg_bank_swap
	pop af 
	pop hl
	inc hl 
	inc a
	cp a,4 
	jr nz,.loop
	pop hl
	ret 
	
; l = value to write	
SXROM.write_prg:
	push bc 
	; first or second 256KB ? 
	ex de,hl 
	res 4,e 
	ld hl,SXROM.prg_outer 
	bit 0,(hl)
	jr z,$+4 
	set 4,e 
	
	ld a,(SXROM.prg_mode) 
	cp a,2 
	jr c,.mode01 
	jr z,.mode2 
.mode3: 
	ld a,e 
	; x2 for 8KB bank 
	add a,a 
	ld de,0 
	ld e,a
	ld b,2
	xor a,a 
	jr .loop
.mode2: 
	ld a,e 
	; x2 for 8KB bank 
	add a,a 
	ld de,0 
	ld e,a
	ld b,2
	ld a,b
	jr .loop
.mode01: 
	ld a,e 
	; ignore low bit in 32KB mode 
	res 0,a  
	; x2 for 8KB bank 
	add a,a 
	ld b,4 
	ld de,0 
	ld e,a 
	xor a,a 
	
.loop: 
	push bc 
	push de 
	push af 
	call prg_bank_swap
	pop af 
	pop de 
	pop bc 
	inc de 
	inc a
	djnz .loop 
	
	ld de,0 
	pop bc 
	pop af 
	jp acknowledge_bankswap
	