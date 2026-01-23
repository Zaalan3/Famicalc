;use this as an example for writing mappers going forward 
; any mapper specific variables need to go to mapper_area (max. 1.5 kb)

TXROM.R0 := mapper_area 
TXROM.R1 := mapper_area + 1
TXROM.R2 := mapper_area + 2 
TXROM.R3 := mapper_area + 3
TXROM.R4 := mapper_area + 4
TXROM.R5 := mapper_area + 5
TXROM.R6 := mapper_area + 6
TXROM.R7 := mapper_area + 7

TXROM.bank_register := mapper_area + 8
TXROM.irq_latch := mapper_area + 9
TXROM.irq_enabled := mapper_area + 10 
TXROM.mirroring := mapper_area + 11
TXROM.irq_line := mapper_area + 12

; load default mapper configuration and reset any variables
; iy = rom header ptr
TXROM.mapper_reset: 
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
	; set irq test line to dummy line 
	ld hl,$800 + 264*2 
	ld (TXROM.irq_line),hl
	
	ld a,(iy+9)
	ld (TXROM.mirroring),a 
	; MMC3 has an alternate scheme with 4 nametables
	bit 1,a 
	jp z,set_mirroring
	ld hl,ppu_nametables
	ld de,2048
	ld (ppu_nametable_ptr),hl
	add hl,de 
	ld (ppu_nametable_ptr+3),hl
	add hl,de 
	ld (ppu_nametable_ptr+3*2),hl
	add hl,de
	ld (ppu_nametable_ptr+3*3),hl
	ret
	
; as a rule, don't use the exx register set for these. 
; ixl = byte to write. 
; de = address
; preserve af,bc,d(0),iy
TXROM.mapper_write:
	push af 
	ld a,d
	cp a,$60
	jr c,.nowrite 
	cp a,$80 
	jr nc,.find_function
	ex.sis hl,de 
	ld de,jit_nes_ewram-$6000
	add hl,de 
	ld e,ixl 
	ld (hl),e 
	ld de,0
.nowrite: 
	pop af
	ret 
.find_function: 
	; index = (H>>4)&0b0110 + (L&1)
	rra
	rra
	rra
	rra
	and a,0110b
	rr e 
	adc a,0 
	ld l,a 
	ld h,3 
	mlt hl 
	ld de,TXROM.function_lut
	add hl,de 
	ld hl,(hl)
	ld de,0 
	ld e,ixl 
	pop af
	jp (hl)


; hl = address
; a != 0 if no valid write
; hl = function to inline
TXROM.mapper_get_write_function:
	ld a,h 
	cp a,$60	
	jr c,.nofunc
	cp a,$80 
	jr c,.wram
	; index = (H>>4)&0b0110 + (L&1)
	rra
	rra
	rra
	rra
	and a,0110b
	rr l 
	adc a,0 
	ld l,a 
	ld h,3 
	mlt hl 
	ld de,TXROM.function_lut
	add hl,de 
	ld hl,(hl) 
	ld (TXROM.write_register.smc_func),hl 
	ld hl,TXROM.write_register
	xor a,a 
	ret
.wram: 
	ex.sis de,hl
	ld hl,jit_nes_ewram-$6000 
	add hl,de 
	ld (TXROM.write_wram.smc_address),hl 
	ld hl,TXROM.write_wram
	xor a,a  
	ret
.nofunc: 
	ld a,1 
	ret 


TXROM.function_lut: 
	emit 3: TXROM.bank_select,TXROM.bank_data
	emit 3: TXROM.nametable_select,TXROM.write_protect
	emit 3: TXROM.irq_set,TXROM.irq_reload
	emit 3: TXROM.irq_disable,TXROM.irq_enable

TXROM.write_register: 
	db .end - .start 
.start: 
	call 0
.smc_func:=$-3
.end: 

TXROM.write_wram:
	db .end - .start 
.start: 
	ld hl,0
.smc_address:=$-3
	ld (hl),e
.end: 
	
; hl = address
; a = FF if no valid write , 1 if region ignores first write(RMW),2 if region ignores second write(RMW), 0 otherwise
; hl = function to inline
; de = address to smc into region_code
; make sure the address is the same register as would be used in mapper_get_read_function
TXROM.mapper_get_write_region_function:
	ld a,h 
	cp a,$60 
	jr c,.nofunc
	ld de,(iy+1)
	ld hl,TXROM.write_region
	ld a,1 
	ret
.nofunc:
	ld a,$FF 
	ret  

TXROM.write_region: 
	db .end - .start 
.start:
	ld ixl,e
	ex de,hl
	call TXROM.mapper_write
.end: 
	
; returns a = 1 if mapper responds to writeback of RMW instructions, a=2 if it instead responds to modified value
TXROM.mapper_rmw_response: 
	ld a,2 
	ret
	
; set bit 7 of a if write could cause bankswap
;	abs 		:= 1 shl 3		; write absolute addressing 
;	abs_ind 	:= 1 shl 4		; indexed addressing
; preserve de,iy, and a.
; (iy-1) = address to test
TXROM.mapper_test_bankswap:
	ld c,a 
	bit 4,c 
	ld b,$80 
	jr z,$+3
	dec b 
	ld a,(iy-1) 
	; $8000 to $9FFF 
	cp a,b 
	jr c,.end 
	cp a,$A0 
	jr nc,.end 
	set 7,c
.end: 
	ld a,c 
	ret
	

; a=0 if branch is inside of current memory page, otherwise a != 0 
; hl = new memory address
; bc = old memory address
; only values of $8000+ will be given
; preserve de. 
TXROM.mapper_test_long_branch:
	; if any of top 3 bits are different, a != 0 
	ld a,h 
	xor a,b
	and a,11100000b 
	ret  
	
; hl = address 
; carry = true/false
; is this bank fixed to this address? 
TXROM.mapper_test_bank_fixed:
	; true if address >= $E000
	ld a,$E0 - 1 
	cp a,h
	ret 
	
; is this address within 256 bytes of a bank cross?
TXROM.mapper_test_bank_cross: 
	; border of each 8Kb bank
	ld a,h 
	cp a,$9F 
	jr z,.true
	cp a,$BF 
	jr z,.true
	cp a,$DF 
	jr z,.true
	cp a,$FF 		; page $FFxx
	jr z,.true 
	or a,a
	ret 
.true: 
	scf 
	ret 

; Scanline counter IRQ test 
TXROM.mapper_event:
	push af 
	ld ix,jit_scanline_vars
	ld hl,(TXROM.irq_line) 
	res.sis scan_event_mapper,(hl)
	ld a,(TXROM.irq_enabled) 
	or a,a 
	jr z,.schedule 
	; If rendering is off, the counter doesn't tick
	ld a,(ppu_mask) 
	and a,11000b 
	jr z,.schedule
	set 2,(irq_sources) 
.schedule: 
	; find when next scanline event occurs 
	
	; only can trigger during active video
	or a,a 
	sbc hl,hl 
	add.sis hl,sp 
	ld de,$800 + 240*2 
	or a,a 
	sbc hl,de
	add hl,de
	jr c,$+6
	ld hl,$800
	
	ld a,(TXROM.irq_latch) 
	ld de,0
	ld e,a 
	add hl,de 
	add hl,de
	
	ld de,$800 + 240*2 
	or a,a 
	sbc hl,de 
	jr c,$+6
	ld de,$800 
	add hl,de
	set.sis scan_event_mapper,(hl) 
	ld (TXROM.irq_line),hl
	pop af
	ret 
	
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


TXROM.bank_select: 
	push af 
	ld hl,TXROM.bank_register
	ld a,(hl) 
	ld (hl),e 
	; Was the banking scheme changed? 
	and a,11000000b 
	xor a,e
	ld e,a
	jq z,.end  
	rl e 
	jr nc,.prg 
.chr: 
	push de 
	; 2 KB banks 
	call TXROM.load_R0 
	call TXROM.load_R1 
	; 1 KB banks 
	call TXROM.load_R2
	call TXROM.load_R3
	call TXROM.load_R4
	call TXROM.load_R5
	pop de 
.prg: 
	rl e 
	jr nc,.end
	; Is $8000-$9FFF or $C000-$DFFF fixed to second-to-last bank? 
	ld a,(TXROM.bank_register)
	bit 6,a 
	ld a,2 
	jr z,$+3 
	xor a,a 
	ld e,63 - 1 
	call prg_bank_swap
	
	call TXROM.load_R6 
	; R7 always points to same slot, so no need to update
	pop af 
	ld de,0 
	jp acknowledge_bankswap
.end:
	pop af 
	ld de,0
	ret 

TXROM.bank_data: 
	push af
	push bc
	; load appropriate register 
	ld a,(TXROM.bank_register)
	and a,111b
	ld hl,TXROM.R0 
	ld l,a 
	ld (hl),e
	; find update function
	ld l,a 
	ld h,3 
	mlt hl 
	ld de,.register_write_lut
	add hl,de
	ld hl,(hl)
	cp a,6 
	jr c,.chr
.prg: 
	call .callhl 
	ld de,0 
	pop bc
	pop af
	jp acknowledge_bankswap
.chr: 
	call .callhl
	ld de,0 
	pop bc
	pop af
	ret 
.callhl: 
	jp (hl)
.register_write_lut: 
	emit 3: TXROM.load_R0,TXROM.load_R1,TXROM.load_R2,TXROM.load_R3
	emit 3: TXROM.load_R4,TXROM.load_R5,TXROM.load_R6,TXROM.load_R7
	
	
TXROM.nametable_select: 
	; writes ignored if in 4-Nametable configuration
	ld hl,TXROM.mirroring 
	bit 2,(hl) 
	ret nz 
	push af
	push bc 
	ld hl,.nametable_arrangements
	ld bc,12 
	bit 0,e  
	jr z,$+3
	add hl,bc 
	ld de,ppu_nametable_ptr 
	ldir
	ld a,r 
	rla  
	jr nc,.end 
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
.end: 
	pop bc 
	pop af 
	ld de,0
	ret 
	
.nametable_arrangements:
	emit 3: ppu_nametables,ppu_nametables+2048,ppu_nametables,ppu_nametables+2048
	emit 3: ppu_nametables,ppu_nametables,ppu_nametables+2048,ppu_nametables+2048
	
; This register is ignored to keep compatibility with MMC6 
TXROM.write_protect:
	ld de,0
	ret 

TXROM.irq_set: 
	ld hl,TXROM.irq_latch
	ld (hl),e 
	ret

TXROM.irq_reload: 
	push af 
	ld ix,jit_scanline_vars
	ld hl,(TXROM.irq_line) 
	res.sis scan_event_mapper,(hl)
	jq TXROM.mapper_event.schedule 
	
TXROM.irq_enable: 
	ld hl,TXROM.irq_enabled 
	ld (hl),1 
	ret 

TXROM.irq_disable:
	ld ix,jit_scanline_vars
	res 2,(irq_sources)
	ld hl,TXROM.irq_enabled 
	ld (hl),0
	ret 


; 2KB 
TXROM.load_R0: 
	; fetch 2kB chr bank
	ld a,(TXROM.R0) 
	ld e,0
	jr TXROM.R01_shared
	
TXROM.load_R1: 
	; fetch 2kB chr bank
	ld a,(TXROM.R1) 
	ld e,2
	
; e = first slot 
; a = new bank
TXROM.R01_shared:
	or a,a 
	sbc hl,hl 
	; only even banks selectable
	res 0,a
	ld l,a
	ex de,hl 
	; Does this bank point to $0000 or $1000?
	ld a,(TXROM.bank_register)
	bit 7,a 
	ld a,l
	jr z,$+4 
	add a,4 
	push af 
	push de 
	call chr_bank_swap_render
	pop de 
	pop af 
	inc a 
	inc de 
	jp chr_bank_swap_render
	

TXROM.load_R2: 
	; fetch 1kB chr bank
	ld a,(TXROM.R2) 
	ld e,4
	jr TXROM.R2345_shared

TXROM.load_R3: 
	; fetch 1kB chr bank
	ld a,(TXROM.R3) 
	ld e,5
	jr TXROM.R2345_shared

TXROM.load_R4: 
	; fetch 1kB chr bank
	ld a,(TXROM.R4) 
	ld e,6
	jr TXROM.R2345_shared

TXROM.load_R5: 
	; fetch 1kB chr bank
	ld a,(TXROM.R5) 
	ld e,7

; e = slot 
; a = new bank
TXROM.R2345_shared: 
	or a,a 
	sbc hl,hl 
	ld l,a
	ex de,hl 
	; Does this bank point to $0000 or $1000?
	ld a,(TXROM.bank_register)
	bit 7,a 
	ld a,l
	jr z,$+4 
	sub a,4 
	jp chr_bank_swap_render
	
; PRG ROM registers 
TXROM.load_R6: 
	ld a,(TXROM.R6) 
	ld e,a 
	ld a,(TXROM.bank_register) 
	bit 6,a
	ld a,0 
	jr z,$+4 
	ld a,2 
	jp prg_bank_swap 
	
TXROM.load_R7: 
	ld a,(TXROM.R7) 
	ld e,a 
	ld a,1 
	jp prg_bank_swap
	
	