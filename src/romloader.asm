
include 'ti84pceg.inc' 

section .text 

public _startJIT
public _startJIT.return 

public prg_bank_swap
public chr_bank_swap
public prg_load_wram

include 'vars.inc' 


_startJIT: 
	push ix
	call init_emulator
	ld (.smc_sp),sp 
	; fetch start address
	jp jit_reset
.return: 
	ld sp,0
.smc_sp:=$-3
	pop ix 
	ld a,$D0
	ld mb,a
	ret 

init_emulator:
	; parse NES header and load prg and chr pages 
	
	; point io regions to empty page
	ld bc,256*3 
	ld hl,jit_translation_buffer
	ld de,jit_reserved+128
	ld (hl),de 
	push hl 
	pop de 
	inc de
	inc de
	inc de
	ldir 
	
	; init systems
	ld iy,(_header)
	ld a,(iy+8)		; mapper id
	call mapper_init
	call jit_init
	call io_init 
	
	; init ram addresses in translation buffer 
	ld bc,256 
	ld hl,jit_translation_buffer
	ld de,jit_nes_iwram+128
	ld a,8 		; 2kb = 8 256b pages
	call prg_bank_swap.loop 
	;mirrors 
	ex de,hl 
	ld hl,jit_translation_buffer
	ld bc,3*3*8
	ldir

	ret 
	

; a = 6502 address (80,A0,C0,or E0)
; e = 8kb bank 
prg_bank_swap:
	or a,a
	ld l,a 
	ld h,$20 
	mlt hl 
	ld a,l 
	add a,$80 
	ld h,a 
	ld l,3
	mlt hl 
	ld a,e 
	ld de,jit_translation_buffer
	add hl,de 
	push hl 
	ld e,a 
	ld d,3 
	mlt de 
	ld hl,_prg_banks 
	add hl,de 
	ld hl,(hl) 
	ld bc,128 	; store middle of each page 
	add hl,bc 
	ex de,hl 
	pop hl 
	ld bc,256
	ld a,32 	; 8kb = 32 256b pages
.loop: 
	ld (hl),de 	; update translation buffer 
	inc hl
	inc hl 
	inc hl 
	ex de,hl 
	add hl,bc 
	ex de,hl 
	dec a 
	jr nz,.loop
	ret 

; a = 1kb page (0..7)  
; de = 1kb bank 
chr_bank_swap: 
	or a,a 
	sbc hl,hl 
	add hl,de	; *3 
	add hl,de
	add hl,de
	ex de,hl
	ld h,3 
	ld l,a 
	mlt hl 
	ld bc,ppu_chr_ptr
	add hl,bc 
	ex de,hl 
	ld bc,_chr_banks
	add hl,bc 
	ld hl,(hl) 
	ex de,hl 
	ld (hl),de 
	ret 
	
; loads prg ram page at $6000
; a = 8kb page(0..3) 
prg_load_wram: 
	ld d,a 
	ld e,$20
	mlt de 
	ld d,e 
	ld e,128
	ld hl,jit_nes_ewram
	add hl,de 
	ex de,hl 
	ld hl,jit_translation_buffer+3*$60
	ld a,32
	ld bc,256 
	jp prg_bank_swap.loop


section .bss 

public _header 
public _prg_banks 
public _chr_banks 

_header: rb 3			; pointer to rom header
_prg_banks: rb 3*64		; list of 8kb prg banks, with mirroring applied
_chr_banks: rb 3*512		; list of 1kb chr banks

	
extern jit_init
extern io_init 
extern mapper_init 
extern render_init 
extern render_cleanup

extern jit_translation_buffer
extern ppu_nametable_ptr
extern ppu_chr_ptr
extern jit_nes_ewram
extern jit_convert
extern jit_reset
