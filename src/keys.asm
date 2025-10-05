include 'ti84pceg.inc' 
include 'vars.inc'

section .text

public get_keys

; a = NES key result
get_keys:
	ld a,(ti.mpKeyData+2) 	; return if DEL down 
	and a,$80 
	jp nz,_startJIT.return
	
	call set_save_slot 
	
	ld hl,ti.mpKeyData
	ld de,key_list 
	ld b,8 
	ld c,0 
.loop: 
	ld a,(de) 	; read row 
	inc de
	add a,a 
	add a,$10 
	ld l,a 
	ld a,(de) 	; read column 
	inc de 
	and a,(hl)
	jr z,.off 	; shift in bit
.on: 	
	scf 
.off:
	rl c 
	djnz .loop 	
	ld a,c 
	ld de,0
	ret

key_list: 
	db 1,1 shl 5 		; A
	db 2,1 shl 7 		; B
	db 3,1 shl 7 		; Select
	db 1,1 shl 6 		; Start
	db 7,1 shl 3 		; Up
	db 7,1 shl 0 		; Down
	db 7,1 shl 1 		; Left
	db 7,1 shl 2 		; Right
	

set_save_slot: 
	ld ix,jit_scanline_vars
	ld hl,ti.mpKeyData
	ld de,number_list 
	ld b,10 
	ld c,0 
.loop: 
	ld a,(de) 	; read row 
	inc de
	add a,a 
	add a,$10 
	ld l,a 
	ld a,(de) 	; read column 
	inc de 
	and a,(hl)
	jr z,.off 	
.on:
	ld a,b
	dec a 
	ld (save_slot),a 
	add a,$30 
	ld (.message_num),a 
	ld hl,.message 
	ld (message_ptr),hl 
	ld (message_len),30 
	ret 
.off:
	djnz .loop 	
	ret

.message: 
	db 'Save Slot '
.message_num: 
	db '0',0
	
number_list: 
	db 5,1 shl 3 		; 9
	db 4,1 shl 3 		; 8
	db 3,1 shl 3 		; 7
	db 5,1 shl 2 		; 6
	db 4,1 shl 2 		; 5
	db 3,1 shl 2 		; 4
	db 5,1 shl 1 		; 3
	db 4,1 shl 1 		; 2
	db 3,1 shl 1 		; 1
	db 3,1 shl 0 		; 0
	
	
extern _startJIT.return