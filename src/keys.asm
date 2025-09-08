include 'ti84pceg.inc' 

section .text

public get_keys

; a = NES key result
get_keys:
	ld a,(ti.mpKeyData+2) 	; return if DEL down 
	and a,$80 
	jp nz,_startJIT.return
	
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
	
extern _startJIT.return