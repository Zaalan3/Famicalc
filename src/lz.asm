	
section .text 

public lz_compress
public lz_decompress

extern jit_nes_ewram

hash_table := jit_nes_ewram

; 0LLL LLLL : literal block of length L
; 10LL LLLL NNNN NNNN : Reference to offset N+1 with length L+3
; 11LL LLLL NNNN NNNN NNNN NNNN : Reference to offset N+1 with length L+3


	; iy = input 
	; hl = output
	; de = length
	; OUT: 
	; hl = end of output
lz_compress:
	push ix 
	call .start 
	pop ix 
	exx 
	ret 
.start: 
	ld (hl),$FF
	push de
	push hl 
	pop de 
	exx
	lea hl,iy
	ld (.smc_input),hl
	pop de
	add hl,de 
	ld (.smc_input_end),hl
	; reset hash table
	ld hl,hash_table 
	ld de,hash_table+1 
	ld (hl),$FF 
	ld bc,32768 - 1
	ldir 
.next: 
	lea hl,iy 
	ld de,0
.smc_input_end:=$-3
	or a,a 
	sbc hl,de 
	ret nc
	; find hash for value 

	;((h >> 10) - h) & 0x3FFF
	ld d,(iy+1) 
	ld e,(iy+0)
	ld h,(iy+2)
	ld l,d 
	srl h 
	rr l 
	srl h 
	rr l 
	or a,a
	sbc.sis hl,de
	ld a,h
	and a,$3F 
	ld h,a 
	
	; find slot and offset for hash value,
	; and replace previous offset with current
	add hl,hl 
	ld de,hash_table
	add hl,de
	lea de,iy
	ld c,(hl)
	ld (hl),e 
	inc hl 
	ld b,(hl)
	ld (hl),d 
	; is this slot empty? 
	inc b 
	jq z,.literal_byte
	dec b 
	ld ix,0
.smc_input:=$-3
	add ix,bc
	; find match length
	lea hl,iy
	lea de,ix
	ld b,63+3
.match: 
	ld a,(de) 
	cp a,(hl) 
	jr nz,.match_length 
	inc hl 
	inc de 
	djnz .match
.match_length:
	; match length = 0 => literal byte 
	ld a,63+3
	sub a,b
	ld b,a
	; match needs to be at least 3 bytes to break even 
	sub a,3
	jq c,.literal_byte
	; mark command as reference 
	set 7,a 
	; find offset from reference
	or a,a 
	sbc hl,de 
	dec hl 
	; only output 2 reference bytes if more than 256 bytes away
	inc h 
	dec h 
	jq z,$+4 
	set 6,a 
	; write reference command 
	push hl 
	exx
	pop bc 
	ld (hl),a 
	inc hl
	ld (hl),c
	inc hl 
	bit 6,a 
	jr z,$+4 
	ld (hl),b 
	inc hl 
	; reset literal pointer 
	ld (hl),$FF
	push hl 
	pop de 
	exx 
	; add match length to offset 
	ld c,b 
	ld b,0 
	add iy,bc
	jq .next 

.literal_byte: 
	exx 
	; if literal command ptr = end of output, fetch new literal byte
	or a,a 
	sbc hl,de 
	jr nz,$+3 
	inc hl 
	add hl,de
	; if > 128 literal bytes, grab a new literal 
	ex de,hl 
	inc (hl) 
	jp p,.no_new_command 
	dec (hl) 
	push de 
	pop hl 
	ld (hl),0
	inc de 
.no_new_command: 
	ld a,(iy) 
	ld (de),a 
	inc de 
	ex de,hl 
	inc iy 
	exx 
	jq .next 


; iy = input 
; hl = output
; de = length
lz_decompress: 
	add hl,de
	ld (.smc_output_end),hl 
	or a,a
	sbc hl,de
	ex de,hl
	ld bc,0
.next: 
	ld hl,0 
.smc_output_end:=$-3 
	sbc hl,de 
	ret z 
	ret c 
	
	ld a,(iy) 
	bit 7,a 
	jr nz,.match 
.literal: 
	; copy A+1 bytes from input 
	ld c,a 
	inc bc 
	inc iy 
	lea hl,iy
	add iy,bc 
	ldir 
	jr .next 
.match: 
	; find offset 
	or a,a 
	sbc hl,hl 
	ld l,(iy+1)
	lea iy,iy+2
	; if bit 6 is set, fetch 2 bytes for offset 
	bit 6,a 
	jr z,.cont
	ld h,(iy) 
	inc iy 
.cont: 
	; subtract offset from current location 
	inc hl 
	push de 
	ex de,hl 
	or a,a 
	sbc hl,de 
	pop de
	; length = bottom 6 bits + 3 
	and a,$3F 
	add a,3 
	ld c,a 
	ldir 
	jq .next 
	