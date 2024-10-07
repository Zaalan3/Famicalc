

store De Bruijn sequences of 4 pixels (4^4 = 256) for each palette, then cache tiles as 2 indexes into the sequence per line (same size as native!) 


	

store 32x4 bytes per nametable line 
	+0	: palette offset 
	+2	: tile offset 

keep list of flags to update nametables at end of frame
(keep LUT of palette byte -> offsets?)


palette layout: 

0000tttt - bg colors 
ssss1111 - sprite colors 
1111tttt - bg colors (reversed) 


high_priority_sprite:
	ld c,$10
	ld hl,sprite_palette_patterns 
.start: 
	ld b,2
.loop: 
	ld h,(iy+0)
	inc iy
repeat 4
	ld a,(de) 
	cp a,c 		; skip if pixel already drawn to (any bit in top nibble set) 
	jr nc,$+4 	; 14/7 
	or a,(hl) 	; set sprite pixel 
	ld (de),a 
	inc e 
	inc l
end repeat 
	djnz .loop
	inc d 
	ld e,ixl 
	dec ixh 
	jr nz,.start 
	ret 
	
	
	bit 7,(hl) 
	jr nz,.skip 
	ld a,(de) 
	rla 
	jr nc,.skip 
	
	inc hl 
	inc e 
	
low_priority_sprite:
	ld hl,sprite_palette_patterns
.start: 
	ld c,2 
.outer: 
	ld h,(iy+0)
	inc iy
	ld b,2
.inner:
repeat 2
	bit 3,(hl)	; skip if sprite pixel is transparent (opaque pixels = xF)
	jr z,$+11
	ld a,(de) 
	cp a,$10 		; skip if pixel already drawn to (any bit in top nibble set) 
	jr nc,$+6 
	or a,$F0  	; set top nibble to F if pixel is opaque. 
	or a,(hl) 	; set sprite pixel 
	ld (de),a 
	inc e 
	inc hl 
end repeat 
	djnz .inner 
	dec c 
	jr nz,.outer 
	inc d 
	ld e,ixl 
	dec ixh 
	ret z  
	jr .start 


	; line: 
	; +0 # of lines (0=special cases)
	; +1 y offset 
	; +2 x offset 
	; +3 len1 (len2 = 31 - len1) 
	; +4 nametable1 
	; +6 nametable2 
	; bg disabled lines: 
	; +0 0 
	; +1 0
	; +2 #lines to blank 
	; bankswap: 
	; +0 0 
	; +1 1 
	; +2 bank0..3 

next_line: 
	ld ixh,d
	ld hl,i 		; hl = line ptr
	ld a,(hl) 
	inc hl 
	or a,a 
	jq z,.special 	; len=0 
	rra
	ld d,a
	jq c,.odd		; draw an extra line
	ld iy,0 
	add iy,sp 		; y offset 
	ld a,(hl)
	inc hl 
	ld iyl,a 
	ld sp,iy 
	ld a,(hl) 		; x offset 
	inc hl 
	ld ixl,a 
	ld c,(hl)		; len1 
	inc hl 
	ld a,31 		; len2 = 31 - len1 
	sub a,c 
	ld e,a 
	ld iy,(hl) 		; nametable ptr 1 
	ld.sis sp,iy 
	inc hl 
	inc hl 
	ld i,hl 		; 
	ld hl,.part2	; hl = return address 
	ld a,4 
	jp drawtiles 
.part2: 
	ld hl,i 		; nametable ptr 2 
	ld iy,(hl) 
	ld.sis sp,iy 
	inc hl 
	inc hl 
	ld i,hl 
	ld c,e
	ld hl,next_line
	jp drawtiles
	
	
; #lines > 1 
drawtiles: 
.outer: 
	exx
	ld d,ixh
	ld ixl,e	; +8
	ld hl,(iy+0) ; l = tile#, h = palette
	ld a,h 
	ld h,c 
	add hl,hl 	 ; get offset for tile
	ld.sis hl,(hl)
	ld.sis sp,hl 
	exx 
	ld b,d 		; number of lines to draw
	ld h,a
	ld sp,hl 
.inner:
	exx 
	ld e,ixl 
	pop.sis hl 	; fetch line bytes 
	ld a,h 
	ld h,b 
	add hl,sp 	; set palette 
	ld c,4
	ldir 
	ld l,a  
	ld c,4 
	ldir 
	inc d 
	exx 
	djnz .inner ;107 / loop
	lea iy,iy+2
	dec c
	djnz .outer 
	jp nextline 
 


; update tiles
.loop:
repeat 16
11	ldi 
2	inc de 
end repeat 
2	dec a 
7	jr nz,.loop 
	ret 
	 
; update attributes
.start: 
	ld b,8 
.loop:
	ld a,(de)	; compare to last frame's attributes 
	cp a,(hl) 
	jr nz,.update 
.return: 
	inc hl
	inc de 
	lea iy,iy+8 
	djnz .loop 
	lea iy,iy+96		; inner loop adds 64, 96+32=128
	dec c 
	jr nz,.start 
	
.update: 
	exx
	ld b,a 		; fetch pattern 
	
	and a,c 	; mask out top six bits
	rr b 
	rr b 
	
	ld (iy+0),a 
	ld (iy+2),a 
	ld (iy+64),a 
	ld (iy+64+2),a 
	
	ld a,b 
	and a,c 
	rr b 
	rr b 
	
	ld (iy+4),a
	ld (iy+6),a
	ld (iy+64+4),a
	ld (iy+64+6),a
	
	add iy,de 	; iy += 128 
	
	ld a,b 
	and a,c 
	rr b 
	rr b 
	
	ld (iy+0),a 
	ld (iy+2),a 
	ld (iy+64),a 
	ld (iy+64+2),a 
	
	ld a,b 
	and a,c 
	
	ld (iy+4),a
	ld (iy+6),a
	ld (iy+64+4),a
	ld (iy+64+6),a
	
	exx 
	jq .return 
	
	

.loop: 
	ld a,(iy+0) 
	ld (hl),a 
	inc hl
	inc hl 
	ld (hl),a 