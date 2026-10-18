include 'ti84pceg.inc'
include 'vars.inc'

section .text

public row_unchanged
public row_forget
public row_frame_start
public row_note_overlays
public row_note_label
public row_mark_lines
public row_scale

; Row skipping. render_background_loop draws the background one tile row at
; a time. Before a row is drawn, row_unchanged checks whether it would look
; exactly like the same row of the last drawn frame: same place on the
; screen, same scroll, and the same tile cache entry for every tile (a hash
; of them). Such a row is not drawn again, unless sprites or labels covered
; some of its lines in the last drawn frame (those pixels must be redrawn).
; Anything that makes the frame buffer or the tile cache untrustworthy
; (cache flushes, the pause menu, state loads, blank frames) calls row_forget.

max_rows := 40
row_size := 10			; line, lines, y_fine, x_start, x_len1, x_len2, mask, hash (3)

; start of render_background
row_frame_start:
	xor a,a
	ld (row_index),a
	ret

; forget every row: the next frame is drawn in full. Destroys af, de, hl
row_forget:
	push bc
	ld hl,row_records
	ld de,row_records+1
	ld bc,max_rows*row_size - 1
	ld (hl),$FF
	ldir
	pop bc
	ret

; in: b = lines in this row, iyh = first line + $20, ix = jit_scanline_vars,
;     MB = $D6 (the tile set), SPL a free stack
; out: z = the row looks as it did in the last drawn frame: don't draw it
; keeps b, ix, iy and the alternate registers; uses SPS
row_unchanged:
	push bc
	ld a,(row_index)
	cp a,max_rows
	jp nc,.draw		; more rows than records: just draw
	ld hl,row_skip_off	; tests/pc compares frames with and without skipping
	bit 0,(hl)
	jp nz,.draw
	inc a
	ld (row_index),a
	dec a
	ld l,a
	ld h,row_size
	mlt hl
	ld de,row_records
	add hl,de
	push hl
	; this row's place and scroll
	ld de,row_key
	ld a,iyh
	ld (de),a
	inc de
	ld a,b
	ld (de),a
	inc de
	ld a,(y_fine)
	ld (de),a
	inc de
	ld a,(x_start)
	ld (de),a
	inc de
	ld a,(x_len1)
	ld (de),a
	inc de
	ld a,(x_len2)
	ld (de),a
	inc de
	ld a,(mask)
	ld (de),a
	; different from the last frame (scrolling): draw, and leave the tile
	; hash out (it can only match again once the scroll stays the same)
	pop de
	push de
	ld hl,row_key
	ld b,row_size-3
	ld c,0
.compare:
	ld a,(de)
	cp a,(hl)
	jr z,.same
	inc c
	ld a,(hl)
	ld (de),a
.same:
	inc hl
	inc de
	djnz .compare
	ld a,c
	or a,a
	jr z,.hash_tiles
	ex de,hl
	ld de,-1		; no hash
	ld (hl),de
	pop de
	jp .draw
.hash_tiles:
	; hash of the tile cache entries, left nametable then right one
	ld de,0
	ld a,(x_len1)
	or a,a
	jr z,.right
	lea hl,t_nametable_0
	ld b,(nametable_select)
	ld c,3
	mlt bc
	add hl,bc
	ld hl,(hl)
	ld b,(x_course)
	ld c,2
	mlt bc
	add hl,bc
	push de
	ld e,64
	ld d,(y_course)
	mlt de
	add hl,de
	pop de
	ld.sis sp,hl
	ld b,a
	call .hash
.right:
	ld a,(x_len2)
	or a,a
	jr z,.hashed
	lea hl,t_nametable_0
	ld a,(nametable_select)
	xor a,1
	ld c,a
	ld b,3
	mlt bc
	add hl,bc
	ld hl,(hl)
	push de
	ld e,64
	ld d,(y_course)
	mlt de
	add hl,de
	pop de
	ld.sis sp,hl
	ld a,(x_len2)
	ld b,a
	call .hash
.hashed:
	; compare with the record's hash and keep the new one
	pop hl
	ld bc,row_size-3
	add hl,bc
	ld bc,(hl)
	ld (hl),de
	ex de,hl
	or a,a
	sbc hl,bc
	jr nz,.draw
	; same tiles: still draw it if sprites or labels covered it
	pop bc
	push bc
	ld a,iyh
	sub a,$20
	ld hl,dirty_lines
	ld de,0
	ld e,a
	add hl,de
.lines:
	ld a,(hl)
	or a,a
	jr nz,.draw
	inc hl
	djnz .lines
	ld hl,(rows_skipped)
	inc hl
	ld (rows_skipped),hl
	pop bc
	xor a,a			; z: skip
	ret
.draw:
	pop bc
	ld a,iyh
	sub a,$20
	call row_mark_lines
	or a,1			; nz: draw
	ret

; de = hash, b = tiles at SPS (nametable entries)
.hash:
	pop.sis hl		; nametable entry
	add hl,hl
	ld.sis hl,(hl)		; its tile cache entry (MB = $D6)
	ex de,hl
	add hl,hl		; hash = rotate left 1 + entry
	jr nc,$+3
	inc hl
	add hl,de
	ex de,hl
	djnz .hash
	ret

; a = first line, b = lines: drawn this frame. Keeps bc
row_mark_lines:
	push bc
	ld hl,lines_drawn
	ld de,0
	ld e,a
	add hl,de
.mark:
	ld (hl),1
	inc hl
	djnz .mark
	pop bc
	ret

; The wide picture: the lines that changed this frame (drawn, or covered by
; this frame's sprites) are stretched from vbuffer (256 pixels per line) to
; wide_buffer (320): every 4th pixel twice. Destroys af, bc, de, hl, ix, iy
row_scale:
	ld a,(_wide_buffer+2)
	or a,a
	ret z
	ld ix,lines_drawn
	ld iy,dirty_lines
	ld hl,vbuffer
	ld de,(_wide_buffer)
	ld b,224
.line:
	ld a,(ix+0)
	or a,(iy+0)
	ld (ix+0),0
	jr z,.next
	push bc
	push de
	push hl
	call scale_line
	pop hl
	pop de
	pop bc
.next:
	inc h
	push bc
	ex de,hl
	ld bc,320
	add hl,bc
	ex de,hl
	pop bc
	inc ix
	inc iy
	djnz .line
	ret

; hl = 256 pixels -> de = 320 pixels
scale_line:
repeat 64
	ld bc,4
	ldir
	dec hl
	ldi
end repeat
	ret

; After a frame is drawn: note which lines its sprites and labels cover; the
; next frame draws the background rows there again. Destroys af, bc, de, hl, ix
row_note_overlays:
	ld hl,dirty_lines
	ld de,dirty_lines+1
	ld bc,224-1
	ld (hl),0
	ldir
	; sprites: drawn from frame buffer row OAM y - 8 (render_sprites), up to 16 rows
	ld ix,ppu_oam
	ld b,64
.sprite:
	ld a,(ix+0)
	cp a,239
	jr nc,.next_sprite
	push bc
	ld c,16			; rows
	sub a,8
	jr nc,.top_ok
	add a,c			; starts above the top: fewer rows
	jr nc,.skip		; all of them above
	ld c,a
	xor a,a
.top_ok:
	or a,a
	sbc hl,hl
	ld l,a			; first row
	add a,c
	sub a,224
	jr c,.bottom_ok
	neg			; c - overflow rows
	add a,c
	jr z,.skip
	ld c,a
.bottom_ok:
	ld de,dirty_lines
	add hl,de
	ld (hl),1
	ld a,c
	dec a
	jr z,.skip
	ld bc,0
	ld c,a
	push hl
	pop de
	inc de
	ldir
.skip:
	pop bc
.next_sprite:
	lea ix,ix+4
	djnz .sprite
	ret

; a label covers 12 rows from row a. Destroys af, b, de, hl
row_note_label:
	ld hl,dirty_lines
	ld de,0
	ld e,a
	add hl,de
	ld b,12
.row:
	ld (hl),1
	inc hl
	djnz .row
	ret

extern _wide_buffer

section .bss

public row_skip_off
public rows_skipped
private row_index
private row_key
private row_records
private dirty_lines
private lines_drawn
row_skip_off: rb 1
rows_skipped: rb 3		; for tests/pc
row_index: rb 1
row_key: rb row_size
row_records: rb max_rows*row_size
dirty_lines: rb 224
lines_drawn: rb 224
