include 'ti84pceg.inc'
include 'vars.inc'

section .text

public row_unchanged
public row_forget
public row_frame_start
public row_note_overlays
public row_note_label

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
	; this row's key
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
	ld (row_key+7),de
	; compare with the record, then keep the new key
	pop de			; record
	ld hl,row_key
	ld b,row_size
	ld c,0			; differences
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
	jp nz,.draw
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
	pop bc
	xor a,a			; z: skip
	ret
.draw:
	pop bc
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

; After a frame is drawn: note which lines its sprites and labels cover; the
; next frame draws the background rows there again. Destroys af, bc, de, hl
row_note_overlays:
	ld hl,dirty_lines
	ld de,dirty_lines+1
	ld bc,224-1
	ld (hl),0
	ldir
	; sprites: drawn from frame buffer row OAM y - 8 (render_sprites), up to 16 rows
	ld hl,ppu_oam
	ld b,64
.sprite:
	ld a,(hl)
	cp a,239
	jr nc,.next_sprite
	push hl
	push bc
	sub a,8
	ld c,a			; first row (can be below 0)
	ld b,16+1		; rows, enough for 8x16 sprites
	ld hl,dirty_lines
	ld de,0
.rows:
	ld a,c
	cp a,224
	jr nc,.off		; above the top (negative) or below the bottom
	ld e,a
	push hl
	add hl,de
	ld (hl),1
	pop hl
.off:
	inc c
	djnz .rows
	pop bc
	pop hl
.next_sprite:
	inc hl
	inc hl
	inc hl
	inc hl
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

section .bss

public row_skip_off
private row_index
private row_key
private row_records
private dirty_lines
row_skip_off: rb 1
row_index: rb 1
row_key: rb row_size
row_records: rb max_rows*row_size
dirty_lines: rb 224
