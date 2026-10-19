include 'ti84pceg.inc'
include 'vars.inc'

section .text

public row_unchanged
public row_forget
public row_frame_start
public row_note_overlays
public row_note_label
public row_partial
public row_partial_on

; Row skipping. render_background_loop draws the background one tile row at
; a time. Before a row is drawn, row_unchanged checks whether it would look
; exactly like the same row of the last drawn frame: same place on the
; screen, same scroll, and the same tile cache entry for every tile (a hash
; of them). Such a row is not drawn again, except for the tiles that sprites
; or labels covered in the last drawn frame (row_partial redraws those).
; Anything that makes the frame buffer or the tile cache untrustworthy
; (cache flushes, the pause menu, state loads, blank frames) calls row_forget.

max_rows := 28			; bands of 8 lines

key_size := 9			; line, lines, y_fine, x_start, x_len1, x_len2, mask, y_course, nametable_select
hash_at := key_size		; the tiles' hash (3)
sig_at := key_size + 3		; the banks and nametables they came from (3)
row_size := key_size + 6

; start of render_background
; Unless a nametable was written (nt_changed), a tile translated (the
; tile cache's render_tile_next moved) or CHR RAM changed since the last
; drawn frame, a row with the same place, scroll, banks and nametables has the
; same tiles: no need to hash them (rs_same). ix = jit_scanline_vars
row_frame_start:
	ld a,(nt_changed)
	ld (nt_changed),0
	ld hl,(render_tile_next)
	ld de,(rs_last_next)
	ld (rs_last_next),hl
	or a,a
	jr nz,.changed
	sbc hl,de
	ld a,1
	jr z,.same
.changed:
	xor a,a
.same:
	ld (rs_same),a
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
	ld hl,row_skip_off	; tests/pc compares frames with and without skipping
	bit 0,(hl)
	jp nz,.draw
	; a record for each band of 8 lines (every band has a row starting in
	; it): rows are matched by place, so a split that moves by a line only
	; redraws the rows next to it
	ld a,iyh
	sub a,$20
	rrca
	rrca
	rrca
	and a,31
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
	inc de
	ld a,(y_course)
	ld (de),a
	inc de
	ld a,(nametable_select)
	ld (de),a
	; different from the last frame (scrolling): draw, and leave the tile
	; hash out (it can only match again once the scroll stays the same)
	pop de
	push de
	ld hl,row_key
	ld b,key_size
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
	; the same banks and nametables as last time, and nothing changed since
	; the last drawn frame: the same tiles
	call .signature
	pop de
	push de
	push hl
	ld hl,sig_at
	add hl,de
	ld de,(hl)
	pop bc
	ld (hl),bc
	ex de,hl
	or a,a
	sbc hl,bc
	jr nz,.rehash
	ld a,(rs_same)
	or a,a
	jr z,.rehash
	pop de
	jp .same_tiles
.rehash:
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
	ld bc,hash_at
	add hl,bc
	ld bc,(hl)
	ld (hl),de
	ex de,hl
	or a,a
	sbc hl,bc
	jp nz,.draw
.same_tiles:
	; same tiles: redraw only the tiles that sprites or labels covered in
	; the last drawn frame (marked in the bands of its first and last line)
	pop bc
	push bc
	ld a,iyh
	sub a,$20
	ld c,a
	add a,b
	dec a
	rrca
	rrca
	rrca
	and a,31
	ld e,a			; last band
	ld a,c
	rrca
	rrca
	rrca
	and a,31
	ld d,a			; first band
	ld hl,dirty_any
	ld bc,0
	ld c,e
	add hl,bc
	ld a,(hl)
	ld hl,dirty_any
	ld c,d
	add hl,bc
	or a,(hl)
	jr nz,.partial
	ld hl,(rows_skipped)
	inc hl
	ld (rows_skipped),hl
	pop bc
	xor a,a			; z: skip
	ret
.partial:
	ld l,d
	ld h,32
	mlt hl
	ld bc,dirty_tiles
	add hl,bc		; the first band's tiles
	ld a,d
	cp a,e
	jp z,.one_band
	; two bands: both their marks
	push hl
	ld l,e
	ld h,32
	mlt hl
	add hl,bc
	pop de
	ld bc,row_tiles_both
repeat 32
	ld a,(de)
	or a,(hl)
	ld (bc),a
	inc de
	inc hl
	inc bc
end repeat
	ld hl,row_tiles_both
.one_band:
	ld (row_tiles),hl
	ld a,1
	ld (row_partial_on),a
	or a,a			; nz
	pop bc
	ret
.draw:
	pop bc
	xor a,a
	ld (row_partial_on),a
	inc a			; nz: draw
	ret

; hl = signature of the banks and nametables rows are drawn from now
.signature:
	ld hl,(t_bank0)
	add hl,hl
	ld de,(t_bank1)
	add hl,de
	add hl,hl
	ld de,(t_bank2)
	add hl,de
	add hl,hl
	ld de,(t_bank3)
	add hl,de
	add hl,hl
	ld de,(t_nametable_0)
	add hl,de
	add hl,hl
	ld de,(t_nametable_1)
	add hl,de
	add hl,hl
	ld de,(t_nametable_2)
	add hl,de
	add hl,hl
	ld de,(t_nametable_3)
	add hl,de
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

;  After a frame is drawn: note which tiles its sprites and labels cover, a
; byte per tile (the row's tiles left to right, 1 = draw again) for each band
; of 8 lines; the next frame draws those tiles again. A sprite at x covers
; tiles x/8 - 1 to (x+7)/8 whatever the fine scroll (and one more to the left
; in the wide picture, whose sprites can be a pixel off the tiles' grid).
; Destroys af, bc, de, hl, ix
row_note_overlays:
	; forget the last frame's marks
	ld hl,dirty_any
	ld de,dirty_tiles
	ld b,max_rows
.clear:
	ld a,(hl)
	or a,a
	jr z,.clean
	ld (hl),0
	push bc
	push hl
	push de
	ex de,hl
	ld (hl),0
	push hl
	pop de
	inc de
	ld bc,31
	ldir
	pop de
	pop hl
	pop bc
.clean:
	inc hl
	ex de,hl
	push bc
	ld bc,32
	add hl,bc
	pop bc
	ex de,hl
	djnz .clear
	ld a,(_wide_buffer+2)
	or a,a
	ld a,1
	jr z,$+3
	inc a
	ld (.smc_left),a
	ld ix,ppu_oam
	ld b,64
.sprite:
	ld a,(ix+3)
	cp a,256-8
	jp nc,.next_sprite
	ld a,(ix+0)
	cp a,239
	jp nc,.next_sprite
	push bc
	; rows: drawn from frame buffer row OAM y - 8 (render_sprites), up to 16
	ld c,16
	sub a,8
	jr nc,.top_ok
	add a,c			; starts above the top: fewer rows
	jr nc,.skip		; all of them above
	ld c,a
	xor a,a
.top_ok:
	ld b,a			; first row
	add a,c
	dec a
	cp a,224
	jr c,$+4
	ld a,223
	rrca
	rrca
	rrca
	and a,31
	ld c,a			; last band
	ld a,b
	rrca
	rrca
	rrca
	and a,31
	ld b,a			; first band
	; tiles: lo = x/8 - 1 (or 2), hi = (x+7)/8
	ld a,(ix+3)
	add a,7
	rra
	rrca
	rrca
	and a,31
	ld e,a			; hi
	ld a,(ix+3)
	rrca
	rrca
	rrca
	and a,31
	sub a,1
.smc_left := $-1
	jr nc,$+3
	xor a,a
	ld d,a			; lo
.band:
	push bc
	push de
	ld hl,dirty_any
	ld de,0
	ld e,b
	add hl,de
	ld (hl),1
	ld l,b
	ld h,32
	mlt hl
	ld de,dirty_tiles
	add hl,de
	pop de
	push de
	ld bc,0
	ld c,d
	add hl,bc
	ld a,e
	sub a,d
	inc a
	ld b,a
.tile:
	ld (hl),1
	inc hl
	djnz .tile
	pop de
	pop bc
	inc b
	ld a,c
	cp a,b
	jr nc,.band
.skip:
	pop bc
.next_sprite:
	lea ix,ix+4
	dec b
	jp nz,.sprite
	ret

; a label covers 12 rows from row a: all the tiles of their bands.
; Destroys af, bc, de, hl
row_note_label:
	ld c,a
	add a,11
	rrca
	rrca
	rrca
	and a,31
	ld b,a
	ld a,c
	rrca
	rrca
	rrca
	and a,31
	ld c,a			; first band
	ld a,b
	sub a,c
	inc a
	ld b,a			; bands
.band:
	push bc
	ld hl,dirty_any
	ld de,0
	ld e,c
	add hl,de
	ld (hl),1
	ld l,c
	ld h,32
	mlt hl
	ld de,dirty_tiles
	add hl,de
	ld b,32
.tile:
	ld (hl),1
	inc hl
	djnz .tile
	pop bc
	inc c
	djnz .band
	ret

; Redraws the tiles of the current row marked in row_tiles (row_unchanged
; found it otherwise unchanged). Jumped to from render_background_loop (or
; the wide one) after it set up the row: alternate de = where tile 0 goes,
; draw_tile's entry offset (and SPL in the normal picture). Calls draw_tile
; once per run of tiles in each nametable; no stack. hl = where to go on.
row_partial:
	ld (.smc_ret),hl
	ld a,(y_len)
	cp a,5
	ld a,1
	jr c,$+3
	inc a
	ld (rp_groups),a
	ld a,(_wide_buffer+2)
	or a,a
	ld a,8
	jr z,$+4
	ld a,10
	ld (rp_width),a
	; nametable entries of tile 0 and of the right nametable's first tile
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
	ld e,64
	ld d,(y_course)
	mlt de
	add hl,de
	ld (rp_left),hl
	lea hl,t_nametable_0
	ld a,(nametable_select)
	xor a,1
	ld c,a
	ld b,3
	mlt bc
	add hl,bc
	ld hl,(hl)
	add hl,de
	ld (rp_right),hl
	ld a,(x_len1)
	ld c,a
	ld a,(x_len2)
	add a,c
	ld (rp_total),a
	ld hl,(row_tiles)
	ld (rp_scan),hl
	xor a,a
	ld (rp_cur),a
.scan:
	; next tile to redraw: bc = tiles left to look at
	ld a,(row_tiles)
	ld c,a
	ld a,(rp_scan)
	sub a,c			; tiles looked at
	ld c,a
	ld a,(rp_total)
	sub a,c
	jp c,.done
	jp z,.done
	ld bc,0
	ld c,a
	ld hl,(rp_scan)
	ld a,1
	cpir
	jp nz,.done
	dec hl
	ld a,(row_tiles)
	ld e,a
	ld a,l
	sub a,e
	ld (rp_first),a
	inc hl
	; and the tiles right after it, up to the next one that isn't marked
	ld a,b
	or a,c
	jr z,.to_end
	xor a,a
	cpir
	jr nz,.to_end
	ld (rp_scan),hl
	dec hl
	ld a,(row_tiles)
	ld e,a
	ld a,l
	sub a,e
	ld (rp_end),a
	jr .draw
.to_end:
	ld (rp_scan),hl
	ld a,(rp_total)
	ld (rp_end),a
.draw:
	; move the destination from tile rp_cur to rp_first
	ld a,(rp_cur)
	ld c,a
	ld a,(rp_first)
	sub a,c
	ld l,a
	ld a,(rp_width)
	ld h,a
	mlt hl
	ld (rp_tmp),hl
	exx
	ld hl,(rp_tmp)
	add hl,de
	ex de,hl
	exx
	; tiles in the left nametable
	ld a,(rp_first)
	ld c,a
	ld a,(x_len1)
	sub a,c
	jr c,.right
	jr z,.right
	ld b,a			; left tiles from rp_first on
	ld a,(rp_end)
	sub a,c
	cp a,b
	jr c,$+3
	ld a,b
	ld c,a			; tiles
	ld a,(rp_first)
	ld hl,(rp_left)
	ld de,0
	ld e,a
	add hl,de
	add hl,de
	ld.sis sp,hl
	ld a,(rp_groups)
	ld b,a
	ld e,a
	ld a,(rp_width)
	ld hl,.right
	jp sha_scratch
.right:
	; tiles in the right nametable
	ld a,(x_len1)
	ld c,a
	ld a,(rp_end)
	sub a,c
	jr c,.ran
	jr z,.ran
	ld b,a			; rp_end - x_len1
	ld a,(rp_first)
	sub a,c
	jr nc,$+3
	xor a,a			; first right tile
	ld e,a
	ld a,b
	sub a,e
	ld c,a			; tiles
	ld hl,(rp_right)
	ld a,e
	ld de,0
	ld e,a
	add hl,de
	add hl,de
	ld.sis sp,hl
	ld a,(rp_groups)
	ld b,a
	ld e,a
	ld a,(rp_width)
	ld hl,.ran
	jp sha_scratch
.ran:
	ld a,(rp_end)
	ld (rp_cur),a
	jp .scan
.done:
	jp 0
.smc_ret := $-3

section .bss

public row_skip_off
public rows_skipped
private rs_same
private rs_last_next
private row_key
private row_records
private dirty_tiles
private dirty_any
private row_tiles
private row_tiles_both
row_skip_off: rb 1
rows_skipped: rb 3		; for tests/pc
rs_same: rb 1			; row_frame_start
rs_last_next: rb 3
row_key: rb row_size
row_records: rb max_rows*row_size
dirty_tiles: rb max_rows*32	; 1 = draw this tile again (row_note_overlays)
dirty_any: rb max_rows		; 1 = some tile of this band is marked
row_tiles: rb 3			; the current row's marks
row_tiles_both: rb 32		; for a row across two bands
rp_scan: rb 3
row_partial_on: rb 1
rp_groups: rb 1
rp_width: rb 1
rp_total: rb 1
rp_cur: rb 1
rp_first: rb 1
rp_end: rb 1
rp_left: rb 3
rp_right: rb 3
rp_tmp: rb 3

extern _wide_buffer
extern render_tile_next
