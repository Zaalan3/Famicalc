include 'ti84pceg.inc'
include 'vars.inc'

temp_stack := $D02400		; (as in render.asm)

section .text

public row_unchanged
public row_forget
public row_forget_lines
public row_frame_start
public row_note_overlays
public row_note_label
public row_partial
public row_partial_on
public rs_fixup
public row_unchanged.compare_mode
public row_unchanged.classic
public row_unchanged.draw
public row_unchanged.skip
public row_unchanged.slots_forget
public scroll_pick

; Row skipping. render_background_loop draws the background one tile row at
; a time. Before a row is drawn, row_unchanged checks whether it would look
; exactly like the same row of the last drawn frame: same place on the
; screen, same scroll, and the same tile cache entry for every tile (a hash
; of them). Such a row is not drawn again, except for the tiles that sprites
; or labels covered in the last drawn frame (row_partial redraws those).
; Anything that makes the frame buffer or the tile cache untrustworthy
; (cache flushes, the pause menu, state loads, blank frames) calls row_forget.

max_rows := 28			; bands of 8 lines

; a row's key: its place and scroll. The vertical part (key_v bytes): line,
; lines, y_fine, mask, y_course, vertical nametable; then horizontal
; nametable, x_start, x_len1, x_len2 and the first tile's frame buffer
; column (with the rotation)
key_v := 6
key_size := 11
hash_at := key_size		; the tiles' hash (3)
sig_at := key_size + 3		; the banks and nametables they came from (3)
dmg_at := key_size + 6		; 1: sprites or labels covered some of its slots (row_note_overlays)
row_size := key_size + 7

; a = a key byte: compared with the record's at hl (c += 1 if it differs),
; which keeps it; hl moves on
macro key_field
	cp a,(hl)
	jr z,$+4
	ld (hl),a
	inc c
	inc hl
end macro

; Compare mode (rotation): for each band, what each 8-column slot of the
; frame buffer shows: the tile cache entry (2 bytes) drawn there, $FFFF if
; unknown. Records of 64 bytes, 64-byte aligned (render.asm run_tiles
; finds a slot's column from the low byte of its address).
slot_size := 64

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
	; rotation on this frame? (scroll_pick ran: not wide, not rot_off)
	ld a,(_wide_buffer+2)
	ld hl,rot_off
	or a,(hl)
	ld a,0
	jr nz,$+3
	inc a
	ld hl,rs_rot
	cp a,(hl)
	ld (hl),a
	call nz,row_forget	; the marks and records mean other things now
	ld hl,slot_area + slot_size - 1
	ld a,l
	and a,-slot_size
	ld l,a
	ld (rs_slot_base),hl
	xor a,a
	ld (rs_sig_ok),a
	ret

; The frame buffer is drawn rotated: the panel shows column x + S at column
; x (spi.asm spiScroll), with S = fb_scroll, the horizontal scroll of this
; frame's longest run of lines with one scroll (mod 256). Rows with that
; scroll have the column of the level mod 256 in each frame buffer column,
; so when the game scrolls, their tiles stay where they are and only the
; new ones are drawn (compare mode, row_unchanged). 0 in the wide picture
; and when rot_off is set (tests). ix = jit_scanline_vars.
; Destroys af, bc, de, hl
scroll_pick:
	ld (fb_scroll),0
	ld a,(_wide_buffer+2)
	ld hl,rot_off
	or a,(hl)
	ret nz
	ld c,(ppu_x_backup)	; the scroll of the current run
	ld b,8			; its first line (lines 8-231 are shown)
	ld de,0			; d = the longest run's lines, e = its scroll
	ld hl,render_event_list
.event:
	ld a,(hl)		; the line the event takes effect on
	cp a,232
	jr nc,.last
	inc hl
	push af
	ld a,(hl)		; its type
	inc hl
	cp a,ppu_event_scroll_x
	jr z,.scroll
	cp a,ppu_event_address
	jr z,.address
	; another kind: skip its data
	push hl
	ld hl,.sizes
	push bc
	ld bc,0
	ld c,a
	add hl,bc
	ld c,(hl)
	pop af			; (bc)
	pop hl
	add hl,bc
	push af
	pop bc
	pop af
	jr .event
.address:
	inc hl			; PPU control, y, then x
	inc hl
.scroll:
	pop af
	call .run_end
	ld c,(hl)		; the new scroll
	inc hl
	jr .event
.last:
	ld a,232
	call .run_end
	ld (fb_scroll),e
	ret
; the run with scroll c from line b ends at line a; the next starts there.
; Keeps hl
.run_end:
	cp a,b
	ret c			; (both above the shown lines)
	push af
	sub a,b
	cp a,d
	jr c,.shorter
	jr z,.shorter
	ld d,a
	ld e,c
.shorter:
	pop af
	ld b,a
	ret
.sizes:	db 1,0,1,3,1,3,12	; data bytes of each event type

; forget every row: the next frame is drawn in full. Destroys af, de, hl
row_forget:
	push bc
	ld hl,row_records
	ld de,row_records+1
	ld bc,(max_rows + 1)*row_size - 1
	ld (hl),$FF
	ldir
	; nothing kept under sprites to put back (render.asm spr_restore)
	ld hl,spr_save
	ld (spr_save_ptr),hl
	xor a,a
	ld (spr_overflow),a
	pop bc
	ret

; b lines from iyh (- $20) are blanked (rendering off): the rows over them,
; recorded in their bands or the band before, are drawn again next time.
; Keeps bc, de, iy
row_forget_lines:
	push bc
	push de
	ld a,iyh
	sub a,$20
	ld c,a
	add a,b
	dec a
	rrca
	rrca
	rrca
	and a,31
	ld e,a			; the last band
	ld a,c
	rrca
	rrca
	rrca
	and a,31
	ld d,a			; the first
	jr nz,.not_top
	ld hl,row_records + max_rows*row_size
	ld (hl),$FF		; (the spare one too, for a short row at the top)
	jr .bands
.not_top:
	; the row recorded in the band before, if it reaches the first line
	dec a
	ld l,a
	ld h,row_size
	mlt hl
	ld bc,row_records
	add hl,bc
	ld a,(hl)
	inc hl
	add a,(hl)		; its end (+ $20)
	dec hl
	jr c,.before
	ld b,a
	ld a,iyh
	cp a,b
	jr nc,.bands
.before:
	ld (hl),$FF		; no line matches: the key differs
.bands:
	ld a,e
	cp a,max_rows
	jr c,$+4
	ld a,max_rows - 1
	sub a,d
	inc a
	ld b,a			; bands
	ld l,d
	ld h,row_size
	mlt hl
	ld de,row_records
	add hl,de
	ld de,row_size
.band:
	ld (hl),$FF
	add hl,de
	djnz .band
	pop de
	pop bc
	ret

; in: b = lines in this row, iyh = first line + $20, ix = jit_scanline_vars,
;     MB = $D6 (the tile set), SPL a free stack
; out: z = the row looks as it did in the last drawn frame: don't draw it.
;      nz = draw it: only the tiles whose slots differ in compare mode
;      (fb_cmp), the runs in row_tiles if row_partial_on, else all; then go
;      on at rs_after (rs_fixup for a tile across the end of the lines)
; keeps b, ix, iy and the alternate registers; uses SPS
row_unchanged:
	push bc
	xor a,a
	ld (fb_cmp),a
	ld (row_partial_on),a
	ld hl,(rs_nodraw)
	ld (rs_after),hl
	; a record for each band of 8 lines (every band has a row starting in
	; it): rows are matched by place, so a split that moves by a line only
	; redraws the rows next to it
	ld a,iyh
	sub a,$20
	jr nz,.band_of_line
	ld a,b
	cp a,8
	ld a,max_rows		; a short row at the top: the next one starts in
	jr c,.band_known	; band 0 too, this one gets the spare record
	xor a,a
.band_of_line:
	rrca
	rrca
	rrca
	and a,31
.band_known:
	ld (rs_band),a
	ld l,a
	ld h,row_size
	mlt hl
	ld de,row_records
	add hl,de
	ld (rs_record),hl
	ld a,b
	ld (rs_lines),a
	; compare this row's place and scroll with the record's, keeping the new
	; ones there: c = differences in the vertical part, then in all of it
	ld c,0
	ld a,iyh
	key_field
	ld a,b
	key_field
	ld a,(y_fine)
	key_field
	ld a,(mask)
	key_field
	ld a,(y_course)
	key_field
	ld a,(nametable_select)
	and a,2
	key_field
	ld a,c
	ld (rs_vdiff),a
	ld a,(nametable_select)
	and a,1
	key_field
	ld a,(x_start)
	key_field
	ld a,(x_len1)
	key_field
	ld a,(x_len2)
	key_field
	ld a,(x_start)
	add a,(fb_scroll)
	ld (rs_col),a		; the first tile's column in the rotated frame buffer
	key_field
	ld a,c
	ld (rs_diff),a
	ld a,(row_skip_off)	; tests/pc compares frames with and without skipping
	or a,a
	jp nz,.draw
	ld a,(rs_rot)
	or a,a
	jp z,.classic
	; at the frame's scroll (x = 8 * (x_course - 1) + x_fine)?
	ld a,(x_course)
	dec a
	add a,a
	add a,a
	add a,a
	or a,(x_fine)
	cp a,(fb_scroll)
	jp nz,.classic
	; compare mode: this band's slots
	call .slots
	ld a,(rs_vdiff)
	or a,a
	call nz,.slots_forget	; other lines or tile rows: nothing known
	; nothing changed at all, nothing drawn over it: skip
	call .sig_check
	ld hl,(rs_record)
	ld bc,dmg_at
	add hl,bc
	ld b,(hl)
	ld (hl),0		; (row_note_overlays forgot its slots under them)
	jr nz,.compare_mode
	ld a,(rs_diff)
	or a,b
	jr nz,.compare_mode
	ld a,(rs_same)
	or a,a
	jp nz,.skip
.compare_mode:
	call .no_hash
	ld (fb_cmp),1
	pop bc
	or a,1			; nz
	ret

.classic:
	ld a,(rs_diff)
	or a,a
	jr z,.hash_tiles
	; a different place or scroll (scrolling): draw, and leave the tile hash
	; out (it can only match again once the scroll stays the same)
	call .no_hash
	jp .draw
.hash_tiles:
	; the same banks and nametables as last time, and nothing changed since
	; the last drawn frame: the same tiles
	call .sig_check
	jr nz,.rehash
	ld a,(rs_same)
	or a,a
	jp nz,.same_tiles
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
	ld hl,(rs_record)
	ld bc,hash_at
	add hl,bc
	ld bc,(hl)
	ld (hl),de
	ex de,hl
	or a,a
	sbc hl,bc
	jp nz,.draw
.same_tiles:
	; same tiles: draw again only the tiles that sprites or labels covered in
	; the last drawn frame (marked in the bands of its first and last line)
	call .marks
	jp z,.skip
	ld a,(rs_rot)
	or a,a
	jp nz,.draw		; (rotated, off the frame's scroll: all of it)
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
.skip:
	ld hl,(rows_skipped)
	inc hl
	ld (rows_skipped),hl
	pop bc
	xor a,a			; z: skip
	ret
.draw:
	ld a,(rs_rot)
	or a,a
	jr z,.draw_all
	; the band's slots won't hold what their records say
	call .slots
	call .slots_forget
	; a rotation that isn't a multiple of 8: does a tile go across the end
	; of the lines? (tile j = -column/8, if the row has that many)
	ld a,(rs_col)
	and a,7
	jr z,.draw_all
	ld (rs_fix_cols),a
	ld a,(rs_col)
	neg
	rrca
	rrca
	rrca
	and a,31
	ld c,a
	ld a,(x_len1)
	ld b,a
	ld a,(x_len2)
	add a,b
	cp a,c
	jr c,.draw_all
	jr z,.draw_all
	; it writes its last columns at the start of the next lines: keep the
	; line below the row's first 8 columns, rs_fixup sorts it out
	lea hl,iy+0
	ld l,0
	ld de,0
	ld a,(rs_lines)
	ld d,a
	add hl,de
	ld de,rs_fix_save
	ld bc,8
	ldir
	ld hl,rs_fixup
	ld (rs_after),hl
.draw_all:
	pop bc
	or a,1			; nz: draw
	ret

; z: the same banks and nametables as the record's (which keeps the new).
; The signature is worked out again when they may have changed (rs_sig_ok)
.sig_check:
	ld a,(rs_sig_ok)
	or a,a
	jr nz,.sig_known
	call .signature
	ld (rs_sig),hl
	ld a,1
	ld (rs_sig_ok),a
.sig_known:
	ld hl,(rs_record)
	ld bc,sig_at
	add hl,bc
	ld de,(hl)
	ld bc,(rs_sig)
	ld (hl),bc
	ex de,hl
	or a,a
	sbc hl,bc
	ret

; the record has no tile hash
.no_hash:
	ld hl,(rs_record)
	ld bc,hash_at
	add hl,bc
	ld bc,-1
	ld (hl),bc
	ret

; rs_slots = this band's slot records
.slots:
	ld a,(rs_band)
	ld l,a
	ld h,slot_size
	mlt hl
	ld de,(rs_slot_base)
	add hl,de
	ld (rs_slots),hl
	ret

; nothing known about any slot of the band
.slots_forget:
	ld hl,(rs_slots)
	push hl
	pop de
	inc de
	ld (hl),$FF
	ld bc,slot_size - 1
	ldir
	ret

; nz: tiles of the row's bands are marked; d = first band, e = last
.marks:
	ld a,iyh
	sub a,$20
	ld c,a
	ld a,(rs_lines)
	add a,c
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

; After a row drawn with a rotation that isn't a multiple of 8 (rs_after,
; from row_unchanged): its tile across the end of the lines wrote its last
; rs_fix_cols columns at the start of the line below each of its lines.
; Moves them up a line, puts back the 8 columns kept from the line below
; the row, and goes on with the next row. iy = the row's first line
rs_fixup:
	ld sp,temp_stack
	lea de,iy+0
	ld e,0
	ld a,(y_len)
	ld b,a
.line:
	push bc
	ld hl,256
	add hl,de
	push hl
	ld a,(rs_fix_cols)
	ld bc,0
	ld c,a
	ldir
	pop de
	pop bc
	djnz .line
	ld hl,rs_fix_save
	ld bc,8
	ldir
	ld hl,(rs_nodraw)
	jp (hl)

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
	ld a,(fb_scroll_addr)
	ld (.smc_scroll),a
	ld (.smc_scroll_kept),a
	; the normal picture keeps the background under sprites (render.asm
	; spr_save): only those it couldn't keep (spr_overflow bit 0), all if it
	; ran out of room (bit 1)
	ld a,(_wide_buffer+2)
	or a,a
	jr nz,.all
	ld a,(spr_overflow)
	or a,a
	ret z
	and a,2
.all:
	ld (.smc_all),a
	ld ix,ppu_oam
	ld b,64
.sprite:
	ld a,(ix+3)
	cp a,256-8
	jp nc,.next_sprite
	ld a,(ix+0)
	cp a,239
	jp nc,.next_sprite
	ld a,0
.smc_all := $-1
	or a,a
	jr nz,.mark
	ld a,(ix+3)
	add a,0
.smc_scroll_kept := $-1
	cp a,256-8+1
	jp c,.next_sprite	; its columns don't go across the end of the lines
.mark:
	ld a,(ix+0)
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
	; b = first band, c = last -> c = first, b = bands
	ld a,c
	sub a,b
	inc a
	ld c,b
	ld b,a
	ld a,(rs_rot)
	or a,a
	jr nz,.rotated
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
	call mark_bands
	jr .skip
.rotated:
	; the frame buffer's slots under its columns x + S to x + S + 7
	ld a,(ix+3)
	add a,0
.smc_scroll := $-1
	ld d,a
	add a,7
	rrca
	rrca
	rrca
	and a,31
	ld e,a			; last slot
	ld a,d
	rrca
	rrca
	rrca
	and a,31
	ld d,a			; first slot
	cp a,e
	jr c,.one_run
	jr z,.one_run
	; across the end of the lines: slots 31 and 0
	push bc
	ld e,a
	call mark_slots
	pop bc
	ld de,0
.one_run:
	call mark_slots
.skip:
	pop bc
.next_sprite:
	lea ix,ix+4
	dec b
	jp nz,.sprite
	ret

; a label covers 12 rows from row a and the frame buffer columns d to
; d + bc - 1: the tiles under it in the bands of those rows (with the same
; margin as sprites for the fine scroll; the frame buffer's slots under it
; when it is rotated). Destroys af, bc, de, hl
row_note_label:
	push bc			; width
	call .bands
	pop hl
	ld a,(rs_rot)
	or a,a
	jr nz,.rotated
	; tiles: lo = x/8 - 2, hi = (x + width + 7)/8
	push bc
	ld bc,7
	add hl,bc
	ld bc,0
	ld c,d
	add hl,bc
	push bc			; x
	call label_tile
	pop hl
	push af			; hi
	call label_tile
	sub a,2
	jr nc,$+3
	xor a,a
	ld d,a			; lo
	pop af
	ld e,a			; hi
	pop bc
	jp mark_bands
.rotated:
	; columns d + S to d + S + width - 1, mod 256
	ld a,(fb_scroll_addr)
	add a,d
	ld de,0
	ld e,a
	add hl,de
	dec hl			; the last one (up to 510)
	rrca
	rrca
	rrca
	and a,31
	ld d,a			; first slot
	ld a,l
	rrca
	rrca
	rrca
	and a,31
	ld e,a			; last slot
	bit 0,h
	jr z,mark_slots
	; across the end of the lines: slots d to 31, then 0 to e
	push bc
	push de
	ld e,31
	call mark_slots
	pop de
	pop bc
	ld d,0
	jr mark_slots
; a = the first row -> c = the first band, b = bands. Keeps de
.bands:
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
	ld c,a
	ld a,b
	sub a,c
	inc a
	ld b,a
	ret

; In the rotated frame buffer: slots d to e of the rows over b bands from
; band c (and of the row from the band before, if it reaches into band c)
; were drawn over. Their slot records are forgotten and the rows flagged
; (dmg_at), and the bands marked for rows off the frame's scroll (dirty_any).
; Keeps de
mark_slots:
	push bc
	push de
	ld a,c
	or a,a
	jr nz,.not_top
	ld a,max_rows		; band 0: the spare record's row too (a short
	call .forget		; row at the top)
	jr .own
.not_top:
	; the row recorded in the band before: does it reach into band c?
	dec a
	ld l,a
	ld h,row_size
	mlt hl
	ld de,row_records
	add hl,de
	ld a,(hl)		; its first line + $20
	inc hl
	add a,(hl)		; + its lines
	jr c,.before
	ld e,a
	ld a,c
	add a,a
	add a,a
	add a,a
	add a,$20		; band c's first line + $20
	cp a,e
	jr nc,.own
.before:
	pop de
	push de
	ld a,c
	dec a
	call .forget
.own:
	pop de
	pop bc
.band:
	ld a,c
	call .forget
	inc c
	djnz .band
	ret
; band a. Keeps bc, de
.forget:
	push bc
	ld bc,0
	ld c,a
	ld hl,dirty_any
	add hl,bc
	ld (hl),1
	ld l,a
	ld h,row_size
	mlt hl
	ld bc,row_records + dmg_at
	add hl,bc
	ld (hl),1
	ld l,a
	ld h,slot_size
	mlt hl
	ld bc,(rs_slot_base)
	add hl,bc
	ld bc,0
	ld c,d
	add hl,bc
	add hl,bc		; slot d
	ld a,e
	sub a,d
	inc a
	add a,a
	ld b,a			; bytes
.slot:
	ld (hl),$FF
	inc hl
	djnz .slot
	pop bc
	ret

; marks tiles d to e in b bands from band c. Keeps de
mark_bands:
	push bc
	push de
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
	inc c
	djnz mark_bands
	ret

; hl = frame buffer column -> a = the tile column there: hl/8, or hl/10 in
; the wide picture (its columns are 5/4 of the NES's); at most 31.
; Destroys de, hl
label_tile:
	ld de,8
	ld a,(_wide_buffer+2)
	or a,a
	jr z,.divide
	ld e,10
.divide:
	ld a,-1
.loop:
	inc a
	or a,a
	sbc hl,de
	jr nc,.loop
	cp a,32
	ret c
	ld a,31
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
public rot_off
public rows_skipped
public rs_slots
public rs_rot
public rs_sig_ok
public rs_band
public rs_record
public rs_lines
public rs_vdiff
public rs_slot_base
public row_records
public rs_nodraw
public rs_after
private rs_same
private rs_last_next
private row_key

private dirty_tiles
private dirty_any
private row_tiles
private row_tiles_both
row_skip_off: rb 1
rot_off: rb 1			; 1: no rotation (tests/pc)
rows_skipped: rb 3		; for tests/pc
rs_rot: rb 1			; 1: the frame buffer is rotated this frame (row_frame_start)
rs_band: rb 1
rs_record: rb 3
rs_col: rb 1			; the row's first tile's frame buffer column
rs_vdiff: rb 1
rs_lines: rb 1
rs_sig: rb 3			; the banks and nametables rows come from now (.signature)
rs_sig_ok: rb 1			; 0: work rs_sig out again
rs_diff: rb 1
rs_slot_base: rb 3		; slot_area, aligned
rs_slots: rb 3			; the current row's slot records (render.asm run_tiles)
rs_nodraw: rb 3			; where render_background_loop goes on after a row
rs_after: rb 3			; after this row: rs_nodraw or rs_fixup
rs_fix_cols: rb 1
rs_fix_save: rb 8
slot_area: rb (max_rows + 1)*slot_size + slot_size - 1
rs_same: rb 1			; row_frame_start
rs_last_next: rb 3
row_key: rb row_size
row_records: rb (max_rows + 1)*row_size	; (+ the spare: a short row at the top)
dirty_tiles: rb max_rows*32	; 1 = draw this tile again (row_note_overlays)
dirty_any: rb max_rows + 1	; 1 = some tile of this band is marked (+ the spare record's, unused)
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
extern spr_save
extern spr_save_ptr
extern spr_overflow
extern render_tile_next
