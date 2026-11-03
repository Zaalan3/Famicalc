include 'ti84pceg.inc'
include 'vars.inc'

temp_stack := $D02400		; (as in render.asm)

section .text

public row_unchanged
public row_forget
public row_forget_lines
public row_frame_start
public row_scroll
public row_refill
public row_shift
public rs_dy
public rs_up
public rs_k
public row_note_overlays
public row_note_label
public row_note_kept
public row_partial
public row_partial_on
public rs_fixup
public row_unchanged.compare_mode
public row_unchanged.classic
public row_unchanged.draw
public row_unchanged.skip
public row_unchanged.slots_forget
public scroll_pick
public mark_bands
public mark_slots
public row_hash_set_byte

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
	; rotation on this frame? (as scroll_pick decided)
	call rs_rotating
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

; After row_frame_start: the frame buffer gets back the background under the
; last drawn frame's sprites (render.asm spr_restore), unless no row can
; keep what it shows: this frame starts at another y than the last drawn
; one, and neither has another y in the middle (scroll_pick: then every
; row's place, fine y or tile row differs from its band's record). If the
; picture moved by up to 216 lines, the frame buffer and the records follow
; it instead (row_shift). ix = jit_scanline_vars, keeps ix and iy
row_scroll:
	ld hl,rs_valid
	ld c,(hl)
	ld (hl),1		; (row_forget clears it)
	ld a,(rs_y_now+2)
	ld hl,rs_y_last+2
	or a,(hl)
	ld hl,(rs_y_now)
	ld de,(rs_y_last)
	ld (rs_y_last),hl
	jp nz,spr_restore
	or a,a
	sbc hl,de
	jp z,spr_restore
	; the records hold the last drawn frame (no row_forget since), and the
	; rows aren't all drawn again anyway (tests/pc)?
	ld a,(row_skip_off)
	dec c
	or a,c
	jr nz,.redraw
	; k, the bands the rows move by: ceil(y/8) - ceil(last y/8)
	push hl
	ex de,hl
	call .ceil8
	ld c,a
	add hl,de
	call .ceil8
	sub a,c
	ld (rs_k),a
	pop hl
	; dy lines (down: -), up to 216 (across 479 -> 0: drawn again)
	ld a,1
	bit 7,h
	jr z,.up
	ex de,hl
	or a,a
	sbc hl,hl
	sbc hl,de
	xor a,a
.up:
	ld (rs_up),a
	ld de,217
	or a,a
	sbc hl,de
	jr nc,.redraw
	ld a,l
	add a,e
	ld (rs_dy),a
	call spr_restore
	; unrotated, the marks of tiles covered (row_note_overlays, row_note_kept,
	; labels) are by band and wouldn't follow: drawn again where they are
	; (rotated, they are in the records: slots and dmg_at)
	ld a,(rs_rot)
	or a,a
	jr nz,row_shift
	ld hl,dirty_any
	ld b,max_rows
.marked:
	or a,(hl)
	inc hl
	djnz .marked
	or a,a
	jr z,row_shift
	ret
.redraw:
	ld hl,spr_save		; (all of it is drawn again)
	ld (spr_save_ptr),hl
	xor a,a
	ld (spr_overflow),a
	ret
; a = ceil(hl/8) (hl < 505). Keeps bc, de, hl
.ceil8:
	push hl
	push de
	ld de,7
	add hl,de
	ld a,l
	srl h
	rra
	rrca
	rrca
	and a,63
	pop de
	pop hl
	ret

; The picture moved by rs_dy lines (up if rs_up): the frame buffer's lines
; move with it, and each band record (and slot record) to the band its row
; is in now, rs_k bands on, with the line, lines and fine y it has there
; (cut where it leaves the top). A row of this frame found
; at a moved one's place and scroll then isn't drawn again, or only where
; its tiles changed. Rows moved in from outside have no record. While the
; panel takes the LCD's data, the move goes in pieces behind the LCD's
; reads (pacing.asm pace_row_guard, which also locks the panel near its
; next transfer). ix = jit_scanline_vars, keeps ix and iy
row_shift:
	push iy
	ld a,(rs_dy)
	ld c,a
	neg
	add a,224
	ld b,a			; lines moved
	ld a,(rs_up)
	or a,a
	ld a,c			; up: line dy to line 0 on, first to last
	ld c,0
	ld e,$B0		; (ldir)
	jr nz,.fb
	ld a,b			; down: line 224 - dy to 224, last to first
	ld c,224
	ld e,$B8		; (lddr)
.fb:
	ld hl,.smc_move
	ld (hl),e
	push bc
	call .line_address
	push hl
	ld a,c
	call .line_address
	ex de,hl
	pop hl
	pop bc
	ld a,(rs_up)
	or a,a
	jr nz,$+4
	dec hl
	dec de
	ld iyh,$20
	ld a,b
.fb_piece:
	; a = lines left: the next up to 16
	or a,a
	jr z,.fb_moved
	ld c,16
	cp a,c
	jr nc,$+3
	ld c,a
	sub a,c
	push af
	push hl
	push de
	ld b,c			; (up: the lines it writes)
	ld a,(rs_up)
	or a,a
	jr nz,.guard
	ld iyh,$20
	ld b,224		; (down: the LCD must have read the whole frame)
.guard:
	call pace_row_guard	; (keeps bc, iy)
	ld a,c
	call .line_bytes
	push hl
	pop bc
	pop de
	pop hl
	ldir
.smc_move := $-1
	ld a,iyh
	add a,16
	ld iyh,a
	pop af
	jr .fb_piece
.fb_moved:
	pop iy
	; band j gets band j + k's record (-1: the spare), in the order that
	; reads each before it is written over: up, from -1 on; down, from 27
	ld a,(rs_up)
	or a,a
	ld bc,(max_rows + 1) shl 8 + $FF
	ld d,1
	jr nz,.record_band
	ld c,max_rows - 1
	ld d,-1
.record_band:
	push bc
	push de
	ld a,c
	call .index
	ld b,a
	ld a,(rs_k)
	add a,c
	inc a
	cp a,max_rows + 1
	jr nc,.record_none	; (no such band)
	dec a
	call .index
	cp a,b
	call nz,.copy_record
	ld a,b
	call .record
	call .moved_key
	jr .record_next
.record_none:
	ld a,b
	call .record
	call .no_row
.record_next:
	pop de
	pop bc
	ld a,c
	add a,d
	ld c,a
	djnz .record_band
	ret

; a = the record of band a (-1: the spare's, max_rows)
.index:
	inc a
	jr nz,$+4
	ld a,max_rows + 1
	dec a
	ret

; hl = record a. Destroys de
.record:
	ld l,a
	ld h,row_size
	mlt hl
	ld de,row_records
	add hl,de
	ret

; hl = slot record a. Destroys de
.slot_record:
	ld l,a
	ld h,slot_size/2
	mlt hl
	add hl,hl
	ld de,(rs_slot_base)
	add hl,de
	ret

; record a and its slot record to b. Keeps b
.copy_record:
	push bc
	ld c,a
	call .slot_record
	push hl
	ld a,b
	call .slot_record
	ex de,hl
	pop hl
	push bc
	ld bc,slot_size
	ldir
	pop bc
	ld a,c
	call .record
	push hl
	ld a,b
	call .record
	ex de,hl
	pop hl
	ld bc,row_size
	ldir
	pop bc
	ret

; hl = the frame buffer's line a (.line_bytes: its offset). Destroys af, de
.line_address:
	call .line_bytes
	ld de,(game_fb)
	add hl,de
	ret
.line_bytes:
	or a,a
	sbc hl,hl
	ld h,a
	ld a,(_wide_buffer+2)
	or a,a
	ret z
	ld d,h
	ld e,64
	mlt de
	add hl,de		; (320 bytes a line)
	ret

; the record at hl: its row moved by rs_dy lines (up if rs_up). The line,
; lines and fine y it has now, cut to the frame buffer; no row if it had
; none or none of it is left
.moved_key:
	ld a,(hl)
	sub a,$20
	jr c,.no_row		; (no row: line bytes below $20)
	ld c,a			; first line
	inc hl
	ld a,(hl)
	dec hl
	dec a
	cp a,8
	jr nc,.no_row		; (or lines not 1 to 8)
	inc a
	ld b,a			; lines
	add a,c
	cp a,224+1
	jr nc,.no_row
	ld a,(rs_dy)
	ld e,a
	ld a,(rs_up)
	or a,a
	ld a,c
	jr z,.key_down
	sub a,e
	jr nc,.key_line
	; its first lines left the top: the rest from line 0
	neg
	ld e,a			; lines cut
	sub a,b
	jr nc,.no_row
	neg
	inc hl
	ld (hl),a		; lines
	inc hl
	ld a,(hl)
	add a,e
	ld (hl),a		; fine y
	dec hl
	dec hl
	xor a,a
	jr .key_line
.key_down:
	add a,e
	jr c,.no_row
	ld c,a
	add a,b
	cp a,224+1
	jr nc,.no_row		; (it left the bottom: drawn again whole)
	ld a,c
.key_line:
	add a,$20
	ld (hl),a
	ret
; band a's record: no row (a = -1: the spare)
.forget:
	call .index
	call .record
.no_row:
	ld (hl),$FF
	inc hl
	ld (hl),$FF
	ret

; The frame buffer is drawn rotated: the panel shows column x + S at column
; x (spi.asm spiScroll), with S = fb_scroll, the horizontal scroll of this
; frame's longest run of lines with one scroll (mod 256). Rows with that
; scroll have the column of the level mod 256 in each frame buffer column,
; so when the game scrolls, their tiles stay where they are and only the
; new ones are drawn (compare mode, row_unchanged). 0 in the wide picture
; and when rot_off is set (tests). Also this frame's y for row_scroll.
; ix = jit_scanline_vars. Destroys af, bc, de, hl
scroll_pick:
	ld (fb_scroll),0
	or a,a
	sbc hl,hl
	ld (fb_scroll_w),hl
	; y down the two vertical nametables (0-479), if no tile row starts on
	; an attribute row (y_course 30, 31: rows don't follow each other in
	; order there) and (below) no $2006 write moves it in the middle
	ld a,(ppu_y_backup)
	ld l,a
	bit 1,(ppu_ctrl_backup)
	jr z,$+7
	ld de,240
	add hl,de
	cp a,240
	sbc a,a
	inc a
	ld (rs_y_now),hl
	ld (rs_y_now+2),a	; 0: y is the frame's (row_scroll)
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
	ld (rs_y_now+2),a	; (another y from there)
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
	call rs_rotating
	ret nz
	ld (fb_scroll),e
	; the wide picture's rotation, in its columns: xtab[S] (renderwide.asm)
	ld a,(_wide_buffer+2)
	or a,a
	ret z
	ld d,2
	mlt de
	ld hl,xtab
	add hl,de
	ld de,0
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (fb_scroll_w),de
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

; nz: the frame buffer isn't rotated (rot_off; the wide picture with the FPS
; counter, whose strip above the picture would scroll too). Destroys af
rs_rotating:
	ld a,(rot_off)
	or a,a
	ret nz
	ld a,(_wide_buffer+2)
	or a,a
	ret z
	ld a,(_show_fps)
	or a,a
	ret

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
	ld (rs_valid),a		; (row_scroll: nothing to move)
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
	ld (hl),$FF		; (and its lines: a 1-line row at line 223 has line byte $FF)
	inc hl
	ld (hl),$FF
	dec hl
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
	; a whole row at the top: the spare record's row isn't in the frame
	; buffer any more (its line byte: no row starts on line max_rows - $20)
	ld (row_records + max_rows*row_size),a
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
	jr nz,.refill		; other lines or tile rows: nothing known
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
	jp .drawing
.refill:
	; nothing known about its slots: all its tiles are drawn, as runs
	; (compare mode drew them one by one), and row_refill records them
	call .sig_check
	ld hl,(rs_record)
	ld bc,dmg_at
	add hl,bc
	ld (hl),0
	call .no_hash
	ld (fb_cmp),2		; (renderwide.asm: as runs at the frame's scroll, not in pieces)
	ld hl,row_refill
	ld (rs_after),hl
	pop bc
	jp .drawing

.classic:
	; (the record takes the banks and nametables the row is drawn from in
	; any case: a row drawn at another place used to keep the older ones,
	; and matched them when they came back)
	call .sig_check
	ld a,(rs_diff)
	jr nz,.new_sig
	or a,a
	jr nz,.new_place
	; the same banks and nametables as last time, and nothing changed since
	; the last drawn frame: the same tiles
	ld a,(rs_same)
	or a,a
	jp nz,.same_tiles
	jr .rehash
.new_sig:
	or a,a
	jr z,.rehash
.new_place:
	; a different place or scroll (scrolling): draw, and leave the tile hash
	; out (it can only match again once the scroll stays the same)
	call .no_hash
	jp .draw
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
	ld a,(rs_rot)
	or a,a
	jr z,.same_marks
	; (rotated, off the frame's scroll: all of it, if mark_slots noted
	; anything in its record: the record follows the row in row_shift)
	call .damaged
	jp z,.skip
	jp .draw
.same_marks:
	call .marks
	jp z,.skip
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
	ex (sp),hl		; (the first band's)
	ld de,row_tiles_both
	ld bc,32
	ldir
	pop hl			; the second's
	ld de,row_tiles_both
	ld b,32
.both:
	ld a,(de)
	or a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz .both
	ld hl,row_tiles_both
.one_band:
	ld (row_tiles),hl
	ld a,1
	ld (row_partial_on),a
	pop bc
	jr .drawing
.skip:
	ld hl,(rows_skipped)
	inc hl
	ld (rows_skipped),hl
	pop bc
	ld a,(spiUnlock.locked)
	or a,a
	call z,pace_skip_guard	; (pacing.asm; nothing to check when locked: ~5 units a frame)
	xor a,a			; z: skip
	ret
.draw:
	ld a,(rs_rot)
	or a,a
	jr z,.draw_all
	; the band's slots won't hold what their records say, and nothing will
	; be covered
	call .slots
	call .slots_forget
	call .damaged
	ld (hl),0
	; (the wide picture: renderwide.asm w_rot_row sees to tiles across the end)
	ld a,(_wide_buffer+2)
	or a,a
	jr nz,.draw_all
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
.drawing:
	; the panel mustn't take the frame half drawn (pacing.asm)
	call pace_row_guard
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

; nz: the record notes slots covered by sprites or labels (hl = there)
.damaged:
	ld hl,(rs_record)
	ld bc,dmg_at
	add hl,bc
	ld a,(hl)
	or a,a
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
	res 4,h			; the tile set in use (render.asm tile_set_patch)
row_hash_set_byte := $-1
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

; After a row drawn whole at the frame's scroll because nothing was known
; about its slots (rs_after, from row_unchanged): each slot record gets the
; tile cache entry its tile shows (the drawing translated them all; a cache
; flush in between forgot the row), and the two slots left of the row's
; first tile, which it doesn't reach, $FFFF. Then on with the next row.
; ix = jit_scanline_vars, MB = $D6
row_refill:
	ld sp,temp_stack
	push iy
	ld e,64
	ld d,(y_course)
	mlt de			; the row's entries in its nametables
	; (fill_slots writes 3 bytes a slot: the one after slot 31 is kept)
	ld iy,(rs_slots)
	ld a,(iy+slot_size)
	push af
	; the left nametable's tiles: slots x_course on (render_background_loop)
	ld a,(x_len1)
	or a,a
	jr z,.right
	lea hl,t_nametable_0
	ld b,(nametable_select)
	ld c,3
	mlt bc
	add hl,bc
	ld hl,(hl)
	add hl,de
	ld b,(x_course)
	ld c,2
	mlt bc
	add hl,bc
	add iy,bc
	ld b,a
	call .fill
.right:
	; the right one's: slots 0 on
	ld a,(x_len2)
	or a,a
	jr z,.done
	lea hl,t_nametable_0
	ld a,(nametable_select)
	xor a,1
	ld c,a
	ld b,3
	mlt bc
	add hl,bc
	ld hl,(hl)
	add hl,de
	ld iy,(rs_slots)
	ld a,(x_len2)
	ld b,a
	call .fill
.done:
	ld de,(rs_slots)
	pop af
	ld hl,slot_size
	add hl,de
	ld (hl),a
	; the slots it doesn't reach (where the runs' last third bytes went)
	ld a,(x_course)
	sub a,2
	call .forget
	ld a,(x_course)
	dec a
	call .forget
	pop iy
	ld hl,(rs_nodraw)
	jp (hl)
; b tiles from the entries at hl, slot records from iy (cursor RAM's loop)
.fill:
	ld.sis sp,hl
	jp fill_slots
; slot a mod 32 of the slot records at de: $FFFF. Keeps de
.forget:
	and a,31
	add a,a
	or a,e
	push de
	pop hl
	ld l,a
	ld (hl),$FF
	inc hl
	ld (hl),$FF
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
	ld a,(fb_scroll_addr)
	ld (.smc_scroll),a
	ld (.smc_scroll_kept),a
	; the normal picture keeps the background under sprites (render.asm
	; spr_save): only those it couldn't keep (spr_overflow bit 0), all if it
	; ran out of room (bit 1)
	; (the wide picture keeps them too: renderwide.asm w_sprite_save)
	ld a,(spr_overflow)
	or a,a
	ret z
	and a,2
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
	jp nc,.skip		; all of them above
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
	ld a,(_wide_buffer+2)
	or a,a
	jr nz,.rotated_wide
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
	jr .skip
.rotated_wide:
	; the wide picture: its column as renderwide.asm w_sprite works it out
	or a,a
	sbc hl,hl
	ld l,(ix+3)
	add hl,hl
	ld de,(w_sprite.smc_xtab)
	add hl,de
	ld de,0
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld hl,(w_sprite.smc_shift)
	add hl,de
	ld de,-320
	add hl,de
	jr c,.column_w
	ld de,320
	add hl,de
.column_w:
	call w_slots_under
.skip:
	pop bc
.next_sprite:
	lea ix,ix+4
	dec b
	jp nz,.sprite
	ret

; A sprite row whose background spr_restore couldn't put back (the LCD
; hadn't read it): frame buffer line a, columns e to e + 7 (not across the
; end of the lines). Its tiles are drawn again this frame: the slots under
; it (rotated), or the tiles with the margin sprites get (row_note_overlays).
; After row_frame_start. Destroys af, bc, de
row_note_kept:
	push hl
	rrca
	rrca
	rrca
	and a,31
	ld c,a			; its band
	ld b,1
	ld a,(rs_rot)
	or a,a
	jr z,.tiles
	ld a,e
	add a,7
	rrca
	rrca
	rrca
	and a,31
	ld d,a			; last slot
	ld a,e
	rrca
	rrca
	rrca
	and a,31
	ld e,d
	ld d,a			; first slot
	call mark_slots
	pop hl
	ret
.tiles:
	ld a,e
	add a,7
	rra
	rrca
	rrca
	and a,31
	ld d,a			; hi = (x+7)/8
	ld a,e
	rrca
	rrca
	rrca
	and a,31
	sub a,1
	jr nc,$+3
	xor a,a
	ld e,d
	ld d,a			; lo = x/8 - 1
	call mark_bands
	pop hl
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
	ld a,(_wide_buffer+2)
	or a,a
	jr nz,.rotated_wide
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
.rotated_wide:
	; the wide picture: columns d + S_w to d + S_w + width - 1, around 320
	push hl
	ld a,d
	ld de,0
	ld e,a
	ld hl,(fb_scroll_w)
	add hl,de
	ld de,-320
	add hl,de
	jr c,.column_w
	ld de,320
	add hl,de
.column_w:
	pop de
	dec de
	jp w_slots_span
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
public fb_scroll_w
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
fb_scroll_w: rb 3		; the wide picture's rotation, in its columns (scroll_pick)
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
slot_area: rb (max_rows + 1)*slot_size + slot_size	; (+ the byte after the last: row_refill)
rs_same: rb 1			; row_frame_start
rs_last_next: rb 3
rs_y_now: rb 3			; this frame's y, vertical nametable, 0 (scroll_pick; else another y in the middle)
rs_y_last: rb 3			; the last drawn frame's (row_scroll)
rs_valid: rb 1			; 0: row_forget since the last drawn frame
rs_dy: rb 1			; row_shift: lines the picture moved (1-216)
rs_up: rb 1			; 1: up (y went up)
rs_k: rb 1			; bands the records move by (down: -)
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

extern pace_row_guard
extern pace_skip_guard
extern spiUnlock.locked
extern _wide_buffer
extern _show_fps
extern xtab
extern w_slots_under
extern w_slots_span
extern w_sprite.smc_xtab
extern w_sprite.smc_shift
extern spr_save
extern spr_save_ptr
extern spr_overflow
extern render_tile_next
extern spr_restore
extern game_fb
extern fill_slots
