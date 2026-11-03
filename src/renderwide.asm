include 'ti84pceg.inc'
include 'vars.inc'

section .text

public wide_code
public w_drawtile_set_byte
public w_fetch_tile_addr
public w_spr_restore
public w_slots_under
public w_slots_span
public w_sprite.smc_xtab
public w_sprite.smc_shift
public w_run_tiles_set_byte
public w_run_tiles.smc_set
public w_fill_slots_set_byte
public w_render_background_loop.nodraw
public xtab
public w_sprite_save
public render_sprites_wide
; for tests/pc's profiler
public w_row_setup
public w_blank_lines
public w_sprite
public w_set_flips
public mul320
public w_render_background_loop
public w_fetch_tile
public w_spr_hi
public w_spr_lo

; The wide picture: the game drawn straight into a 320x224 frame buffer at
; wide_buffer (320 bytes per line), 5 pixels for every 4 of the NES. Tiles
; are cached 10 pixels wide (their pixels 3 and 7 twice) and sprites are drawn
; 10 pixels wide the same way, so a frame costs about 5/4 of a normal one.
; The panel shows columns 10-309 (the NES's 8-247, like the normal picture).
;
; Same structure as render.asm: render_background (shared) runs the loop below
; from LCD cursor RAM; it differs in where pixels go. Lines are still counted
; as iyh = line + $20.

temp_stack := $D02400
stride := 320

; one sprite pixel: 2 bits from the planes in d and e, drawn unless the
; pixel already has a sprite (bit 7); then the step to the next pixel
macro px_hi step
	local skip
	xor a,a
	rl d
	adc a,a
	rl e
	adc a,a
	jr z,skip
	bit 7,(hl)
	jr nz,skip
	add a,c
	ld (hl),a
skip:
step:
	inc hl
end macro

; a behind-the-background sprite's pixel a: drawn where the background is
; transparent, otherwise only marked as a sprite pixel (bit 7)
macro put_lo
	local zero, done
	bit 7,(hl)
	jr nz,done
	bit 4,(hl)
	jr z,zero
	set 7,(hl)
	jr done
zero:
	ld (hl),a
done:
end macro

macro px_lo step
	local skip
	xor a,a
	rl d
	adc a,a
	rl e
	adc a,a
	jr z,skip
	add a,c
	put_lo
skip:
step:
	inc hl
end macro


; for render_load_code (render.asm): the code to copy to cursor RAM
wide_code:
	dl w_render_src, w_render_len, w_drawtile_src, w_drawtile_len

; hl = a * 320. Destroys de
mul320:
	ld l,a
	ld h,stride/5
	mlt hl
	push hl
	pop de
	add hl,hl
	add hl,hl
	add hl,de
	ret

; before a tile row is drawn: b = lines (y_len), iyh = first line + $20.
; Sets up w_draw_tile and puts where the row's first tile goes in de'
; (biased: w_draw_tile's prologue moves it to the next tile)
w_row_setup:
	; where to enter the unrolled row loop: skip `exx` and 6 bytes per row
	ld a,8
	sub a,b
	and a,11b
	ld c,a
	ld b,6
	mlt bc
	ld a,c
	inc a
	ld (w_draw_tile.smc_offset),a
	; first row of the tiles in the cache: render_cache + y_fine*10
	ld e,(y_fine)
	ld d,10
	mlt de
	ld hl,render_cache
	add hl,de
	ld (w_draw_tile.smc_src),hl
	; each tile moves the destination 320 per row; the next tile is 10 on
	ld a,(y_len)
	call mul320
	ld de,-10
	add hl,de
	push hl			; rows*320 - 10
	ex de,hl
	or a,a
	sbc hl,hl
	sbc hl,de
	ld (w_draw_tile.smc_k),hl
	; first tile: line start + 10 - w(fine scroll); w(f) = f + (f >= 4)
	ld a,iyh
	sub a,$20
	call mul320
	ld de,(_wide_buffer)
	add hl,de
	pop de
	add hl,de
	ld (w_row_base),hl	; the row's line, biased as w_draw_tile wants
	ld a,(x_start)		; 8 - fine scroll
	cp a,5
	ccf
	adc a,1
	or a,a
	sbc hl,hl
	ld l,a
	ld a,(rs_rot)
	or a,a
	jr z,.column
	; rotated: + S_w, around the end of the lines
	ld de,(fb_scroll_w)
	add hl,de
	ld de,-stride
	add hl,de
	jr c,.column
	ld de,stride
	add hl,de
.column:
	ld (w_col0),hl
	ld de,(w_row_base)
	add hl,de
	push hl
	exx
	pop de
	exx
	; rotated and off the frame's scroll (not compare mode, not runs at the
	; frame's scroll: fb_cmp 2, rowskip.asm): in pieces
	ld a,(rs_rot)
	or a,a
	ret z
	ld a,(fb_cmp)
	or a,a
	ret nz
	pop hl			; (not back to the row loop)
	jp w_rot_row

; A row of the rotated wide picture not drawn in compare mode (rows off the
; frame's scroll: status bars...), from w_row_setup: its tiles go from column
; w_col0 on, around the end of the lines, in up to 3 pieces (where the right
; nametable starts and where the lines end). A tile across the end of the
; lines writes its last columns on the next lines: moved up after
; (w_lines_up). ix = jit_scanline_vars; SPL is a stack until the first piece
w_rot_row:
	; nametable entries of tile 0 and of the right nametable's first one
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
	ld (wr_left),hl
	lea hl,t_nametable_0
	ld a,(nametable_select)
	xor a,1
	ld c,a
	ld b,3
	mlt bc
	add hl,bc
	ld hl,(hl)
	add hl,de
	ld (wr_right),hl
	; tiles from jw on start past the end of the lines: jw = (329 - col0) / 10
	ld hl,stride + 9
	ld de,(w_col0)
	or a,a
	sbc hl,de
	call div10
	ld (wr_jw),a
	; and tile jw - 1 goes across the end if col0 + 10*jw > 320: k columns
	xor a,a
	ld (wr_fix),a
	ld a,(wr_jw)
	or a,a
	jr z,.pieces
	ld e,a
	ld d,10
	mlt de
	ld hl,(w_col0)
	add hl,de
	ld de,-stride
	add hl,de		; k (0: none)
	ld a,l
	or a,a
	jr z,.pieces
	ld c,a
	ld a,(x_len1)
	ld b,a
	ld a,(x_len2)
	add a,b
	ld b,a			; tiles
	ld a,(wr_jw)
	dec a
	cp a,b
	jr nc,.pieces		; (not drawn)
	; keep the k columns of the line below the row
	ld a,c
	ld (wr_fix),a
	ld a,iyh
	sub a,$20
	add a,(y_len)
	call mul320
	ld de,(_wide_buffer)
	add hl,de
	ld de,wr_save
	ld bc,10
	ldir
.pieces:
	; pieces [0, b1), [b1, b2), [b2, n): b1, b2 = x_len1 and jw in order
	ld a,(x_len1)
	ld b,a
	ld a,(x_len2)
	add a,b
	ld (wr_n),a
	ld a,(wr_jw)
	ld c,a
	cp a,b
	jr nc,$+4
	ld a,b			; a = max, c or b = min
	ld b,c
	ld hl,wr_n
	cp a,(hl)
	jr c,$+3
	ld a,(hl)		; (at most n)
	ld (wr_b2),a
	ld a,b
	cp a,c
	jr c,$+3
	ld a,c
	cp a,(hl)
	jr c,$+3
	ld a,(hl)
	ld (wr_b1),a		; (min)
	ld hl,wr_pieces
	ld (wr_ptr),hl
	xor a,a
	ld (wr_count),a
	ld a,(wr_b1)
	call .piece		; [0, b1)
	ld a,(wr_b1)
	ld b,a
	ld a,(wr_b2)
	call .piece_from	; [b1, b2)
	ld a,(wr_b2)
	ld b,a
	ld a,(wr_n)
	call .piece_from	; [b2, n)
	; draw them
	ld hl,wr_pieces
	ld (wr_ptr),hl
	ld a,(wr_count)
	or a,a
	jp z,.drawn
.draw:
	ld hl,(wr_ptr)
	ld de,(hl)
	ex de,hl
	ld.sis sp,hl		; nametable entries
	ex de,hl
	inc hl
	inc hl
	inc hl
	ld de,(hl)
	push de
	exx
	pop de			; where (biased)
	exx
	inc hl
	inc hl
	inc hl
	ld c,(hl)		; tiles
	inc hl
	ld (wr_ptr),hl
	ld b,1
	ld a,(y_len)
	cp a,5
	jr c,$+3
	inc b
	ld e,b
	ld hl,.drawn_piece
	ld a,10
	jp w_draw_tile
.drawn_piece:
	ld hl,wr_count
	dec (hl)
	jr z,.drawn
	ld sp,temp_stack	; (for .draw's push)
	jr .draw
.drawn:
	ld sp,temp_stack
	ld ix,jit_scanline_vars
	ld a,(wr_fix)
	or a,a
	jp z,w_render_background_loop.nodraw
	ld c,a
	ld a,iyh
	sub a,$20
	ld b,(y_len)
	ld hl,wr_save
	call w_lines_up
	jp w_render_background_loop.nodraw

; a piece of tiles [0, a) (.piece) or [b, a) (.piece_from), if not empty
.piece:
	ld b,0
.piece_from:
	sub a,b
	ret z
	ret c
	ld c,a			; tiles
	ld hl,(wr_ptr)
	; nametable entries: the left nametable's from tile b, or the right's
	push hl
	ld a,(x_len1)
	ld e,b
	cp a,e
	jr c,.from_right
	jr z,.from_right
	ld hl,(wr_left)
	jr .entries
.from_right:
	ld d,a
	ld a,e
	sub a,d
	ld e,a
	ld hl,(wr_right)
.entries:
	ld a,e
	ld de,0
	ld e,a
	add hl,de
	add hl,de		; + 2 * the tile's index in it
	ex de,hl
	pop hl
	ld (hl),de
	inc hl
	inc hl
	inc hl
	; where: w_row_base + (col0 + 10*b) mod 320
	push hl
	ld e,b
	ld d,10
	mlt de
	ld hl,(w_col0)
	add hl,de
	ld de,-stride
	add hl,de
	jr c,.wrapped
	ld de,stride
	add hl,de
.wrapped:
	ld de,(w_row_base)
	add hl,de
	ex de,hl
	pop hl
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ld (hl),c
	inc hl
	ld (wr_ptr),hl
	ld hl,wr_count
	inc (hl)
	ret

; Moves c columns from line a + 1 up to line a, for b lines (a row whose
; tile across the end of the lines wrote them a line too low), then puts the
; c columns at hl back in the line below them. Destroys af, bc, de, hl
w_lines_up:
	push hl
	push bc
	call mul320
	ld de,(_wide_buffer)
	add hl,de
	ex de,hl		; line a
	pop bc
.line:
	push bc
	ld hl,stride
	add hl,de
	push hl
	ld b,0
	ldir
	pop de
	pop bc
	djnz .line
	pop hl
	ld b,0
	ldir
	ret

; a = hl / 10 (hl < 2560). Destroys de, hl
div10:
	ld de,-10
	ld a,-1
.sub:
	inc a
	add hl,de
	jr c,.sub
	ret

; background off: b lines from line iyh - $20 in the backdrop colour
w_blank_lines:
	ld a,b
	add a,iyh
	jr nc,$+3
	xor a,a
	sub a,iyh		; lines on the screen
	ret z
	ld c,a
	ld a,iyh
	sub a,$20
	call mul320
	ld de,(_wide_buffer)
	add hl,de
	push hl
	ld a,c
	call mul320
	dec hl
	push hl
	pop bc
	pop hl
	ld (hl),0
	ld a,b
	or a,c
	ret z
	push hl
	pop de
	inc de
	ldir
	ret

; Sprites' x in the wide picture, lined up with the background's 10 pixel
; tiles: for fine scroll f, xtab[x + f] - w(f), with xtab[t] = 10*(t/8) +
; w(t%8) and w(k) = k + (k >= 4) (2 bytes per entry)
xtab:
repeat 256 + 8
	if ((% - 1) and 7) >= 4
		dw 10*((% - 1) shr 3) + ((% - 1) and 7) + 1
	else
		dw 10*((% - 1) shr 3) + ((% - 1) and 7)
	end if
end repeat

; render_sprites (render.asm) jumps here in the wide picture, ix = jit_scanline_vars
render_sprites_wide:
	bit 5,(ppu_ctrl_backup)
	jr nz,.big
	ld (s_size),8
	or a,a
	sbc hl,hl
	bit 3,(ppu_ctrl_backup)
	jr z,.l1
	ld l,4*3
.l1:
	lea de,chr_ptr_backup_0
	add hl,de
	lea de,s_bank0
	ld bc,3*4
	ldir
	jr .goto
.big:
	ld (s_size),16
	lea hl,chr_ptr_backup_0
	lea de,s_bank0
	ld bc,3*8
	ldir
.goto:
	ld a,(s_topclip)
	add a,$20 - 7
	ld (w_spr_hi.smc_top),a
	ld (w_spr_lo.smc_top),a
	; rows drawn: (row - top) mod 256 < botclip - topclip, as in the normal
	; picture (and the lines past the buffer's end wrap to below $20)
	ld a,(s_botclip)
	sub a,(s_topclip)
	ld (w_spr_hi.smc_range),a
	ld (w_spr_lo.smc_range),a
	; this frame's fine scroll f: x table from x + f, minus w(f) (in the base)
	ld a,(ppu_x_backup)
	and a,7
	ld c,a
	ld b,2
	mlt bc
	ld hl,xtab
	add hl,bc
	ld (w_sprite.smc_xtab),hl
	cp a,4
	ccf
	adc a,0			; w(f)
	; columns: x - w(f), + S_w when rotated (w_sprite works it out)
	ld de,0
	ld e,a
	or a,a
	sbc hl,hl
	ld a,(rs_rot)
	or a,a
	jr z,.shift
	ld hl,(fb_scroll_w)
.shift:
	or a,a
	sbc hl,de
	ld (w_sprite.smc_shift),hl
	ld hl,(_wide_buffer)
	ld de,-8*stride
	add hl,de
	ld (w_sprite.smc_base),hl
	ld iy,ppu_oam
.loop:
repeat 4
	call w_sprite
	lea iy,iy+4
end repeat
	ld a,iyl
	or a,a
	jr z,.done
	call pace_sprite_tick	; (pacing.asm: every 4 sprites)
	jr .loop
.done:
	ld ix,jit_scanline_vars
	ld a,(s_botclip)
	ld (s_topclip),a
	ld (s_botclip),232-1
	ret

; sets w_spr_hi's or w_spr_lo's steps for a sprite's flips: hl = the
; routine's smc addresses (w_hi_smc or w_lo_smc), e = attributes
w_set_flips:
	ld a,$14		; inc d (next line down)
	ld bc,stride
	bit 7,e
	jr z,.down
	inc a			; dec d
	ld bc,-stride
.down:
	push hl
	ld hl,(hl)
	ld (hl),a		; smc_ydir
	pop hl
	inc hl
	inc hl
	inc hl
	push hl
	ld hl,(hl)
	ld (hl),bc		; smc_skip
	pop hl
	inc hl
	inc hl
	inc hl
	ld a,$23		; inc hl
	push hl
	ld hl,-10
	bit 6,e
	jr z,.right
	ld hl,10
	ld a,$2B		; dec hl
.right:
	add hl,bc
	push hl
	pop bc			; row adjustment after drawing
	pop hl
	push hl
	ld hl,(hl)
	ld (hl),bc		; smc_adj
	pop hl
	ld b,6
.steps:
	inc hl
	inc hl
	inc hl
	push hl
	ld hl,(hl)
	ld (hl),a
	pop hl
	djnz .steps
	ret

w_hi_smc:
	dl w_spr_hi.smc_ydir, w_spr_hi.smc_skip, w_spr_hi.smc_adj
	dl w_spr_hi.s1, w_spr_hi.s2, w_spr_hi.s3, w_spr_hi.s4, w_spr_hi.s5, w_spr_hi.s6
w_lo_smc:
	dl w_spr_lo.smc_ydir, w_spr_lo.smc_skip, w_spr_lo.smc_adj
	dl w_spr_lo.s1, w_spr_lo.s2, w_spr_lo.s3, w_spr_lo.s4, w_spr_lo.s5, w_spr_lo.s6

; one sprite, iy = its OAM entry, ix = jit_scanline_vars (kept)
w_sprite:
	; x clipping
	ld a,(iy+3)
	or a,a
	ret z
	cp a,256-8
	ret nc
	ld b,(s_size)
	; y clipping
	ld e,(iy+0)
	ld a,(s_botclip)
	sub a,e
	ret c
	ret z
	ld a,e
	cp a,(s_topclip)
	jr nc,.tile
	sub a,(s_topclip)
	neg
	cp a,b
	ret nc
.tile:
	; flips and priority: set the drawing routine up if they changed
	ld a,(iy+2)
	and a,$E0
	ld e,a
	bit 5,a
	jr nz,.lo_flips
	ld hl,w_hi_flips
	cp a,(hl)
	jr z,.flips_set
	ld (hl),a
	ld hl,w_hi_smc
	call w_set_flips
	jr .flips_set
.lo_flips:
	ld hl,w_lo_flips
	cp a,(hl)
	jr z,.flips_set
	ld (hl),a
	ld hl,w_lo_smc
	call w_set_flips
.flips_set:
	ld b,(s_size)
	; tile data
	ld a,(iy+1)
	bit 4,b
	jr nz,.big
	ld h,4
	ld l,a
	jr .shared
.big:
	rrca
	ld l,a
	ld h,8
.shared:
	mlt hl
	ld a,l
	ld l,3
	mlt hl
	lea de,s_bank0
	add hl,de
	ld de,(hl)
	or a,a
	sbc hl,hl
	ld l,a
	add hl,hl
	add hl,hl
	add hl,de
	push hl
	; first row drawn: the top one, or the bottom one when flipped vertically
	ld c,(iy+0)
	bit 7,(iy+2)
	jr z,.down
	ld a,c
	add a,b
	dec a
	ld c,a
.down:
	; where: base + row*320 + x (+9 when flipped horizontally)
	or a,a
	sbc hl,hl
	ld l,(iy+3)
	add hl,hl
	ld de,xtab
.smc_xtab := $-3
	add hl,de
	ld de,0
	ld e,(hl)
	inc hl
	ld d,(hl)		; x
	; its column: x - w(f) (+ S_w, around the end of the lines: rotated)
	ld hl,0
.smc_shift := $-3
	add hl,de
	ld de,-stride
	add hl,de
	jr c,.column
	ld de,stride
	add hl,de
.column:
	ld (w_spr_col),hl
	ex de,hl
	or a,a
	sbc hl,hl
	ld h,c
	add hl,de		; row*256 + column
	ld d,c
	ld e,stride-256
	mlt de
	add hl,de		; + row*64
	ld de,0
.smc_base := $-3
	add hl,de
	bit 6,(iy+2)
	jr z,.left
	ld de,9
	add hl,de
.left:
	; palette: sprite colours are at $80 + 4*palette
	ld a,(iy+2)
	and a,11b
	add a,a
	add a,a
	add a,$80
	push hl
	exx
	pop hl
	ld c,a
	exx
	ld a,c
	add a,$20 - 8
	ld d,a			; line + $20
	ld e,b			; rows
	; keep the background under it (spr_restore puts it back next frame)
	call w_sprite_save
	pop ix
	; across the end of the lines (rotated, column 311 on)? its last k
	; columns land on the next lines: the background there goes down a
	; line first, and back up after
	xor a,a
	ld (w_spr_k),a
	ld hl,(w_spr_col)
	push de
	ld de,-311
	add hl,de
	pop de
	call c,w_across
	bit 5,(iy+2)
	jr nz,.behind
	call w_spr_hi
	ld a,e
	cp a,16
	jr nz,.done
	lea ix,ix+8
	call w_spr_hi
	jr .done
.behind:
	call w_spr_lo
	ld a,e
	cp a,16
	jr nz,.done
	lea ix,ix+8
	call w_spr_lo
.done:
	ld a,(w_spr_k)
	or a,a
	jr z,.not_across
	ld c,a
	ld a,(w_spr_lines)
	ld b,a
	ld a,(w_spr_line)
	ld hl,w_spr_save
	call w_lines_up
.not_across:
	ld ix,jit_scanline_vars
	ret

; w_sprite's sprite across the end of the lines: l + 1 = k columns on the
; next lines. Its lines (from (iy+0) - 8, e of them, within 0-223) move down
; a line in those columns. Keeps de, ix, iy
w_across:
	ld a,l
	inc a
	ld c,a			; k
	push de
	ld b,e			; the sprite's lines
	ld a,(iy+0)
	sub a,8			; the first line
	jr nc,.top_ok
	add a,b			; above the top: fewer lines
	jr nc,.none
	jr z,.none
	ld b,a
	xor a,a
.top_ok:
	ld d,a
	add a,b
	jr c,.clip
	cp a,224
	jr c,.lines
.clip:
	ld a,224
	sub a,d
	jr c,.none
	jr z,.none
	ld b,a
.lines:
	ld a,c
	ld (w_spr_k),a
	ld a,d
	ld (w_spr_line),a
	ld a,b
	ld (w_spr_lines),a
	ld a,d
	ld hl,w_spr_save
	call w_lines_down
.none:
	pop de
	ret

; Moves c columns of b lines from line a down a line (from the bottom one
; up), keeping the line below them's at hl first. Destroys af, bc, de, hl
w_lines_down:
	push hl
	push bc
	add a,b
	call mul320
	ld de,(_wide_buffer)
	add hl,de		; the line below
	pop bc
	pop de
	push hl
	push bc
	ld b,0
	ldir			; kept
	pop bc
	pop de
.line:
	push bc
	ld hl,-stride
	add hl,de		; the line above
	push hl
	ld b,0
	ldir
	pop de
	pop bc
	djnz .line
	ret

; Keeps the wide buffer bytes under the sprite at iy (OAM): its rows on the
; lines w_spr_hi/w_spr_lo draw (10 bytes each), then where the first one is
; (3 bytes), its x, its first line + $20 and rows, for w_spr_restore. e = its
; height (8 or 16). Out of room: spr_overflow bit 1 (every sprite's tiles are
; drawn again next frame, row_note_overlays). Keeps bc, de, hl, ix, iy, the
; alternate registers
w_sprite_save:
	push bc
	push de
	push hl
	ld b,e
	ld a,(iy+0)
	add a,$20 - 8
	ld l,a			; its first line + $20
	ld e,a
	ld a,(w_spr_hi.smc_top)
	ld c,a
	cp a,e
	jr c,$+3
	ld e,a			; from max(the sprite's, the clip's)
	; (counts, not ends: the clip can end at line 256)
	ld a,e
	sub a,l
	ld h,a			; its rows above that
	ld a,b
	sub a,h
	jp c,.none
	jp z,.none
	ld b,a
	ld a,e
	sub a,c
	ld d,a
	ld a,(w_spr_hi.smc_range)
	sub a,d
	jp c,.none
	jp z,.none
	cp a,b
	jr nc,$+3
	ld b,a			; rows kept
	ld a,e
	ld (.smc_first),a
	ld a,b
	ld (.smc_rows),a
	; room?
	ld hl,(spr_save_ptr)
	ld de,spr_save_end - (16*10 + w_spr_trailer)
	or a,a
	sbc hl,de
	jp nc,.full
	; the first row's top left: the buffer + line*320 + its column
	ld a,(.smc_first)
	sub a,$20		; its line in the buffer
	call mul320
	ld de,(_wide_buffer)
	add hl,de
	ld de,(w_spr_col)
	add hl,de
	ld (.smc_addr),hl
	; columns past the end of the lines (rotated): k = column - 310
	ex de,hl
	ld bc,-(stride - 10)
	add hl,bc
	ld a,l
	jr c,$+3
	xor a,a
	ld (.smc_k),a
	ld (.smc_k2),a
	neg
	add a,10
	ld (.smc_n),a		; 10 - k before the end
	ex de,hl
	ld de,(spr_save_ptr)
	ld a,(.smc_k)
	or a,a
	ld a,(.smc_rows)
	jr nz,.row
.row_fast:
	ld bc,10
	ldir
	ld bc,stride - 10
	add hl,bc
	dec a
	jr nz,.row_fast
	jr .rows_kept
.row:
	push hl
	ld bc,10
.smc_n := $-3
	ldir
	ld bc,0
.smc_k := $-3
	; (the rest from the line's start: -320)
	inc c
	dec c
	jr z,.row_kept
	push bc
	ld bc,-stride
	add hl,bc
	pop bc
	ldir
.row_kept:
	pop hl
	ld bc,stride
	add hl,bc
	dec a
	jr nz,.row
.rows_kept:
	ex de,hl
	ld de,0
.smc_addr := $-3
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ld a,(iy+3)
	ld (hl),a		; x
	inc hl
	ld de,(w_spr_col)
	ld (hl),e		; column
	inc hl
	ld (hl),d
	inc hl
	ld (hl),0		; k
.smc_k2 := $-1
	inc hl
	ld (hl),0
.smc_first := $-1
	inc hl
	ld (hl),0
.smc_rows := $-1
	inc hl
	ld (spr_save_ptr),hl
.none:
	pop hl
	pop de
	pop bc
	ret
.full:
	ld hl,spr_overflow
	set 1,(hl)
	jr .none

w_spr_trailer := 9	; where (3), x, column (2), k, first line + $20, rows

; Puts back what w_sprite_save kept, the last sprite first (render.asm
; spr_restore for the wide picture). A sprite whose rows the LCD hasn't
; all read yet stays: its tiles are drawn again this frame instead
w_spr_restore:
	or a,a
	sbc hl,hl
	ld (.seen),hl
	ld hl,(spr_save_ptr)
	ld de,spr_save
	or a,a
	sbc hl,de
	ret z
	add hl,de
.sprite:
	dec hl
	ld b,(hl)		; rows
	dec hl
	ld c,(hl)		; first line + $20
	dec hl
	ld a,(hl)		; columns past the end of the lines (k)
	ld (.smc_k),a
	neg
	add a,10
	ld (.smc_n),a
	dec hl
	dec hl
	ld de,(hl)		; column (and k)
	ld (.col),de
	dec hl
	ld a,(hl)		; x
	ld (.smc_x),a
	dec hl
	dec hl
	dec hl
	ld de,(hl)		; where the first row goes
	push de
	ld e,b
	ld d,10
	mlt de
	or a,a
	sbc hl,de		; its bytes
	pop de
	push hl			; (the sprite before it ends there)
	ld a,(spiUnlock.locked)
	or a,a
	jr nz,.put
	; has the LCD read its last row? (past it, or past the buffer's end)
	push hl
	push de
	ld h,b
	ld l,stride/2
	mlt hl
	add hl,hl		; rows*320
	add hl,de
	ex de,hl
	ld hl,0			; (the address read last first: the LCD only moves on)
.seen := $-3
	or a,a
	sbc hl,de
	jr nc,.read
	call lcd_read_address	; (pacing.asm: a read across $xxFFFF can be 64 KB off)
	ld (.seen),hl
	or a,a
	sbc hl,de
.read:
	pop de
	pop hl
	jr c,.mark
.put:
	ld a,(.smc_k)
	or a,a
	ld a,b
	jr nz,.row
.row_fast:
	ld bc,10
	ldir
	ex de,hl
	ld bc,stride - 10
	add hl,bc
	ex de,hl
	dec a
	jr nz,.row_fast
	jr .next
.row:
	push de
	ld bc,10
.smc_n := $-3
	ldir
	ld bc,0
.smc_k := $-3
	inc c
	dec c
	jr z,.row_put
	; (the rest at the line's start: -320)
	ex de,hl
	push bc
	ld bc,-stride
	add hl,bc
	pop bc
	ex de,hl
	ldir
.row_put:
	pop de			; the row's start
	push hl
	ld hl,stride
	add hl,de
	ex de,hl		; the next row's
	pop hl			; (the bytes)
	dec a
	jr nz,.row
.next:
	pop hl
	ld de,spr_save
	or a,a
	sbc hl,de
	add hl,de
	jp nz,.sprite
	ld (spr_save_ptr),hl
	xor a,a
	ld (spr_overflow),a
	ret
.mark:
	; its tiles in the bands of its lines: x/8 - 2 to (x+7)/8 (the margin
	; row_note_overlays gives wide sprites), or in the rotated picture the
	; slots under its columns
	ld a,(rs_rot)
	or a,a
	jr z,.mark_tiles
	ld a,c
	sub a,$20
	ld c,a
	add a,b
	dec a
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
	xor a,a
	ld (.col+2),a		; (its third byte is k: else div10 counted k*65536/10)
	ld hl,(.col)
	call w_slots_under
	jr .next
.mark_tiles:
	ld a,c
	sub a,$20
	ld c,a
	add a,b
	dec a
	rrca
	rrca
	rrca
	and a,31
	ld b,a			; last band
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
	ld a,0
.smc_x := $-1
	add a,7
	rra
	rrca
	rrca
	and a,31
	ld e,a			; hi
	ld a,(.smc_x)
	rrca
	rrca
	rrca
	and a,31
	sub a,2
	jr nc,$+3
	xor a,a
	ld d,a			; lo
	call mark_bands
	jp .next
.col: rb 3

; Marks the slots under the 10 columns from column hl (w_slots_under), or
; under columns hl to hl + de (w_slots_span), of the rotated wide picture
; (hl 0-319, de < 320), in b bands from band c (rowskip.asm mark_slots;
; across the end of the lines: two runs). Destroys af, bc, de, hl
w_slots_under:
	ld de,9
w_slots_span:
	push hl
	add hl,de
	call div10
	ld e,a			; the last slot (32 on: past the end of the lines)
	pop hl
	push de
	call div10
	pop de
	ld d,a			; the first
	ld a,e
	cp a,32
	jr c,.one_run
	sub a,32
	push af
	push bc
	ld e,31
	call mark_slots
	pop bc
	pop af
	ld d,0
	ld e,a
.one_run:
	jp mark_slots

; the hot code, copied to LCD cursor RAM by render_load_code
virtual at $E30800

; render.asm fill_slots, at the same place
w_fill_slots:
	pop.sis hl
	add hl,hl
	res 4,h			; the tile set in use (render.asm tile_set_patch)
.smc_set := $-1
	ld.sis hl,(hl)
	ld (iy+0),hl
	lea iy,iy+2
	djnz w_fill_slots
	ret
assert w_fill_slots.smc_set = $E30804	; (as render.asm's)

; render_background jumps here (the same address as render_background_loop)
assert $ = $E30800 + 17
w_render_background_loop:
	ld bc,0
	exx
.drawloop:
	; compute how many lines to draw
	ld a,8
	sub a,(y_fine)
	ld b,a
	add a,iyh
	jr c,.bottom_clip
	cp a,(end_y)
	jr c,.noclip
.clip:
	sub a,b
	sub a,(end_y)
	neg
	jp z,render_background.nextevent
	ld b,a
	jr .noclip
.bottom_clip:
	; clip to bottom of screen
	ld a,iyh
	neg
	ld b,a
.noclip:
	ld (y_len),b
	ld a,iyh
	cp a,$20
	jq nc,.draw
	add a,b
	cp a,$20+1	; is the end of this tile on screen?
	jq c,.nodraw
	; clip to top of screen
	sub a,$20
	ld (y_len),a
	ld b,a
	ld a,$20
	sub a,iyh
	add a,(y_fine)
	ld (y_fine),a
	ld iyh,$20
.draw:
	; is rendering on?
	bit 3,(mask)
	jr nz,.noblank
.blank:
	ld sp,temp_stack
	call row_forget_lines
	call w_blank_lines
	jq .nodraw
.noblank:
	; the same as in the last drawn frame? (rowskip.asm)
	ld sp,temp_stack
	call row_unchanged
	jp z,.nodraw
	call w_row_setup
	; only some tiles to draw again? (rowskip.asm)
	ld a,(row_partial_on)
	or a,a
	jr z,.whole
	ld hl,.nodraw
	jp row_partial
.whole:
	ld a,(x_len1)
	or a,a
	jr z,.right
	; find left nametable
	lea hl,t_nametable_0
	ld b,(nametable_select)
	ld c,3
	mlt bc
	add hl,bc
	ld hl,(hl)
	; + 2*(x_course+1)
	ld b,(x_course)
	ld c,2
	mlt bc
	add hl,bc
	; + 64*y_course
	ld e,64
	ld d,(y_course)
	mlt de
	add hl,de
	ld.sis sp,hl
	; loop if length > 4
	ld b,1
	ld a,(y_len)
	cp a,5
	jr c,$+3
	inc b
	ld e,b
	ld c,(x_len1)
	ld hl,.right
	ld a,(x_course)		; its first slot (compare mode)
	jp w_run_tiles
.right:
	; only draw right nametable if xlen2 > 0
	ld a,(x_len2)
	or a,a
	jr z,.right_none
	lea hl,t_nametable_0
	ld a,(nametable_select)
	xor a,1 		; toggle nametable
	ld c,a
	ld b,3
	mlt bc
	add hl,bc
	ld hl,(hl)
	ld e,64
	ld d,(y_course)
	mlt de
	add hl,de
	ld.sis sp,hl
	ld b,1
	ld a,(y_len)
	cp a,5
	jr c,$+3
	inc b
	ld e,b
	ld c,(x_len2)
	ld hl,(rs_after)	; .nodraw, or rowskip.asm row_refill
	xor a,a			; first slot 0
	jp w_run_tiles
.right_none:
	ld hl,(rs_after)
	jp (hl)
.nodraw:
	; compute y scroll for next line
	ld a,(y_fine)
	add a,(y_len)
	ld (y_fine),a
	cp a,8
	jr nz,.noincrement
.increment:
	ld (y_fine),0
	ld a,(y_course)
	inc a
	cp a,30 		; 30 resets to 0, but 31 doesnt
	jr nz,$+4
	inc a
	inc a
	cp a,32
	jr nz,.nonameswap
.nameswap:
	ld a,(nametable_select)
	xor a,10b 		; swap selected y nametable
	ld (nametable_select),a
	xor a,a
.nonameswap:
	ld (y_course),a
.noincrement:
	ld a,iyh
	add a,(y_len)
	jp c,render_background.exit
	ld iyh,a
	jq .drawloop

; a tile not in the cache yet: translate it (10 pixels per row, pixels 3
; and 7 twice). Entered from w_draw_tile in the alternate registers
w_fetch_tile:
	ld (.smc_de),de
	ld sp,temp_stack
	push ix
	ld ix,jit_scanline_vars	; (w_run_tiles has it elsewhere)
	push iy
	ld b,0			; (as render.asm fetch_tile)
	call pace_row_guard
	dec.sis sp
	dec.sis sp
	pop.sis hl
	push hl
	ld a,l
	; find chr data
	ld h,4 		; high 2 bits for bank
	mlt hl
	ld c,h
	ld l,6
	mlt hl
	lea de,t_bank0
	add hl,de
	ld iy,(hl)
	and a,111111b	; low 6 bits for offset
	ld d,a
	ld e,16
	mlt de
	add iy,de	; iy = chr data ptr

	ld de,(render_tile_next)
	ld hl,render_cache
	add hl,de
	ld bc,render_cache_end
	or a,a
	sbc hl,bc
	jr nz,.valid
	call flush_bank_cache_reload
	ld de,0
.valid:
	ld hl,80
	add hl,de
	ld (render_tile_next),hl

	pop hl
	ld a,(render_palettes shr 8) and $FF
	add a,h
	ld ix,render_palettes
	ld ixh,a
	call tile_store_ptr	; set tile ptr
	ld hl,(w_draw_tile.smc_src)
	add hl,de
	push hl		; the rows to draw now
	ld hl,render_cache
	add hl,de
	ex de,hl	; de = cache ptr
	ld ixl,8
	ld bc,0
.loop:
	; find debruijn entries for each set of 4 pixels
	ld hl,interleave_lut
	ld l,(iy+0)
	ld c,(hl)
	inc h
	ld b,(hl)
	ld l,(iy+8)
	ld a,(hl)
	dec h
	ld l,(hl)
	ld h,a
	add hl,hl
	add hl,bc
	ld b,h
	ld c,l
	ld hl,debrujin_mapping
	ld l,b
	ld b,(hl)
	ld l,c
	ld a,(hl)
	; copy from entries to cache, the 4th pixel of each twice
	lea hl,ix
	ld l,b
	ld bc,4
	ldir
	dec hl
	ldi
	lea hl,ix
	ld l,a
	ld bc,4
	ldir
	dec hl
	ldi
	inc bc
	inc iy
	dec ixl
	jr nz,.loop

	pop hl
	pop iy
	pop ix
	ld sp,stride - 10
	ld de,0
.smc_de := $-3
	ld a,10
	jp w_draw_tile.return

; sprites in front of the background. main: d = line + $20, ix = tile data;
; alternate: hl = first pixel, c = palette. The steps and
; adjustments are set by w_sprite for the sprite's flips
w_spr_hi:
	ld b,8
.outer:
	ld a,d
	sub a,0
.smc_top:=$-1
	cp a,0
.smc_range:=$-1
	jr nc,.skip
	exx
	ld d,(ix+8)
	ld e,(ix)
	ld a,d
	or a,e
	jr z,.skip_alt	; no pixels in this row
	ld b,2
.loop:
	px_hi .s1
	px_hi .s2
	px_hi .s3
	; the 4th pixel twice
	xor a,a
	rl d
	adc a,a
	rl e
	adc a,a
	jr z,.t4
	add a,c
	bit 7,(hl)
	jr nz,$+3
	ld (hl),a
.s4:	inc hl
	bit 7,(hl)
	jr nz,$+3
	ld (hl),a
	jr .s5
.t4:
.s6:	inc hl
.s5:	inc hl
	djnz .loop
	ld de,0
.smc_adj := $-3
	add hl,de
	exx
.next:
	inc ix
.smc_ydir:
	inc d
	djnz .outer
	ret
.skip:
	exx
.skip_alt:
	ld de,0
.smc_skip := $-3
	add hl,de
	exx
	jr .next

; sprites behind the background: drawn only where it is transparent, and
; they still hide the sprites after them (bit 7)
w_spr_lo:
	ld b,8
.outer:
	ld a,d
	sub a,0
.smc_top:=$-1
	cp a,0
.smc_range:=$-1
	jp nc,.skip
	exx
	ld d,(ix+8)
	ld e,(ix)
	ld a,d
	or a,e
	jp z,.skip_alt	; no pixels in this row
	ld b,2
.loop:
	px_lo .s1
	px_lo .s2
	px_lo .s3
	xor a,a
	rl d
	adc a,a
	rl e
	adc a,a
	jr z,.t4
	add a,c
	put_lo
.s4:	inc hl
	put_lo
	jr .s5
.t4:
.s6:	inc hl
.s5:	inc hl
	djnz .loop
	ld de,0
.smc_adj := $-3
	add hl,de
	exx
.next:
	inc ix
.smc_ydir:
	inc d
	dec b
	jp nz,.outer
	ret
.skip:
	exx
.skip_alt:
	ld de,0
.smc_skip := $-3
	add hl,de
	exx
	jr .next

repeat 1, n: $ - $E30800
	display 'wide render code in cursor RAM: ', `n, ' of 960 bytes', 10
end repeat
; Draws a run of c tiles (w_draw_tile's registers, hl = where to go on). In
; compare mode (fb_cmp, rowskip.asm) only the ones whose slot record
; differs from the tile cache entry they show now: a = the first one's slot,
; drawn at w_row_base + 10 * slot (the rotated frame buffer)
w_run_tiles:
	bit 0,(fb_cmp)
	jr nz,.compare
	; all of them: at the frame's scroll (fb_cmp 2), the right nametable's
	; from slot 0 (column 0 of the rotated frame buffer)
	bit 1,(fb_cmp)
	jr z,.run
	or a,a
	jr nz,.run
	exx
	ld de,(w_row_base)
	exx
.run:
	ld a,10
	jp w_draw_tile
.compare:
	ld (.smc_ret),hl
	ld d,c
	ld ix,(rs_slots)
	add a,a
	add a,ixl
	ld ixl,a
.tile:
	pop.sis hl
	add hl,hl
	res 4,h			; the tile set in use (render.asm tile_set_patch)
.smc_set := $-1
	ld.sis hl,(hl)		; its tile cache entry
	ld a,l
	cp a,(ix+0)
	jr nz,.draw
	ld a,h
	cp a,(ix+1)
	jr z,.next
.draw:
	bit 0,l
	jr z,.record
	scf
	sbc hl,hl		; not translated yet: record nothing
.record:
	ld (ix+0),l
	ld (ix+1),h
	dec.sis sp
	dec.sis sp
	ld a,ixl
	and a,63		; (records are 64-byte aligned): 2 * slot
	exx			; (no stack here: SPL is w_draw_tile's)
	ld l,a
	ld h,5
	mlt hl			; 10 * slot
	ld de,(w_row_base)
	add hl,de
	ex de,hl
	exx
	ld c,1
	ld hl,.next
	ld a,10
	jp w_draw_tile
.next:
	lea ix,ix+2
	dec d
	jr nz,.tile
	ld ix,jit_scanline_vars
	jp 0
.smc_ret := $-3

; w_draw_tile's way to w_fetch_tile (a jr from the SHA scratch area): a jp
; render_load_code puts in the last 4 bytes before it
w_fetch_tile_jp := $E30BBC
assert $ <= w_fetch_tile_jp
load w_render_data:$-$$ from $$
w_render_len := $-$$
end virtual

w_render_src:
	db w_render_data

; tile rows, 10 pixels each, into 320 pixel lines. Main: a = 10, b = groups
; of 4 rows, e = the same for every tile, c = tiles, hl = return address;
; alternate: de = the last tile's start + rows*320, SPS = nametable entries
virtual at sha_scratch
w_draw_tile:
.outer:
	exx
	ld hl,0			; -(rows*320 - 10)
.smc_k := $-3
	add hl,de
	ex de,hl		; de = this tile's top left pixel
	pop.sis	hl 		; tile ptr
	add hl,hl
	res 4,h			; the tile set in use (render.asm tile_set_patch)
.smc_set := $-1
	ld.sis hl,(hl)
	bit 0,l
	jr nz,w_fetch_tile_jp
	ld sp,0			; render_cache + y_fine*10
.smc_src := $-3
	add hl,sp
	ld sp,stride - 10
.return:
	jr $
.smc_offset:= $-1
.loop:
	exx
repeat 4
	ld c,a
	ldir
	ex de,hl
	add hl,sp
	ex de,hl
end repeat
	exx
	djnz .loop
	ld b,e
	dec c
	jr nz,.outer
	jp (hl)

assert $-$$ <= 64
load w_drawtile_data:$-$$ from $$
w_drawtile_len := $-$$
end virtual

w_drawtile_src:
	db w_drawtile_data
w_drawtile_set_byte := w_drawtile_src + (w_draw_tile.smc_set - w_draw_tile)
w_run_tiles_set_byte := w_render_src + (w_run_tiles.smc_set - $E30800)
w_fill_slots_set_byte := w_render_src + (w_fill_slots.smc_set - $E30800)
w_fetch_tile_addr:
	dl w_fetch_tile		; (render_load_code: the jump at w_fetch_tile_jp)

section .bss

public w_hi_flips
public w_lo_flips
w_row_base: rb 3		; the current row's line, biased for w_draw_tile (w_row_setup)
w_spr_col: rb 3			; w_sprite's column
w_spr_k: rb 1			; its columns across the end of the lines (0: none)
w_spr_line: rb 1
w_spr_lines: rb 1
w_spr_save: rb 10
w_col0: rb 3			; its first tile's column (rotated: + S_w, around 320)
wr_left: rb 3			; w_rot_row's state
wr_right: rb 3
wr_jw: rb 1
wr_fix: rb 1
wr_n: rb 1
wr_b1: rb 1
wr_b2: rb 1
wr_count: rb 1
wr_ptr: rb 3
wr_pieces: rb 3*7
wr_save: rb 10
w_hi_flips: rb 1		; the flips w_spr_hi is set up for ($FF: none yet)
w_lo_flips: rb 1

extern _wide_buffer
extern spr_save
extern rs_slots
extern fb_scroll_w
extern rs_rot
extern rs_after
extern render_background_loop.nodraw
extern spr_save_end
extern spr_save_ptr
extern spr_overflow
extern mark_bands
extern mark_slots
extern spiUnlock.locked
extern render_cache
extern render_cache_end
extern render_tile_next
extern render_background.nextevent
extern render_background.exit
extern row_unchanged
extern row_forget
extern row_forget_lines
extern row_partial
extern row_partial_on
extern flush_bank_cache_reload
extern tile_store_ptr
extern lcd_read_address
extern pace_sprite_tick
extern pace_row_guard
