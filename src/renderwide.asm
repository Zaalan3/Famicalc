include 'ti84pceg.inc'
include 'vars.inc'

section .text

public wide_code
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
	ld a,(x_start)		; 8 - fine scroll
	cp a,5
	ccf
	adc a,1
	ld de,0
	ld e,a
	add hl,de
	push hl
	exx
	pop de
	exx
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
; w(t%8) and w(k) = k + (k >= 4) (3 bytes per entry)
xtab:
repeat 256 + 8
	if ((% - 1) and 7) >= 4
		dl 10*((% - 1) shr 3) + ((% - 1) and 7) + 1
	else
		dl 10*((% - 1) shr 3) + ((% - 1) and 7)
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
	; this frame's fine scroll f: x table from x + f, minus w(f) (in the base)
	ld a,(ppu_x_backup)
	and a,7
	ld c,a
	ld b,3
	mlt bc
	ld hl,xtab
	add hl,bc
	ld (w_sprite.smc_xtab),hl
	cp a,4
	ccf
	adc a,0
	ld hl,(_wide_buffer)
	ld de,-8*stride
	add hl,de
	ld de,0
	ld e,a
	or a,a
	sbc hl,de
	ld (w_sprite.smc_base),hl
	ld iy,ppu_oam
.loop:
	call w_sprite
	lea iy,iy+4
	ld a,iyl
	or a,a
	jr nz,.loop
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
	push hl
	pop de
	add hl,hl
	add hl,de
	ld de,xtab
.smc_xtab := $-3
	add hl,de
	ld de,(hl)		; x
	or a,a
	sbc hl,hl
	ld h,c
	add hl,de		; row*256 + x
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
	pop ix
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
	ld ix,jit_scanline_vars
	ret

; the hot code, copied to LCD cursor RAM by render_load_code
virtual at $E30800

; render_background jumps here (the same address as render_background_loop)
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
	ld a,10
	jp w_draw_tile
.right:
	; only draw right nametable if xlen2 > 0
	ld a,(x_len2)
	or a,a
	jr z,.nodraw
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
	ld hl,.nodraw
	ld a,10
	jp w_draw_tile
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
	push iy
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
	add hl,hl 	; set tile ptr
	ld.sis (hl),e
	inc hl
	ld.sis (hl),d
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
	ld ix,jit_scanline_vars
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
	cp a,0
.smc_top:=$-1
	jr c,.skip
	cp a,$20
	jr c,.skip
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
	cp a,0
.smc_top:=$-1
	jp c,.skip
	cp a,$20
	jp c,.skip
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

assert $ <= $E30BC0
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
	ld.sis hl,(hl)
	bit 0,l
	jp nz,w_fetch_tile
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

section .bss

public w_hi_flips
public w_lo_flips
w_hi_flips: rb 1		; the flips w_spr_hi is set up for ($FF: none yet)
w_lo_flips: rb 1

extern _wide_buffer
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
