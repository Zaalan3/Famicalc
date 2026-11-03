include 'ti84pceg.inc' 
include 'vars.inc'

section .text 

public _ui_init
public _ui_cleanup
public _ui_restart
public _ui_text
public _ui_text_width
public _pic_draw_runs
public _pic_capture
public _ui_icon
public _pic_sums
public _ui_fill
public _ui_background
public _ui_screen_full
public _ui_screen_lcd
public _ui_screen_game
public overlay_label
public overlay_message
public fps_panel_setup
public _fps_panel_erase
public fps_panel_numbers
	
_ui_init: 
	call render_init
	; set color 1 to white
	ld hl,ti.mpLcdPalette + $FF*2 
	ld (hl),$FF 
	inc hl 
	ld (hl),$FF 
	ret 
	
; sets the screen up again between games (unlike ui_cleanup, leaves
; pixelShadow alone: the C program's variables are there)
_ui_restart:
	call render_cleanup
	jr _ui_init

_ui_cleanup: 
	call render_cleanup
	; clear pixelShadow to prevent trash on homescreen
	ld hl,ti.pixelShadow
	ld de,ti.pixelShadow+1 
	ld bc,8400*2
	ld (hl),0
	ldir
	ret

; Screen modes. The game uses a 256x224 window (8 bpp, frame buffer at
; vbuffer, 256 bytes per line). The menus use the whole 320x240 screen
; (8 bpp, 320 bytes per line) with the frame buffer at the address given.

; void ui_screen_full(uint8_t *frame_buffer)
_ui_screen_full:
	call lcd_wait_frame
	call spiEndVSync
	xor a,a
	ld (spiUnlock.locked),a
	ld iy,0
	add iy,sp
; the OS's timing, saved by render_init (8 bpp: 320x240 bytes per frame),
; with the frame buffer at (iy+3)
os_timing:
	ld hl,lcd_timing_backup
	ld de,ti.mpLcdTiming0
	ld bc,8
	ldir
	ld hl,239
	ld (ti.mpLcdTiming2+2),hl
	ld hl,(iy+3)
	ld (ti.mpLcdUpbase),hl
	ld hl,320
	ld (ui_stride),hl
	ret

; void ui_screen_lcd(uint8_t *frame_buffer): while the panel is locked, the
; LCD's side of ui_screen_full ahead of it, in frames as long as the game's:
; the OS's timing (it takes ~8 ms a frame) with longer lines (HBP 256, HFP
; 107: 16.9 ms) and a front porch of 8 lines (0.4 ms). The LCD takes new
; timing at a frame's sync, and the game's frames end in a porch of 11.7 ms;
; from these, the OS's timing that ui_screen_full sets applies 0.4 ms after
; the frame it waits for (spiEndVSync takes 0.16 ms). Frames this long let
; the panel finish each refresh of its own before the next one starts (else
; it skips the LCD's next frame, and part of the old picture stays for one
; more refresh)
_ui_screen_lcd:
	ld iy,0
	add iy,sp
	call os_timing
	ld hl,ti.mpLcdTiming0+2
	ld (hl),107 - 1		; HFP
	inc hl
	ld (hl),256 - 1		; HBP
	ld l,ti.lcdTiming1+2 and $FF
	ld (hl),8		; VFP
	ret

; void ui_screen_game(void): back to the game's window
; or the wide picture (320x224 at wide_buffer) when wide_buffer is set
_ui_screen_game:
	call render_load_code
	call lcd_wait_frame
	; the FPS counter goes into the panel first: keep it off the LCD's data
	ld a,(_show_fps)
	or a,a
	jr z,.no_fps
	call spiLock
	push ix
	push iy
	call fps_panel_setup
	pop iy
	pop ix
.no_fps:
	ld a,(_wide_buffer+2)
	or a,a
	jr nz,.wide
	ld hl,lcdTiming
	call .timing
	ld hl,vbuffer
	ld (ti.mpLcdUpbase),hl
	ld (game_fb),hl
	ld hl,vbuffer + 256*224
	ld (lcd_frame_end),hl
	call spiInitVSync
	ld hl,910		; time the LCD takes to send a frame (pacing.asm units)
	ld de,256
	jr .done
.wide:
	ld hl,lcdTimingWide
	call .timing
	ld hl,(_wide_buffer)
	ld (ti.mpLcdUpbase),hl
	ld (game_fb),hl
	ld de,320*224
	add hl,de
	ld (lcd_frame_end),hl
	call spiInitVSyncWide
	ld hl,1138
	ld de,320
.done:
	ld (lcd_transfer_time),hl
	ld (ui_stride),de
	xor a,a
	ld (spiUnlock.locked),a
	ret
.timing:
	ld de,ti.mpLcdTiming0
	ld bc,8
	ldir
	ld hl,1023
	ld (ti.mpLcdTiming2+2),hl
	xor a,a
	ld (ti.mpLcdTiming2),a
	ret

; void ui_background(uint8_t *dest, uint8_t color): the menus' background,
; 240 lines of 320 bytes: the 8 colors from color on, top to bottom, each
; fading into the next over 30 lines through a 4x4 ordered dither. A line
; repeats its 4 pixels: 4 registers hold them 3 times over (12 bytes) and
; are pushed from the line's end (interrupts off). ~1,500 cycles a line
_ui_background:
	ld iy,0
	add iy,sp
	ld a,(iy+6)
	ld (.color),a
	ld hl,(iy+3)
	ld de,320
	add hl,de		; line 0's end
	push ix
	ld a,i			; P/V = interrupts were on
	push af
	di
	ld (.sp),sp
	ld sp,hl
	xor a,a
	ld (.level),a
	ld iy,240 * 256		; iyh = lines left, iyl = the level's fifteenths
.line:
	; the line's 4 pixels (x mod 4) at .pixels, the first 3 again after them
	ld a,iyh
	neg
	and a,3			; the line's row of the dither
	add a,a
	add a,a
	ld hl,.bayer
	ld de,0
	ld e,a
	add hl,de
	ld de,.pixels
	ld c,0			; sixteenths into the next band
.level := $-1
	ld b,4
.pixel:
	ld a,(hl)
	cp a,c			; carry: below the level, the next band's color
	ld a,0			; the band's color
.color := $-1
	adc a,0
	ld (de),a
	inc hl
	inc de
	djnz .pixel
	ld hl,(.pixels)
	ld (.pixels+4),hl
	; pushed from x = 319 down, 3 bytes each: x 317-319 are pixels 1-3,
	; 314-316 2, 3, 0, 311-313 3, 0, 1, 308-310 0-2 (then every 12 bytes)
	ld hl,(.pixels+1)
	ld de,(.pixels+2)
	ld bc,(.pixels+3)
	ld ix,(.pixels)
	ld a,2
.push:
	repeat 13
	push hl
	push de
	push bc
	push ix
	end repeat
	dec a
	jr nz,.push
	push hl			; x 5-7
	push de			; x 2-4
	dec sp
	dec sp
	ex (sp),ix		; x 0-2
	ld hl,640		; the next line's end
	add hl,sp
	ld sp,hl
	; 16 levels in 30 lines: 8/15 of a level a line
	ld a,iyl
	add a,8
	cp a,15
	jr c,.level_same
	sub a,15
	ld iyl,a
	ld hl,.level
	ld a,(hl)
	inc a
	and a,15
	ld (hl),a
	jr nz,.next
	ld hl,.color		; the next band
	inc (hl)
	jr .next
.level_same:
	ld iyl,a
.next:
	dec iyh
	jp nz,.line
	ld sp,0
.sp := $-3
	pop af
	jp po,$+5
	ei
	pop ix
	ret
.pixels: rb 7
.bayer:
	db 0, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5

; void pic_capture(struct picture *pic): the frame on screen (pictures.h):
; its colors, then every other pixel of every other line. The normal
; picture: lines of 256 at vbuffer, pixel x at column 8 + S + 2x (mod 256);
; the wide one: lines of 320 at wide_buffer, at column 10 + S_w + 5x/2 (mod
; 320: steps of 2 and 3). A row runs to the line's end, then on from its
; start. ~20 cycles a pixel
_pic_capture:
	ld iy,0
	add iy,sp
	ld de,(iy+3)
	ld hl,ti.mpLcdPalette
	ld bc,2 * PIC_COLORS
	ldir
	push de			; the pixels
	ld a,(_wide_buffer+2)
	or a,a
	jr z,.normal
	ld iy,(_wide_buffer)	; line 0
	ld hl,(scroll_want_w)
	ld de,10
	add hl,de		; column 0 (up to 329)
	ld bc,320		; the line's length
	ld de,$0123		; d: an odd pixel's step is 1 more, e: inc hl
	jr .go
.normal:
	ld iy,vbuffer
	ld a,(spi_scroll_sent)
	add a,8
	or a,a
	sbc hl,hl
	ld l,a
	ld bc,256
	ld de,0			; e: nop
.go:
	ld a,e
	ld (grab.extra),a
	or a,a
	sbc hl,bc
	jr nc,.in_line
	add hl,bc
.in_line:
	ld (.col0),hl
	; e = the row's pixels before the line's end, hl = the column of the next
	ld e,0
.split:
	or a,a
	sbc hl,bc
	jr nc,.split_end	; hl: on from the line's start
	add hl,bc
	inc hl
	inc hl
	ld a,e
	and a,d
	jr z,.even
	inc hl
.even:
	inc e
	ld a,e
	cp a,PIC_W
	jr c,.split
.split_end:
	ld (.col1),hl
	ld a,e
	ld (.count0),a
	ld (.odd1),a
	ld a,PIC_W
	sub a,e
	ld (.count1),a
	sbc hl,hl		; (no carry: e <= PIC_W)
	add hl,bc
	add hl,bc
	ld (.stride),hl
	pop de
	ld a,PIC_H
.row:
	push af
	lea hl,iy+0
	ld bc,0
.col0 := $-3
	add hl,bc		; no carry: from an even pixel
	ld a,0
.count0 := $-1
	call grab
	lea hl,iy+0
	ld bc,0
.col1 := $-3
	add hl,bc
	ld a,0
.odd1 := $-1
	rra			; carry: the rest starts at an odd pixel
	ld a,0
.count1 := $-1
	call grab
	ld bc,0
.stride := $-3
	add iy,bc
	pop af
	dec a
	jr nz,.row
	ret

; a pixels (0-120) of a picture row from hl to de, 2 columns apart, after
; an odd pixel 1 more in the wide picture (.extra); the first one is an odd
; pixel if carry
grab:
	ld c,255		; ldi counts bc down: b stays
	jr nc,.from_even
	inc a			; as if from the even pixel before it
	ld b,a
	srl b
	jr .odd
.from_even:
	ld b,a
	srl b
	jr z,.last
.pair:
	ldi
	inc hl
.odd:
	ldi
	inc hl
	nop			; inc hl in the wide picture
.extra := $-1
	djnz .pair
.last:
	rra			; carry: a last even pixel
	ret nc
	ldi
	ret

; void ui_icon(uint8_t *dest, const uint16_t *rows, uint8_t color): an
; 11x11 icon (icons.h: the leftmost pixel in bit 15), 320 bytes per line
_ui_icon:
	ld iy,0
	add iy,sp
	ld hl,(iy+3)
	ld de,(iy+6)
	ld c,(iy+9)
	ld a,11
.row:
	push af
	push hl
	inc de
	ld a,(de)		; the high byte: pixels 0-7
	ld b,8
.hi:
	add a,a
	jr nc,.hi_off
	ld (hl),c
.hi_off:
	inc hl
	djnz .hi
	dec de
	ld a,(de)		; pixels 8-10
	inc de
	inc de
	ld b,3
.lo:
	add a,a
	jr nc,.lo_off
	ld (hl),c
.lo_off:
	inc hl
	djnz .lo
	pop hl
	ld a,c
	ld bc,320
	add hl,bc
	ld c,a
	pop af
	dec a
	jr nz,.row
	ret

; void pic_sums(const uint8_t *dest, unsigned int sums[3]): red, green and
; blue (0-31 each) added up over a picture drawn at dest (320 bytes per
; line): every 4th pixel of every 7th line (480), colors from the LCD
; palette's 1555 entries. The sums are kept in the other register set
; (interrupts off meanwhile)
_pic_sums:
	ld iy,0
	add iy,sp
	push ix
	ld ix,(iy+3)
	ld a,i			; P/V = interrupts were on
	push af
	di
	exx
	ld bc,0			; red
	ld de,0			; green
	ld hl,0			; blue
	exx
	ld hl,ti.mpLcdPalette	; the upper byte stays
	ld c,16
.line:
	push ix
	ld b,30
.px:
	ld a,(ix+0)
	add a,a
	ld l,a
	ld a,0
	adc a,ti.mpLcdPalette shr 8 and $FF
	ld h,a
	ld e,(hl)		; gggbbbbb
	inc l
	ld d,(hl)		; 0rrrrrgg
	ld a,e
	and a,31
	exx
	add a,l
	ld l,a
	jr nc,.blue
	inc h
.blue:
	exx
	ld a,d
	rrca
	rrca
	and a,31
	exx
	add a,c
	ld c,a
	jr nc,.red
	inc b
.red:
	exx
	ld a,e
	rlca
	rlca
	rlca
	and a,7
	ld e,a
	ld a,d
	and a,3
	add a,a
	add a,a
	add a,a
	or a,e
	exx
	add a,e
	ld e,a
	jr nc,.green
	inc d
.green:
	exx
	lea ix,ix+4
	djnz .px
	pop ix
	ld de,320 * 7
	add ix,de
	dec c
	jr nz,.line
	ld hl,(iy+6)
	exx
	push hl
	push de
	push bc
	exx
	pop de
	ld (hl),de
	inc hl
	inc hl
	inc hl
	pop de
	ld (hl),de
	inc hl
	inc hl
	inc hl
	pop de
	ld (hl),de
	pop af
	jp po,.no_ei
	ei
.no_ei:
	pop ix
	ret

; void ui_fill(uint8_t *dest, unsigned int width, unsigned int height, uint8_t color)
; fills a rectangle of the frame buffer (ui_stride bytes per line), width >= 1
_ui_fill:
	ld iy,0
	add iy,sp
	; whole lines of the menus' frame buffer: one block, filled from its end
	; 3 lines (960 bytes, 320 pushes of 3 bytes) at a time
	ld hl,(iy+6)
	ld de,320
	or a,a
	sbc hl,de
	jr nz,.rect
	ld hl,(ui_stride)
	or a,a
	sbc hl,de
	jr nz,.rect
	ld a,(iy+12)
	ld (.color3),a
	ld (.color3+1),a
	ld (.color3+2),a
	; the block's end: dest + lines * 320
	or a,a
	sbc hl,hl
	ld l,(iy+9)
	push hl
	pop de
	add hl,hl
	add hl,hl
	add hl,de		; lines * 5
	ld b,6
.x64:
	add hl,hl
	djnz .x64
	ld de,(iy+3)
	add hl,de
	; no interrupt may push on the frame buffer meanwhile
	ld a,i			; P/V = interrupts were on
	push af
	di
	ld (.sp),sp
	ld sp,hl
	ld de,(.color3)
	ld a,(iy+9)
.three:
	sub a,3
	jr c,.left
	ld c,40
.push8:
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	push de
	dec c
	jr nz,.push8
	jr .three
.left:
	ld sp,0
.sp := $-3
	ld c,a
	pop af
	jp po,$+5
	ei
	ld a,c
	add a,3			; 0-2 lines left at the block's start: the usual way
	ret z
	ld c,a
	ld hl,(iy+3)
	ld a,(iy+12)
	jr .row
.color3: rb 3
.rect:
	ld hl,(iy+3)
	ld a,(iy+12)
	ld c,(iy+9)
.row:
	push bc
	push hl
	ld (hl),a
	push hl
	pop de
	inc de
	ld bc,(iy+6)
	dec bc
	ld a,b
	or a,c
	ld a,(hl)
	jr z,.one
	ldir
.one:
	pop hl
	ld de,(ui_stride)
	add hl,de
	pop bc
	dec c
	jr nz,.row
	ret

; void pic_draw_runs(const uint8_t *runs, uint8_t *dest)
; a stored picture's pixels (pictures.c: 0x00-0x9F a pixel, 0xA0-0xFF the last
; one again 2-97 times), 120 x 112, at dest (320 bytes per line)
PIC_W := 120
PIC_H := 112
PIC_COLORS := $A0		; palette entries a picture keeps (pictures.h)
_pic_draw_runs:
	ld iy,0
	add iy,sp
	ld de,(iy+3)
	ld hl,(iy+6)
	ld a,PIC_H
	ld (.lines),a
	ld b,PIC_W		; pixels left on this line
	ld c,0			; the last pixel
.token:
	ld a,(de)
	inc de
	cp a,$A0
	jr nc,.run
	ld c,a
	ld a,1
	jr .pixel
.run:
	sub a,$A0-2
.pixel:
	ld (hl),c
	inc hl
	djnz .next
	; the next line
	push de
	ld de,320-PIC_W
	add hl,de
	pop de
	ld b,PIC_W
	push af
	ld a,0
.lines := $-1
	dec a
	ld (.lines),a
	jr z,.done
	pop af
.next:
	dec a
	jr nz,.pixel
	jr .token
.done:
	pop af
	ret

; Text in FamiCalc's fonts (fonts.c; menu.c's fonts_init sets the offsets):
; struct font { uint8_t rows; const uint8_t *widths, *bits; const uint16_t *offsets; }
; A glyph is rows x width bits, row after row, leftmost pixel in the high bit.
FONT_ROWS := 0
FONT_WIDTHS := 1
FONT_BITS := 4
FONT_OFFSETS := 7

; int ui_text(const struct font *font, uint8_t *dest, const char *s, uint8_t color)
; draws the set pixels of s with its top left corner at dest (ui_stride bytes
; per line); returns its width (the glyphs and the 1 pixel gaps between them)
_ui_text:
	ld iy,0
	add iy,sp
	ld a,(iy+12)
	ld hl,(iy+3)
	ld de,(iy+6)
	ld bc,(iy+9)
; hl = font, de = dest, bc = text, a = color -> hl = width; .text = its end
; destroys af, bc, de, iy
text_draw:
	ld (.color),a
	push ix
	ld (.font),hl
	push hl
	pop ix
	ld a,(ix+FONT_ROWS)
	ld (.rows),a
	ld (.dest),de
	ld (.text),bc
	or a,a
	sbc hl,hl
	ld (.width),hl
.char:
	ld hl,(.text)
	ld a,(hl)
	or a,a
	jr z,.end
	inc hl
	ld (.text),hl
	ld ix,0
.font := $-3
	call glyph_find		; de = code, c = width
	ld a,c
	ld (.glyph_w),a
	ld hl,(ix+FONT_OFFSETS)
	add hl,de
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld hl,(ix+FONT_BITS)
	add hl,de
	push hl
	pop iy			; iy = its bits
	ld hl,(ui_stride)
	ld de,0
	ld e,c
	or a,a
	sbc hl,de
	ex de,hl		; de = stride - width
	ld hl,(.dest)
	ld c,0
.color := $-1
	ld ixl,0		; rows left
.rows := $-1
	ld a,$80		; the bits being read, then a 1 (only the 1 left: the next byte)
.row:
	ld b,0
.glyph_w := $-1
.pixel:
	add a,a
	jr c,.set_or_new
.clear:
	inc hl
	djnz .pixel
.row_end:
	add hl,de
	dec ixl
	jr nz,.row
	; the next glyph a pixel further
	ld hl,(.dest)
	ld de,0
	ld a,(.glyph_w)
	ld e,a
	inc de
	add hl,de
	ld (.dest),hl
	ld hl,(.width)
	add hl,de
	ld (.width),hl
	jr .char
.set_or_new:
	jr z,.new
.set:
	ld (hl),c
	inc hl
	djnz .pixel
	jr .row_end
.new:
	ld a,(iy+0)
	inc iy
	adc a,a			; the 1 shifted out goes back in after the byte
	jr nc,.clear
	jr .set
.end:
	pop ix
	ld hl,(.width)
	jr text_width.no_gap
.dest: rb 3
.text: rb 3
.width: rb 3

; a = character, ix = font -> de = its glyph ('?' if the font has none),
; c = its width. Destroys af, hl
glyph_find:
	and a,$7F
	ld de,0
	ld e,a
	ld hl,(ix+FONT_WIDTHS)
	add hl,de
	ld c,(hl)
	inc c
	dec c
	ret nz
	ld e,'?'
	ld hl,(ix+FONT_WIDTHS)
	add hl,de
	ld c,(hl)
	ret

; int ui_text_width(const struct font *font, const char *s)
_ui_text_width:
	ld iy,0
	add iy,sp
	ld hl,(iy+3)
	ld bc,(iy+6)
; hl = font, bc = text -> hl = its width, bc = its end (the 0). Destroys af, de
text_width:
	push ix
	push hl
	pop ix
	or a,a
	sbc hl,hl
.loop:
	ld a,(bc)
	or a,a
	jr z,.end
	inc bc
	push bc
	push hl
	call glyph_find
	pop hl
	ld de,0
	ld e,c
	inc de			; and the gap after it
	add hl,de
	pop bc
	jr .loop
.end:
	pop ix
.no_gap:
	ld a,h
	or a,l
	ret z
	dec hl			; no gap after the last glyph
	ret

; In-game labels (messages): text on a small dark label, drawn into the
; game's frame after it has been drawn.

; ix = list of parts: color byte, then 0-terminated text; color 0 ends it
; d = x, e = y of the label's top left corner (game frame coordinates).
; It is drawn in label_stage (256 bytes per line, 12 lines, the corner
; pixels left out), then copied into the frame (label_blit).
; destroys af, bc, de, hl, iy, ix
overlay_label:
	push de
	; width: the parts, 3 pixels between them, 4 at each end
	ld hl,8-3
	lea bc,ix+0
.measure:
	ld a,(bc)
	or a,a
	jr z,.measured
	inc bc
	push hl
	ld hl,_font_body
	call text_width		; hl = width, bc = its end
	inc bc
	pop de
	add hl,de
	ld de,3
	add hl,de
	jr .measure
.measured:
	ld (.width),hl
	pop de
	push de
	push hl
	pop bc
	ld a,e
	call row_note_label	; the tiles under it are drawn again next frame
	; draw it at label_stage's top left, then copy it in
	ld hl,(ui_stride)
	push hl
	ld hl,256
	ld (ui_stride),hl
	call .draw
	pop hl
	ld (ui_stride),hl
	pop de
	jp label_blit
.draw:
	; lines 1-10 whole, 0 and 11 without their end pixels
	ld hl,label_stage + 256
	ld bc,10
	ld de,(.width)
	call .rect
	ld hl,label_stage + 1
	ld bc,1
	ld de,(.width)
	dec de
	dec de
	call .rect
	ld hl,label_stage + 11*256 + 1
	ld bc,1
	ld de,(.width)
	dec de
	dec de
	call .rect
	; the parts: 4 pixels in, a line down
	ld de,label_stage + 256 + 4
.part:
	ld a,(ix+0)
	or a,a
	ret z
	inc ix
	push de
	lea bc,ix+0
	ld hl,_font_body
	call text_draw		; hl = width
	ld ix,(text_draw.text)
	inc ix			; past the text's 0
	pop de
	add hl,de
	ld de,3
	add hl,de
	ex de,hl		; the next part 3 pixels further
	jr .part

; hl = top left, de = width, bc = height
.rect:
	push hl
	ld hl,P_SURFACE
	ex (sp),hl
	push bc
	push de
	push hl
	call _ui_fill
	pop hl
	pop de
	pop bc
	pop hl
	ret

; hl += b lines (b = 0 to 255); keeps de (label_blit)
.down:
	inc b
	dec b
	ret z
	push de
	ld de,(ui_stride)
.line:
	add hl,de
	djnz .line
	pop de
	ret

.width: rb 3

; hl = message: a label centered at the bottom of the game screen
; destroys af, bc, de, hl, iy, ix
overlay_message:
	ld de,message_label
	ld a,P_TEXT1
	ld (de),a
	inc de
.copy:
	ld a,(hl)
	ld (de),a
	inc hl
	inc de
	or a,a
	jr nz,.copy
	ld (de),a
	; x = screen width / 2 - (width + 8) / 2
	ld bc,message_label + 1
	ld hl,_font_body
	call text_width
	push hl
	pop bc
	srl b
	rr c
	ld hl,(ui_stride)
	srl h
	rr l
	dec hl
	dec hl
	dec hl
	dec hl
	or a,a
	sbc hl,bc
	ld d,l
	ld e,206
	ld ix,message_label
	jp overlay_label

; The FPS counter, outside the game's picture: in the panel's own memory,
; where the LCD doesn't write (spiInitVSync). The normal picture has it in
; the black band to its left (the panel's fixed columns 0-39), the wide one
; in the black strip above it. It goes there over SPI (panel_put): all of
; it when the game's screen is set up (ui_screen_game), its numbers when
; they change (pacing.asm fps_panel_try, about once a second).

; all of the counter. The panel must not be taking the LCD's data.
; destroys af, bc, de, hl, ix, iy
fps_panel_setup:
	ld b,0			; all of the layout's parts
	jr fps_panel_numbers.layout

; its numbers (the layouts' first two parts). The panel must not be taking
; the LCD's data. destroys af, bc, de, hl, ix, iy
fps_panel_numbers:
	ld b,2
.layout:
	ld hl,fps_layout
	ld a,(_wide_buffer+2)
	or a,a
	jr z,$+6
	ld hl,fps_layout_wide
	ld a,(hl)		; the number of parts
	inc hl
	inc b
	dec b
	jr nz,.part
	ld b,a
.part:
	; part (fps_label + offset), x, y, box width (0: the text's), font, lines
	push bc
	ld de,0
	ld e,(hl)
	ld ix,fps_label
	add ix,de
	inc hl
	ld e,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ld c,(hl)
	inc hl
	ld a,(hl)
	inc hl
	ld (panel_text.font),a
	ld a,(hl)
	inc hl
	ld (panel_put.rows),a
	push hl
	call panel_text
	pop hl
	pop bc
	djnz .part
	ret

; fps_label's parts (pacing.asm), each ending its own list
FPS_NES := 0
FPS_FPS := 5
FPS_DRAWN_NUM := 15
FPS_DRAWN := 20
; font 1 is the hud font (menu.c): bold digits on lines 0-10, small caps on
; lines 0-4. Normal: the numbers with FPS and DRAWN under them, centered in
; the 40 pixel band left of the picture
fps_layout:
	db 4
	db FPS_NES, 12, 8, 15, 1, 11
	db FPS_DRAWN_NUM, 12, 36, 15, 1, 11
	db FPS_FPS, 0, 23, 40, 1, 5
	db FPS_DRAWN, 0, 51, 40, 1, 5
; wide: "60 FPS  56 DRAWN" in the strip above the picture (lines 0-7): body
; digits, the small caps on their baseline, from its left edge (x = 10)
fps_layout_wide:
	db 4
	db FPS_NES, 10, 0, 11, 0, 8
	db FPS_DRAWN_NUM, 50, 0, 11, 0, 8
	db FPS_FPS, 24, 3, 0, 1, 5
	db FPS_DRAWN, 64, 3, 0, 1, 5

; void fps_panel_erase(void): black where the counter was (it was turned
; off in a game: the panel's memory there keeps whatever is written to it)
_fps_panel_erase:
	push ix
	push iy
	call spiLock
	ld ix,.empty
	xor a,a
	ld (panel_text.font),a
	ld a,(_wide_buffer+2)
	or a,a
	jr nz,.wide
	ld a,52			; the band: lines 8-59
	ld (panel_put.rows),a
	ld de,0
	ld bc,8*256 + 40
	jr .write
.wide:
	ld a,8			; the strip: lines 0-7, columns 10-91
	ld (panel_put.rows),a
	ld de,10
	ld bc,0*256 + 82
.write:
	call panel_text
	pop iy
	pop ix
	ret
.empty: db P_BLACK,0,0

; Writes a part of a label list (color, text, 0) into the panel (panel_put):
; centered in a box of c pixels (0: the text's width) by panel_put.rows
; lines at x = de, y = b, on black; .font: 0 body, 1 display.
; in: ix = the part. destroys af, bc, de, hl, ix, iy
panel_text:
	ld (.x),de
	ld a,b
	ld (.y),a
	ld a,c
	ld (.w),a
	ld hl,_font_body
	ld a,0
.font := $-1
	or a,a
	jr z,$+6
	ld hl,_font_hud
	ld (.fontp),hl
	lea bc,ix+1
	call text_width		; hl = the text's width
	ld a,(.w)
	or a,a
	jr nz,$+7		; (ld a,l and ld (.w),a: 5 bytes)
	ld a,l
	ld (.w),a
	; blank columns before the text: (box - text) / 2
	sub a,l
	jr nc,$+3
	xor a,a
	rra
	ld (.blank),a
	; label_stage: w x lines of black, w bytes per line
	ld de,0
	ld a,(.w)
	ld e,a
	ld a,(panel_put.rows)
	ld d,a
	mlt de
	dec de
	push de
	pop bc
	ld hl,label_stage
	ld (hl),P_BLACK
	ld de,label_stage+1
	ldir
	ld hl,(ui_stride)
	push hl
	ld de,0
	ld a,(.w)
	ld e,a
	ld (ui_stride),de
	ld a,(.blank)
	ld e,a
	ld hl,label_stage
	add hl,de
	ex de,hl
	lea bc,ix+1
	ld a,(ix+0)
	ld hl,(.fontp)
	call text_draw		; its lines past the box are not sent
	pop hl
	ld (ui_stride),hl
	ld hl,label_stage
	ld de,(.x)
	ld a,(.y)
	ld b,a
	ld a,(.w)
	ld c,a
	jp panel_put
.x: rb 3
.fontp: rb 3
.y: rb 1
.w: rb 1
.blank: rb 1

; Copies the label in label_stage (overlay_label.width x 12; the corner
; pixels stay the game's) to d = x, e = y in the frame: at column x + S in
; the rotated frame buffer (rowskip.asm), across the end of the lines if
; need be. Destroys af, bc, de, hl
label_blit:
	; the column: x, + the rotation, around the end of the lines
	or a,a
	sbc hl,hl
	ld l,d
	ld a,(rs_rot)
	or a,a
	jr z,.column_known
	push de
	ld a,(_wide_buffer+2)
	or a,a
	jr nz,.wide
	ld a,(fb_scroll_addr)
	add a,l
	ld l,a
	jr .rotated
.wide:
	ld de,(fb_scroll_w)
	add hl,de
	ld de,-320
	add hl,de
	jr c,.rotated
	ld de,320
	add hl,de
.rotated:
	pop de
.column_known:
	ld (.column),hl
	ld hl,(game_fb)
	ld b,e
	call overlay_label.down
	ld de,label_stage
	call .corner
	ld b,10
.rows:
	push bc
	push hl
	push de
	ld bc,(overlay_label.width)
	xor a,a
	call .copy
	pop hl
	ld bc,256
	add hl,bc
	ex de,hl		; the next staged line
	pop hl
	ld bc,(ui_stride)
	add hl,bc		; the next frame line
	pop bc
	djnz .rows
; a top or bottom line: without its corners. hl = frame line, de = staged
.corner:
	push hl
	push de
	inc de
	ld bc,(overlay_label.width)
	dec bc
	dec bc
	ld a,1
	call .copy
	pop hl
	ld bc,256
	add hl,bc
	ex de,hl
	pop hl
	ld bc,(ui_stride)
	add hl,bc
	ret
; bc bytes from de to column (.column) + a of the frame line hl, going on
; from the line's start at its end (the rotated frame buffer)
.copy:
	ld (.bytes),bc
	push de
	push hl
	ld de,0
	ld e,a
	ld hl,(.column)
	add hl,de
	ld de,(ui_stride)
	or a,a
	sbc hl,de
	jr nc,$+3
	add hl,de		; the column
	ex de,hl
	or a,a
	sbc hl,de
	ld (.left),hl		; columns left in the line
	pop hl
	add hl,de
	ex de,hl		; de = where it goes
	pop hl			; hl = staged
	ld bc,(.left)
	push hl
	ld hl,(.bytes)
	or a,a
	sbc hl,bc
	pop hl
	jr c,.fits
	jr z,.fits
	; across the end of the line: the columns left, then from its start
	ldir
	push hl
	ld hl,(ui_stride)
	ex de,hl
	or a,a
	sbc hl,de
	ex de,hl		; the line's start
	ld hl,(.bytes)
	ld bc,(.left)
	or a,a
	sbc hl,bc
	push hl
	pop bc			; the rest
	pop hl
	ldir
	ret
.fits:
	ld bc,(.bytes)
	ldir
	ret
.column: rb 3
.bytes: rb 3
.left: rb 3

message_label: rb 40

extern render_init 
extern render_load_code
extern render_cleanup
extern spiEndVSync
extern spiInitVSync
extern spiUnlock.locked
extern lcd_timing_backup
extern lcdTiming
extern lcdTimingWide
extern spiInitVSyncWide
extern lcd_transfer_time
extern lcd_frame_end
extern _wide_buffer
extern scroll_want_w
extern spi_scroll_sent
extern lcd_wait_frame
extern _font_body
extern _font_hud
extern fps_label
extern row_note_label
extern _show_fps
extern spiLock
extern panel_put
extern panel_put.rows
extern rs_rot
extern fb_scroll_w

section .data

private ui_stride
public _ui_stride
public game_fb
_ui_stride:
ui_stride: dl 256
game_fb: dl vbuffer		; the frame buffer the game is shown from
label_stage := ti.heapBot + 1024	; 3 KB of the OS's heap ($D18C7C, "used rarely"), after its flash routines: 12 lines of 256
