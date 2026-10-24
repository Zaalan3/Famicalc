include 'ti84pceg.inc' 
include 'vars.inc'

section .text 

public _ui_init
public _ui_cleanup
public _ui_restart
public _ui_text
public _ui_text_width
public _pic_draw_runs
public _ui_fill
public _ui_screen_full
public _ui_screen_game
public overlay_label
public overlay_message
public fps_panel_setup
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
	ld iy,0
	add iy,sp
	ld hl,(iy+3)
	push hl
	call lcd_wait_frame
	call spiEndVSync
	xor a,a
	ld (spiUnlock.locked),a
	; the OS's timing, saved by render_init (8 bpp: 320x240 bytes per frame)
	ld hl,lcd_timing_backup
	ld de,ti.mpLcdTiming0
	ld bc,8
	ldir
	ld hl,239
	ld (ti.mpLcdTiming2+2),hl
	pop hl
	ld (ti.mpLcdUpbase),hl
	ld hl,320
	ld (ui_stride),hl
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
	push hl
	pop ix
	ld a,(ix+FONT_ROWS)
	ld (.rows),a
	ld hl,(ui_stride)
	ld (.stride),hl
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
	pop iy			; iy = its bits, a = the byte being read, e = bits left in it
	ld a,(iy+0)
	inc iy
	ld e,8
	ld hl,(.dest)
	ld d,0
.rows := $-1
.row:
	push hl
	ld b,0
.glyph_w := $-1
.pixel:
	add a,a
	jr nc,.clear
	ld (hl),0
.color := $-1
.clear:
	inc hl
	dec e
	jr nz,.next
	ld a,(iy+0)
	inc iy
	ld e,8
.next:
	djnz .pixel
	pop hl
	push de
	ld de,0
.stride := $-3
	add hl,de
	pop de
	dec d
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
FPS_FPS_CAPS := 28
FPS_DRAWN_CAPS := 34
; normal: big numbers (display font, its digits are lines 0-10) with "fps"
; and "drawn" under them, centered in the 40 pixel band left of the picture
fps_layout:
	db 4
	db FPS_NES, 13, 8, 14, 1, 11
	db FPS_DRAWN_NUM, 13, 36, 14, 1, 11
	db FPS_FPS, 0, 21, 40, 0, 10
	db FPS_DRAWN, 0, 49, 40, 0, 10
; wide: "60 FPS  56 DRAWN" in the strip above the picture (lines 0-7: capitals
; and digits only), from its left edge (x = 10)
fps_layout_wide:
	db 4
	db FPS_NES, 10, 0, 11, 0, 8
	db FPS_DRAWN_NUM, 46, 0, 11, 0, 8
	db FPS_FPS_CAPS, 24, 0, 0, 0, 8
	db FPS_DRAWN_CAPS, 60, 0, 0, 0, 8

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
	ld hl,_font_display
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
	ld a,(rs_rot)
	or a,a
	ld a,d
	jr z,$+7
	ld hl,fb_scroll_addr
	add a,(hl)
	ld (.column),a
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
	ld a,(.column)
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
	ld a,(.column)
	inc a
	call .copy
	pop hl
	ld bc,256
	add hl,bc
	ex de,hl
	pop hl
	ld bc,(ui_stride)
	add hl,bc
	ret
; bc bytes from de to column a of the frame line hl, wrapping around to its
; start in the rotated frame buffer
.copy:
	push bc
	ld bc,0
	ld c,a
	add hl,bc
	pop bc
	ex de,hl
	ld a,(rs_rot)
	or a,a
	jr z,.all
	xor a,a
	sub a,e			; columns left in the line (0: all of them)
	jr z,.all
	cp a,c
	jr nc,.all
	ld b,a
	ld a,c
	sub a,b
	push af			; the rest, from the line's start
	ld c,b
	ld b,0
	ldir
	ex de,hl
	ld bc,-256
	add hl,bc
	ex de,hl
	pop af
	ld c,a
.all:
	ldir
	ret
.column: rb 1

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
extern lcd_wait_frame
extern _font_body
extern _font_display
extern fps_label
extern row_note_label
extern _show_fps
extern spiLock
extern panel_put
extern panel_put.rows
extern rs_rot

section .data

private ui_stride
public _ui_stride
public game_fb
_ui_stride:
ui_stride: dl 256
game_fb: dl vbuffer		; the frame buffer the game is shown from
label_stage := ti.heapBot + 1024	; 3 KB of the OS's heap ($D18C7C, "used rarely"), after its flash routines: 12 lines of 256
