include 'ti84pceg.inc' 
include 'vars.inc'

section .text 

public _ui_init
public _ui_cleanup
public _ui_restart
public _ui_printString
public _ui_glyph
public _ui_fill
public _ui_screen_full
public _ui_screen_game
public overlay_label
public overlay_text_width
public overlay_message
public overlay_fps
	
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

; draws the set pixels of one 8x8 glyph (menu.c's proportional text)
; void ui_glyph(uint8_t *dest, const uint8_t *glyph, uint8_t left, uint8_t color)
; dest: frame buffer (ui_stride bytes per line), left: blank columns to skip
_ui_glyph:
	ld iy,0
	add iy,sp
	ld hl,(iy+3)
	ld de,(iy+6)
	ld c,(iy+12)
	ld b,8
.row:
	push bc
	push hl
	ld a,(de)
	inc de
	ld b,(iy+9)
	inc b
	jr .shift_start
.shift:
	add a,a
.shift_start:
	djnz .shift
.bit:
	add a,a
	jr nc,.clear
	ld (hl),c
.clear:
	inc hl
	or a,a
	jr nz,.bit
	pop hl
	push de
	ld de,(ui_stride)
	add hl,de
	pop de
	pop bc
	djnz .row
	ret

; In-game labels (FPS counter, messages): text on a small dark rounded
; label, drawn into the game's frame after it has been drawn.
; Uses the proportional font metrics from menu.c (glyph_left/glyph_width).

; hl = text -> bc = its width in pixels; destroys a, de, hl
overlay_text_width:
	ld bc,0
.loop:
	ld a,(hl)
	or a,a
	jr z,.done
	inc hl
	push hl
	and a,$7F
	or a,a
	sbc hl,hl
	ld l,a
	ld de,_glyph_width
	add hl,de
	ld a,(hl)
	inc a
	pop hl
	add a,c
	ld c,a
	jr nc,.loop
	inc b
	jr .loop
.done:
	ld a,b
	or a,c
	ret z
	dec bc
	ret

; ix = list of parts: color byte, then 0-terminated text; color 0 ends it
; d = x, e = y of the label's top left corner (game frame coordinates).
; It is drawn in label_stage (256 bytes per line), then copied into the
; frame (label_blit).
; destroys af, bc, de, hl, iy, ix
overlay_label:
	xor a,a
	ld (fps_cache_ok),a	; label_stage won't hold the FPS counter
	push de
	; total width
	lea hl,ix+0
	ld de,0
.measure:
	ld a,(hl)
	or a,a
	jr z,.measured
	inc hl
	push de
	call overlay_text_width
	pop de
	inc hl
	ex de,hl
	add hl,bc
	inc hl
	inc hl
	inc hl
	ex de,hl
	jr .measure
.measured:
	; label: width + 8 (minus the last part's 3 pixel gap), 12 tall
	ld hl,8-3
	add hl,de
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
	ld hl,label_stage
	ld (.origin),hl
	call .draw
	pop hl
	ld (ui_stride),hl
	pop de
	jp label_blit
.draw:
	; body, then the shorter top and bottom lines
	ld b,1
	call .down
	ld bc,10
	ld de,(.width)
	call .rect
	ld hl,(.origin)
	inc hl
	ld bc,1
	ld de,(.width)
	dec de
	dec de
	push hl
	call .rect
	pop hl
	ld b,11
	call .down
	ld bc,1
	ld de,(.width)
	dec de
	dec de
	call .rect
	; text: 4 pixels in, 2 down
	ld hl,(.origin)
	ld b,2
	call .down
	ld bc,4
	add hl,bc
.part:
	ld a,(ix+0)
	or a,a
	ret z
	ld (.color),a
	inc ix
.char:
	ld a,(ix+0)
	inc ix
	or a,a
	jr z,.next_part
	and a,$7F
	push hl
	; ui_glyph(dest, glyph, left, color)
	or a,a
	sbc hl,hl
	ld l,a
	push hl
	ld de,_glyph_left
	add hl,de
	ld e,(hl)
	pop hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld bc,_ui_font
	add hl,bc
	ld bc,0
.color := $-3
	push bc
	push de
	push hl
	ld hl,9
	add hl,sp
	ld hl,(hl)		; dest
	push hl
	call _ui_glyph
	pop hl
	pop bc
	pop de		; e = left
	pop bc
	; advance by the glyph's width + 1
	ld a,(ix-1)
	and a,$7F
	push hl
	or a,a
	sbc hl,hl
	ld l,a
	ld de,_glyph_width
	add hl,de
	ld a,(hl)
	inc a
	pop hl
	pop hl
	ld bc,0
	ld c,a
	add hl,bc
	jr .char
.next_part:
	inc hl		; 3 pixel gap between parts
	inc hl
	inc hl
	jr .part

; hl += b lines (b = 0 to 255); keeps de
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

; hl = top left, de = width, bc = height
.rect:
	push hl
	ld hl,C_BG
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

.width: rb 3
.origin: rb 3

; hl = message: a label centered at the bottom of the game screen
; destroys af, bc, de, hl, iy, ix
overlay_message:
	push hl
	call overlay_text_width
	pop hl
	ld de,message_label
	ld a,C_TEXT
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

; The FPS counter in the top left corner. Its numbers change about once a
; second (fps_update): it is drawn then, and label_stage keeps it for the
; frames in between, which only copy it in (much faster than drawing its
; text). label_stage is in the OS's heap, so the copy is dropped
; (fps_cache_ok = 0) whenever the OS may have run (pace_resync, fps_reset),
; and when another label used it.
; destroys af, bc, de, hl, iy, ix
overlay_fps:
	ld ix,fps_label
	ld de,12*256+6
	ld a,(fps_cache_ok)
	or a,a
	jr nz,.copy
	call overlay_label
	ld a,1
	ld (fps_cache_ok),a
	ret
.copy:
	ld bc,(overlay_label.width)
	ld a,e
	push de
	call row_note_label	; the tiles under it are drawn again next frame
	pop de
	; (label_blit)

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

;ui_printString(uint8_t x,uint8_t y,char* string);
_ui_printString: 
	ld iy,3 
	add iy,sp 
	ld de,$D40000
	ld a,(iy+3) 	; load x and y 
	add a,$20
	ret c 
	ld d,a
	ld e,(iy+0)
	push de
	ld de,(iy+6) 	; de = string ptr
	pop iy			; iy = screen ptr
.loop: 
	ld a,(de) 
	or a,a 
	ret z 
	inc de
; fetch character 
	ld b,8 
	ld c,a 
	mlt bc 
	ld hl,font 
	add hl,bc 
	ld c,8
.outer: 
	ld a,(hl)
	inc hl
	ld b,8
.inner: 	
	rla 
	jr c,.one 
	ld (iy),$FE
	inc iyl
	djnz .inner 
.rep:
	lea iy,iy-8
	inc iyh
	dec c 
	jr nz,.outer
	ld a,iyl
	cp a,$F0 	; if line ended, return 
	ret nc 
	ld a,iyh 
	sub a,8 
	ld iyh,a 
	lea iy,iy+8
	jr .loop
.one: 
	ld (iy),$FF
.smc_color := $-1
	inc iyl
	djnz .inner 
	jr .rep

	
section .data

public _ui_font

_ui_font:
font: 
	; curtesy of Graphx
	db	$00,$00,$00,$00,$00,$00,$00,$00 ;  
	db	$7E,$81,$A5,$81,$BD,$BD,$81,$7E ; ☺
	db	$7E,$FF,$DB,$FF,$C3,$C3,$FF,$7E ; ☻
	db	$6C,$FE,$FE,$FE,$7C,$38,$10,$00 ; ♥
	db	$10,$38,$7C,$FE,$7C,$38,$10,$00 ; ♦
	db	$38,$7C,$38,$FE,$FE,$10,$10,$7C ; ♣
	db	$00,$18,$3C,$7E,$FF,$7E,$18,$7E ; ♠
	db	$00,$00,$18,$3C,$3C,$18,$00,$00 ; •
	db	$FF,$FF,$E7,$C3,$C3,$E7,$FF,$FF ; ◘
	db	$00,$3C,$66,$42,$42,$66,$3C,$00 ; ○
	db	$FF,$C3,$99,$BD,$BD,$99,$C3,$FF ; ◙
	db	$0F,$07,$0F,$7D,$CC,$CC,$CC,$78 ; ♂
	db	$3C,$66,$66,$66,$3C,$18,$7E,$18 ; ♀
	db	$3F,$33,$3F,$30,$30,$70,$F0,$E0 ; ♪
	db	$7F,$63,$7F,$63,$63,$67,$E6,$C0 ; ♫
	db	$99,$5A,$3C,$E7,$E7,$3C,$5A,$99 ; *
	db	$80,$E0,$F8,$FE,$F8,$E0,$80,$00 ; ►
	db	$02,$0E,$3E,$FE,$3E,$0E,$02,$00 ; ◄
	db	$18,$3C,$7E,$18,$18,$7E,$3C,$18 ; ↕
	db	$66,$66,$66,$66,$66,$00,$66,$00 ; ‼
	db	$7F,$DB,$DB,$7B,$1B,$1B,$1B,$00 ; ¶
	db	$3F,$60,$7C,$66,$66,$3E,$06,$FC ; §
	db	$00,$00,$00,$00,$7E,$7E,$7E,$00 ; ▬
	db	$18,$3C,$7E,$18,$7E,$3C,$18,$FF ; ↨
	db	$18,$3C,$7E,$18,$18,$18,$18,$00 ; ↑
	db	$18,$18,$18,$18,$7E,$3C,$18,$00 ; ↓
	db	$00,$18,$0C,$FE,$0C,$18,$00,$00 ; →
	db	$00,$30,$60,$FE,$60,$30,$00,$00 ; ←
	db	$00,$00,$C0,$C0,$C0,$FE,$00,$00 ; └
	db	$00,$24,$66,$FF,$66,$24,$00,$00 ; ↔
	db	$00,$18,$3C,$7E,$FF,$FF,$00,$00 ; ▲
	db	$00,$FF,$FF,$7E,$3C,$18,$00,$00 ; ▼
	db	$00,$00,$00,$00,$00,$00,$00,$00 ;
	db	$C0,$C0,$C0,$C0,$C0,$00,$C0,$00 ; !
	db	$D8,$D8,$D8,$00,$00,$00,$00,$00 ; "
	db	$6C,$6C,$FE,$6C,$FE,$6C,$6C,$00 ; #
	db	$18,$7E,$C0,$7C,$06,$FC,$18,$00 ; $
	db	$00,$C6,$CC,$18,$30,$66,$C6,$00 ; %
	db	$38,$6C,$38,$76,$DC,$CC,$76,$00 ; &
	db	$30,$30,$60,$00,$00,$00,$00,$00 ; '
	db	$30,$60,$C0,$C0,$C0,$60,$30,$00 ; (
	db	$C0,$60,$30,$30,$30,$60,$C0,$00 ; )
	db	$00,$66,$3C,$FF,$3C,$66,$00,$00 ; *
	db	$00,$30,$30,$FC,$FC,$30,$30,$00 ; +
	db	$00,$00,$00,$00,$00,$60,$60,$C0 ; ,
	db	$00,$00,$00,$FC,$00,$00,$00,$00 ; -
	db	$00,$00,$00,$00,$00,$C0,$C0,$00 ; .
	db	$06,$0C,$18,$30,$60,$C0,$80,$00 ; /
	db	$7C,$CE,$DE,$F6,$E6,$C6,$7C,$00 ; 0
	db	$30,$70,$30,$30,$30,$30,$FC,$00 ; 1
	db	$7C,$C6,$06,$7C,$C0,$C0,$FE,$00 ; 2
	db	$FC,$06,$06,$3C,$06,$06,$FC,$00 ; 3
	db	$0C,$CC,$CC,$CC,$FE,$0C,$0C,$00 ; 4
	db	$FE,$C0,$FC,$06,$06,$C6,$7C,$00 ; 5
	db	$7C,$C0,$C0,$FC,$C6,$C6,$7C,$00 ; 6
	db	$FE,$06,$06,$0C,$18,$30,$30,$00 ; 7
	db	$7C,$C6,$C6,$7C,$C6,$C6,$7C,$00 ; 8
	db	$7C,$C6,$C6,$7E,$06,$06,$7C,$00 ; 9
	db	$00,$C0,$C0,$00,$00,$C0,$C0,$00 ; :
	db	$00,$60,$60,$00,$00,$60,$60,$C0 ; ;
	db	$18,$30,$60,$C0,$60,$30,$18,$00 ; <
	db	$00,$00,$FC,$00,$FC,$00,$00,$00 ; =
	db	$C0,$60,$30,$18,$30,$60,$C0,$00 ; >
	db	$78,$CC,$18,$30,$30,$00,$30,$00 ; ?
	db	$7C,$C6,$DE,$DE,$DE,$C0,$7E,$00 ; @
	db	$38,$6C,$C6,$C6,$FE,$C6,$C6,$00 ; A
	db	$FC,$C6,$C6,$FC,$C6,$C6,$FC,$00 ; B
	db	$7C,$C6,$C0,$C0,$C0,$C6,$7C,$00 ; C
	db	$F8,$CC,$C6,$C6,$C6,$CC,$F8,$00 ; D
	db	$FE,$C0,$C0,$F8,$C0,$C0,$FE,$00 ; E
	db	$FE,$C0,$C0,$F8,$C0,$C0,$C0,$00 ; F
	db	$7C,$C6,$C0,$C0,$CE,$C6,$7C,$00 ; G
	db	$C6,$C6,$C6,$FE,$C6,$C6,$C6,$00 ; H
	db	$7E,$18,$18,$18,$18,$18,$7E,$00 ; I
	db	$06,$06,$06,$06,$06,$C6,$7C,$00 ; J
	db	$C6,$CC,$D8,$F0,$D8,$CC,$C6,$00 ; K
	db	$C0,$C0,$C0,$C0,$C0,$C0,$FE,$00 ; L
	db	$C6,$EE,$FE,$FE,$D6,$C6,$C6,$00 ; M
	db	$C6,$E6,$F6,$DE,$CE,$C6,$C6,$00 ; N
	db	$7C,$C6,$C6,$C6,$C6,$C6,$7C,$00 ; O
	db	$FC,$C6,$C6,$FC,$C0,$C0,$C0,$00 ; P
	db	$7C,$C6,$C6,$C6,$D6,$DE,$7C,$06 ; Q
	db	$FC,$C6,$C6,$FC,$D8,$CC,$C6,$00 ; R
	db	$7C,$C6,$C0,$7C,$06,$C6,$7C,$00 ; S
	db	$FF,$18,$18,$18,$18,$18,$18,$00 ; T
	db	$C6,$C6,$C6,$C6,$C6,$C6,$FE,$00 ; U
	db	$C6,$C6,$C6,$C6,$C6,$7C,$38,$00 ; V
	db	$C6,$C6,$C6,$C6,$D6,$FE,$6C,$00 ; W
	db	$C6,$C6,$6C,$38,$6C,$C6,$C6,$00 ; X
	db	$C6,$C6,$C6,$7C,$18,$30,$E0,$00 ; Y
	db	$FE,$06,$0C,$18,$30,$60,$FE,$00 ; Z
	db	$F0,$C0,$C0,$C0,$C0,$C0,$F0,$00 ; [
	db	$C0,$60,$30,$18,$0C,$06,$02,$00 ; \
	db	$F0,$30,$30,$30,$30,$30,$F0,$00 ; ]
	db	$10,$38,$6C,$C6,$00,$00,$00,$00 ; ^
	db	$00,$00,$00,$00,$00,$00,$00,$FF ; _
	db	$C0,$C0,$60,$00,$00,$00,$00,$00 ; `
	db	$00,$00,$7C,$06,$7E,$C6,$7E,$00 ; a
	db	$C0,$C0,$C0,$FC,$C6,$C6,$FC,$00 ; b
	db	$00,$00,$7C,$C6,$C0,$C6,$7C,$00 ; c
	db	$06,$06,$06,$7E,$C6,$C6,$7E,$00 ; d
	db	$00,$00,$7C,$C6,$FE,$C0,$7C,$00 ; e
	db	$1C,$36,$30,$78,$30,$30,$78,$00 ; f
	db	$00,$00,$7E,$C6,$C6,$7E,$06,$FC ; g
	db	$C0,$C0,$FC,$C6,$C6,$C6,$C6,$00 ; h
	db	$60,$00,$E0,$60,$60,$60,$F0,$00 ; i
	db	$06,$00,$06,$06,$06,$06,$C6,$7C ; j
	db	$C0,$C0,$CC,$D8,$F8,$CC,$C6,$00 ; k
	db	$E0,$60,$60,$60,$60,$60,$F0,$00 ; l
	db	$00,$00,$CC,$FE,$FE,$D6,$D6,$00 ; m
	db	$00,$00,$FC,$C6,$C6,$C6,$C6,$00 ; n
	db	$00,$00,$7C,$C6,$C6,$C6,$7C,$00 ; o
	db	$00,$00,$FC,$C6,$C6,$FC,$C0,$C0 ; p
	db	$00,$00,$7E,$C6,$C6,$7E,$06,$06 ; q
	db	$00,$00,$FC,$C6,$C0,$C0,$C0,$00 ; r
	db	$00,$00,$7E,$C0,$7C,$06,$FC,$00 ; s
	db	$30,$30,$FC,$30,$30,$30,$1C,$00 ; t
	db	$00,$00,$C6,$C6,$C6,$C6,$7E,$00 ; u
	db	$00,$00,$C6,$C6,$C6,$7C,$38,$00 ; v
	db	$00,$00,$C6,$C6,$D6,$FE,$6C,$00 ; w
	db	$00,$00,$C6,$6C,$38,$6C,$C6,$00 ; x
	db	$00,$00,$C6,$C6,$C6,$7E,$06,$FC ; y
	db	$00,$00,$FE,$0C,$38,$60,$FE,$00 ; z
	db	$1C,$30,$30,$E0,$30,$30,$1C,$00 ; {
	db	$C0,$C0,$C0,$00,$C0,$C0,$C0,$00 ; |
	db	$E0,$30,$30,$1C,$30,$30,$E0,$00 ; }
	db	$76,$DC,$00,$00,$00,$00,$00,$00 ; ~
	db	$00,$10,$38,$6C,$C6,$C6,$FE,$00 ; △

	
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
extern _glyph_left
extern _glyph_width
extern fps_label
extern row_note_label
extern rs_rot

section .data

private ui_stride
public _ui_stride
public game_fb
public fps_cache_ok
_ui_stride:
ui_stride: dl 256
game_fb: dl vbuffer		; the frame buffer the game is shown from
fps_cache_ok: db 0		; label_stage holds the FPS counter as last drawn
label_stage := ti.heapBot + 1024	; 3 KB of the OS's heap ($D18C7C, "used rarely"), after its flash routines: 12 lines of 256
