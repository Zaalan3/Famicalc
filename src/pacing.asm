include 'ti84pceg.inc'
include 'vars.inc'

section .text

public pace_init
public pace_resync
public pace_render_frame
public pace_frame_end
public pace_draw_start
public pace_draw_end
public fps_reset

; Frame pacing: the NES runs at 60 frames per second, and a frame is drawn
; when the emulator is on schedule or so little behind that the drawing will
; still be done before the LCD's next transfer. Drawing makes it fall behind;
; skipped frames catch up. At least every 4th frame is drawn.
; Timer 1 counts CPU cycles (48 MHz); times here are in units of 256 cycles.
;
; The LCD refreshes once per NES frame (lcdTiming) and the schedule is lined
; up with it: a frame is due just after the LCD starts sending the previous
; one. Drawing can then go on behind the LCD's reads, which are faster, as
; long as it ends before the next transfer (pace_draw_start).

frame_time := 3120		; 798,720 cycles: 60.096 frames/s (NTSC NES: 60.0988)
start_offset := 8		; frames are due this long after a transfer starts
draw_margin := 64		; added to the last drawing time for the next estimate
draw_decay := 32		; how fast the estimate follows shorter drawing times
max_skipped := 3		; frames skipped in a row at most
late_limit := 4*frame_time	; further behind than this: give up catching up

; starts timer 1 and the schedule (start of a game)
pace_init:
	or a,a
	sbc hl,hl
	ld (ti.mpTmr1Counter),hl
	xor a,a
	ld (ti.mpTmr1Counter+3),a
	ld hl,ti.tmr1Enable or ti.tmr1CountUp
	ld (ti.mpTmrCtrl),hl
	ld hl,frame_time
	ld (draw_estimate),hl
	call fps_reset
; schedule the next frame for the LCD's next transfer (after a pause, a state load...)
pace_resync:
	call lcd_wait_frame
	call lcd_sync
	ld hl,(lcd_ref)
	ld de,frame_time+start_offset
	add hl,de
	ld (pace_deadline),hl
	xor a,a
	ld (pace_late),a
	ld (pace_skipped),a
	ret

; hl = CPU cycles / 256 (wraps every 89 seconds)
pace_now:
	ld hl,(ti.mpTmr1Counter+1)
	ret

; the LCD has just finished sending a frame: note when that frame started
lcd_sync:
	ld hl,ti.mpLcdIcr
	set 3,(hl)
	call pace_now
	ld de,(lcd_transfer_time)
	or a,a
	sbc hl,de
	ld (lcd_ref),hl
	ret

; hl = time since the LCD started its last transfer (0 to frame_time-1)
; destroys af, bc, de
pace_lcd_phase:
	call pace_now
	push hl
	ld de,(lcd_ref)
	or a,a
	sbc hl,de
	ld bc,frame_time
	call __idvrmu		; hl = remainder
	pop de
	push hl
	ex de,hl
	or a,a
	sbc hl,de
	ld (lcd_ref),hl		; keep the reference recent
	pop hl
	ret

; Before drawing a frame into vbuffer. If the drawing will be done before
; the LCD's next transfer, it goes on behind the LCD's reads (the panel takes
; the frame at that transfer). Otherwise the panel is kept on the last frame
; (spiLock) until this one is done; it is unlocked between two transfers.
; Destroys af, bc, de, hl
pace_draw_start:
	call pace_lcd_phase
	push hl
	ld de,(draw_estimate)
	add hl,de
	ld de,frame_time
	or a,a
	sbc hl,de		; carry: fits before the next transfer
	pop hl
	ld a,(spiUnlock.locked)
	jr nc,.no_fit
	or a,a
	jr z,.start		; draw behind the LCD
	; the panel is still locked on an older frame: if the LCD is between
	; transfers, let it take this one at the next transfer instead
	ld de,(lcd_transfer_time)
	inc de
	inc de
	inc de
	inc de
	or a,a
	sbc hl,de
	call nc,spiUnlock
	jr .start
.no_fit:
	or a,a
	jr nz,.start		; already locked
	; let the LCD finish sending the last frame, then keep the panel on it
.wait:
	call pace_lcd_phase
	ld de,(lcd_transfer_time)
	inc de
	inc de
	inc de
	inc de
	or a,a
	sbc hl,de
	jr c,.wait
	call spiLock
.start:
	call pace_now
	ld (draw_start),hl
	ret

; unlocks the panel if the LCD is between two transfers. Destroys af, bc, de, hl
pace_unlock:
	ld a,(spiUnlock.locked)
	or a,a
	ret z
	call pace_lcd_phase
	ld de,(lcd_transfer_time)
	inc de
	inc de
	inc de
	inc de
	or a,a
	sbc hl,de
	ret c
	jp spiUnlock

; after drawing a frame: the estimate for the next one follows longer
; drawing times at once and shorter ones slowly
pace_draw_end:
	call pace_now
	ld de,(draw_start)
	or a,a
	sbc hl,de
	ld de,draw_margin
	add hl,de		; this frame's time + margin
	ex de,hl
	ld hl,(draw_estimate)
	ld bc,-draw_decay
	add hl,bc		; the last estimate, a bit lower
	or a,a
	sbc hl,de
	add hl,de
	jr nc,$+3		; keep the larger
	ex de,hl
	ld (draw_estimate),hl
	ret

; start of a frame. ix = jit_scanline_vars
; out: a = $80 to draw this frame, 0 to skip it
pace_render_frame:
	; no drawing for a while after starting or loading a state
	ld a,(frameskip)
	or a,a
	jr z,.decide
	dec a
	ld (frameskip),a
	xor a,a
	ret
.decide:
	ld a,(pace_late)
	or a,a
	jr z,.draw
	ld a,(pace_skipped)
	cp a,max_skipped
	jr nc,.draw
	inc a
	ld (pace_skipped),a
	xor a,a
	ret
.draw:
	xor a,a
	ld (pace_skipped),a
	ld hl,fps_drawn_count
	inc (hl)
	ld a,$80
	ret

; end of a frame (after drawing it, if it was drawn): wait until it is due
; destroys af, de, hl
pace_frame_end:
	ld hl,fps_nes_count
	inc (hl)
	call pace_now
	ex de,hl
	ld hl,(pace_deadline)
	or a,a
	sbc hl,de		; due - now
	jp m,.late
	; early: wait. Meanwhile give the screen its frame as soon as the
	; LCD has finished sending the previous one, and keep up with the LCD.
	ld hl,ti.mpLcdIcr
	set 3,(hl)
.wait:
	ld a,(ti.mpLcdUpcurr+2)
	cp a,$D5
	call z,spiUnlock
	ld a,(ti.mpLcdRis)
	bit 3,a
	call nz,lcd_sync
	call pace_now
	ex de,hl
	ld hl,(pace_deadline)
	or a,a
	sbc hl,de
	jp p,.wait
	xor a,a
	jr .next
.late:
	ld de,late_limit
	add hl,de		; carry: less than late_limit behind
	jr nc,.resync
	; a little behind, keep drawing: the next frame is due at a transfer
	; start, and is drawn behind the LCD if it will still be done before the
	; next transfer: late by less than frame_time - draw_estimate
	push hl
	ld hl,frame_time - start_offset
	ld de,(draw_estimate)
	or a,a
	sbc hl,de
	jr nc,$+6
	or a,a
	sbc hl,hl		; drawing takes more than a frame: no lateness allowed
	ex de,hl		; de = allowed lateness
	ld hl,late_limit
	or a,a
	sbc hl,de
	ex de,hl		; de = late_limit - allowed
	pop hl
	or a,a
	sbc hl,de		; carry: later than allowed
	ld a,0
	jr nc,.next
	inc a
	jr .next
.resync:
	; far behind (a slow scene, or the game was paused): start over from now
	call pace_now
	ld (pace_deadline),hl
	ld a,1
.next:
	ld (pace_late),a
	push af
	call pace_unlock
	pop af
	ld hl,(pace_deadline)
	ld de,frame_time
	add hl,de
	ld (pace_deadline),hl
	; FPS counter, about once a second
	ld a,(fps_nes_count)
	cp a,60
	ret c
	jq fps_update

; FPS counter label: NES frames per second (gold, red when the game is
; slowed down) and frames drawn per second
fps_update:
	call pace_now
	ld de,(fps_start)
	or a,a
	sbc hl,de
	ld (fps_time),hl
	ld a,(fps_nes_count)
	ld b,a
	call fps_rate
	ld c,C_GOLD
	cp a,58
	jr nc,$+4
	ld c,C_ERROR
	ld hl,fps_label
	ld (hl),c
	ld de,fps_label+1
	call fps_two_digits
	ld a,(fps_drawn_count)
	ld b,a
	call fps_rate
	ld de,fps_label_drawn
	call fps_two_digits
fps_reset:
	call pace_now
	ld (fps_start),hl
	xor a,a
	ld (fps_nes_count),a
	ld (fps_drawn_count),a
	ret

; a = round(b * 187500 / fps_time) (b frames over fps_time units; 187500 units = 1 s), at most 99
fps_rate:
	ld a,b
	or a,a
	ret z
	sbc hl,hl
	ld de,187500
.mul:
	add hl,de
	djnz .mul
	ld bc,(fps_time)
	call __idvrmu	; de = hl / bc, hl = remainder
	add hl,hl	; round half up
	or a,a
	sbc hl,bc
	jr c,.down
	inc de
.down:
	ex de,hl
	ld de,100
	or a,a
	sbc hl,de
	add hl,de
	ld a,l
	ret c
	ld a,99
	ret

; writes a (0-99) as two characters at de, advances de
fps_two_digits:
	ld b,'0'-1
.tens:
	inc b
	sub a,10
	jr nc,.tens
	add a,'0'+10
	ld c,a
	ld a,b
	cp a,'0'
	jr nz,$+4
	ld a,' '
	ld (de),a
	inc de
	ld a,c
	ld (de),a
	inc de
	ret

section .data

public fps_label
private fps_label_drawn

; overlay_label's list of colored parts
fps_label:
	db C_GOLD,'--',0
	db C_DIM,'fps',0
	db C_FAINT,7,0
	db C_TEXT
fps_label_drawn:
	db '--',0
	db C_DIM,'drawn',0
	db 0

section .bss

public lcd_transfer_time
private pace_deadline
private pace_late
private lcd_ref
private draw_start
private draw_estimate
private pace_skipped
private fps_start
private fps_time
private fps_nes_count
private fps_drawn_count
lcd_transfer_time: rb 3	; how long the LCD takes to send a frame (set by ui_screen_game)
pace_deadline: rb 3
pace_late: rb 1
lcd_ref: rb 3
draw_start: rb 3
draw_estimate: rb 3
pace_skipped: rb 1
fps_start: rb 3
fps_time: rb 3
fps_nes_count: rb 1
fps_drawn_count: rb 1

extern spiUnlock
extern spiUnlock.locked
extern spiLock
extern lcd_wait_frame
extern __idvrmu
