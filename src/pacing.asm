include 'ti84pceg.inc'
include 'vars.inc'

section .text

public pace_init
public pace_resync
public pace_render_frame
public pace_frame_end
public pace_draw_start
public pace_draw_end
public lcd_unlock_if_sent
public pace_do_unlock
public pace_aborted
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
draw_decay := 8		; how fast the estimate follows shorter drawing times, besides 1/8 of the difference
max_skipped := 3		; frames skipped in a row at most
late_limit := 4*frame_time	; further behind than this: give up catching up
align_step := 16		; how far a frame's deadline moves toward its LCD phase per frame
fps_panel_time := 800		; writing the FPS counter's numbers into the panel takes ~450
late_allowed := frame_time - start_offset	; behind by more: skip the next frame (pace_render_frame)

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
; If the panel hasn't taken the last frame yet and can't before this one is
; drawn, this one isn't drawn: out: a = pace_aborted = 1 (else 0).
; Destroys af, bc, de, hl
pace_draw_start:
	xor a,a
	ld (pace_aborted),a
	; this frame's rotation goes to the panel with the frame (spiScroll;
	; the last one's is kept for a frame not drawn after all)
	ld a,(scroll_want)
	ld (scroll_kept),a
	ld hl,(scroll_want_w)
	ld (scroll_kept_w),hl
	ld a,(fb_scroll_addr)
	ld (scroll_want),a
	ld hl,(fb_scroll_w)
	ld (scroll_want_w),hl
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
	call lcd_unlock_if_sent
	jr .start
.no_fit:
	or a,a
	jr nz,.start		; already locked
	; let the LCD finish sending the last frame, then keep the panel on it.
	; The panel must have taken a whole transfer since it was unlocked
	; (always between transfers): if that was in this gap (after the last
	; transfer started), wait for the next
	call lcd_sending
	jr c,.wait
	call pace_now
	ld de,(unlock_time)
	or a,a
	sbc hl,de
	push hl			; time since the unlock
	call pace_lcd_phase	; time since the last transfer started
	pop de
	or a,a
	sbc hl,de
	jr c,.lock		; unlocked before that transfer: the panel has the frame
	; unlocked in this gap: the panel gets the last frame at the next
	; transfer; skip drawing this one rather than wait for that
	ld hl,fps_drawn_count
	dec (hl)
	; (the frame buffer keeps the last frame: its rotation too)
	ld a,(scroll_kept)
	ld (scroll_want),a
	ld hl,(scroll_kept_w)
	ld (scroll_want_w),hl
	ld a,1
	ld (pace_aborted),a
	ret
.wait:
	ld bc,8192
.sending:
	call lcd_sending
	jr nc,.lock
	dec bc
	ld a,b
	or a,c
	jr nz,.sending
.lock:
	; the read address reaches the end before the LCD's FIFO has sent the
	; last pixels: give them time (~1200 cycles) to reach the panel
	ld b,100
	djnz $
	call spiLock
.start:
	; unlocked: the panel takes this frame at the next transfer, and its
	; scroll with it (locked: spiUnlock sends it)
	ld a,(spiUnlock.locked)
	or a,a
	jr nz,.timed
	call spiScroll
.timed:
	call pace_now
	ld (draw_start),hl
	xor a,a
	ret

; unlocks the panel if the LCD has sent the whole frame buffer (its read
; address is at the end of it: between two transfers). Keeps bc, de, hl
lcd_unlock_if_sent:
	ld a,(spiUnlock.locked)
	or a,a
	ret z
	push hl
	push de
	ld hl,(ti.mpLcdUpcurr)
	ld de,(lcd_frame_end)
	or a,a
	sbc hl,de
	pop de
	pop hl
	ret c
	; let the LCD's FIFO send the last pixels first (see pace_draw_start)
	push bc
	ld b,100
	djnz $
	pop bc
; unlocks the panel and notes when. Keeps bc, de, hl
pace_do_unlock:
	push hl
	call pace_now
	ld (unlock_time),hl
	pop hl
	jp spiUnlock

; unlocks the panel if the LCD is between two transfers. Destroys af, bc, de, hl
pace_unlock := lcd_unlock_if_sent

; carry: the LCD is sending a frame (its read address is in the frame
; buffer); the wait loops above take ~200 cycles per call. Destroys de, hl
lcd_sending:
	push bc
	ld b,8
	djnz $
	pop bc
	ld hl,(ti.mpLcdUpcurr)
	ld de,(lcd_frame_end)
	or a,a
	sbc hl,de
	ret

; after drawing a frame: the estimate for the next one follows longer
; drawing times at once and shorter ones by an eighth of the difference
; (and draw_decay) per frame
pace_draw_end:
	call pace_now
	ld de,(draw_start)
	or a,a
	sbc hl,de
	ld de,draw_margin
	add hl,de		; this frame's time + margin
	ex de,hl
	ld hl,(draw_estimate)
	or a,a
	sbc hl,de		; estimate - this frame
	jr c,.longer
	; shorter: estimate -= difference/8 + draw_decay, down to this frame's
	push de
	push hl
	ld bc,8
	call __idvrmu		; de = difference/8
	ld hl,draw_decay
	add hl,de
	ex de,hl		; de = the decrease
	pop hl
	or a,a
	sbc hl,de
	jr nc,$+5
	or a,a
	sbc hl,hl
	pop de
	add hl,de
	ld (draw_estimate),hl
	ret
.longer:
	ld (draw_estimate),de
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
	; LCD has finished sending the previous one, keep up with the LCD, and
	; give the FPS counter its new numbers.
	ld hl,ti.mpLcdIcr
	set 3,(hl)
.wait:
	call lcd_unlock_if_sent
	ld a,(fps_panel_dirty)
	or a,a
	call nz,fps_panel_try
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
	; a little behind (up to a frame): keep drawing; pace_draw_start decides
	; whether the next frame is drawn behind the LCD or with the panel locked.
	; (Skipping as soon as the drawing wouldn't fit before the next transfer
	; dropped every frame after a long one: smb3-run drew 33 frames/s, 42 now)
	ld de,late_limit - late_allowed
	or a,a
	sbc hl,de		; carry: later than allowed
	ld a,0
	jr nc,.next
	inc a
	jr .next
.resync:
	; far behind (a slow scene, or the game was paused): start over from the
	; LCD's last transfer start (keeps frames lined up with the LCD)
	call pace_lcd_phase
	ex de,hl
	call pace_now
	or a,a
	sbc hl,de
	ld de,start_offset
	add hl,de
	ld (pace_deadline),hl
	ld a,1
.next:
	ld (pace_late),a
	push af
	call pace_unlock
	ld a,(fps_panel_dirty)
	or a,a
	call nz,fps_panel_try
	pop af
	ld hl,(pace_deadline)
	ld de,frame_time
	add hl,de
	ld (pace_deadline),hl
	call pace_align
	; FPS counter, about once a second
	ld a,(fps_nes_count)
	cp a,60
	ret c
	jq fps_update

; Moves pace_deadline by up to align_step toward start_offset after a
; transfer start (it can drift: the LCD's clock isn't the timer's, and
; catching up can leave it anywhere). Destroys af, bc, de, hl
pace_align:
	; hl = the deadline's phase - start_offset + frame_time/2, 0 to frame_time-1
	ld hl,(pace_deadline)
	ld de,(lcd_ref)
	or a,a
	sbc hl,de
	ld de,frame_time + frame_time/2 - start_offset
	add hl,de
	ld bc,frame_time
	call __idvrmu
	; clamp to frame_time/2 +- align_step, as 0 to 2*align_step
	ld de,frame_time/2 - align_step
	or a,a
	sbc hl,de
	jr nc,$+5
	or a,a
	sbc hl,hl
	ld de,2*align_step
	or a,a
	sbc hl,de
	add hl,de
	jr c,$+3
	ex de,hl
	; deadline -= the clamped difference
	ex de,hl
	ld hl,(pace_deadline)
	ld bc,align_step
	add hl,bc
	or a,a
	sbc hl,de
	ld (pace_deadline),hl
	ret

; The FPS counter's numbers go into the panel's memory over SPI (ui.asm
; fps_panel_numbers), which works only while the panel isn't taking the
; LCD's data: right away if the panel is locked, else locked just for that
; between two transfers, if the panel has taken a whole transfer since it
; was unlocked (see pace_draw_start) and the next one is far enough away.
; Else this is tried again (pace_frame_end). Destroys af, de, hl
fps_panel_try:
	push bc
	push ix
	push iy
	ld a,(spiUnlock.locked)
	or a,a
	jr nz,.write
	call lcd_sending
	jr c,.done
	call pace_now
	ld de,(unlock_time)
	or a,a
	sbc hl,de
	push hl			; time since the unlock
	call pace_lcd_phase	; time since the last transfer started
	pop de
	push hl
	or a,a
	sbc hl,de
	pop hl
	jr nc,.done		; unlocked in this gap
	ld de,fps_panel_time - frame_time
	add hl,de
	jr c,.done		; too close to the next transfer
	; the LCD's last pixels first (see pace_draw_start)
	ld b,100
	djnz $
	call spiLock
	call fps_panel_numbers
	call pace_do_unlock
	jr .written
.write:
	call fps_panel_numbers
.written:
	xor a,a
	ld (fps_panel_dirty),a
.done:
	pop iy
	pop ix
	pop bc
	ret

; FPS counter label: NES frames per second (white, red when the game is
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
	ld c,P_TEXT1
	cp a,58
	jr nc,$+4
	ld c,P_ACCENT
	ld hl,fps_label
	ld (hl),c
	ld de,fps_label+1
	call fps_two_digits
	ld a,(fps_drawn_count)
	ld b,a
	call fps_rate
	ld de,fps_label_drawn
	call fps_two_digits
	ld a,(_show_fps)
	ld (fps_panel_dirty),a
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

; the FPS counter's colored parts, each a label list of its own (ui.asm
; fps_layout)
fps_label:
	db P_TEXT1,'--',0,0
	db P_TEXT2,'FPS',0,0
	db P_TEXT3,7,0,0
	db P_TEXT2
fps_label_drawn:
	db '--',0,0
	db P_TEXT2,'DRAWN',0,0

section .bss

private unlock_time
pace_aborted: rb 1		; pace_draw_start skipped drawing this frame
fps_panel_dirty: rb 1		; the FPS counter has new numbers for the panel
unlock_time: rb 3		; when pace_do_unlock last unlocked the panel

public lcd_transfer_time
public lcd_frame_end
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
lcd_frame_end: rb 3	; the end of the frame buffer the LCD sends (set by ui_screen_game)
pace_deadline: rb 3
pace_late: rb 1
lcd_ref: rb 3
draw_start: rb 3
scroll_kept: rb 1		; the rotation of the frame in the frame buffer (pace_draw_start)
scroll_kept_w: rb 3
draw_estimate: rb 3
pace_skipped: rb 1
fps_start: rb 3
fps_time: rb 3
fps_nes_count: rb 1
fps_drawn_count: rb 1

extern spiUnlock
extern spiUnlock.locked
extern spiLock
extern spiScroll
extern scroll_want
extern scroll_want_w
extern fb_scroll_w
extern lcd_wait_frame
extern __idvrmu
extern fps_panel_numbers
extern _show_fps
