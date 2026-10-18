include 'ti84pceg.inc'
include 'vars.inc'

section .text

public pace_init
public pace_resync
public pace_render_frame
public pace_frame_end
public fps_reset
public fps_text

; Frame pacing: the NES runs at 60 frames per second, and a frame is drawn
; when the emulator is less than half a frame behind schedule. Drawing makes
; it fall behind; skipped frames catch up. At least every 4th frame is drawn.
; Timer 1 counts CPU cycles (48 MHz); times here are in units of 256 cycles.

frame_time := 3120		; 798,720 cycles: 60.096 frames/s (NTSC NES: 60.0988)
max_skipped := 3		; frames skipped in a row at most
tolerance := frame_time/2	; drawn frames may make the emulator this late
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
	call fps_reset
; schedule the next frame one frame from now (after a pause, a state load...)
pace_resync:
	call pace_now
	ld de,frame_time
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
	; LCD has finished sending the previous one.
.wait:
	ld a,(ti.mpLcdUpcurr+2)
	cp a,$D5
	call z,spiUnlock
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
	; up to "tolerance" behind, keep drawing: skipping a frame catches up
	; most of a frame, so small delays are better spread over several frames
	ld de,late_limit - tolerance
	or a,a
	sbc hl,de		; carry: more than tolerance behind
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
	ld hl,(pace_deadline)
	ld de,frame_time
	add hl,de
	ld (pace_deadline),hl
	; FPS counter, about once a second
	ld a,(fps_nes_count)
	cp a,60
	ret c
	jq fps_update

; "NN fps  NN drawn": NES frames and drawn frames per second
fps_update:
	call pace_now
	ld de,(fps_start)
	or a,a
	sbc hl,de
	ld (fps_time),hl
	ld a,(fps_nes_count)
	ld b,a
	call fps_rate
	ld de,fps_text
	call fps_two_digits
	ld hl,fps_text_mid
	ld bc,fps_text_mid_len
	ldir
	push de
	ld a,(fps_drawn_count)
	ld b,a
	call fps_rate
	pop de
	call fps_two_digits
	ld hl,fps_text_end
	ld bc,fps_text_end_len
	ldir
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

private fps_text_mid
private fps_text_end
fps_text_mid: db ' fps  '
fps_text_mid_len := $ - fps_text_mid
fps_text_end: db ' drawn',0
fps_text_end_len := $ - fps_text_end

section .bss

private pace_deadline
private pace_late
private pace_skipped
private fps_start
private fps_time
private fps_nes_count
private fps_drawn_count
pace_deadline: rb 3
pace_late: rb 1
pace_skipped: rb 1
fps_start: rb 3
fps_time: rb 3
fps_nes_count: rb 1
fps_drawn_count: rb 1
fps_text: rb 20

extern spiUnlock
extern __idvrmu
