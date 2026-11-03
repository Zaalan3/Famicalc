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
public pace_frame_drawn
public pace_do_unlock
public pace_aborted
public pace_row_guard
public pace_late_lock
public pace_draw_start.abort
public pace_draw_start.lock
public pace_sprite_guard.sending
public pace_row_guard.ahead
public pace_sprite_guard
public pace_sprites_done
public pace_skip_guard
public pace_mid_sprites
public pace_sprite_tick
public lcd_read_address
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
fps_panel_time := 1100		; writing the FPS counter's numbers into the panel takes 986-1057 units (normal picture; + the FIFO wait)
fps_panel_time_wide := 660	; and 565-616 in the wide one (fps_panel_try)
fps_wait := 40			; frames the numbers wait for a moment that costs no frame (fps_panel_try)
late_allowed := frame_time - start_offset	; behind by more: skip the next frame (pace_render_frame)
guard_margin := 128		; the panel is locked 2*guard_margin before the LCD's next transfer if the frame isn't done (pace_row_guard)
unlock_margin := 29		; no unlock this long before lcd_ref's next phase 0 (lcd_unlock_if_sent)
; waits for the LCD give up after ~2,300 units, two wide transfers (tests/pc
; lcd_timeouts; polls read its address twice, lcd_read_address)
lcd_polls := 1280		; lcd_sending calls (~460 cycles each) in a wait for the end of a transfer
row_polls := 2304		; polls (~250 cycles) in pace_row_guard's wait for a row's lines
lcd_lead := 5			; lcd_ref is this long after the LCD's first read (lcd_track; 21 after its sync, as lcd_sync measures it)

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

; Keeps lcd_ref on the LCD: while the LCD sends a frame, its read address
; says how long ago the transfer started (63 bytes a unit, normal and wide).
; Measured only from the VCOMP flag, lcd_ref went stale: lcd_wait_frame can
; time out after the panel is set up again (a menu), a flag seen late puts it
; late, and scenes that never wait in pace_frame_end never measure it again
; (smb3-run with the FPS counter on: 914 units off; frames drawn behind the
; LCD then went to the panel at the next frame's rotation). Called once per
; frame (pace_draw_start). Destroys af, bc, de, hl
lcd_track:
	call lcd_read_address
	ex de,hl
	call pace_now
	push hl
	ex de,hl
	ld de,(game_fb)
	or a,a
	sbc hl,de		; bytes read
	jr c,.no
	push hl
	ld hl,(lcd_frame_end)
	or a,a
	sbc hl,de		; the frame's size
	pop de
	or a,a
	sbc hl,de
	jr c,.no
	jr z,.no		; at the end: between transfers
	; de = units since the first read: bytes / 64 + bytes / 4096 (bytes /
	; 63.02; __idvrmu took ~8 units)
	ex de,hl
	add hl,hl
	add hl,hl		; bytes * 4 (less than 2^19)
	ld (lcd_track_tmp),hl
	ld de,(lcd_track_tmp+1)	; / 256 (lcd_track_tmp+3 stays 0)
	add hl,hl
	add hl,hl
	ld (lcd_track_tmp),hl
	ld a,(lcd_track_tmp+2)	; bytes * 16 / 65536
	ex de,hl
	ld de,0
	ld e,a
	add hl,de
	ex de,hl
	pop hl
	or a,a
	sbc hl,de
	ld de,lcd_lead
	add hl,de
	ld (lcd_ref),hl
	ret
.no:
	pop hl
	ret

; hl = time since the LCD started its last transfer (0 to frame_time-1)
; destroys af, bc, de
pace_lcd_phase:
	call pace_now
	push hl
	ld de,(lcd_ref)
	or a,a
	sbc hl,de
	; remainder by frame_time: lcd_ref is recent (subtractions take ~150
	; cycles, the division ~2000)
	ld bc,frame_time
	jp p,.mod_start
	add hl,bc		; lcd_ref just after now (lcd_track's lcd_lead): the last period's end
.mod_start:
	ld a,8
.mod:
	or a,a
	sbc hl,bc
	jr c,.rem
	dec a
	jr nz,.mod
	call __idvrmu		; (further from lcd_ref) hl = remainder
	jr .phase
.rem:
	add hl,bc
.phase:
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
	call lcd_track
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
	; it fits. Locked (on an older frame): drawn so, unlocked after
	; (pace_frame_end); unlocking now would show the frame half drawn if
	; the drawing takes longer than estimated
	or a,a
	jr nz,.start
	; draw behind the LCD, if the panel has the frame in the frame buffer
	call unlocked_in_gap
	jr nc,.abort
	; pace_row_guard locks the panel if the next transfer comes first
	ld hl,(lcd_ref)
	ld de,frame_time - 2*guard_margin
	add hl,de
	ld (guard_lock_at),hl
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
	call unlocked_in_gap
	jr c,.lock		; unlocked before that transfer: the panel has the frame
.abort:
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
	ld bc,lcd_polls
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

; carry: the panel was unlocked before the LCD's last transfer started, so
; it has taken a whole transfer since (the frame in the frame buffer); nc:
; unlocked after that (it takes the next one). Destroys af, bc, de, hl
unlocked_in_gap:
	call pace_now
	ld de,(unlock_time)
	or a,a
	sbc hl,de
	push hl			; time since the unlock
	call pace_lcd_phase	; time since the last transfer started
	pop de
	or a,a
	sbc hl,de
	ret

; Before a background row is drawn (rowskip.asm row_unchanged) while the
; panel takes the LCD's data: the frame being drawn must reach the panel
; whole, with the rotation it is drawn for. Near the LCD's next transfer
; (pace_draw_start set guard_lock_at), the panel is locked on the last frame
; (pace_late_lock). While the LCD sends the last frame, waits until it has
; read the row's lines: rows found unchanged can put the drawing ahead of it.
; Each piece of the drawing is checked before it: rows (this), rows found
; unchanged (pace_skip_guard), sprites (pace_mid_sprites, pace_sprite_guard,
; with their time in the last drawn frame). guard_lock_at is 2*guard_margin
; before the transfer: a row took up to 193 units (ff1-intro, wide), lcd_ref
; is ~20 units late and the lock takes ~4. The sprites' check needed that
; much before the transfer already, so the others lock no frame it would
; have let the panel take.
; in: iyh = the row's first line + $20, b = its lines.
; Keeps bc, ix, iy and the alternate registers. Destroys af, de, hl
pace_row_guard:
	ld a,(spiUnlock.locked)
	or a,a
	ret nz
	call pace_now
	ld de,(guard_lock_at)
	or a,a
	sbc hl,de
	jp p,pace_late_lock
	; the address after the row's last line
	ld a,iyh
	add a,b
	sub a,$20
; waits until the LCD has read the lines above line a (or isn't sending)
.line:
	or a,a
	sbc hl,hl
	ld h,a			; line * 256
	ld a,(_wide_buffer+2)
	or a,a
	jr z,.address
	ld d,h
	ld e,64
	mlt de
	add hl,de		; line * 320
.address:
	ld de,(game_fb)
	add hl,de
	ex de,hl
	push bc
	ld bc,row_polls		; (a timeout: the LCD is ~2.6 times faster than drawing)
.ahead:
	; between transfers, the read address is at the end: past the row
	call lcd_read_address
	or a,a
	sbc hl,de
	jr nc,.read
	dec bc
	ld a,b
	or a,c
	jr nz,.ahead
.read:
	pop bc
	ret

; Every 8 sprites while they are drawn (render_sprites_loop, every 4 in the
; wide picture, w_sprites): their check before them (pace_sprite_guard,
; pace_mid_sprites) takes their time in the last drawn frame, which can be
; much shorter (the first frame with sprites in the middle after one
; without: 0 for them; tests/pc panel_sprites: 700 units in one frame of
; 16), so the panel is locked here if the LCD's next transfer is near (as
; after a row found unchanged: 8 sprites take less than 2*guard_margin).
; Keeps bc, ix, iy. Destroys af, de, hl
pace_sprite_tick:
	ld a,(spiUnlock.locked)
	or a,a
	ret nz
	ld hl,(ti.mpTmr1Counter+1)	; (pace_now)
	ld de,(guard_lock_at)
	or a,a
	sbc hl,de
	ret m
	jp pace_late_lock

; A row found unchanged (rowskip.asm): nothing is drawn, but the next piece
; is checked as after a drawn row (a run of such rows took up to 320 units).
; Keeps bc, ix, iy. Destroys af, de, hl
pace_skip_guard:
	or a,a
	sbc hl,hl
; Before a piece of the drawing that takes hl units, while the panel takes
; the LCD's data: locks it on the last frame (pace_late_lock) if the piece
; wouldn't be done by guard_lock_at. Keeps bc, ix, iy. Destroys af, de, hl
pace_check:
	ld a,(spiUnlock.locked)
	or a,a
	ret nz
	ex de,hl
	call pace_now
	add hl,de
	ld de,(guard_lock_at)
	or a,a
	sbc hl,de
	ret m
	jp pace_late_lock

; Sprites drawn in the middle of a frame (render_background, where the
; sprite size or pattern table changes), on lines above there. They can take
; 300-500 units (SMB3 draws the playfield's sprites at its status bar):
; checked as a piece as long as all of them in the last drawn frame. The LCD
; must have read their lines first, as for rows (rows found unchanged can put
; the drawing ahead of it). ix = jit_scanline_vars. Destroys af, bc, de, hl
; (as render_sprites)
pace_mid_sprites:
	ld hl,(spr_mid_time)
	call pace_check
	ld a,(spiUnlock.locked)
	or a,a
	jr nz,.draw
	ld a,(s_botclip)
	sub a,7			; (render_sprites: their last line is s_botclip - 8)
	jr c,.draw		; (none: line -1 would wait for the timeout)
	cp a,225
	jr nc,.draw		; ($FF, an event at line 0: none either)
	call pace_row_guard.line
.draw:
	call pace_now
	push hl
	call render_sprites
	pop de
	call pace_now
	or a,a
	sbc hl,de
	ld de,(spr_mid_sum)
	add hl,de
	ld (spr_mid_sum),hl
	ret

; Before the sprites are drawn (anywhere on the screen) while the panel
; takes the LCD's data: locks it on the last frame if they wouldn't be done
; (as long as last time) by guard_lock_at, else waits until the LCD has sent
; the last frame. Destroys af, bc, de, hl
pace_sprite_guard:
	call pace_now
	ld (spr_start),hl
	ld hl,(spr_time)
	call pace_check
	ld a,(spiUnlock.locked)
	or a,a
	ret nz
	ld bc,lcd_polls
.sending:
	call lcd_sending
	ret nc
	dec bc
	ld a,b
	or a,c
	jr nz,.sending
	ret

; after the sprites: their time, for the next frame's pace_sprite_guard
; (and the ones drawn in the middle of the frame for pace_mid_sprites)
; destroys af, de, hl
pace_sprites_done:
	call pace_now
	ld de,(spr_start)
	or a,a
	sbc hl,de
	ld (spr_time),hl
	ld hl,(spr_mid_sum)
	ld (spr_mid_time),hl
	or a,a
	sbc hl,hl
	ld (spr_mid_sum),hl
	ret

; The drawing won't be done before the LCD's next transfer: the panel keeps
; the last frame (it has taken a whole transfer since it was unlocked:
; pace_draw_start), with that frame's rotation: pace_draw_start sent this
; one's. pace_frame_end unlocks it when the frame is done, which sends this
; one's again. Keeps bc, ix, iy. Destroys af, de, hl
pace_late_lock:
	push bc
	call spiLock
	ld a,(scroll_want)
	ld c,a
	ld hl,(scroll_want_w)
	push hl
	ld a,(scroll_kept)
	ld (scroll_want),a
	ld hl,(scroll_kept_w)
	ld (scroll_want_w),hl
	push bc
	call spiScroll
	pop bc
	pop hl
	ld (scroll_want_w),hl
	ld a,c
	ld (scroll_want),a
	pop bc
	ret

; render_draw is done: the FPS counter's numbers if the panel is locked and
; they are done in time (fps_panel_try), then the unlock. Keeps bc, de, hl
pace_frame_drawn:
	ld a,(fps_panel_dirty)
	or a,a
	jr z,lcd_unlock_if_sent
	push de
	push hl
	call pace_now
	push hl
	call fps_panel_try
	; its time (~1,000 units when written) isn't drawing time: draw_start
	; moves on by it (pace_draw_end, draw_estimate)
	call pace_now
	pop de
	or a,a
	sbc hl,de
	ld de,(draw_start)
	add hl,de
	ld (draw_start),hl
	pop hl
	pop de
	; (into lcd_unlock_if_sent)

; unlocks the panel if the LCD has sent the whole frame buffer (its read
; address is at the end of it: between two transfers), and the unlock is
; done before the LCD's sync: the panel takes the scroll (spiScroll, sent
; first by spiUnlock) and RM ($B0) at its frame start. One across it put the
; new scroll on the last frame (wide SMB3's status bar 10 px off); one after
; it isn't taken at that transfer although unlocked_in_gap counts it (the
; next frame is drawn behind the LCD: 27 torn in 6 s of zelda-title without
; the margin, pacetrace). RM is on 3.1-3.7 units after pace_do_unlock, 4.8
; with a new scroll (panel-trace), the phase is read after the FIFO wait,
; and lcd_track keeps lcd_ref 21-23 units after the sync: with
; unlock_margin every RM and VSP change came 11.8+ units before the panel's
; frame start. Keeps bc, de, hl
lcd_unlock_if_sent:
	ld a,(spiUnlock.locked)
	or a,a
	ret z
	push hl
	push de
	call lcd_read_address
	ld de,(lcd_frame_end)
	or a,a
	sbc hl,de
	jr c,.keep
	push bc
	; let the LCD's FIFO send the last pixels first (see pace_draw_start)
	ld b,100
	djnz $
	; the next transfer may have started meanwhile
	call lcd_read_address
	or a,a
	sbc hl,de
	jr c,.keep_bc
	call pace_lcd_phase
	ld de,-(frame_time - unlock_margin)
	add hl,de		; carry: too close to the sync
	call nc,pace_do_unlock
.keep_bc:
	pop bc
.keep:
	pop de
	pop hl
	ret
; unlocks the panel and notes when. Keeps bc, de, hl
pace_do_unlock:
	push hl
	call pace_now
	ld (unlock_time),hl
	pop hl
	jp spiUnlock

; unlocks the panel if the LCD is between two transfers. Destroys af, bc, de, hl
pace_unlock := lcd_unlock_if_sent

; hl = the LCD's read address (mpLcdUpcurr). Its 3 bytes are read one at a
; time while the LCD's DMA moves it: across $xxFFFF a read can take the upper
; byte after the carry and the others before, 64 KB too far (wide Zelda:
; $D2FFF0 read as $D3FFF0, past the frame buffer's end, so lcd_sending said
; the transfer was over and pace_draw_start locked the panel 22 lines into
; it: the panel kept the rest of the frame before, drawn at another
; rotation, and showed the HUD 30 pixels off). Read until two reads in a row
; are less than 256 bytes apart. Keeps a, bc, de
lcd_read_address:
	push de
.again:
	ld de,(ti.mpLcdUpcurr)
	ld hl,(ti.mpLcdUpcurr)
	push hl
	or a,a
	sbc hl,de
	ld de,$100
	or a,a
	sbc hl,de		; carry: 0 to 255 bytes further on
	pop hl
	jr nc,.again
	pop de
	ret

; carry: the LCD is sending a frame (its read address is in the frame
; buffer); a wait loop's poll takes ~460 cycles with it (lcd_polls).
; Destroys de, hl
lcd_sending:
	push bc
	ld b,8
	djnz $
	pop bc
	call lcd_read_address
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
	jr z,.vcomp
	call fps_panel_try
	; (it can take ~1,050 units: a transfer end seen after it would be late,
	; and lcd_sync would put lcd_ref that much after the LCD's)
	ld hl,ti.mpLcdIcr
	set 3,(hl)
.vcomp:
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
	; the FPS counter's numbers waited a frame more (fps_panel_try)
	ld hl,fps_panel_dirty
	ld a,(hl)
	or a,a
	jr z,.counted
	inc (hl)		; (forced from fps_wait on: kept from wrapping to 0)
	jr nz,.counted
	dec (hl)
.counted:
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
; LCD's data. Locked by pacing (the end of the drawing, pace_frame_drawn,
; or of the frame, pace_frame_end): written if done before the unlock's
; last moment (unlock_margin before the next transfer). Unlocked: locked
; just for them between two transfers, if the panel has taken a whole
; transfer since it was unlocked (see pace_draw_start) and they are done
; before that moment too. Numbers that found neither for fps_wait frames
; (fps_panel_dirty counts them: scenes drawn locked up to the sync found
; none for seconds, wide SMB3's map) are written at the next of these
; moments anyway, the panel left locked (lcd_unlock_if_sent unlocks it):
; a frame reaches the panel a transfer later. Destroys af, de, hl
fps_panel_try:
	push bc
	push ix
	push iy
	ld a,(spiUnlock.locked)
	or a,a
	jr nz,.locked
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
	; a frame drawn since that transfer started (behind the LCD) isn't in
	; the panel yet, but its scroll is (pace_draw_start): locked on the last
	; frame, the last frame's scroll goes back (pace_late_lock), else a lock
	; past the sync (a late or forced write) showed it at the new rotation
	ex de,hl
	call pace_now
	ld bc,(draw_start)
	or a,a
	sbc hl,bc
	or a,a
	sbc hl,de		; carry: drawn since
	ex de,hl		; (hl = the phase)
	push af
	; the LCD's last pixels first (see pace_draw_start)
	ld b,100
	djnz $
	call .room
	call c,.forced
	jr c,.skip
	pop af
	jr nc,.lock
	call pace_late_lock
	jr .lock_done
.lock:
	call spiLock
.lock_done:
	call fps_panel_numbers
	; unlocked only between transfers (lcd_unlock_if_sent), whether the
	; write was expected in time or forced: switching the panel back to
	; the LCD's data while the LCD sends a frame restarts the panel's
	; write address there, and the rest of the frame lands from its first
	; pixel (the picture shows shifted until the next transfer). If the
	; write ran into the next transfer, pacing unlocks after it
	call lcd_unlock_if_sent
	jr .written
.skip:
	pop af
	jr .done
.locked:
	call pace_lcd_phase
	call .room
	call c,.forced
	jr c,.done
	call fps_panel_numbers
.written:
	xor a,a
	ld (fps_panel_dirty),a
.done:
	pop iy
	pop ix
	pop bc
	ret
; c: the numbers written from LCD phase hl wouldn't be done by the
; unlock's last moment (lcd_unlock_if_sent). Destroys de, hl
.room:
	ld de,fps_panel_time - (frame_time - unlock_margin)
	ld a,(_wide_buffer+2)
	or a,a
	jr z,.normal
	ld de,fps_panel_time_wide - (frame_time - unlock_margin)
.normal:
	add hl,de
	ret
; nc: the numbers waited fps_wait frames: written even if late
.forced:
	ld a,(fps_panel_dirty)
	cp a,fps_wait
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
lcd_track_tmp: rb 4		; (its last byte stays 0)
fps_panel_dirty: rb 1		; (tests/pc: pace_aborted + 5) the FPS counter has new numbers for the panel (0: none; the frames they waited + 1)
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
guard_lock_at: rb 3	; the panel is locked from then on if the frame isn't drawn yet (pace_row_guard)
spr_start: rb 3
spr_time: rb 3		; the last drawn frame's sprites took this long
spr_mid_time: rb 3	; and the ones drawn in its middle (pace_mid_sprites)
spr_mid_sum: rb 3	; this frame's so far
pace_skipped: rb 1
fps_start: rb 3
fps_time: rb 3
fps_nes_count: rb 1
fps_drawn_count: rb 1

extern spiUnlock
extern spiUnlock.locked
extern spiLock
extern spiScroll
extern render_sprites
extern scroll_want
extern scroll_want_w
extern fb_scroll_w
extern lcd_wait_frame
extern __idvrmu
extern fps_panel_numbers
extern _show_fps
extern _wide_buffer
extern game_fb
