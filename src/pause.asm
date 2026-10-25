include 'ti84pceg.inc'
include 'vars.inc'

section .text

public open_menu
public open_hotkey
public quit_game

; Pause menu. Entered from set_save_slot (start of a frame) when [clear] is
; down, with the same stack as create_savestate/load_savestate expect:
; hl, bc, af of the running game pushed, ix = jit_scanline_vars.
; Runs the C menu (menu.c) on the C stack, then resumes the game or carries
; out the chosen action.
open_menu:
	ld hl,_pause_menu
	call c_call
	; the menu used render_cache (the tile cache) for its own data
	push af
	call flush_bank_cache
	pop af
	jr action

; a hotkey C handles (menu.c hotkey()), entered the same way
open_hotkey:
	ld hl,_hotkey
	call c_call
action:
	; the pause shouldn't count as a slow frame
	push af
	call pace_resync
	call fps_reset
	pop af
	ld ix,jit_scanline_vars
	or a,a
	jr z,.resume
	dec a
	jp z,create_savestate
	dec a
	jp z,load_savestate
	dec a
	jp z,create_savestate	; screen changed: go through a state (no file)
	; reset, the library or quit (_exit_reason); Save on quit saves first
	; (create_savestate leaves after that)
	ld a,(_quit_saving)
	or a,a
	jp nz,create_savestate
	jp _startJIT.return
.resume:
	pop hl
	pop bc
	pop af
	ret

; [del] in a game: quits, with a state first if Save on quit is on
quit_game:
	xor a,a
	ld (_exit_reason),a	; EXIT_QUIT
	ld a,(_save_on_quit)
	ld (_quit_saving),a
	or a,a
	jp nz,create_savestate
	jp _startJIT.return

; saves everything the emulator keeps in registers, calls the C function hl
; out: a = action (0 resume, 1 save state, 2 load state, 3 restart the
; emulator from its current state, 4 leave the emulator)
c_call:
	ld (.target),hl
	push de
	push iy
	exx
	push bc
	push de
	push hl
	exx
	ex af,af'
	push af
	ex af,af'
	ld hl,i
	push hl
	; 16-bit stack pointer: scanline event stack
	ld.sis hl,0
	add.sis hl,sp
	push hl
	ld a,mb
	push af
	ld (.smc_sp),sp
	; C environment: C stack (below where startJIT was called), MB and IY for the OS
	ld sp,(_startJIT.return+1)
	ld a,$D0
	ld mb,a
	ld iy,ti.flags
	call 0
.target := $-3
	di			; OS calls in the menu can enable interrupts
	ld sp,0
.smc_sp:=$-3
	ld (.smc_action),a
	pop af
	ld mb,a
	pop hl
	ld.sis sp,hl
	pop hl
	ld i,hl
	ex af,af'
	pop af
	ex af,af'
	exx
	pop hl
	pop de
	pop bc
	exx
	pop iy
	pop de
	ld a,0
.smc_action:=$-1
	ret

extern create_savestate
extern load_savestate
extern _startJIT.return
extern _pause_menu
extern _hotkey
extern _exit_reason
extern _save_on_quit
extern _quit_saving
extern fps_reset
extern pace_resync
extern flush_bank_cache
