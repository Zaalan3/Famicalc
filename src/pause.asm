include 'ti84pceg.inc'
include 'vars.inc'

section .text

public open_menu

; Pause menu. Entered from set_save_slot (start of a frame) when [clear] is
; down, with the same stack as create_savestate/load_savestate expect:
; hl, bc, af of the running game pushed, ix = jit_scanline_vars.
; Runs the C menu (menu.c) on the C stack, then resumes the game or carries
; out the chosen action.
open_menu:
	call menu_call
	; the menu used render_cache (the tile cache) for its own data, and the
	; pause shouldn't count as a slow frame
	push af
	call flush_bank_cache
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
	; reset, ROM list or quit: _exit_reason says which
	jp _startJIT.return
.resume:
	pop hl
	pop bc
	pop af
	ret

; saves everything the emulator keeps in registers, calls pause_menu()
; out: a = action (0 resume, 1 save state, 2 load state, 3 leave the emulator)
menu_call:
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
	call _pause_menu
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
extern fps_reset
extern pace_resync
extern flush_bank_cache
