include 'ti84pceg.inc' 
include 'vars.inc'

section .text

public get_keys
public get_keys.skip_savestate
public set_save_slot
public slot_message
public _nes_keys
public _hot_keys
public _hot_state
public _clear_held
public _start_load
public _hotkey_pressed

HOT_COUNT := 8

; a = NES key result ([del] quits: set_save_slot)
get_keys:
.skip_savestate:
	ld hl,ti.mpKeyData
	ld de,_nes_keys 
	ld b,8 
	ld c,0 
.loop: 
	ld a,(de) 	; read row 
	inc de
	add a,a 
	add a,$10 
	ld l,a 
	ld a,(de) 	; read column 
	inc de 
	and a,(hl)
	jr z,.off 	; shift in bit
.on: 	
	scf 
.off:
	rl c 
	djnz .loop 	
	ld a,c 
	ld de,0
	ret

; the keys of the NES's buttons: group, mask (main.c sets them from the
; settings; these are the defaults)
_nes_keys:
	db 1,1 shl 5 		; A: 2nd
	db 2,1 shl 7 		; B: alpha
	db 3,1 shl 7 		; Select: X,T,theta,n
	db 1,1 shl 6 		; Start: mode
	db 7,1 shl 3 		; Up
	db 7,1 shl 0 		; Down
	db 7,1 shl 1 		; Left
	db 7,1 shl 2 		; Right

; the hotkeys (menu.h HOT_*), the same way: save state, load state, load the
; newest, next slot, previous slot, FPS counter, brighter, darker
_hot_keys:
	db 2,1 shl 1		; sto
	db 2,1 shl 2		; ln
	db 2,1 shl 3		; log
	db 5,1 shl 4		; )
	db 4,1 shl 4		; (
	db 5,1 shl 6		; vars
	db 6,1 shl 1		; +
	db 6,1 shl 2		; -


set_save_slot: 
	ld ix,jit_scanline_vars
	push af
	push bc 
	push hl
	
	; the library's Continue or Play from a state: load it now
	ld a,(_start_load)
	or a,a
	jr z,.keys
	xor a,a
	ld (_start_load),a
	jp load_savestate
.keys:
	; [del] quits (Save on quit: with a state first)
	ld a,(ti.mpKeyData+2)
	and a,$80
	jp nz,quit_game
	; [clear] opens the pause menu, unless it is still down from the
	; menu (which resumes as it goes down: _clear_held)
	ld hl,_clear_held
	ld a,(ti.mpKeyData+12)
	and a,1 shl 6
	jr z,.clear
	cp a,(hl)
	jp nz,open_menu
.clear:
	ld (hl),a		; still down from the menu, or let go
	
	; the hotkeys, once per press: c = the ones down (bit n: hotkey n)
	ld hl,ti.mpKeyData
	ld de,_hot_keys
	ld b,HOT_COUNT
	ld c,0
.hot:
	ld a,(de)
	inc de
	add a,a
	add a,$10
	ld l,a
	ld a,(de)
	inc de
	and a,(hl)
	add a,$FF		; carry: down
	rr c
	djnz .hot
	ld a,(_hot_state)
	cpl
	and a,c
	ld b,a
	ld a,c
	ld (_hot_state),a
	ld a,b
	or a,a
	jr z,.numbers
	; the first one pressed: save and load here, the others in C (menu.c hotkey)
	ld b,-1
.which:
	inc b
	rra
	jr nc,.which
	ld a,b
	or a,a
	jp z,create_savestate
	dec a
	jp z,load_savestate
	ld a,b
	ld (_hotkey_pressed),a
	jp open_hotkey
.numbers:
	
	; [0]..[9] pick slots 0-9 (the menu reaches all of them)
	ld hl,ti.mpKeyData
	ld de,number_list 
	ld b,10 
	ld c,0 
.loop: 
	ld a,(de) 	; read row 
	inc de
	add a,a 
	add a,$10 
	ld l,a 
	ld a,(de) 	; read column 
	inc de 
	and a,(hl)
	jr z,.off 	
.on:
	ld a,b
	dec a 
	or a,a
	sbc hl,hl
	ld l,a
	ld (_save_slot),hl
	ld hl,slot_selected_text
	call slot_message
	jr .end  
.off:
	djnz .loop 	
.end:
	pop hl 
	pop bc 
	pop af 
	ret

; shows a message: text at hl followed by the current slot as 3 digits
; ix = jit_scanline_vars, destroys af, bc, de, hl
slot_message:
	ld de,message_buffer
.copy:
	ld a,(hl)
	or a,a
	jr z,.number
	ld (de),a
	inc hl
	inc de
	jr .copy
.number:
	ld hl,(_save_slot)
	ld bc,-100
	call .digit
	ld bc,-10
	call .digit
	ld a,l
	add a,'0'
	ld (de),a
	inc de
	xor a,a
	ld (de),a
	ld hl,message_buffer
	ld (message_ptr),hl
	ld (message_len),60
	ret
.digit:
	ld a,'0'-1
.count:
	inc a
	add hl,bc
	jr c,.count
	sbc hl,bc
	ld (de),a
	inc de
	ret

slot_selected_text:
	db 'Slot ',0

number_list: 
	db 5,1 shl 3 		; 9
	db 4,1 shl 3 		; 8
	db 3,1 shl 3 		; 7
	db 5,1 shl 2 		; 6
	db 4,1 shl 2 		; 5
	db 3,1 shl 2 		; 4
	db 5,1 shl 1 		; 3
	db 4,1 shl 1 		; 2
	db 3,1 shl 1 		; 1
	db 3,1 shl 0 		; 0
	
	
extern create_savestate
extern load_savestate
extern open_menu
extern open_hotkey
extern quit_game
extern _save_slot

section .bss

private message_buffer
message_buffer: rb 32
_hot_state: rb 1		; the hotkeys down last frame
_clear_held: rb 1		; 1 shl 6: [clear] doesn't open the menu until it is let go
_start_load: rb 1		; load the state in start_slot at the first frame (main.c)
_hotkey_pressed: rb 1		; the hotkey for menu.c hotkey()
