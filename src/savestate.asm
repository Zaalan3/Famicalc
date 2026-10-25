include 'ti84pceg.inc' 
include 'vars.inc'

section .text

public create_savestate
public load_savestate
public _garbage_collect_preserve
public _garbage_collect_restore

size_of_savestate := 54345
savedata := render_cache

create_savestate: 
	; start by copying all relevant data to page $D50000
	; IWRAM at $D50000 
	; scanline events at $D50800 
	; CHR RAM 
	ld ix,jit_scanline_vars
	bit 0,(chr_ram_enable) 
	jr nz,.chr 
	; clear CHR-RAM area if used for other things
	ld hl,ppu_chr_ram 
	ld (hl),0 
	push hl 
	pop de 
	inc de 
	ld bc,8*1024 - 1 
	ldir 
.chr: 
	ld de,$D50C00
	ld hl,ppu_chr_ram
	ld bc,8*1024 
	ldir 
	; EWRAM 
	ld hl,jit_nes_ewram
	ld bc,32*1024 
	ldir 
	; Mapper data + nametables 
	ld hl,mapper_area
	ld bc,1024 + 512 + 8*1024 
	ldir 
	; Scanline vars 
	ld hl,jit_scanline_vars - $80 
	ld bc,256 
	ldir 
	; OAM + palettes 
	ld hl,ppu_oam 
	ld bc,256+32
	ldir 
	; PPU nametable / chr banks 
	ld hl,ppu_nametable_ptr
	ld bc,12+16
	ldir
	; page banks 
	ld hl,prg_page_bank
	ld c,4 
	ldir 
	; WRAM page 
	ld a,(jit_wram_bank) 
	ld (de),a 
	inc de 
	; finally, copy registers 
	pop hl 
	pop bc 
	pop af 
	ex af,af'
	ex de,hl 
	; A 
	ld (hl),a 
	inc hl 
	; X 
	ld (hl),b 
	inc hl 
	; Y 
	ld (hl),c 
	inc hl 
	; PC 
	ld (hl),de 
	inc hl
	inc hl 
	exx 
	push bc 
	push hl 
	exx 
	; S 
	pop de
	ld (hl),e 
	inc hl
	; P 
	pop bc 
	ld (hl),c
	inc hl
	ld (hl),b 
	inc hl 
	;de = size of savestate
	ex.sis de,hl
	
	; compress to $D40000
	; wait until front porch to ensure last buffer got sent  
	ld hl,ti.mpLcdIcr
	set 3,(hl)
	ld hl,ti.mpLcdRis
.l1: 	
	bit 3,(hl)  
	jr z,.l1
	
	call spiLock	; disable DMA to lcd driver; lets us mess with framebuffer
	
	; compress file to $D40000 
	ld hl,savedata 
	ld iy,$D50000 
	call lz_compress 
	
	; find length of compressed file
	ld de,savedata
	or a,a 
	sbc hl,de 
	ex de,hl
	
	; only going through the state to restart the emulator (screen change)?
	ld a,(_state_reload_only)
	or a,a
	jr z,.to_slot
	xor a,a
	ld (_state_reload_only),a
	ld (load_state_from_buffer.smc_quiet),a
	ld iy,savedata
	jr .continue
.to_slot:
	ld a,1
	ld (load_state_from_buffer.smc_quiet),a
	 
	; saveToSlot(savedata, size) writes the slot chosen in _save_slot
	ld sp,(_startJIT.return+1)
	push de
	ld hl,savedata 
	push hl
	ld iy,ti.flags
	
if safe_mode = 0 
	call port_setup 
	call port_unlock
	ld a,($E10001)
	in0	a,($06)
	res 2,a
	out0	($06),a
	call port_lock
end if 
	
	ld a,$D0
	ld mb,a
	call _saveToSlot
	di		; this call reenables interrupts
	pop de 
	pop de
	; a state on the way out (Save on quit): that was all (the OS's archive
	; calls left the keypad scanning once: back to continuous, like a load does)
	ld a,(_quit_saving)
	or a,a
	jr z,.saved
	ld a,3
	ld (ti.mpKeyMode),a
	jp _startJIT.return
.saved:
	ld de,0 
	xor a,a 
	sbc hl,de
	; savestate failed if return address = 0
	jr nz,.succeeded
	inc a
	ld iy,savedata
	jr .continue
.succeeded: 
	push hl 
	pop iy
.continue: 
	ld (load_state_from_buffer.smc_message),a 
	ld a,1
	ld (_state_is_new),a
	ld hl,$D50000 
	ld de,size_of_savestate
	call lz_decompress
	;unlock SHA scrap area 
if safe_mode = 0
	call port_setup 
	call port_unlock
	in0	a,($06)
	set 2,a
	out0	($06),a
	call port_lock
end if 

	jq load_state_from_buffer 
	
load_savestate:
	ld a,$D0
	ld mb,a
	ld iy,ti.flags
	call _getSaveSlot	; -> start of the compressed state, or 0
	ld a,jit_cache_page
	ld mb,a
	ld ix,jit_scanline_vars
	ld de,0
	or a,a 
	sbc hl,de 
	jr z,.failed 
	ld a,2
	ld (load_state_from_buffer.smc_message),a
	push hl 
	pop iy 
	ld hl,$D50000
	ld de,size_of_savestate
	call lz_decompress
	jq load_state_from_buffer
.failed: 
	ld ix,jit_scanline_vars
	ld hl,load_failed 
	call slot_message
	pop hl 
	pop bc 
	pop af 
	ld iy,$D50080
	
	ret 

	
load_state_from_buffer:
	ld sp,jit_call_stack_bot
	ld hl,$4000 
	push hl
	push hl
	push hl
	call flush_cache.skip_stack_reset
	call load_jit_search
	; mark all tiles as dirty
	ld de,render_chrram_flags+1
	ld hl,render_chrram_flags 
	ld (hl),1
	ld bc,512 - 1 
	ldir
	; set keyboard to continuous scan 
	ld a,3 
	ld (ti.mpKeyMode),a 
	; reload buffers 
	ld hl,$D50C00
	ld de,ppu_chr_ram
	ld bc,8*1024 
	ldir
	ld de,jit_nes_ewram
	ld bc,32*1024 
	ldir
	ld de,mapper_area
	ld bc,1024 + 512 + 8*1024
	ldir 
	; scanline variables: 256 bytes from ix-128, or from ix+0 in 0.2.1 states
	call state_block_is_v021
	jr z,.v021
	ld de,jit_scanline_vars - $80
	ld bc,256 
	ldir 
	jr .block_done
.v021:
	; 0.2.1 saved ix+0..255. Like 0.2.1, keep the game's current ix-128..-1
	; and leave out ix+128..255 (now the address translation table).
	ld de,jit_scanline_vars
	ld bc,128
	ldir
	ld bc,128
	add hl,bc
.block_done:
	ld de,ppu_oam 
	ld bc,256+32
	ldir
	ld de,ppu_nametable_ptr
	ld bc,12+16
	ldir
	ld de,prg_page_bank+4
	ld c,4 
	ldir 
	ld a,(hl)  
	inc hl 
	push hl 
	call prg_load_wram 
	; load chr pages
	ld ix,ppu_chr_bank
repeat 8 
	ld a,%-1 
	ld hl,(ix + ((%-1)*2))
	ex.sis de,hl 
	call chr_bank_swap
end repeat 
	; load PRG banks 
	ld hl,prg_page_bank
repeat 4 
	ld (hl),$FF 
	inc hl 
end repeat 
repeat 4 
	ld a,(prg_page_bank+4+%-1) 
	ld e,a 
	ld a,%-1 
	call prg_bank_swap
end repeat 
	pop hl
	; A 
	ld a,(hl)
	inc hl 
	; X 
	ld b,(hl) 
	inc hl 
	; Y 
	ld c,(hl) 
	inc hl 
	; PC 
	ld de,(hl) 
	inc hl
	inc hl 

	push hl 
	exx 
	pop hl 
	; S 
	ld de,$D50100
	ld e,(hl) 
	inc hl
	; P 
	ld c,(hl)
	inc hl
	ld b,(hl) 
	ex de,hl
	ld de,0
	exx 
	ex de,hl 
	push hl
	ex af,af'
	
	; clear rendering cache where the savestate was placed
	call flush_bank_cache
	
	; bc holds the NES's X and Y from here on
	push bc
	ld l,0 
.smc_message:=$-1
	ld h,3 
	mlt hl 
	ld de,.messages 
	add hl,de 
	ld hl,(hl) 
	ld ix,jit_scanline_vars 
	call slot_message
	ld a,1
.smc_quiet:=$-1
	or a,a
	jr nz,.message
	ld (message_len),a	; no message after a screen change
.message:
	ld a,1
	ld (.smc_quiet),a
	ld (frameskip),6
	call pace_resync
	pop bc
	ld a,jit_cache_page 
	ld mb,a 
	ld a,scanline_cycle_count
	ex af,af'
	ld iy,$D50080 
	ld de,0 
	pop hl 
	call jit_search 
	jp (ix)

.messages: 
	emit 3: save_succeeded,save_failed,load_succeeded
	
save_failed: 
	db 'Not enough memory for slot ',0 
save_succeeded: 
	db 'Saved slot ',0 
load_failed: 
	db 'Nothing saved in slot ',0
load_succeeded: 
	db 'Loaded slot ',0

; hl = scanline variable block of a decompressed state
; z if it was saved by FamiCalc 0.2.1 (block = ix+0..255). Its t_nametable
; pointers (ix+28..39) point into ppu_nametables ($D62000-$D63FFF); in the
; later layout the same bytes hold APU and IRQ variables. States with a header
; are always the later layout. Preserves hl.
state_block_is_v021:
	ld a,(_state_is_new)
	or a,a
	jr nz,.no
	push hl
	ld de,28
	add hl,de
	ld b,4
.check:
	inc hl
	ld a,(hl)
	and a,$E0
	cp a,$20
	jr nz,.pop
	inc hl
	ld a,(hl)
	cp a,$D6
	jr nz,.pop
	inc hl
	djnz .check
.pop:
	pop hl
	ret
.no:
	or a,a		; nz
	ret


_garbage_collect_preserve:
	di
	call spiUnlock
	; move data from VRAM 
	ld hl,$D60000 
	ld de,jit_nes_ewram
	ld bc,$5800 
	ldir 
	; change graphics mode 
	call render_cleanup
	ld hl,$D40000 
	ld de,$D40001 
	ld (hl),$FF 
	ld bc,150*1024 - 1
	ldir
	ret

_garbage_collect_restore: 
	; restore graphics mode 
	call _ui_init
	; restore previous state 
	ld de,$D60000 
	ld hl,jit_nes_ewram
	ld bc,$5800 
	ldir 
	; relocate ROM vars
	call _reloadROM
	; and the game's screen mode (ui_init set up the normal one)
	jp _ui_screen_game
	
extern jit_nes_ewram
extern spiLock
extern spiUnlock
extern _saveToSlot
extern _quit_saving
extern lz_compress
extern lz_decompress
extern port_setup
extern port_lock
extern port_unlock
extern _startJIT.return
extern ppu_video_start.skip_keys 
extern get_keys.skip_savestate
extern scanline_cycle_count
extern flush_cache.skip_stack_reset
extern load_jit_search
extern jit_search
extern ppu_nametable_ptr
extern jit_wram_bank
extern prg_load_wram
extern ppu_chr_bank
extern chr_bank_swap
extern prg_bank_swap
extern _getSaveSlot
extern render_cleanup
extern _ui_init
extern render_cache
extern flush_bank_cache
extern _reloadROM
extern _state_is_new
extern _state_reload_only
extern _ui_screen_game
extern pace_resync
extern slot_message