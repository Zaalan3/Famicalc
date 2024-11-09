
ppu_chr_ram := $D40000				; 8kb of CHRRAM
vbuffer := $D42000					; 256x224 

jit_cache_page := $D3
jit_nes_iwram := $D30000			; 2 KB for nes internal work ram 
jit_event_stack_top := $D30800		; 16-bit stack used for per line events 
jit_event_stack_bot := jit_event_stack_top + 261*2 
jit_call_stack_bot := $D30F80 		
jit_cache_start := $D31000			; 59kb cache
jit_cache_end	:= $D3FC00			; 
jit_cache_ram_block := $E30800		; where translated ram block is put (should be able to move this)

jit_PPU_page := $D6 

render_tile_set := $D60000		; 16 bit pointers to current tiles 
render_chrram_flags := $D60800 	; CHR RAM tile update flags (1 per 2 tiles)
ppu_oam	:= $D60A00 				; 
ppu_palettes := $D60B00			; 
render_event_list := $D60C00	; render event queue 
render_banks := $D61000 		; pointers to tiles for each 1kb bank(64 tiles)  
ppu_nametables := $D63000		;8kb nametable expansion (64x31 + 64 attributes) 
render_lines := $D65000			; 
jit_scanline_vars := $E30B80	; 128 bytes for scanline handlers (ix/y access)


; scanline event types: 
scan_event_bank_swap := 0 		; performs branch if bankswap swapped out active code
scan_event_video_start := 1		; resets to start of scanline event stack, refreshes keys, resets sprite 0
scan_event_video_end := 2		; start of vblank, draws screen 
scan_event_sprite_zero := 3
scan_event_apu_irq := 4
scan_event_dmc_irq := 5
scan_event_mapper := 6 			; various mapper events, typically timers


; ppu event types: 
; ppu event list format: 
; +0 : scanline 
; +1 : type 
; +..: data 
ppu_event_ctrl := 0 	; 1 byte of data 
ppu_event_read := 1 	; 0
ppu_event_scroll_x := 2 ; 1
ppu_event_scroll_y := 3	; 1 
ppu_event_address := 4	; 2 
ppu_event_mask := 5		; 1 
ppu_event_bank := 6 	; 2 
ppu_event_end  := 7


; Scanline variables
scanline_counter := ix+0

apu_status := ix+1
joypad1_input := ix+2
joypad1_shift := ix+3
joypad2_input := ix+4
joypad2_shift := ix+5
frame_counter_irq_line := ix+7

ppu_status := ix+10
ppu_write_latch := ix+11
in_vblank := ix+12 
oam_address := ix+13 
ppu_ctrl := ix+14 
ppu_mask := ix+15 
ppu_address := ix+16
ppu_address_increment := ix+19 
ppu_x_scroll := ix+20 
ppu_y_scroll := ix+21
ppu_address_new_high := ix+22 
ppu_read_buffer := ix+23
ppu_mirroring := ix+24 
ppu_event_list := ix+25

ppu_ctrl_backup := ix+28
ppu_mask_backup := ix+29
ppu_x_backup := ix+30
ppu_y_backup := ix+31
ppu_mirroring_backup := ix+32
