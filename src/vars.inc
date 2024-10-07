
ppu_chr_ram := $D40000				; 8kb of CHRRAM
vbuffer := $D42000					; 256x224 

jit_cache_page := $D3
jit_nes_iwram := $D30000			; 2 KB for nes internal work ram 
jit_event_stack_top := $D30800		; 16-bit stack used for per line events 
jit_event_stack_bot := jit_event_stack_top + 261*2 
jit_call_stack_bot := $D30F80 		
jit_cache_start := $D31000			; 59kb cache
jit_cache_end	:= $D3FC00			; 
jit_cache_ram_block := $E30800		; where translated ram block is put

jit_PPU_page := $D6 

render_tile_set := $D60000		; 16 bit pointers to current tiles 
debrujin_mapping := $D60200		; for tile conversion
ppu_page_reserved := $D60300	; 
render_chrram_flags := $D60400 	; CHR RAM tile update flags (1 per 2 tiles)
ppu_oam	:= $D60600 				; 
ppu_palettes := $D60700			; 
render_event_list := $D60800	; render event queue 
render_palettes := $D61000 		; generated palette patterns 
ppu_nametables := $D62000		;
render_name_expand := $D63000	; 8kb nametable expansion (64x31 + 64 attributes) 
render_lines := $D65000			; 

jit_scanline_vars := $E30B80		; 128 bytes for scanline handlers (ix/y access)


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
ppu_event_mask := 2 	; 1 
ppu_event_scroll_x := 3 ; 1
ppu_event_scroll_y := 4	; 1 
ppu_event_address := 5	; 2 
ppu_event_mirroring := 6; 1 
ppu_event_chr_bank := 7	; 16 (2 per bank)
