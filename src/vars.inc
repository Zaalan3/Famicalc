
ppu_chr_ram := $D40000				; 8kb of CHRRAM
vbuffer := $D42000					; 256x224 

jit_cache_page := $D5
jit_nes_iwram := $D50000			; 2 KB for nes internal work ram 
jit_event_stack_top := $D50800		; 16-bit stack used for per line events 
jit_event_stack_bot := jit_event_stack_top + 261*2 
jit_call_stack_bot := $D50F80 		
jit_cache_start := $D51000			; 59kb cache
jit_cache_end	:= $D5FC00			; 
jit_cache_ram_block := $D5FD00		; where translated ram block is put 
jit_reserved := $D5FF00				; unused space 

jit_PPU_page := $D6 

render_tile_set := $D60000		; 16 bit pointers to current tiles 
render_chrram_flags := $D60800 	; CHR RAM tile update flags (1 per tile)
ppu_oam	:= $D60A00 				; 
ppu_palettes := $D60B00			; 
jit_scanline_vars := $D60F80	; 256 bytes for scanline handlers (ix/y access)
render_palettes := $D61000 		; debrujin palette sequences (8 x 256 + 3 ) 
debrujin_mapping := $D61900		; used to compute debrujin tiles and palettes
mapper_area	:= $D61A00			; for use by mappers (1.5 kb) 
ppu_nametables := $D62000		; 8kb nametable expansion (64x31 + 64 attributes) 
bg_pages := $D64000				; cache addresses for pages in BG cache 
render_event_list := $D65000	; render event queue 



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
ppu_event_ctrl := 0 	; +1: new data in register 
ppu_event_read := 1 	; 0, no data needed
ppu_event_scroll_x := 2 ; 1 
ppu_event_address := 3	; 3
ppu_event_mask := 4		; 1 
ppu_event_bank := 5 	; +2: slot, +3..5: address
ppu_event_mirroring := 6 ; 12: 4 3-byte pointers 


; Scanline variables
current_frame := ix+0

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
ppu_event_list := ix+25

cycle_backup := ix-64

; render variables

ppu_ctrl_backup := ix+28
ppu_mask_backup := ix+29
ppu_x_backup := ix+30
ppu_y_backup := ix+31
chr_ptr_backup_0 := ix+33 
chr_ptr_backup_1 := ix+36 
chr_ptr_backup_2 := ix+39 
chr_ptr_backup_3 := ix+42 
chr_ptr_backup_4 := ix+45
chr_ptr_backup_5 := ix+48
chr_ptr_backup_6 := ix+51 
chr_ptr_backup_7 := ix+54 

nametable_backup := ix-128
frameskip := ix-1
chr_ram_enable := ix-2

s_offset := ix+57
s_bank0 := ix+58
s_bank1 := ix+61
s_bank2 := ix+64
s_bank3 := ix+67
s_bank4 := ix+70
s_bank5 := ix+73
s_bank6 := ix+76
s_bank7 := ix+79


t_nametable_0 := ix+80
t_nametable_1 := ix+83
t_nametable_2 := ix+86
t_nametable_3 := ix+89
t_bank0 := ix+92
t_next0 := ix+95 
t_bank1 := ix+96
t_next1 := ix+99 
t_bank2 := ix+100
t_next2 := ix+103 
t_bank3 := ix+104
t_next3 := ix+107 

x_course := ix+108 
x_fine := ix+109
y_course := ix+110
y_fine := ix+111
nametable_select := ix+112 
end_y := ix+113
y_len := ix+114
x_start := ix+115
x_len1 := ix+116
x_len2 := ix+117
mask := ix+118
ctrl := ix+120 
x_new := ix+121
