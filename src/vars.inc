
ppu_chr_ram := $D40000				; 8kb of CHRRAM
vbuffer := $D42000					; 256x224 

jit_cache_page := $D5
jit_nes_iwram := $D50000			; 2 KB for nes internal work ram 
jit_event_stack_top := $D50800		; 16-bit stack used for per line events 
jit_event_stack_bot := jit_event_stack_top + 261*2 ; ~A00		
jit_cache_start := $D50C00			; 60kb cache
jit_cache_end	:= $D5FC00			; 
jit_cache_ram_block := $D5FD00		; where translated ram block is put
jit_cache_temp_page := $D5FE00		; where borderline blocks are copied
jit_reserved := $D5FF00				; unused space 

jit_PPU_page := $D6 

render_tile_set := $D60000		; 16 bit pointers to current tiles 
render_chrram_flags := $D60800 	; CHR RAM tile update flags (1 per tile)
ppu_oam	:= $D60A00 				; 
ppu_palettes := $D60B00			;
prg_page_bank := $D60C00		; list which 8kb bank is in each page
jit_register_file := $D60C10	; 
jit_scanline_vars := $D60F80	; 256 bytes for scanline handlers (ix/y access)
render_palettes := $D61000 		; debrujin palette sequences (8 x 256 + 3 ) 
debrujin_mapping := $D61900		; used to compute debrujin tiles and palettes
mapper_area	:= $D61A00			; for use by mappers (1.5 kb) 
ppu_nametables := $D62000		; 8kb nametable expansion (64x31 + 64 attributes) 
jit_block_bucket := $D64000		; 1024*3 byte buckets for block addresses
jit_call_stack_top := $D64C00	;
jit_call_stack_bot := $D64FFC	; call/return address stack 
render_event_list := $D65000	; render event queue 
render_event_list_end := $D65800



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
current_frame := ix-128

apu_status := ix-127
joypad1_input := ix-126
joypad1_shift := ix-125
joypad2_input := ix-124
joypad2_shift := ix-123
frame_counter_irq_line := ix-122
frame_irq_enabled := ix-121

ppu_status := ix-120
ppu_write_latch := ix-119
in_vblank := ix-118
oam_address := ix-117 
ppu_ctrl := ix-116
ppu_mask := ix-115 
ppu_address := ix-114
ppu_address_increment := ix-111 
ppu_x_scroll := ix-110
ppu_y_scroll := ix-109
ppu_address_new_high := ix-108 
ppu_read_buffer := ix-107
ppu_event_list := ix-105

frameskip := ix-102
chr_ram_enable := ix-101
cycle_backup := ix-100
bankswap_ack := ix-99

; render variables

nametable_backup := ix-32

ppu_ctrl_backup := ix+0
ppu_mask_backup := ix+1
ppu_x_backup := ix+2
ppu_y_backup := ix+3
chr_ptr_backup_0 := ix+4 
chr_ptr_backup_1 := ix+7 
chr_ptr_backup_2 := ix+10 
chr_ptr_backup_3 := ix+13 
chr_ptr_backup_4 := ix+16
chr_ptr_backup_5 := ix+19
chr_ptr_backup_6 := ix+22 
chr_ptr_backup_7 := ix+25 

t_nametable_0 := ix+28
t_nametable_1 := ix+31
t_nametable_2 := ix+34
t_nametable_3 := ix+37
t_bank0 := ix+40
t_next0 := ix+43 
t_bank1 := ix+46
t_next1 := ix+49 
t_bank2 := ix+52
t_next2 := ix+55 
t_bank3 := ix+58
t_next3 := ix+61 

x_course := ix+64 
x_fine := ix+65
y_course := ix+66
y_fine := ix+67
nametable_select := ix+68 
end_y := ix+69
y_len := ix+70
x_start := ix+71
x_len1 := ix+72
x_len2 := ix+73
mask := ix+74
ctrl := ix+75 
x_new := ix+76

s_offset := ix+80
s_bank0 := ix+83
s_bank1 := ix+86
s_bank2 := ix+89
s_bank3 := ix+92
s_bank4 := ix+95
s_bank5 := ix+98
s_bank6 := ix+101
s_bank7 := ix+104

s_topclip := ix+105	; first pixel of clip plane (fullscreen => 7)
s_botclip := ix+106	; first pixel after clip plane (fullscreen => 231)
s_size := ix+107 
s_update := ix+108

