include 'ti84pceg.inc' 

section .text

public jit_init 
public jit_search 
public jit_add_block

public jit_cache_start 
public load_jit_search

include 'vars.inc'

temp_stack := $D02400

;TODO: 
jit_init:  	
	ld a,jit_cache_page 
	ld mb,a 
	call flush_cache
	
	;unlock SHA scrap area 
	call port_setup 
	call port_unlock
	in a,($06) 
	set 2,a 
	out ($06),a
	call port_lock 
	
	call load_jit_search
	
	; initialize scanline event stack 
	ld hl,jit_event_stack_top 
	ld (hl),0 
	push hl 
	pop de 
	inc de 
	ld bc,261*2 + 1 
	ldir 
	ld hl,jit_event_stack_top+2*261
	set scan_event_video_start,(hl) 
	ld hl,jit_event_stack_top+2*240 
	set scan_event_video_end,(hl) 
	ret 


load_jit_search:
	; load jit_search
	ld hl,search_src 
	ld bc,search_len 
	ld de,$E10010		; SHA scratch area
	ldir
	ret 
	
virtual at $E10010

; find a block in the list. If said block doesnt exist, add it.
; in: hl = address
; out: ix = cache ptr
jit_search:  
	push af
	push hl
	ld a,h 
	cp a,$80 
	jp c,block_ram		; <$8000 means ram code 
	
	ld d,l 			; find bucket
	ld e,3
	mlt de 
	ld ix,jit_block_bucket-3 
	add ix,de 
	
	ld d,a 			; find page bank
	ld e,3 
	mlt de
	ld hl,jit_translation_buffer+1
	add hl,de
	ld e,(hl)		; top 16 of physical address
	inc hl 
	ld d,(hl) 
	ld a,$FF
.loop: 
	; ix+0 = key 
	; ix+3 = next extry 
	; ix+6 = cache offset 
	ld ix,(ix+3)
	ld hl,(ix+0) 
	cp a,h 
	jr z,.notfound 
	sbc hl,de  
	jr nz,.loop
.found: 
	ld ix,(ix+6) 
	inc sp
	inc sp
	inc sp
	pop af 
	ld d,0 
	ret 
.notfound:
	jp block_not_found 
	
assert $-$$ <= 64
load search_data:$-$$ from $$ 
search_len := $-$$

end virtual 

search_src: 
	db search_data

block_found: 
	ld ix,(ix+6) 
	inc sp
	inc sp
	inc sp
	pop af 
	ld d,0 
	ret 
	
block_not_found:
	; if not in the cache, translate code for this address 
	; find 6502 code 
	pop hl 
	pop af 
	ld d,h 
	ld e,3 
	mlt de 
	ld iy,jit_translation_buffer
	add iy,de 
	ld iy,(iy+0) 
	lea iy,iy-128
	ld d,0 
	ld e,l 
	add iy,de 
	ld de,(jit_cache_free) 		; default branch target
	ld (cache_branch_target),de
	ld (.smc_sp),sp 
	ld sp,temp_stack 
	push af 
	push bc 
	exx 
	push hl 
	push bc 
	exx 
	ex af,af' 
	push af 
	call jit_convert 
	pop af 
	ex af,af'
	exx 
	pop bc 
	pop hl 
	exx 
	pop bc 
	pop af
	ld sp,0 
.smc_sp := $-3
	ld de,0 
	ld iy,jit_nes_iwram+$80
	ld ix,(cache_branch_target)
	ret 
	
block_ram:
	; find physical location of ram code 
	ld a,l
	ld d,h 
	ld e,3 
	mlt de
	ld hl,jit_translation_buffer
	add hl,de 	
	ld iy,(hl)
	lea iy,iy-128 
	ld d,0 
	ld e,a 
	add iy,de 
	; move free area to ram block area 
	pop hl 
	ld de,(jit_cache_free) 
	push de 
	ld de,jit_cache_ram_block 
	ld (jit_cache_free),de 
	
	push bc 
	exx 
	push hl 
	push bc 
	exx 
	ex af,af' 
	push af 
	call jit_convert 
	pop af 
	ex af,af'
	exx 
	pop bc 
	pop hl 
	exx 
	pop bc 
	pop de 
	ld (jit_cache_free),de 
	ld de,0 
	pop af
	ld iy,jit_nes_iwram+$80
	ld ix,jit_cache_ram_block
	ret 

; hl = address
jit_add_block: 
	xor a,a 
	bit 7,h 	; disregard ram blocks
	ret z
	ex de,hl 
	ld hl,(jit_block_list_next)	
	ld bc,jit_block_list+9*2048 
	or a,a 
	sbc hl,bc 		; check if out of space in list
	jp z,flush_cache 
	ld hl,(jit_cache_free) 
	ld a,h 
	cp a,(jit_cache_end shr 8) and $FF 	; on last page of cache 
	jp nc,flush_cache
	ex.sis de,hl
	push iy 
	push hl 
	ld iy,(jit_block_list_next)
	; find bucket
	ld a,h
	ld e,l 
	ld d,3 
	mlt de 			
	ld hl,jit_block_bucket 
	add hl,de 
	push hl 
	ld ix,(hl)
	; find page bank
	ld d,a 
	ld e,3 
	mlt de
	ld hl,jit_translation_buffer+1
	add hl,de
	ld e,(hl)		; top 16 of physical address
	inc hl 
	ld d,(hl) 
	ex de,hl
	
	; look to see if code is already cached
	ld a,$FF
.loop: 
	; ix+0 = key 
	; ix+3 = next extry 
	; ix+6 = cache offset 
	ld de,(ix+0)	; compare entry to key 
	cp a,d 			; if top 8 of entry = FF, weve reached the end of the bucket
	jr z,.notfound
	sbc hl,de 
	jr z,.found 
	add hl,de
	ld ix,(ix+3) 	; iterate through linked list
	jr .loop 
.notfound: 
	pop ix
	; construct entry 
	ld (iy+0),hl 	; +0 = key 
	ld hl,(ix+0) 
	ld (iy+3),hl 	; +3 = next entry in bucket 
	ld hl,(jit_cache_free) 
	ld (iy+6),hl 	; +6 = cache location 
	
	lea hl,iy+0 	; replace top of bucket with new entry 
	ld (ix+0),hl 
	
	lea iy,iy+9 	; next block entry
	ld (jit_block_list_next),iy
	
	pop hl
	pop iy 
	xor a,a 
	ret 
.found: 
	; code is already in the cache, so we can just return 
	pop de
	ld de,(ix+6)
	pop hl
	pop iy
	ld a,1 
	ret 
	
	
flush_cache: 
	; reset call stack
	ld hl,(block_not_found.smc_sp) 
	ld de,(hl) 
	ld hl,jit_call_stack_bot-9
	ld (hl),de
	ld (block_not_found.smc_sp),hl
	
	ld hl,jit_block_bucket
	ld de,block_null 			; set all buckets to point to end bucket 
	ld (hl),de 
	push hl 
	pop de 
	inc de
	inc de
	inc de
	ld bc,255*3
	ldir
	
	ld hl,jit_block_list		; reset lists to start 
	ld (jit_block_list_next),hl 
	ld hl,jit_cache_start
	ld (jit_cache_free),hl 
	ld (cache_branch_target),hl
	
	ld a,$FF
	ret 
	
	
block_null: 
	emit 3: $00FF00
	

section .bss 

public jit_block_bucket
public jit_block_list

public jit_cache_free 
public jit_block_list_next
public cache_branch_target
public jit_call_stack_ptr

public jit_translation_buffer

public jit_nes_ewram

public ppu_nametable_ptr
public ppu_chr_ptr


jit_block_bucket: rb 3*256
jit_block_list:	rb 9*2048

jit_block_list_next: rb 3 
jit_cache_free: rb 3 
cache_branch_target: rb 3 
jit_call_stack_ptr: rb 3

ppu_nametable_ptr: rb 3*4			; ptr's to current vram configuration. 
ppu_chr_ptr: rb 3*7 

jit_translation_buffer: rb 3*256 	; 3 bytes * 256 pages for virtual -> physical address translation

rb $100 - ($ and $FF)				; align to 256 byte page boundary 

jit_nes_ewram: rb 16*1024			; TODO: a few big games up to use 32kb. 



extern port_setup
extern port_lock
extern port_unlock

extern jit_convert 
