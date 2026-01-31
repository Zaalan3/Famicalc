include 'ti84pceg.inc' 

section .text

public jit_init 
public jit_search 
public jit_add_block

public jit_cache_start 
public load_jit_search
public flush_cache
public flush_cache.skip_stack_reset

include 'vars.inc'

temp_stack := $D02400

;TODO: 
jit_init:  	
	ld a,jit_cache_page 
	ld mb,a 
	call flush_cache.skip_stack_reset
	
	call load_jit_search
	
	; initialize scanline event stack 
	ld hl,jit_event_stack_top + 2*239 + 1
	ld b,239 
	; set scanline #'s 
.loop:
	ld (hl),b 
	dec hl 
	dec hl 
	djnz .loop
	ld (hl),b 
	
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
	push hl
	bit 7,h
	jp z,block_ram		; <$8000 means ram code 
	
	ld e,a 
	ld a,h 
	and a,11b 
	ld d,a 
	ld a,e 
	ld e,l 
	ld ix,jit_block_bucket-2 
	add ix,de 
	add ix,de 
	add ix,de 
	
	ex de,hl
	ld e,3
	mlt de			; find page bank
	ld hl,jit_translation_buffer+1
	add hl,de
	ld e,(hl)		; top 16 of physical address
	inc hl 
	ld d,(hl)
	inc d
.loop: 
	; ix+0 = key 
	; ix+2 = next extry 
	; ix+5 = cache offset 
	ld ix,(ix+2)
	ld hl,(ix+0) 
	inc h 
	jr z,.not_found
	or a,a
	sbc.sis hl,de  
	jr nz,.loop
.found: 
	ld ix,(ix+5)
	ld d,h
	pop hl 
	ret 
.not_found:
	jp block_not_found
	
assert $-$$ <= 64
load search_data:$-$$ from $$ 
search_len := $-$$

end virtual 

search_src: 
	db search_data


block_not_found:
	; if not in the cache, translate code for this address 
	; find page
	pop hl 
	push hl
	
	ld d,h 
	ld e,3 
	mlt de 
	ld iy,jit_translation_buffer
	add iy,de 
	; find if discontinuity exists between this page and the next
	ld de,(iy+0) 
	ld hl,(iy+3) 
	or a,a 
	sbc hl,de 
	ld de,256  
	sbc hl,de 
	jr z,.continuity 
.discontinuity: 
	; is the block within 64 bytes of the boundary? 
	pop hl 
	push hl 
	ld h,a 
	ld a,l 
	cp a,$C0
	ld a,h
	jr c,.continuity
	push bc 
	ld de,jit_cache_temp_page 
	ld hl,(iy+0) 
	ld bc,64 	; copy 64 bytes from first page, and 64 from second
	add hl,bc 
	ldir 
	ld hl,(iy+3) 
	ld c,128
	or a,a 
	sbc hl,bc 
	ld c,64 
	ldir 
	pop bc 
	ld iy,jit_cache_temp_page
	pop hl 
	ld de,0 
	ld e,l 
	res 7,e 	; - $C0 
	res 6,e 
	jr .l1
.continuity: 
	pop hl 
	ld iy,(iy+0)
	lea iy,iy-128
	ld d,0 
	ld e,l 
.l1: 
	add iy,de 
	ld de,(jit_cache_free) 		; default branch target
	ld (cache_branch_target),de
	ld (.smc_sp),sp 
	ld sp,temp_stack 
	push af
	push bc 
	exx 
	push hl 
	push de 
	push bc 
	exx 
	ex af,af' 
	push af 
	ld a,64			; length limit
	call jit_convert 
	pop af 
	ex af,af'
	exx 
	pop bc 
	pop de
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
	pop hl 
	push af
	push hl
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
	call jit_convert_ram 
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
	ld ix,jit_cache_ram_block+block_header_skip_len
	ret 

; hl = address
jit_add_block: 
	xor a,a 
	bit 7,h 	; disregard ram blocks
	ret z
	ex de,hl 
	ld hl,(jit_block_list_next)	
	ld bc,jit_block_list_end
	or a,a 
	sbc hl,bc 		; check if out of space in list
	jp z,flush_cache 
	ld a,(jit_cache_free+2) 
	ld hl,(jit_cache_free)
	cp a,jit_cache_page 
	jr z,.main_page
.extend:
	ld bc,(_jit_cache_extend_end)
	or a,a 
	sbc hl,bc 
	jp nc,flush_cache
	jr .l1
.main_page:  
	ld bc,jit_cache_end 
	or a,a 
	sbc hl,bc 
	jr c,.l1
	; if out of space in main cache, move to extended cache
	ld hl,(_jit_cache_extend)
	ld (jit_cache_free),hl 
	ld (cache_branch_target),hl
	jr .extend
.l1: 
	ex.sis de,hl
	push iy 
	push hl 
	ld iy,(jit_block_list_next)
	; find bucket
	ld a,h 
	and a,11b 
	ld d,a 
	ld e,l 
	ld ix,jit_block_bucket
	add ix,de 
	add ix,de 
	add ix,de 
	push ix 
	ld ix,(ix)
	; find page bank
	ld d,h
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
	; ix+2 = next extry 
	; ix+5 = cache offset 
	ld de,(ix+0)	; compare entry to key 
	cp a,d 			; if top 8 of entry = FF, weve reached the end of the bucket
	jr z,.notfound
	sbc.sis hl,de 
	jr z,.found 
	add hl,de
	ld ix,(ix+2) 	; iterate through linked list
	jr .loop 
.notfound: 
	pop ix
	; construct entry 
	ld (iy+0),hl 	; +0 = key 
	ld hl,(ix+0) 
	ld (iy+2),hl 	; +2 = next entry in bucket 
	ld hl,(jit_cache_free) 
	ld de,block_header_skip_len	; skip start of header
	add hl,de 
	ld (cache_branch_target),hl
	ld (iy+5),hl 	; +5 = cache location 
	
	lea hl,iy+0 	; replace top of bucket with new entry 
	ld (ix+0),hl 
	
	lea iy,iy+8 	; next block entry
	ld (jit_block_list_next),iy
	
	pop hl
	pop iy 
	xor a,a 
	ret 
.found: 
	; code is already in the cache, so we can just return 
	pop de
	ld de,(ix+5)
	pop hl
	pop iy
	ld a,1 
	ret 
	
	
flush_cache: 
	; reset call stack
	ld hl,(block_not_found.smc_sp) 
	ld de,(hl) 
	ld hl,jit_call_stack_bot-12
	ld (hl),de
	ld (block_not_found.smc_sp),hl
.skip_stack_reset: 

	ld hl,jit_block_bucket
	ld de,block_null 			; set all buckets to point to end bucket 
	ld (hl),de 
	push hl 
	pop de 
	inc de
	inc de
	inc de
	ld bc,1023*3
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

public jit_block_list
public jit_block_list_end

public jit_cache_free 
public jit_block_list_next
public cache_branch_target
public jit_call_stack_ptr

public jit_translation_buffer

public jit_wram_bank

jit_block_list:	rb 8*(2048+256)

jit_block_list_end:
jit_translation_buffer: rb 3*256 	; 3 bytes * 256 pages for virtual -> physical address translation

jit_block_list_next: rb 3 
jit_cache_free: rb 3 
cache_branch_target: rb 3 
jit_call_stack_ptr: rb 3

jit_wram_bank: rb 1

section .rodata

public jit_nes_ewram

rb $100 - ($ and $FF)				; align to 256 byte page boundary 
jit_nes_ewram: db 32*1024 dup 0


extern port_setup
extern port_lock
extern port_unlock

extern jit_convert 
extern jit_convert_ram
extern _jit_cache_extend
extern _jit_cache_extend_end


extern block_header_skip_len