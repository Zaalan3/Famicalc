include 'ti84pceg.inc' 

section .text

public jit_init 
public jit_search 
public jit_add_block

public jit_cache_start 
public load_jit_search
public flush_cache
public flush_cache.skip_stack_reset
public wram_write_check
public wram_checked_store
public wram_page_code
public wram_mark_page

include 'vars.inc'

temp_stack := $D02400

;TODO: 
jit_init:  	
	ld a,jit_cache_page 
	ld mb,a 
	; a new game: no WRAM page holds code yet. Without WRAM, $6000-$7FFF all
	; read one empty page: volatile (per instruction) from the start.
	ld a,(jit_wram_bank)
	add a,1
	sbc a,a
	and a,$20
	ld hl,wram_pages
	ld b,33
.pages:
	ld (hl),a
	inc hl
	djnz .pages
	ld a,$60
	ld (wram_dirty),a
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
	ld de,sha_scratch		
	ldir
	ret 
	
virtual at sha_scratch

; find a block in the list. If said block doesnt exist, add it.
; in: hl = address
; out: ix = cache ptr
jit_search:  
	push hl
	ld e,a 
	ld a,h 
	cp a,$60		; below: internal RAM code, per instruction (block_ram)
.smc_threshold := $-1		; $80 while wram_dirty
	jp c,block_ram
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
	lea ix,ix+5+block_header_skip_len
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

; wram_dirty is jit_search's threshold in this copy (load_jit_search copies it).
; $60: WRAM ($6000-$7FFF) code is looked up in the buckets like ROM code. $80: a
; page holding cached code was written (wram_write_check), so every WRAM lookup
; goes block_ram -> block_not_found -> jit_add_block, which flushes and sets $60.
wram_dirty := search_src + jit_search.smc_threshold - jit_search

block_not_found:
	; if not in the cache, translate code for this address 
	; find page
	pop hl 
	push hl
	bit 7,h
	jr nz,.cached
	; $6000-$7FFF: per instruction (like internal RAM) if this page is
	; volatile, or the next one and a block from here can run into it (64
	; bytes at most: from offset $C1 on)
	ld ixl,a		; (the NES A)
	ld de,0
	ld e,h
	ld iy,wram_pages-$60
	add iy,de
	ld a,l
	add a,$3F		; carry: a block's 64th byte would be in the next page
	sbc a,a
	and a,(iy+1)
	or a,(iy+0)
	and a,$20
	ld a,ixl
	jp nz,block_ram.per_instruction
.cached:
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
	; from jit_search: a = h, e = the NES A. $6000-$7FFF comes here only while
	; wram_dirty is set: block_not_found then flushes (jit_add_block).
	cp a,$60
	ld a,e
	jp nc,block_not_found
.per_instruction:
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

; hl = address ($6000 on: block_not_found sends nothing lower)
jit_add_block: 
	bit 7,h
	jr nz,.rom
	; $6000-$7FFF
	ld a,(wram_dirty)
	rla
	jr nc,.wram_clean
	; a page holding code was written (wram_write_check): flush
	ld a,$60
	ld (wram_dirty),a
	ld (jit_search.smc_threshold),a
.wram_flush:
	jp flush_cache
.wram_clean:
	; this page holds code from now on (and the next one if the block runs
	; into it: jit_convert marks it at the block's end). A page's first code
	; flushes the cache if stores to it were inlined unchecked.
	call wram_mark_page
	jr z,.wram_flush
.rom:
	push iy 
	push hl 
	ex de,hl 
	
	ld iy,(jit_cache_free) 
	lea hl,iy+5
	ld a,(jit_cache_free+2) 
	cp a,jit_cache_page 
	jr z,.main_page
	
.extend:
	ld bc,(_jit_cache_extend_end)
	or a,a 
	sbc hl,bc 
	jr c,.l1 
	pop hl 
	pop iy
	jp flush_cache
	
.main_page:  
	ld bc,jit_cache_end 
	or a,a 
	sbc hl,bc 
	jr c,.l1
	; if out of space in main cache, move to extended cache
	ld iy,(_jit_cache_extend)
	lea hl,iy+5
	jr .extend
.l1: 
	ex.sis de,hl
	
	; find bucket
	ld a,h 
	and a,11b 
	ld d,a 
	ld e,l 
	ld ix,jit_block_bucket
	add ix,de 
	add ix,de 
	add ix,de 

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

	; construct entry 
	ld (iy+0),hl 	; +0 = key 
	ld hl,(ix) 
	ld (iy+2),hl 	; +2 = next entry in bucket 
	lea hl,iy+5+block_header_skip_len
	ld (cache_branch_target),hl
	
	lea hl,iy+0 	; replace top of bucket with new entry 
	ld (ix),hl 
	
	lea hl,iy+5
	ld (jit_cache_free),hl 
	
	pop hl
	pop iy 
	xor a,a 
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
	
	ld hl,jit_cache_start
	ld (jit_cache_free),hl 
	ld (cache_branch_target),hl
	
	ld a,$FF
	ret 
	
	
block_null: 
	emit 3: $00FF00
	

; wram_pages (vars.inc): NES pages $60-$7F: bit 7 = holds cached code (kept
; until the game starts again), bit 6 = an unchecked store to it was
; translated (inlined while the page held no code), bit 5 = volatile, bits
; 0-2 = invalidations; entry 32 ($80, never code) is the page after $7F for
; the next-page checks.

; A mapper's WRAM branch wrote a byte: a = its page ($60-$7F). The first write
; to a page holding cached code after a WRAM flush counts one invalidation
; for that page (the WRAM_VOLATILE-th makes it volatile: per
; instruction until the game starts again, so a page mixing data and code can't
; flush over and over) and sets wram_dirty: the next WRAM lookup flushes. Never
; flushes here (flush_cache needs block_not_found's stack). Cached code that
; overwrites its own block keeps running the old code until the block exits.
; Clobbers a, de, hl (the mappers restore af and return de = 0).
WRAM_VOLATILE := 4
wram_write_check:
	ld de,0
	ld e,a
	ld hl,wram_pages-$60
	add hl,de
	bit 7,(hl)
	ret z
	ld a,(wram_dirty)
	rla
	ret c
	inc (hl)
	ld a,(hl)
	and a,7
	cp a,WRAM_VOLATILE
	jr c,.dirty
	ld (hl),$20
.dirty:
	ld a,$80
	ld (wram_dirty),a
	ld (jit_search.smc_threshold),a
	ret

; nz if NES page h ($60-$80) holds cached code; de = its entry in
; wram_pages. Preserves a, hl, bc, iy.
wram_page_code:
	push hl
	ld de,0
	ld e,h
	ld hl,wram_pages-$60
	add hl,de
	bit 7,(hl)
	ex de,hl
	pop hl
	ret

; NES page h ($60-$7F) holds cached code from now on. z: the caller flushes
; the cache first: the page held none and a cached block may store to it
; unchecked (bit 6); c: it held none and nothing was inlined into it.
; Preserves hl, bc, iy; destroys a, de
wram_mark_page:
	call wram_page_code
	ex de,hl
	ld a,(hl)
	set 7,(hl)
	ex de,hl
	and a,$C0
	cp a,$40
	ret

; called by mapper_get_write_function's WRAM case (hl = address). On a page
; holding cached code it answers for the mapper: a = 0, hl = wram_store_checked
; (the store goes through mapper_write, whose WRAM branch notes it). Otherwise
; it returns and the mapper inlines its plain store.
wram_checked_store:
	call wram_page_code
	jr nz,.checked
	ex de,hl
	set 6,(hl)		; a cached block may store to this page unchecked
	ex de,hl
	ret
.checked:
	pop de
	ld (wram_store_checked.smc_address),hl
	ld hl,wram_store_checked
	xor a,a
	ret

wram_store_checked:
	db .end - .start
.start:
	ld ixl,e
	ld de,0
.smc_address := $-3
	call mapper_write
.end:
	

section .bss 

public jit_cache_free 
public cache_branch_target
public jit_call_stack_ptr
public jit_wram_bank
public jit_nes_ewram

jit_cache_free: rb 3 
cache_branch_target: rb 3 
jit_call_stack_ptr: rb 3
jit_wram_bank: rb 1

jit_nes_ewram: rb 32*1024

extern mapper_write
extern port_setup
extern port_lock
extern port_unlock

extern jit_convert 
extern jit_convert_ram
extern _jit_cache_extend
extern _jit_cache_extend_end


extern block_header_skip_len