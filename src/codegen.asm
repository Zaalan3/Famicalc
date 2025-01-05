; JIT recompilation code 

section .text 

public jit_convert
public detect_wait_loop
public interpret_read

include 'vars.inc'

; list of opcode data 
; opcode table entry 
macro opcode index, op, mode, length, cycles, flags, emit_flags 
virtual
	emit 3: op
	emit 3: mode
	db length,cycles,flags,emit_flags
	load temp_data:10 from $$
end virtual 
	store temp_data:10 at opcode_table + index*10
end macro

; opcode code snip
macro opimp name
	name: 
		db 0 
	macro endop
		store $ - name.dat : byte at name
		purge endop
	end macro 
	name.dat: 
end macro

OP_SIZE := 10

op_inst	  := ix + 0
op_mode	  := ix + 3
op_length := ix + 6
op_cycles := ix + 7
op_flags  := ix + 8 
op_emit_flags := ix+9 

flags:
	.uncond 	:= 1 shl 0		; unconditional branch 
	.nz 		:= 1 shl 1		; Flag affection
	.v 			:= 1 shl 2
	.abs 		:= 1 shl 3		; write absolute addressing 
	.abs_ind 	:= 1 shl 4		; indirect addressing 
	.emit_nz 	:= 1 shl 5 		; if set, will emit NZ flag update code 
	.emit_v 	:= 1 shl 6		; if set, will emit V flag update code 
	.eob 		:= 1 shl 7		; signals this opcode ends block 
	
	.none 	:= 0 
	.gen 	:= flags.nz
	.arith	:= flags.nz + flags.v
	
; flag emitter flags 
emit_flags: 
	.none	:= 0
	.acc 	:= 1
	.x		:= 2
	.y		:= 3
	.rmw	:= 4
	.arith	:= 5
	

MAX_CYCLES := 64 

ixvars := $D006C0
block_flag_list := $D00701

virtual_origin 	:= ix + 0 
code_origin		:= ix + 3 
virtual_restart	:= ix + 6 	; for if we run out of space in the middle of translating multiple blocks
code_restart 	:= ix + 9

; iy = pointer to code 
; hl = virtual address
; Returns: 
; HL = Virtual address of byte after last opcode
; A = flags for last opcode
; TODO: add bank boundary tests
jit_convert: 
	ld ix,ixvars
	ld (virtual_restart),hl 
	ld (code_restart),iy
.start:
	call jit_add_block
	ld ix,ixvars 
	or a,a 		
	jr z,.skip
	cp a,1
	jr z,.nocode			; if a = 1, then the code is already cached , de = code ptr 
	ld hl,(virtual_restart) 	; otherwise, there was a cache flush
	ld iy,(code_restart)
	jr .start 
.nocode: 
	ld hl,(jit_cache_free) 
	ld (hl),$C3 			; jp code
	inc hl 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ld (jit_cache_free),hl
	ret 
.skip: 
	ld a,64
	exx 
	or a,a 
	sbc hl,hl 
	push hl 
	pop de 
	ld l,255		; no length restrictions at the moment
	exx
	ld (virtual_origin),hl
	ld (code_origin),iy 
	ld de,block_flag_list
	xor a,a
	ex af,af'
	
; go thru block, adding cycles and storing flags in list
phase1:
	ld ix,opcode_table	; ix = op ptr
	ld b,(iy+0) 
	ld c,OP_SIZE 
	mlt bc
	add ix,bc
	exx 				
	ld e,(op_length)	; length test
	add iy,de
	or a,a 
	sbc hl,de
;	jr c,.toobig
	ld a,e
	or a,a
	exx 
	ld a,(op_flags)
	jp z,MODE_KIL  		; if instruction can't be translated, exit block
	tst a,flags.abs or flags.abs_ind
	call nz,mapper_test_bankswap 	; sets eob flag if write could cause bankswap
	ex af,af' 
	add a,(op_cycles) 
	cp a,MAX_CYCLES		; set eob flag if out of cycles
	jr c,.checkaddress
	ex af,af' 
.set_eob:
	or a,flags.eob 
	ex af,af'
.checkaddress: 
	ex af,af'
	ld (de),a 		; write flags to list
	inc de 
	and a,flags.eob	; continue if eob flag not set
	jr z,phase1
	
; find last instruction to alter each N/Z and V flags
; & set flags to signal emitter function to emit flag code
phase2:
	ex de,hl
	ld c,l
	ld a,flags.nz
; hl = end of flag list + 1 
; a = flags to check against
.check_flag_nz:
	dec l 		; stop when past start of flag list
	jr z,.next 
	tst a,(hl)
    jr z,.check_flag_nz
    set 5,(hl)  ; set flags.emit_nz 
.next: 
	ld l,c	; reload end of flag list
	ld a,flags.v
.check_flag_v:
	dec l 
	jr z,phase3
	tst a,(hl)
    jr z,.check_flag_v
    set 6,(hl)  ; set flags.emit_v

; emit code
phase3: 
	ld l,1				; ld hl,block_flag_list
	ld bc,0
	exx
	ld ix,ixvars 
	ld iy,(code_origin) ; reload block start
	ld de,(jit_cache_free)
	ld hl,(virtual_origin)
	ex af,af' 
	call emit_block_header
.loop:
	ld ix,opcode_table	; ix = op ptr
	ld b,(iy+0) 
	ld c,OP_SIZE 
	mlt bc
	add ix,bc
	ld hl,(op_mode)		; call addressing mode emitter function
	push ix 
	ld ix,(ix+0)		; ix = instruction data
	call call_hl
	pop ix
	ld a,(op_emit_flags)
	or a,a 
	jr z,.skip_flags	; skip if emit_flags.none
	dec a 
	ld b,a				; call associated flag function
	ld c,3 
	mlt bc 
	ld hl,flag_functions
	add hl,bc
	ld hl,(hl)
	call call_hl
.skip_flags:	
	exx 
	ld c,(op_length) 
	add iy,bc 
	ld a,(hl)
	inc hl
	exx
	tst a,flags.eob
	jr z,.loop
	
	; end cleanup 
	ld (jit_cache_free),de
	ld ix,ixvars 
	ld bc,(code_origin)	; find offset 
	lea hl,iy+0 
	or a,a 
	sbc hl,bc
	ld bc,(virtual_origin)
	add hl,bc 
	bit 7,h
	jr z,.ram 			; always end ram blocks with jump to jit_branch_global
	tst a,flags.uncond	; convert next block if the end wasnt an unconditional branch 
	ret nz 
.cond: 
;	jp jit_convert.start	; consider making long block strings a toggleable option
	ex de,hl 
	ld (hl),$21			; ld hl,address 
	inc hl 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ld (hl),$CD 		; call code
	ld de,jit_branch_local
	inc hl 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ld (jit_cache_free),hl
	ret 
.ram: 
	ex de,hl 
	ld (hl),$21			; ld hl,address 
	inc hl 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ld (hl),$CD 		; call code
	ld de,jit_branch_global
	inc hl 
	ld (hl),de 
	ret 
	
	
call_hl: 
	jp (hl)

flag_functions:
	emit 3: EMIT_FLAG_NZ
	emit 3: EMIT_FLAG_NZ
	emit 3: EMIT_FLAG_NZ
	emit 3: EMIT_FLAG_NZ
	emit 3: EMIT_FLAG_ARITH


	
; a = # cycles
; de = cache 
emit_block_header:
	ld (.origin),hl
	ld (.smc),a 
	ld hl,.dat 
	ld bc,.len
	ldir
	ret 
.dat:
	ex af,af' 
	sub a,0 
.smc := $ - 1
	jr nc,.skip 
	ld hl,0
.origin := $-3
	call jit_scanline
.skip:
	ex af,af'
.len := $ - .dat 

	


;----------------------------------------------------------------------
; Flag code 

EMIT_COPY: 
	ld bc,0 
	ld c,(hl)
	inc hl 
	ldir 
	ret 

EMIT_FLAG_NZ:
	exx
	ld a,(hl) 
	exx
	tst a,flags.emit_nz
	ret z
	ld hl,flag_code
	add hl,bc 
	ld hl,(hl) 
	jq EMIT_COPY 

EMIT_FLAG_ARITH:
	exx 
	ld a,(hl) 
	exx 
	tst a,flags.emit_v
	call nz,EMIT_OVERFLOW
	tst a,flags.emit_nz
	ret z
	ld hl,FLAG_ACC_CODE
	jq EMIT_COPY
	
EMIT_OVERFLOW:
	ld hl,.len-1
	ld bc,.len 
	add hl,de 
	ld (.smc),hl 
	ld hl,.dat 
	ldir 
	ret 
.dat: 
	exx 
	res 6,b
	jp po,$
.smc := $ - 3 
	set 6,b 
	exx 
	
.len := $ - .dat 

; emits flags if flags.nz is clear 
opimp FLAG_ACC_CODE
	ld i,a 
endop 

opimp FLAG_X_CODE
	ld h,d
	ld l,b 
	ld i,hl
endop 

opimp FLAG_Y_CODE
	ld h,d 
	ld l,c 
	ld i,hl
endop 

opimp FLAG_RMW_CODE
	ld l,e
	ld h,d 
	ld i,hl
endop

flag_code: 
	emit 3: FLAG_ACC_CODE
	emit 3: FLAG_X_CODE
	emit 3: FLAG_Y_CODE
	emit 3: FLAG_RMW_CODE
	

; de = cache ptr
; iy = 6502 ptr
; ix = opcode ptr
;----------------------------------------------------------------------
; ADDRESSING MODE FUNCTIONS 

MODE_NOOP: 
	ret 

; simple copy
MODE_IMP: 
	ld bc,0 
	lea hl,ix+0 
	ld c,(hl) 
	inc hl 
	ldir 
	ret 
	
; replaces first 0 byte with immediate value in snip 
MODE_IMM: 
	ld c,(iy+1)
.entry:
	lea hl,ix+0
	ld b,(hl) 
	inc hl  
.loop:
	ld a,(hl)
	or a,a 
	jr z,.immfound 
.entry2:
	ld (de),a 
	inc de 
	inc hl 
	djnz .loop
	ret 
.immfound: 
	ld a,c
	ld (de),a 
	inc de
	inc hl 
	dec b 
	ret z 
	ld a,b 
	ld bc,0 
	ld c,a 
	ldir  
	ret
	
; replaces 0 byte with zp offset (zp - 128) 
MODE_ZP: 
	ld a,(iy+1)
	sub a,128 
	ld c,a
	jq MODE_IMM.entry 
	
MODE_ZPY: 
	ld a,$59	; ld e,c 
	jr $+4 
MODE_ZPX: 
	ld a,$58 	; ld e,b 
	ld (.smc_reg),a 
	ld a,(iy+1)
	sub a,128 
	ld (.smc),a
	ld bc,.len 
	ld hl,.dat 
	ldir
	jp MODE_IMP 
.dat: 
	lea hl,iy+0 
.smc := $ - 1
.smc_reg := $
	ld e,b
	add hl,de
	ld h, ( jit_nes_iwram shr 8 ) and $FF 
.len := $ - .dat 


MODE_IZX_WRITE:
	lea hl,ix+0
	ld bc,0 
	ld c,(hl) 
	inc hl 
	ldir 
	ld a,(iy+1)
	ld (.smc),a
	ld c,.len
	ld hl,.dat
	ldir
	ret
.dat:
	ld l,0
.smc := $ - 1
	call izx_write
.len := $ - .dat

MODE_IZX_READ:
	ld a,(iy+1)
	ld (.smc),a
	ld bc,.len 
	ld hl,.dat 
	ldir 
	jp MODE_IMP 
.dat:
	ld l,0
.smc := $ - 1
	call izx_read
.len := $ - .dat

MODE_IZY_WRITE:
	lea hl,ix+0
	ld bc,0 
	ld c,(hl) 
	inc hl 
	ldir 
	ld a,(iy+1)
	ld (.smc),a
	ld c,.len 
	ld hl,.dat 
	ldir 
	ret
.dat: 
	ld l,0 
.smc := $ - 1
	call izy_write 
.len := $ - .dat 

MODE_IZY_READ:
	ld a,(iy+1)
	ld (.smc),a
	ld bc,.len 
	ld hl,.dat 
	ldir
	jp MODE_IMP 
.dat: 
	ld l,0
.smc := $ - 1
	call izy_read 
.len := $ - .dat 


; NOTE: all read/write opcodes use E register as arg
; i.e the result of a read will be placed in E and the byte to write will be placed in E 
MODE_ABS_READ:
	call interpret_read		; write read logic 
	jp MODE_IMP 			; write opcode 
	
MODE_ABS_WRITE:
	call MODE_IMP 			; write opcode before write logic 
	jp interpret_write	

MODE_ABS_RMW:
	call interpret_read 
	call MODE_IMP 
	jp interpret_write
	ret 

MODE_ABSY_READ: 
	ld a,$59	; ld e,c 
	call interpret_read_region
	jp MODE_IMM 
MODE_ABSX_READ:
	ld a,$58	; ld e,b
	call interpret_read_region
	jp MODE_IMM 


MODE_ABSY_WRITE:  
	ld a,$59	; ld e,c 
	jq interpret_write_region
MODE_ABSX_WRITE:
	ld a,$58	; ld e,b
	jq interpret_write_region

MODE_ABSX_RMW:
	ld a,$58	; ld e,b
	call interpret_read_region
	jq interpret_write_region_rmw


; interprets a virtual address and outputs a read routine to handle it.
; TODO: inspect all the code here. Decent chance of random bugs appearing
interpret_read:
	push de 
	ld hl,(iy+1) 	; hl = address 
	ld a,h 			
	cp a,$41 		; call mapper function if >= $4100
	jq nc,.mapper
	cp a,$20
	jq nc,.io 

.RAM: 
	and a,0111b	;mask any mirrors
	ld h,a 
	ex.sis hl,de 
	ld hl,jit_nes_iwram
	add hl,de
	pop de
	jq emit_direct_read

.mapper: 
	call mapper_test_bank_fixed
	jr c,.direct_mapper_read
	ld a,l 
	ld l,3  ; find TLB entry for byte
	mlt hl 
	ld bc,jit_translation_buffer 
	add hl,bc 
	ld (tlb_read_code.smc),hl 
	sub a,128 	; find offset into page 
	ld (tlb_read_code.smcb),a 
	ld hl,tlb_read_code 
	pop de
	jq emit_func_inline 
.direct_mapper_read:
	ld a,l 
	ld l,3  ; find TLB entry for byte
	mlt hl 
	ld bc,jit_translation_buffer 
	add hl,bc 
	ld hl,(hl)
	ld e,128 
	or a,a 
	sbc hl,de 
	ld e,a 
	add hl,de
	
	pop de 
	jq emit_direct_read
	
.io:
	call io_get_read_function 
.branch: 
	pop de 
	jq emit_func_call 
	


interpret_write:
	push de
	ld hl,(iy+1) 
	ld a,h 
	cp a,$41
	jp nc,.mapper
	cp a,$20
	jr nc,.io 

.ram: 
	and a,0111b  ; mask out mirrors 
	ld h,a 
	ex.sis hl,de ; exchange then clear deu/hlu  
	ld hl,jit_nes_iwram
	add hl,de 
	pop de
	jq emit_direct_write
	
.mapper: 
	call mapper_get_write_function 
	pop de 
	or a,a 
	ret nz 		; empty write 
	jq emit_func_inline 
 	
.io:
	call io_get_write_function 
	pop de 
	or a,a 
	ret nz 		; empty write 
	jq emit_func_call 	

emit_direct_read: 
	ld (direct_read_code.smc),hl 
	ld hl,direct_read_code 
	jr emit_func_inline

emit_direct_write: 
	ld (direct_write_code.smc),hl 
	ld hl,direct_write_code 
	jr emit_func_inline
	
emit_func_inline:	; inlined functions use opimp format 
	ld bc,0 
	ld c,(hl) 
	inc hl 
	ldir
	ret

emit_func_call: 
	ex de,hl 
	ld (hl),$CD 	; call mmnn 
	inc hl 
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ex de,hl
	ld bc,0
	ret 
	
tlb_read_code: 
	db .end - .dat 
.dat:
	ld ix,(0)
.smc := $ - 3 
	ld e,(ix+0) 
.smcb := $ - 1 
.end:

direct_read_code: 
	db .end - .dat
.dat:
	ld hl,0 
.smc := $ - 3 
	ld e,(hl) 
.end: 

direct_write_code:
	db .end - .dat
.dat:
	ld hl,0 
.smc := $ - 3 
	ld (hl),e
.end: 


interpret_read_region:
	push de 
	ld (region_code.smc_reg),a 
	ld hl,(iy+1) 
	ld a,h 
	cp a,$41	; $4100 - $FFFF delegated to mapper 
	jq nc,.mapper
	cp a,$40
	jq nc,.io 
	cp a,$20 
	jq nc,.ppu 
	
.ram: 
	cp a,$1F 
	jq z,.generic_absx 
	and a,7			; mask out top bits 
	ld h,a 
	ex.sis hl,de 
	ld hl,jit_nes_iwram
	add hl,de 		
	ld (region_code.smc_addr),hl 
	pop de 
	cp a,7 
	jq nz,.nomirror 
	ld a,l 
	or a,a 
	jq nz,.nomirror

.mirror: 
	ld hl,region_code
	call emit_func_inline 
	ld hl,mirror_read_region
	jp emit_func_inline 
	
.nomirror: 
	ld hl,region_code
	call emit_func_inline
	ld hl,ram_read_region
	jp emit_func_inline
	
.mapper:
	call mapper_get_read_region_function ; de = address , hl = function
	ld (region_code.smc_addr),de 
	pop de 
	push hl 
	ld hl,region_code
	call emit_func_inline
	pop hl 
	jp emit_func_inline 
	
.io: 
	pop de 
	ld (region_code.smc_addr),hl
	ld hl,region_code
	call emit_func_inline
	ld hl,io_read_register_ind
	jp emit_func_call
	
.ppu:
	cp a,$3F
	jq z,.generic_absx
	pop de 
	ld (region_code.smc_addr),hl
	ld hl,region_code
	call emit_func_inline
	ld hl,ppu_read_register_ind
	jp emit_func_call
	
.generic_absx: 
	pop de
	ld (region_code.smc_addr),hl
	ld hl,region_code
	call emit_func_inline
	ld hl,read_byte
	jp emit_func_call

region_code: 
	db .end - .dat 
.dat:
	ld hl,0
.smc_addr := $-3 	
.smc_reg: 
	ld e,b 
	add hl,de  
.end:

opimp ram_read_region
	ld e,(hl)  
endop 

opimp mirror_read_region 
	res 3,h 
	ld e,(hl) 
endop 


interpret_write_region:
	push de 
	ld (region_code.smc_reg),a 
	ld hl,(iy+1) 
	ld a,h 
	cp a,$41	; $4100 - $FFFF delegated to mapper 
	jq nc,.mapper
	cp a,$40
	jq nc,.io 
	cp a,$20 
	jq nc,.ppu 
	
.ram: 
	cp a,$1F 
	jq z,.generic_absx 
	and a,7			; mask out top bits 
	ld h,a 
	ex.sis hl,de 
	ld hl,jit_nes_iwram
	add hl,de 		
	ld (region_code.smc_addr),hl 
	pop de 
	cp a,7 
	jq nz,.nomirror 
	ld a,l 
	or a,a 
	jq nz,.nomirror

.mirror: 
	ld hl,region_code
	call emit_func_inline 		; compute address 
	call MODE_IMM				; e = byte to write 
	ld hl,mirror_write_region	; function call
	jp emit_func_inline 
	
.nomirror: 
	ld hl,region_code
	call emit_func_inline
	call MODE_IMM
	ld hl,ram_write_region
	jp emit_func_inline

.mapper:
	call mapper_get_write_region_function 
	ld (region_code.smc_addr),de 
	pop de 
	cp a,$FF 
	ret z
	push hl 
	ld hl,region_code
	call emit_func_inline 
	call MODE_IMM
	pop hl 
	jp emit_func_inline
	
.io: 
	pop de 
	ld (region_code.smc_addr),hl
	ld hl,region_code
	call emit_func_inline
	call MODE_IMM
	ld hl,io_write_register_ind
	jp emit_func_call
	
.ppu:
	cp a,$3F
	jq z,.generic_absx
	pop de 
	ld (region_code.smc_addr),hl
	ld hl,region_code
	call emit_func_inline
	call MODE_IMM
	ld hl,ppu_write_register_ind
	jp emit_func_call
	
.generic_absx: 
	pop de
	ld (region_code.smc_addr),hl
	ld hl,region_code
	call emit_func_inline
	call MODE_IMM
	ld hl,write_byte
	jp emit_func_call

opimp ram_write_region
	ld (hl),e  
endop 

opimp mirror_write_region 
	res 3,h 
	ld (hl),e
endop 


interpret_write_region_rmw:
	; if write is to $4100 - $FFFF delegated to mapper 
	ld hl,(iy+1) 
	ld a,h 
	cp a,$41
	jq nc,.mapper
	
	call MODE_IMM 
	push de 
	ld hl,(iy+1) 
	ld a,h 
	cp a,$40
	jq nc,.io 
	cp a,$20 
	jq nc,.ppu 
	
.ram: 	; correct address already in HL 
	cp a,$1F 
	jq z,.generic_absx
	pop de 
	ld hl,ram_write_region
	jp emit_func_inline

.mapper:
	call mapper_rmw_response 
	; does this region ignore the write back or the second write?
	cp a,2
	jr nz,$+6 
	call MODE_IMM 
	ld hl,(iy+1) 
	ld (region_code_rmw_mapper.smc_addr),hl
	ld hl,region_code_rmw_mapper 
	jp emit_func_inline
	
.io: ; suck it up and recompute address
	pop de 
	ld (region_code_rmw.smc_addr),hl
	ld hl,region_code_rmw
	call emit_func_inline
	ld hl,io_write_register_ind
	jp emit_func_call
	
.ppu:
	cp a,$3F
	jq z,.generic_absx
	pop de 
	ld (region_code_rmw.smc_addr),hl
	ld hl,region_code_rmw
	call emit_func_inline
	ld hl,ppu_write_register_ind
	jp emit_func_call
	
.generic_absx: 
	pop de
	ld (region_code_rmw.smc_addr),hl
	ld hl,region_code_rmw
	call emit_func_inline
	ld hl,write_byte
	jp emit_func_call


region_code_rmw: 
	db .end - .dat 
.dat:
	ld ixl,e 
	ld hl,0
.smc_addr := $-3 	
	ld e,b 
	add hl,de  
	ld e,ixl
.end:

region_code_rmw_mapper: 
	db .end - .dat 
.dat:
	ld ixl,e 
	ld hl,0
.smc_addr := $-3 	
	ld e,b 
	add hl,de
	ex de,hl
	call mapper_write
.end:


; a=0 if branch is inside of current memory page, otherwise a != 0 
; hl = new memory address
; bc = old memory address
test_long_branch: 
	ld a,b 
	cp a,$80 
	jr c,.ram 
	ld a,h
	cp a,$80 	; addresses below $8000 are RAM or I/O
	jp nc, mapper_test_long_branch
.ram:
	ld a,1 
	ret 
	
	
; TODO: add wait loop detection
MODE_BRANCH:
	call MODE_IMP	; copy base branch code
	push de 
	ld ix,ixvars 	; find virtual address  
	ld de,(code_origin)	
	lea hl,iy+2		; instruction pointer moves +2 before offset applied
	or a,a 
	sbc hl,de
	ld de,(virtual_origin)
	add hl,de
	push hl 
	pop bc			; bc = virtual address of branch center
	
	ld a,(iy+1) 	; sign extend offset
	rlca 
	sbc hl,hl 
	ld l,(iy+1)
	add.sis hl,bc 	; hl = new address
	; compare to start of block 
	push hl 
	xor a,a
	sbc hl,de 
	call z,detect_wait_loop ; if a!=0 , then a wait loop was detected 
	or a,a 
	jr nz,.waitloop
	pop hl
	push hl
	call test_long_branch 	;if a!=0 , then branch is to a different bank
	pop de
	or a,a 
	jr nz,.global 
	
	ld bc,jit_branch_local
	pop hl 
	ld (hl),$21 ; ld hl,mmnn
	inc hl 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ld (hl),$CD ; call mmnn 
	inc hl 
	ld (hl),bc 
	inc hl
	inc hl
	inc hl
	ex de,hl 
	ret 
.global:
	ld bc,jit_branch_global
	jr .write
.waitloop: 
	pop bc 
	ld de,(jit_cache_free)
	ld bc,jit_scanline_skip
.write: 
	pop hl 
	ld (hl),$21 ; ld hl,mmnn
	inc hl 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ld (hl),$C3 ; jp mmnn 
	inc hl 
	ld (hl),bc 
	inc hl
	inc hl
	inc hl
	ex de,hl 
	ret 
	
	
MODE_JUMP_ABS: 
	; try to identify any wait loops
	ld ix,ixvars 
	ld bc,(virtual_origin)	; compare branch address to block start 
	ld hl,(iy+1) 
	or a,a
	sbc.sis hl,bc 
	jr nz,.cont 			; ==0 => `LOOP: JMP LOOP` 
.waitloop:
	ld hl,jit_scanline_skip.nopush
	ex de,hl 
	ld (hl),$CD 
	inc hl
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ex de,hl
.cont: 
	; actual instruction emitting
	ld hl,(iy+1) 
	ex de,hl 
	ld (hl),$21		; ld hl,mmnn 
	inc hl 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ld (hl),$CD		; call mmnn 
	inc hl 
	ex de,hl 
	ld bc,(virtual_origin) ; no need to calculate offset: the origin and this instruction are always in the same bank
	call test_long_branch
	or a,a 
	jr nz,.long
.short: 
	ld hl,jit_branch_local
	jr .fin
.long: 
	ld hl,jit_branch_global
.fin: 	
	ex de,hl 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ex de,hl
	ret 

MODE_JUMP_IND: 
	ld hl,(iy+1)
	ex de,hl 
	ld (hl),$21		; ld hl,mmnn 
	inc hl 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ld (hl),$C3		; jp mmnn 
	inc hl 
	ld de,jit_jump_indirect
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ex de,hl 
	ret 
	
;TODO: add conditions for local branchs
MODE_JSR: 
	ld hl,(iy+1)
	ex de,hl 
	ld (hl),$21		; ld hl,mmnn 
	inc hl 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ex de,hl
	ld ix,ixvars 	; find offset
	ld bc,(code_origin)	 
	lea hl,iy+2		; JSR only increments twice before pushing, up to RTS to do the last increment  
	or a,a 
	sbc hl,bc
	ld bc,(virtual_origin)
	add hl,bc
	ex de,hl 
	ld (hl),$D9		; exx / ld de,return_address-1
	inc hl 
	ld (hl),$11 
	inc hl 
	ld (hl),de
	inc hl 
	inc hl
	inc hl
	ld (hl),$CD		; call jit_call 
	inc hl 
	ex de,hl 
	call test_long_branch
	ex de,hl 
	or a,a 
	jr nz,.global 
	ld de,jit_call_local 
	jr .cont 
.global: 
	ld de,jit_call
.cont: 	 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ex de,hl 
	ret 

MODE_BRK: 
	ld ix,ixvars 		; find offset
	ld bc,(code_origin)	 
	lea hl,iy+2			; BRK skips next byte
	or a,a 
	sbc hl,bc
	ld bc,(virtual_origin)
	add hl,bc
	ex de,hl 
	ld (hl),$D9			; exx / ld de,return_address  
	inc hl 
	ld (hl),$11
	inc hl
	ld (hl),de
	inc hl 
	inc hl
	inc hl				; jp jit_break 
	ld (hl),$C3
	inc hl 
	ld de,jit_break 
	ld (hl),de 
	inc hl
	inc hl
	inc hl
	ex de,hl 
	ret 
	
; error
MODE_KIL:
	ld de,$FC0000
	ld hl,.debug_message
.loop: 
	ld a,(hl) 
	ld (de),a 
	inc hl 
	or a,a 
	jr nz,.loop 
	ret 

.debug_message: 
	db "Illegal Instruction encountered.",0

	
; TODO: add more conditions
detect_wait_loop: 
	push iy 
	ld iy,(code_origin)
	ld a,(iy+0) 
	call compare_branch_ops
	jr z,.match
	cp a,$A5 	; LDA zp 
	jr z,.lda_zp 
	cp a,$AD	; LDA abs 
	jr z,.lda_abs 
	cp a,$24 	; BIT zp 
	jr z,.twoop 
	cp a,$2C	; BIT abs 
	jr z,.twoop 
	cp a,$C9 	; CMP imm
	jr z,.twoop 
	cp a,$C5	; CMP zp 
	jr z,.twoop
.end: 
	pop iy
	xor a,a
	ret 
.lda_abs:
	ld hl,(iy+1) 
	push de 
	ld de,$302002 	; dont match for `LDA $2002 / BMI`. That pattern is for interrupt acknowledge
	or a,a 
	sbc hl,de 
	pop de
	jr z,.end
	inc iy  
.lda_zp: 
	lea iy,iy+2 
	ld a,(iy+0) 
	cp a,$29 	; AND imm 
	jr z,.twoop 
	cp a,$C9 	; CMP imm
	jr z,.twoop 
	cp a,$C5 	; CMP zp
	jr z,.twoop
	call compare_branch_ops
	jr z,.match 
	jr .end 
.twoop: 
	lea iy,iy+2 
	ld a,(iy+0) 
	call compare_branch_ops
	jr nz,.end 
.match:
	pop iy
	ld a,1 
	ret 

compare_branch_ops: 
	cp a,$90	; BCC
	ret z
	cp a,$B0 	; BCS 
	ret z 
	cp a,$F0	; BEQ
	ret z 
	cp a,$D0	; BNE 
	ret z 
	cp a,$10	; BPL
	ret z 
	cp a,$30	; BMI 
	ret z 
	cp a,$50	; BVC 
	ret z 
	cp a,$70	; BVS
	ret
	
include 'optable.inc'

extern izx_read 
extern izx_write 
extern izy_read 
extern izy_write

extern read_byte
extern write_byte

extern io_get_read_function
extern io_get_write_function

extern io_read_register_ind
extern ppu_read_register_ind
extern io_write_register_ind
extern ppu_write_register_ind

extern jit_translation_buffer
extern jit_cache_free
extern jit_add_block 

extern scanline_cycle_count
extern jit_scanline
extern jit_scanline_skip
extern jit_scanline_skip.nopush
extern jit_branch_local 
extern jit_branch_global

extern jit_jump_indirect
extern jit_push_flags
extern jit_pop_flags
extern jit_return
extern jit_return_int
extern jit_break
extern jit_call
extern jit_call_local

extern mapper_test_bankswap
extern mapper_test_long_branch
extern mapper_test_bank_fixed
extern mapper_get_write_function
extern mapper_get_write_region_function
extern mapper_get_read_region_function
extern mapper_rmw_response
extern mapper_write 

