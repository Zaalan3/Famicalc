
section .rodata 

virtual 
	emit 3: OP_NULL 
	emit 3: MODE_KIL
	db 0,0,0,0
	load kil_data:10 from $$
end virtual 

public opcode_table
opcode_table: 
	db 256 dup kil_data	;default opcode is KIL


;----------------------------------------------------------------------
; OPCODE DEFS

;one opcode instructions

opcode $40, OP_RTI, MODE_IMP, 1, 6, flags.uncond + flags.eob + flags.arith, emit_flags.none
opcode $60, OP_RTS, MODE_IMP, 1, 6, flags.uncond + flags.eob, emit_flags.none

opcode $18, OP_CLC, MODE_IMP, 1, 2, flags.none, emit_flags.none 
opcode $D8, OP_CLD, MODE_IMP, 1, 2, flags.none, emit_flags.none 
opcode $58, OP_CLI, MODE_IMP, 1, 2, flags.none, emit_flags.none 
opcode $B8, OP_CLV, MODE_IMP, 1, 2, flags.none, emit_flags.none 
opcode $38, OP_SEC, MODE_IMP, 1, 2, flags.none, emit_flags.none
opcode $F8, OP_SED, MODE_IMP, 1, 2, flags.none, emit_flags.none
opcode $78, OP_SEI, MODE_IMP, 1, 2, flags.none, emit_flags.none

opcode $AA, OP_TAX, MODE_IMP, 1, 2, flags.gen, emit_flags.x
opcode $8A, OP_TXA, MODE_IMP, 1, 2, flags.gen, emit_flags.acc
opcode $A8, OP_TAY, MODE_IMP, 1, 2, flags.gen, emit_flags.y
opcode $98, OP_TYA, MODE_IMP, 1, 2, flags.gen, emit_flags.acc
opcode $BA, OP_TSX, MODE_IMP, 1, 2, flags.gen, emit_flags.x
opcode $9A, OP_TXS, MODE_IMP, 1, 2, flags.none, emit_flags.none 

opcode $E8, OP_INX, MODE_IMP, 1, 2, flags.gen, emit_flags.x 
opcode $C8, OP_INY, MODE_IMP, 1, 2, flags.gen, emit_flags.y
opcode $CA, OP_DEX, MODE_IMP, 1, 2, flags.gen, emit_flags.x 
opcode $88, OP_DEY, MODE_IMP, 1, 2, flags.gen, emit_flags.y

opcode $48, OP_PHA, MODE_IMP, 1, 3, flags.none, emit_flags.none
opcode $08, OP_PHP, MODE_IMP, 1, 3, flags.eob, emit_flags.none
opcode $68, OP_PLA, MODE_IMP, 1, 4, flags.gen, emit_flags.acc
opcode $28, OP_PLP, MODE_IMP, 1, 4, flags.arith, emit_flags.none

;branch 

opcode $90, OP_BCC, MODE_BRANCH, 2, 2, flags.eob, emit_flags.none
opcode $B0, OP_BCS, MODE_BRANCH, 2, 2, flags.eob, emit_flags.none
opcode $F0, OP_BEQ, MODE_BRANCH, 2, 2, flags.eob, emit_flags.none
opcode $D0, OP_BNE, MODE_BRANCH, 2, 2, flags.eob, emit_flags.none
opcode $10, OP_BPL, MODE_BRANCH, 2, 2, flags.eob, emit_flags.none
opcode $30, OP_BMI, MODE_BRANCH, 2, 2, flags.eob, emit_flags.none
opcode $50, OP_BVC, MODE_BRANCH, 2, 2, flags.eob, emit_flags.none
opcode $70, OP_BVS, MODE_BRANCH, 2, 2, flags.eob, emit_flags.none

opcode $00, OP_NULL, MODE_BRK, 1, 7, flags.uncond + flags.eob, emit_flags.none
opcode $4C, OP_NULL, MODE_JUMP_ABS, 3, 3, flags.uncond + flags.eob, emit_flags.none 
opcode $6C, OP_NULL, MODE_JUMP_IND, 3, 5, flags.uncond + flags.eob, emit_flags.none
opcode $20, OP_NULL, MODE_JSR, 3, 6, flags.eob, emit_flags.none 

;LDA 
opcode $A9, OP_LDA_IMM, MODE_IMM, 2, 2, flags.gen, emit_flags.acc
opcode $A5, OP_LDA_ZP, MODE_ZP, 2, 3, flags.gen, emit_flags.acc
opcode $B5, OP_LDA_ZPX, MODE_ZPX, 2, 4, flags.gen, emit_flags.acc
opcode $AD, OP_LDA_MEM, MODE_ABS_READ, 3, 4, flags.gen, emit_flags.acc
opcode $BD, OP_LDA_MEM, MODE_ABSX_READ, 3, 4, flags.gen, emit_flags.acc
opcode $B9, OP_LDA_MEM, MODE_ABSY_READ, 3, 4, flags.gen, emit_flags.acc
opcode $A1, OP_LDA_MEM, MODE_IZX_READ, 2, 6, flags.gen, emit_flags.acc
opcode $B1, OP_LDA_MEM, MODE_IZY_READ, 2, 5, flags.gen, emit_flags.acc 

;LDX
opcode $A2, OP_LDX_IMM, MODE_IMM, 2, 2, flags.gen, emit_flags.x
opcode $A6, OP_LDX_ZP, MODE_ZP, 2, 3, flags.gen, emit_flags.x
opcode $B6, OP_LDX_ZPY, MODE_ZPY, 2, 4, flags.gen, emit_flags.x
opcode $AE, OP_LDX_MEM, MODE_ABS_READ, 3, 4, flags.gen, emit_flags.x
opcode $BE, OP_LDX_MEM, MODE_ABSY_READ, 3, 4, flags.gen, emit_flags.x

;LDY
opcode $A0, OP_LDY_IMM, MODE_IMM, 2, 2, flags.gen, emit_flags.y
opcode $A4, OP_LDY_ZP, MODE_ZP, 2, 3, flags.gen, emit_flags.y
opcode $B4, OP_LDY_ZPX, MODE_ZPX, 2, 4, flags.gen, emit_flags.y
opcode $AC, OP_LDY_MEM, MODE_ABS_READ, 3, 4, flags.gen, emit_flags.y
opcode $BC, OP_LDY_MEM, MODE_ABSX_READ, 3, 4, flags.gen, emit_flags.y

;ADC 
opcode $69, OP_ADC_IMM, MODE_IMM, 2, 2, flags.arith, emit_flags.arith
opcode $65, OP_ADC_ZP, MODE_ZP, 2, 3, flags.arith, emit_flags.arith
opcode $75, OP_ADC_ZPX, MODE_ZPX, 2, 4, flags.arith, emit_flags.arith
opcode $6D, OP_ADC_MEM, MODE_ABS_READ, 3, 4, flags.arith, emit_flags.arith
opcode $7D, OP_ADC_MEM, MODE_ABSX_READ, 3, 4, flags.arith, emit_flags.arith
opcode $79, OP_ADC_MEM, MODE_ABSY_READ, 3, 4, flags.arith, emit_flags.arith
opcode $61, OP_ADC_MEM, MODE_IZX_READ, 2, 6, flags.arith, emit_flags.arith
opcode $71, OP_ADC_MEM, MODE_IZY_READ, 2, 5, flags.arith, emit_flags.arith

;SBC 
opcode $E9, OP_SBC_IMM, MODE_IMM, 2, 2, flags.arith, emit_flags.arith
opcode $E5, OP_SBC_ZP, MODE_ZP, 2, 3, flags.arith, emit_flags.arith
opcode $F5, OP_SBC_ZPX, MODE_ZPX, 2, 4, flags.arith, emit_flags.arith
opcode $ED, OP_SBC_MEM, MODE_ABS_READ, 3, 4, flags.arith, emit_flags.arith
opcode $FD, OP_SBC_MEM, MODE_ABSX_READ, 3, 4, flags.arith, emit_flags.arith
opcode $F9, OP_SBC_MEM, MODE_ABSY_READ, 3, 4, flags.arith, emit_flags.arith
opcode $E1, OP_SBC_MEM, MODE_IZX_READ, 2, 6, flags.arith, emit_flags.arith
opcode $F1, OP_SBC_MEM, MODE_IZY_READ, 2, 5, flags.arith, emit_flags.arith

;BIT 
opcode $24, OP_BIT_ZP, MODE_ZP, 2, 3, flags.arith, emit_flags.none 
opcode $2C, OP_BIT_MEM, MODE_ABS_READ, 3, 4, flags.arith, emit_flags.none 

;CMP
opcode $C9, OP_CMP_IMM, MODE_IMM, 2, 2, flags.gen, emit_flags.none
opcode $C5, OP_CMP_ZP, MODE_ZP, 2, 3, flags.gen, emit_flags.none
opcode $D5, OP_CMP_ZPX, MODE_ZPX, 2, 4, flags.gen, emit_flags.none
opcode $CD, OP_CMP_MEM, MODE_ABS_READ, 3, 4, flags.gen, emit_flags.none
opcode $DD, OP_CMP_MEM, MODE_ABSX_READ, 3, 4, flags.gen, emit_flags.none
opcode $D9, OP_CMP_MEM, MODE_ABSY_READ, 3, 4, flags.gen, emit_flags.none
opcode $C1, OP_CMP_MEM, MODE_IZX_READ, 2, 6, flags.gen, emit_flags.none
opcode $D1, OP_CMP_MEM, MODE_IZY_READ, 2, 5, flags.gen, emit_flags.none

;CPX
opcode $E0, OP_CPX_IMM, MODE_IMM, 2, 2, flags.gen, emit_flags.none
opcode $E4, OP_CPX_ZP, MODE_ZP, 2, 3, flags.gen, emit_flags.none 
opcode $EC, OP_CPX_MEM, MODE_ABS_READ, 3, 4, flags.gen, emit_flags.none

;CPY
opcode $C0, OP_CPY_IMM, MODE_IMM, 2, 2, flags.gen, emit_flags.none
opcode $C4, OP_CPY_ZP, MODE_ZP, 2, 3, flags.gen, emit_flags.none 
opcode $CC, OP_CPY_MEM, MODE_ABS_READ, 3, 4, flags.gen, emit_flags.none

;AND 
opcode $29, OP_AND_IMM, MODE_IMM, 2, 2, flags.gen, emit_flags.acc
opcode $25, OP_AND_ZP, MODE_ZP, 2, 3, flags.gen, emit_flags.acc
opcode $35, OP_AND_ZPX, MODE_ZPX, 2, 4, flags.gen, emit_flags.acc
opcode $2D, OP_AND_MEM, MODE_ABS_READ, 3, 4, flags.gen, emit_flags.acc
opcode $3D, OP_AND_MEM, MODE_ABSX_READ, 3, 4, flags.gen, emit_flags.acc
opcode $39, OP_AND_MEM, MODE_ABSY_READ, 3, 4, flags.gen, emit_flags.acc
opcode $21, OP_AND_MEM, MODE_IZX_READ, 2, 6, flags.gen, emit_flags.acc
opcode $31, OP_AND_MEM, MODE_IZY_READ, 2, 5, flags.gen, emit_flags.acc

;EOR 
opcode $49, OP_EOR_IMM, MODE_IMM, 2, 2, flags.gen, emit_flags.acc
opcode $45, OP_EOR_ZP, MODE_ZP, 2, 3, flags.gen, emit_flags.acc
opcode $55, OP_EOR_ZPX, MODE_ZPX, 2, 4, flags.gen, emit_flags.acc
opcode $4D, OP_EOR_MEM, MODE_ABS_READ, 3, 4, flags.gen, emit_flags.acc
opcode $5D, OP_EOR_MEM, MODE_ABSX_READ, 3, 4, flags.gen, emit_flags.acc
opcode $59, OP_EOR_MEM, MODE_ABSY_READ, 3, 4, flags.gen, emit_flags.acc
opcode $41, OP_EOR_MEM, MODE_IZX_READ, 2, 6, flags.gen, emit_flags.acc
opcode $51, OP_EOR_MEM, MODE_IZY_READ, 2, 5, flags.gen, emit_flags.acc

;ORA
opcode $09, OP_OR_IMM, MODE_IMM, 2, 2, flags.gen, emit_flags.acc
opcode $05, OP_OR_ZP, MODE_ZP, 2, 3, flags.gen, emit_flags.acc
opcode $15, OP_OR_ZPX, MODE_ZPX, 2, 4, flags.gen, emit_flags.acc
opcode $0D, OP_OR_MEM, MODE_ABS_READ, 3, 4, flags.gen, emit_flags.acc
opcode $1D, OP_OR_MEM, MODE_ABSX_READ, 3, 4, flags.gen, emit_flags.acc
opcode $19, OP_OR_MEM, MODE_ABSY_READ, 3, 4, flags.gen, emit_flags.acc
opcode $01, OP_OR_MEM, MODE_IZX_READ, 2, 6, flags.gen, emit_flags.acc
opcode $11, OP_OR_MEM, MODE_IZY_READ, 2, 5, flags.gen, emit_flags.acc

;ASL 
opcode $0A, OP_ASL_IMP, MODE_IMP, 1, 2, flags.gen, emit_flags.acc
opcode $06, OP_ASL_ZP, MODE_ZP, 2, 5, flags.gen, emit_flags.rmw
opcode $16, OP_ASL_ZPX, MODE_ZPX, 2, 6, flags.gen, emit_flags.rmw
opcode $0E, OP_ASL_MEM, MODE_ABS_RMW, 3, 6, flags.gen + flags.abs, emit_flags.rmw
opcode $1E, OP_ASL_MEM, MODE_ABSX_RMW, 3, 7, flags.gen + flags.abs_ind, emit_flags.rmw

;LSR 
opcode $4A, OP_LSR_IMP, MODE_IMP, 1, 2, flags.gen, emit_flags.acc
opcode $46, OP_LSR_ZP, MODE_ZP, 2, 5, flags.gen, emit_flags.rmw
opcode $56, OP_LSR_ZPX, MODE_ZPX, 2, 6, flags.gen, emit_flags.rmw
opcode $4E, OP_LSR_MEM, MODE_ABS_RMW, 3, 6, flags.gen + flags.abs, emit_flags.rmw
opcode $5E, OP_LSR_MEM, MODE_ABSX_RMW, 3, 7, flags.gen + flags.abs_ind, emit_flags.rmw

;ROR 
opcode $6A, OP_ROR_IMP, MODE_IMP, 1, 2, flags.gen, emit_flags.acc
opcode $66, OP_ROR_ZP, MODE_ZP, 2, 5, flags.gen, emit_flags.rmw
opcode $76, OP_ROR_ZPX, MODE_ZPX, 2, 6, flags.gen, emit_flags.rmw
opcode $6E, OP_ROR_MEM, MODE_ABS_RMW, 3, 6, flags.gen + flags.abs, emit_flags.rmw
opcode $7E, OP_ROR_MEM, MODE_ABSX_RMW, 3, 7, flags.gen + flags.abs_ind, emit_flags.rmw

;ROL 
opcode $2A, OP_ROL_IMP, MODE_IMP, 1, 2, flags.gen, emit_flags.acc
opcode $26, OP_ROL_ZP, MODE_ZP, 2, 5, flags.gen, emit_flags.rmw
opcode $36, OP_ROL_ZPX, MODE_ZPX, 2, 6, flags.gen, emit_flags.rmw
opcode $2E, OP_ROL_MEM, MODE_ABS_RMW, 3, 6, flags.gen + flags.abs, emit_flags.rmw
opcode $3E, OP_ROL_MEM, MODE_ABSX_RMW, 3, 7, flags.gen + flags.abs_ind, emit_flags.rmw

;INC 
opcode $E6, OP_INC_ZP, MODE_ZP, 2, 5, flags.gen, emit_flags.rmw
opcode $F6, OP_INC_ZPX, MODE_ZPX, 2, 6, flags.gen, emit_flags.rmw
opcode $EE, OP_INC_MEM, MODE_ABS_RMW, 3, 6, flags.gen + flags.abs, emit_flags.rmw
opcode $FE, OP_INC_MEM, MODE_ABSX_RMW, 3, 7, flags.gen + flags.abs_ind, emit_flags.rmw

;DEC 
opcode $C6, OP_DEC_ZP, MODE_ZP, 2, 5, flags.gen, emit_flags.rmw
opcode $D6, OP_DEC_ZPX, MODE_ZPX, 2, 6, flags.gen, emit_flags.rmw
opcode $CE, OP_DEC_MEM, MODE_ABS_RMW, 3, 6, flags.gen + flags.abs, emit_flags.rmw
opcode $DE, OP_DEC_MEM, MODE_ABSX_RMW, 3, 7, flags.gen + flags.abs_ind, emit_flags.rmw

;STA 
opcode $85, OP_STA_ZP, MODE_ZP, 2, 3, flags.none, emit_flags.none
opcode $95, OP_STA_ZPX, MODE_ZPX, 2, 4, flags.none, emit_flags.none
opcode $8D, OP_STA_MEM, MODE_ABS_WRITE, 3, 4, flags.abs, emit_flags.none
opcode $9D, OP_STA_MEM, MODE_ABSX_WRITE, 3, 5, flags.abs_ind, emit_flags.none
opcode $99, OP_STA_MEM, MODE_ABSY_WRITE, 3, 5, flags.abs_ind, emit_flags.none
opcode $81, OP_STA_MEM, MODE_IZX_WRITE, 2, 6, flags.eob, emit_flags.none
opcode $91, OP_STA_MEM, MODE_IZY_WRITE, 2, 6, flags.eob, emit_flags.none

;STX
opcode $86, OP_STX_ZP, MODE_ZP, 2, 3, flags.none, emit_flags.none
opcode $96, OP_STX_ZPY, MODE_ZPY, 2, 4, flags.none, emit_flags.none
opcode $8E, OP_STX_MEM, MODE_ABS_WRITE, 3, 4, flags.none, emit_flags.none

;STX
opcode $84, OP_STY_ZP, MODE_ZP, 2, 3, flags.none, emit_flags.none
opcode $94, OP_STY_ZPX, MODE_ZPX, 2, 4, flags.none, emit_flags.none
opcode $8C, OP_STY_MEM, MODE_ABS_WRITE, 3, 4, flags.none, emit_flags.none

;NOP 
opcode $EA, OP_NULL, MODE_NOOP, 1, 2, flags.none, emit_flags.none
opcode $1A, OP_NULL, MODE_NOOP, 1, 2, flags.none, emit_flags.none
opcode $3A, OP_NULL, MODE_NOOP, 1, 2, flags.none, emit_flags.none
opcode $5A, OP_NULL, MODE_NOOP, 1, 2, flags.none, emit_flags.none
opcode $7A, OP_NULL, MODE_NOOP, 1, 2, flags.none, emit_flags.none
opcode $DA, OP_NULL, MODE_NOOP, 1, 2, flags.none, emit_flags.none
opcode $FA, OP_NULL, MODE_NOOP, 1, 2, flags.none, emit_flags.none

opcode $80, OP_NULL, MODE_NOOP, 2, 2, flags.none, emit_flags.none
opcode $82, OP_NULL, MODE_NOOP, 2, 2, flags.none, emit_flags.none
opcode $89, OP_NULL, MODE_NOOP, 2, 2, flags.none, emit_flags.none
opcode $C2, OP_NULL, MODE_NOOP, 2, 2, flags.none, emit_flags.none
opcode $E2, OP_NULL, MODE_NOOP, 2, 2, flags.none, emit_flags.none

opcode $04, OP_NULL, MODE_NOOP, 2, 3, flags.none, emit_flags.none
opcode $44, OP_NULL, MODE_NOOP, 2, 3, flags.none, emit_flags.none
opcode $64, OP_NULL, MODE_NOOP, 2, 3, flags.none, emit_flags.none

opcode $14, OP_NULL, MODE_NOOP, 2, 4, flags.none, emit_flags.none
opcode $34, OP_NULL, MODE_NOOP, 2, 4, flags.none, emit_flags.none
opcode $54, OP_NULL, MODE_NOOP, 2, 4, flags.none, emit_flags.none
opcode $74, OP_NULL, MODE_NOOP, 2, 4, flags.none, emit_flags.none
opcode $D4, OP_NULL, MODE_NOOP, 2, 4, flags.none, emit_flags.none
opcode $F4, OP_NULL, MODE_NOOP, 2, 4, flags.none, emit_flags.none

opcode $0C, OP_NULL, MODE_NOOP, 3, 4, flags.none, emit_flags.none
opcode $1C, OP_NULL, MODE_NOOP, 3, 4, flags.none, emit_flags.none
opcode $3C, OP_NULL, MODE_NOOP, 3, 4, flags.none, emit_flags.none
opcode $5C, OP_NULL, MODE_NOOP, 3, 4, flags.none, emit_flags.none
opcode $7C, OP_NULL, MODE_NOOP, 3, 4, flags.none, emit_flags.none
opcode $DC, OP_NULL, MODE_NOOP, 3, 4, flags.none, emit_flags.none
opcode $FC, OP_NULL, MODE_NOOP, 3, 4, flags.none, emit_flags.none

;Illegal Opcodes 
opcode $07, OP_SLO_ZP, MODE_ZP, 2, 5, flags.gen, emit_flags.acc
opcode $B3, OP_LAX_MEM, MODE_IZY_READ, 2, 5, flags.gen, emit_flags.acc 

;TODO: other illegal opcodes

;----------------------------------------------------------------------
; OPCODE EMITTER FUNCTIONS 


opimp OP_NULL
	nop 
endop 

;------------------------------
; Branch instructions 


opimp OP_BCC 
	exx 
	bit 0,c 
	exx
	call z,jit_branch_local
endop

opimp OP_BCS 
	exx 
	bit 0,c 
	exx 
	call nz,jit_branch_local
endop

opimp OP_BNE
	ld e,a 
	ld a,i 
	ld a,e 
	call nz,jit_branch_local
endop

opimp OP_BEQ
	ld e,a 
	ld a,i 
	ld a,e
	call z,jit_branch_local 
endop

opimp OP_BVC
	exx 
	bit 6,b 
	exx 
	call z,jit_branch_local 
endop

opimp OP_BVS
	exx 
	bit 6,b 
	exx  
	call nz,jit_branch_local 
endop

opimp OP_BPL
	ld e,a 
	exx 
	ex de,hl 
	ld hl,i
	ld a,h 
	or a,l 
	ex de,hl
	exx
	ld a,e
	call p,jit_branch_local
endop

opimp OP_BMI
	ld e,a 
	exx 
	ex de,hl
	ld hl,i
	ld a,h 
	or a,l 
	ex de,hl
	exx
	ld a,e
	call m,jit_branch_local
endop

opimp OP_RTS
	jp jit_return 
endop 

opimp OP_RTI
	jp jit_return_int
endop 

;------------------------------
; Implicit instructions 

opimp OP_CLC
	exx
	ld c,0
	exx 
endop 

opimp OP_SEC
	exx
	ld c,1 
	exx 
endop 

opimp OP_CLV
	exx 
	res 6,b
	exx
endop

opimp OP_CLI
	exx 
	res 2,b
	exx 
endop 

opimp OP_SEI
	exx 
	set 2,b
	exx 
endop

opimp OP_CLD
	exx 
	res 3,b
	exx 
endop 

opimp OP_SED
	exx 
	set 3,b
	exx 
endop 

opimp OP_INX
	inc b
endop

opimp OP_INY
	inc c
endop

opimp OP_DEX
	dec b
endop

opimp OP_DEY
	dec c
endop

opimp OP_ASL_IMP
	sla a 
	exx 
	rl c 
	exx
endop

opimp OP_ROL_IMP
	exx 
	srl c 
	rla 
	rl c 
	exx
endop

opimp OP_LSR_IMP
	srl a 
	exx 
	rl c 
	exx
endop

opimp OP_ROR_IMP
	exx 
	srl c 
	rra 
	rl c 
	exx
endop

opimp OP_TAX
	ld b,a
endop

opimp OP_TXA
	ld a,b
endop

opimp OP_TAY
	ld c,a
endop

opimp OP_TYA
	ld a,c
endop

opimp OP_TSX 
	ld e,a 
	exx 
	ld a,l
	exx 
	ld b,a 
	ld a,e
endop 

opimp OP_TXS
	ld e,a 
	ld a,b 
	exx 
	ld l,a 
	exx  
	ld a,e
endop 


opimp OP_PHA
	exx 
	ld (hl),a 
	dec l 
	exx 
endop

opimp OP_PLA
	exx 
	inc l
	ld a,(hl) 
	exx 
endop

opimp OP_PHP
	exx 
	set 4,b 	; B flag
	call jit_push_flags
endop

opimp OP_PLP
	call jit_pop_flags 
endop

; Load / Store 

opimp OP_LDA_IMM 
	ld a,0 
endop 

opimp OP_LDA_ZP 
	ld a,(iy+0) 
endop

opimp OP_LDA_ZPX
	ld a,(hl) 
endop

opimp OP_LDA_MEM 
	ld a,e 
endop 

opimp OP_LDX_IMM 
	ld b,0 
endop 

opimp OP_LDX_ZP 
	ld b,(iy+0) 
endop

opimp OP_LDX_ZPY
	ld b,(hl) 
endop

opimp OP_LDX_MEM 
	ld b,e 
endop 

opimp OP_LDY_IMM 
	ld c,0 
endop 

opimp OP_LDY_ZP 
	ld c,(iy+0) 
endop

opimp OP_LDY_ZPX
	ld c,(hl) 
endop

opimp OP_LDY_MEM 
	ld c,e 
endop 

opimp OP_STA_ZP 
	ld (iy+0),a 
endop 

opimp OP_STA_ZPX 
	ld (hl),a 
endop 

opimp OP_STA_MEM
	ld e,a 
endop 

opimp OP_STX_ZP 
	ld (iy+0),b 
endop 

opimp OP_STX_ZPY
	ld (hl),b
endop 

opimp OP_STX_MEM
	ld e,b
endop 

opimp OP_STY_ZP 
	ld (iy+0),c
endop 

opimp OP_STY_ZPX 
	ld (hl),c
endop 

opimp OP_STY_MEM
	ld e,c
endop 


; Arithmetic 

opimp OP_ADC_IMM
	exx 
	srl c 
	adc a,0
	ld c,h
	jr c,$+4
	res 0,c  
	exx 
endop 

opimp OP_ADC_MEM
	exx 
	srl c 
	ld c,h 
	exx 
	adc a,e
	jr c,$+6
	exx 
	res 0,c 
	exx 
endop 

opimp OP_ADC_ZP
	exx 
	srl c 
	adc a,(iy+0)
	ld c,h
	jr c,$+4
	res 0,c
	exx 
endop 

opimp OP_ADC_ZPX
	exx 
	srl c 
	ld c,h
	exx 
	adc a,(hl)
	jr c,$+6
	exx 
	res 0,c 
	exx 
endop 

opimp OP_SBC_IMM
	exx 
	ld e,a 
	ld a,0 
	cpl 
	srl c 
	adc a,e 
	ld c,h
	jr c,$+4
	res 0,c
	exx 
endop 

opimp OP_SBC_MEM
	ld l,a 
	ld a,e 
	cpl 
	exx 
	srl c 
	ld c,h
	exx 
	adc a,l
	jr c,$+6
	exx 
	res 0,c
	exx 
endop 

opimp OP_SBC_ZP
	exx 
	ld e,a 
	ld a,(iy+0)
	cpl 
	srl c 
	adc a,e 
	ld c,h
	jr c,$+4
	res 0,c 
	exx 
endop 

opimp OP_SBC_ZPX
	ld e,a 
	ld a,(hl) 
	cpl 
	exx 
	srl c 
	ld c,h
	exx 
	adc a,e
	jr c,$+6
	exx 
	res 0,c 
	exx 
endop 

opimp OP_CMP_IMM 
	ld e,a 
	sub a,0
	ccf 
	exx 
	rl c 
	exx
	ld i,a 
	ld a,e
endop

opimp OP_CMP_ZP
	ld e,a 
	sub a,(iy+0)
	ccf 
	exx 
	rl c 
	exx
	ld i,a 
	ld a,e
endop

opimp OP_CMP_ZPX
	ld e,a 
	sub a,(hl)
	ccf 
	exx 
	rl c 
	exx
	ld i,a 
	ld a,e
endop

opimp OP_CMP_MEM
	ld l,a 
	sub a,e
	ccf 
	exx 
	rl c 
	exx
	ld i,a 
	ld a,l
endop

opimp OP_CPX_IMM 
	ld l,a 
	ld a,b 
	sub a,0 
	ccf  
	exx 
	rl c 
	exx
	ld i,a 
	ld a,l
endop 

opimp OP_CPX_ZP
	ld l,a 
	ld a,b 
	sub a,(iy+0) 
	ccf  
	exx 
	rl c 
	exx
	ld i,a 
	ld a,l
endop 

opimp OP_CPX_MEM 
	ld l,a 
	ld a,b 
	sub a,e
	ccf  
	exx 
	rl c 
	exx
	ld i,a 
	ld a,l
endop 

opimp OP_CPY_IMM 
	ld l,a 
	ld a,c 
	sub a,0 
	ccf  
	exx 
	rl c 
	exx
	ld i,a 
	ld a,l
endop 

opimp OP_CPY_ZP
	ld l,a 
	ld a,c
	sub a,(iy+0) 
	ccf  
	exx 
	rl c 
	exx
	ld i,a 
	ld a,l
endop 

opimp OP_CPY_MEM 
	ld l,a 
	ld a,c
	sub a,e
	ccf  
	exx 
	rl c 
	exx
	ld i,a 
	ld a,l
endop 

; Bitwise 

opimp OP_AND_IMM
	and a,0 
endop 

opimp OP_AND_ZP 
	and a,(iy+0) 
endop 

opimp OP_AND_ZPX 
	and a,(hl) 
endop 

opimp OP_AND_MEM  
	and a,e
endop 

opimp OP_OR_IMM
	or a,0 
endop 

opimp OP_OR_ZP 
	or a,(iy+0) 
endop 

opimp OP_OR_ZPX 
	or a,(hl) 
endop 

opimp OP_OR_MEM  
	or a,e
endop 


opimp OP_EOR_IMM
	xor a,0 
endop 

opimp OP_EOR_ZP 
	xor a,(iy+0) 
endop 

opimp OP_EOR_ZPX 
	xor a,(hl) 
endop 

opimp OP_EOR_MEM  
	xor a,e
endop 

opimp OP_BIT_ZP
	ld h,(iy+0)
	ld e,a
	and a,h
	ld l,a
	ld i,hl 
	ld a,e
	bit 6,h
	exx 
	res 6,b  
	jr z,$+4
	set 6,b 
	exx 
endop

opimp OP_BIT_MEM
	ld h,e
	ld e,a
	and a,h
	ld l,a
	ld i,hl 
	ld a,e
	bit 6,h
	exx 
	res 6,b  
	jr z,$+4
	set 6,b 
	exx 
endop


; Read-Modify-Write
opimp OP_INC_ZP 
	lea hl,iy+0 
	ld e,(hl) 
	inc e 
	ld (hl),e 
endop 

opimp OP_INC_ZPX
	ld e,(hl) 
	inc e 
	ld (hl),e
endop 

opimp OP_INC_MEM
	inc e 
endop 

opimp OP_DEC_ZP 
	lea hl,iy+0  
	ld e,(hl) 
	dec e 
	ld (hl),e
endop 

opimp OP_DEC_ZPX
	ld e,(hl) 
	dec e 
	ld (hl),e
endop 

opimp OP_DEC_MEM
	dec e 
endop 

opimp OP_ASL_ZP
	lea hl,iy+0
	ld e,(hl) 
	sla e 
	ld (hl),e 
	exx
	rl c 
	exx 
endop

opimp OP_ASL_ZPX 
	ld e,(hl) 
	sla e 
	ld (hl),e 
	exx
	rl c 
	exx 
endop 

opimp OP_ASL_MEM 
	sla e 
	exx
	rl c 
	exx 
endop 

opimp OP_LSR_ZP
	lea hl,iy+0
	ld e,(hl) 
	srl e 
	ld (hl),e 
	exx
	rl c 
	exx 
endop

opimp OP_LSR_ZPX 
	ld e,(hl) 
	srl e 
	ld (hl),e 
	exx
	rl c 
	exx 
endop 

opimp OP_LSR_MEM 
	srl e 
	exx
	rl c 
	exx 
endop

opimp OP_ROL_ZP
	exx 
	srl c 
	exx
	lea hl,iy+0
	ld e,(hl) 
	rl e 
	ld (hl),e 
	exx
	rl c 
	exx 
endop

opimp OP_ROL_ZPX 
	exx 
	srl c 
	exx
	ld e,(hl) 
	rl e 
	ld (hl),e 
	exx
	rl c 
	exx 
endop 

opimp OP_ROL_MEM 
	exx 
	srl c 
	exx
	rl e 
	exx
	rl c 
	exx 
endop 

opimp OP_ROR_ZP
	exx 
	srl c 
	exx
	lea hl,iy+0
	ld e,(hl) 
	rr e 
	ld (hl),e 
	exx
	rl c 
	exx 
endop

opimp OP_ROR_ZPX 
	exx 
	srl c 
	exx
	ld e,(hl) 
	rr e 
	ld (hl),e  
	exx
	rl c 
	exx 
endop 

opimp OP_ROR_MEM 
	exx 
	srl c 
	exx
	rr e 
	exx
	rl c 
	exx 
endop
 


; Illegal Opcodes used in official games (besides NOPs) 

opimp OP_LAX_MEM 
	ld a,e 
	ld b,e 
endop 

opimp OP_SLO_ZP 
	lea hl,iy+0
	sla (hl)
	exx 
	srl c 
	exx
	or a,(hl) 
endop 
