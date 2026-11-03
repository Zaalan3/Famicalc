#!/usr/bin/env python3
"""mkwramrom.py OUTDIR: WRAM code test ROMs (32 KB PRG, CHR RAM) as FamiCalc
appvars (NAME.8xv, NAMEP0.8xv) for tests/pc/fctest.py wram_code: WRAMT
(MMC1), WRAMN (NROM) and WRAMM (MMC3) run the same program, WRAMB (MMC1)
the page border one.

The 6502 program runs code in cartridge WRAM ($6000-$7FFF), which the JIT
caches, and overwrites it while it is cached. A routine "LDA #v / STA $0310 /
RTS" is copied to the pages $60-$65 and called 20 times each; then one byte of
it (v) is changed on each page by a different kind of store and the routine is
called again; each result ($0310) goes to a table:

  $0320-$0325  first calls, pages $60-$65                      $11 each
  $0330  page $60: STA abs, in a subroutine translated before
         the page held code (inlined unchecked then)           $22
  $0331  page $61: STA abs,X                                   $33
  $0332  page $62: STA (zp),Y                                  $44
  $0333  page $63: INC abs                                     $12
  $0334  page $64: INC abs,X                                   $12
  $0335  page $65: STA abs                                     $66
  $0336-$033B  every page called again                         $22 $33 $44 $12 $12 $66
  $0340-$0349  page $60 rewritten 10 times (STA abs) and
               called after each (it turns volatile)           $70-$79
  $034A  page $61 after that: STA abs                          $99
  $0350  a block from $66FC whose LDA # operand is at $6700    $11
  $0351  the same after STA $6701 (the block ran into $67)     $C3
  $0352  JSR $6800 -> JSR $6900, both pages' first code (the
         cache is flushed in jit_call for code in RAM)         $5A
  $0353  a local loop and 30 JSRs after that (call stack)      $07
  $0354  NROM: LDA $0300,X right after STA $7FF0,X (X = $70:
         $8060, no write) on a code page ($7F)                 $5C
  $03A1-$03AF  $70F8: TXA / LDY #0 / STA $7101,Y / NOP / NOP / LDA #0
         / STA $0317 / RTS: a block running into $71 that rewrites its
         own operand there (indexed)                           X
  $0355  $6C80 (LDA #v / STA / RTS) after STA $6C81,X in a
         subroutine translated before $6C held code (NROM
         inlines it unchecked)                                 $77
  $0361-$036F  $6A00: TXA / STA $6A05 / LDA #0 / STA $0315 /
         RTS called with X = 1-15: the block rewrites its own
         LDA operand (self-modifying code)                     X
  $0371-$037F  $6B00: STX $6B04 / LDA #0 / STA $0316 / RTS
         the same with STX abs                                 X
  $0380-$0385  $6F80 (LDA #v / STA / RTS) rewritten 6 times:
         $6F turns volatile                                    $80-$85
  $0388-$038B  $6EF9: a block running into $6F (its LDA
         operand at $6F00, rewritten 4 times): per instruction $90-$93
  $038C  $6E10, early in $6E: stays cached                     $E5
  $03FF  $A5 when done

WRAMB, the page border (the test counts flush_cache and jit_convert_ram;
it starts when $0700 = $5A):
  $6CF8-$6CFF  a routine ending at the page's last byte: $6D
               holds no code, so 8 data writes to $6D00 neither
               flush the cache nor turn $6C or $6D volatile    $0390-$0397 = $11
  $0398  $6D00 read back                                       $07
  $6AF9-$6B04  a routine running into $6B (LDA operand at $6B00):
  $0399  called 20 times                                       $11
  $039A  after STA $6B00                                       $C3
  $03A0-$03AB  as $0380-$038B above ($6F volatile, a block
               from $6EF9 running into it)                     $80-$85 $90-$93
  $03FE  1: the test counts again, and lets it go on with $0701 = $5A
  $03AC  $6E10, early in $6E (doesn't reach $6F): cached       $E5
  $03FF  $A5 when done

Files are only rewritten when their contents change (the PC harness caches a
CEmu image per game set by file time)."""
import os
import sys

OPS = {  # mnemonic, mode -> opcode
    ('SEI', ''): 0x78, ('CLD', ''): 0xD8, ('TXS', ''): 0x9A, ('INX', ''): 0xE8, ('DEX', ''): 0xCA,
    ('DEY', ''): 0x88, ('TXA', ''): 0x8A, ('CLC', ''): 0x18, ('RTS', ''): 0x60, ('RTI', ''): 0x40,
    ('LDA', '#'): 0xA9, ('LDX', '#'): 0xA2, ('LDY', '#'): 0xA0, ('ADC', '#'): 0x69, ('CPX', '#'): 0xE0,
    ('LDA', 'a'): 0xAD, ('STA', 'a'): 0x8D, ('INC', 'a'): 0xEE, ('JSR', 'a'): 0x20, ('JMP', 'a'): 0x4C,
    ('LDA', 'a,x'): 0xBD, ('STA', 'a,x'): 0x9D, ('INC', 'a,x'): 0xFE,
    ('STA', 'z'): 0x85, ('STA', '(z),y'): 0x91,
    ('BNE', 'r'): 0xD0, ('BPL', 'r'): 0x10,
    ('NOP', ''): 0xEA, ('CMP', '#'): 0xC9,
    ('LDA', 'a,y'): 0xB9, ('STA', 'a,y'): 0x99, ('BIT', 'a'): 0x2C, ('LSR', ''): 0x4A, ('STX', 'a'): 0x8E,
    ('STY', 'a'): 0x8C, ('INY', ''): 0xC8, ('CPY', '#'): 0xC0, ('ORA', '#'): 0x09, ('PHA', ''): 0x48,
    ('PLA', ''): 0x68, ('TAX', ''): 0xAA, ('INC', 'a'): 0xEE, ('LDX', 'a'): 0xAE, ('BEQ', 'r'): 0xF0,
    ('LDY', 'a'): 0xAC, ('AND', '#'): 0x29, ('EOR', '#'): 0x49, ('TYA', ''): 0x98, ('TAY', ''): 0xA8,
}
SIZE = {'': 1, '#': 2, 'z': 2, '(z),y': 2, 'r': 2, 'a': 3, 'a,x': 3, 'a,y': 3}

NOP, LDA_I, STA_A, STX_A, TXA, RTS, JSR = 0xEA, 0xA9, 0x8D, 0x8E, 0x8A, 0x60, 0x20
RC = [NOP] * 4 + [LDA_I, 0x11, STA_A, 0x12, 0x03, RTS]		# $66FC: its LDA operand at $6700
RA = [JSR, 0x00, 0x69, RTS]					# $6800: JSR $6900 / RTS
RB = [LDA_I, 0x5A, STA_A, 0x13, 0x03, RTS]			# $6900
SMC_A = [TXA, STA_A, 0x05, 0x6A, LDA_I, 0x00, STA_A, 0x15, 0x03, RTS]	# $6A00
SMC_X = [STX_A, 0x04, 0x6B, LDA_I, 0x00, STA_A, 0x16, 0x03, RTS]	# $6B00
R3 = [LDA_I, 0x00, STA_A, 0x12, 0x03, RTS]			# $6F80
R4 = [NOP] * 6 + [LDA_I, 0x00, STA_A, 0x12, 0x03, RTS]		# $6EF9: its LDA operand at $6F00
R5 = [LDA_I, 0xE5, STA_A, 0x12, 0x03, RTS]			# $6E10
R6 = [LDA_I, 0x11, STA_A, 0x12, 0x03, RTS]			# $6C80
# $70F8: TXA / LDY #0 / STA $7101,Y / NOP / NOP / LDA #0 ($7100: its operand at $7101) / STA $0317 / RTS
SMC_Y = [TXA, 0xA0, 0x00, 0x99, 0x01, 0x71, NOP, NOP, LDA_I, 0x00, STA_A, 0x17, 0x03, RTS]
R1 = [LDA_I, 0x11, STA_A, 0x12, 0x03, NOP, NOP, RTS]		# $6CF8-$6CFF
R2 = [NOP] * 6 + [LDA_I, 0x11, STA_A, 0x12, 0x03, RTS]		# $6AF9: its LDA operand at $6B00

RESULT = 0x0310
ROUTINE = [0xA9, 0x11, 0x8D, RESULT & 0xFF, RESULT >> 8, 0x60]	# LDA #$11 / STA $0310 / RTS
PAGES = [0x60, 0x61, 0x62, 0x63, 0x64, 0x65]


def assembler_helpers():
    p, tables = [], []
    a = lambda op, mode='', val=None: p.append((op, mode, val))
    label = lambda name: p.append((name,))

    def copy(data, at, name):	# copy a table in ROM to WRAM at
        tables.append((name, data))
        a('LDX', '#', len(data) - 1)
        label(name + '_copy'); a('LDA', 'a,x', name); a('STA', 'a,x', at); a('DEX'); a('BPL', 'r', name + '_copy')

    def sub_result(at, result, slot):	# clear result, call at, keep result in slot
        a('LDA', '#', 0); a('STA', 'a', result); a('JSR', 'a', at); a('LDA', 'a', result); a('STA', 'a', slot)
    return p, a, label, copy, sub_result, tables


def program():
    p, a, label, copy, sub_result, tables = assembler_helpers()

    def call(page, slot):	# clear the result, call the routine on page, keep the result in slot
        a('LDA', '#', 0); a('STA', 'a', RESULT)
        a('JSR', 'a', page << 8)
        a('LDA', 'a', RESULT); a('STA', 'a', slot)

    label('reset')
    a('SEI'); a('CLD'); a('LDX', '#', 0xFF); a('TXS')
    a('LDA', '#', 0); a('LDX', '#', 0)
    label('clear'); a('STA', 'a,x', 0x0300); a('INX'); a('BNE', 'r', 'clear')
    # translated now, while no WRAM page holds code: its STA $6001 is inlined
    a('LDA', '#', 0xEE); a('JSR', 'a', 'store_sub')
    a('LDX', '#', len(ROUTINE) - 1)
    label('copy'); a('LDA', 'a,x', 'routine')
    for pg in PAGES:
        a('STA', 'a,x', pg << 8)
    a('DEX'); a('BPL', 'r', 'copy')
    for i, pg in enumerate(PAGES):
        a('LDY', '#', 20)
        label('loop%d' % i); a('JSR', 'a', pg << 8); a('DEY'); a('BNE', 'r', 'loop%d' % i)
        call(pg, 0x0320 + i)
    a('LDA', '#', 0x22); a('JSR', 'a', 'store_sub'); call(0x60, 0x0330)
    a('LDX', '#', 1); a('LDA', '#', 0x33); a('STA', 'a,x', 0x6100); call(0x61, 0x0331)
    a('LDA', '#', 0x00); a('STA', 'z', 0x00); a('LDA', '#', 0x62); a('STA', 'z', 0x01)
    a('LDY', '#', 1); a('LDA', '#', 0x44); a('STA', '(z),y', 0x00); call(0x62, 0x0332)
    a('INC', 'a', 0x6301); call(0x63, 0x0333)
    a('LDX', '#', 1); a('INC', 'a,x', 0x6400); call(0x64, 0x0334)
    a('LDA', '#', 0x66); a('STA', 'a', 0x6501); call(0x65, 0x0335)
    for i, pg in enumerate(PAGES):
        call(pg, 0x0336 + i)
    a('LDX', '#', 0)
    label('volatile')
    a('TXA'); a('CLC'); a('ADC', '#', 0x70); a('STA', 'a', 0x6001)
    a('LDA', '#', 0); a('STA', 'a', RESULT); a('JSR', 'a', 0x6000)
    a('LDA', 'a', RESULT); a('STA', 'a,x', 0x0340)
    a('INX'); a('CPX', '#', 10); a('BNE', 'r', 'volatile')
    a('LDA', '#', 0x99); a('STA', 'a', 0x6101); call(0x61, 0x034A)
    copy(RC, 0x66FC, 'rc')
    a('LDY', '#', 20); label('rc_loop'); a('JSR', 'a', 0x66FC); a('DEY'); a('BNE', 'r', 'rc_loop')
    sub_result(0x66FC, 0x0312, 0x0350)
    a('LDA', '#', 0xC3); a('STA', 'a', 0x6701)
    sub_result(0x66FC, 0x0312, 0x0351)
    copy(RA, 0x6800, 'ra'); copy(RB, 0x6900, 'rb')
    a('JSR', 'a', 0x6800); a('LDA', 'a', 0x0313); a('STA', 'a', 0x0352)
    a('LDX', '#', 0); label('lp'); a('INX'); a('CPX', '#', 50); a('BNE', 'r', 'lp')
    a('LDY', '#', 30); label('lp2'); a('JSR', 'a', 'sub7'); a('DEY'); a('BNE', 'r', 'lp2')
    a('LDA', 'a', 0x0314); a('STA', 'a', 0x0353)
    # $7F holds code, then an indexed store from $7FF0 carries into $8060
    a('LDA', '#', RTS); a('STA', 'a', 0x7F00); a('JSR', 'a', 0x7F00)
    a('LDA', '#', 0x5C); a('STA', 'a', 0x0370)
    a('LDA', '#', 0); a('LDX', '#', 0x70); a('STA', 'a,x', 0x7FF0); a('LDA', 'a,x', 0x0300); a('STA', 'a', 0x0354)
    # self-modifying code, STA abs and STX abs
    for routine, at, out in [(SMC_A, 0x6A00, 0x0360), (SMC_X, 0x6B00, 0x0370)]:
        copy(routine, at, 'smc%X' % at)
        a('LDX', '#', 1); label('smc%X_loop' % at); a('JSR', 'a', at)
        a('LDA', 'a', 0x0315 if at == 0x6A00 else 0x0316); a('STA', 'a,x', out)
        a('INX'); a('CPX', '#', 16); a('BNE', 'r', 'smc%X_loop' % at)
    # $6F turns volatile; a block from $6EF9 runs into it, one at $6E10 doesn't
    copy(R3, 0x6F80, 'r3'); copy(R4, 0x6EF9, 'r4'); copy(R5, 0x6E10, 'r5')
    for i in range(3):
        a('JSR', 'a', 0x6F80)
    for i in range(6):
        a('LDA', '#', 0x80 + i); a('STA', 'a', 0x6F81); sub_result(0x6F80, 0x0312, 0x0380 + i)
    for i in range(4):
        a('LDA', '#', 0x90 + i); a('STA', 'a', 0x6F00); sub_result(0x6EF9, 0x0312, 0x0388 + i)
    for i in range(3):
        a('JSR', 'a', 0x6E10)
    sub_result(0x6E10, 0x0312, 0x038C)
    # NROM inlines idx_sub's STA $6C80,X ($6C and $6D hold no code), then
    # $6C holds code: that must drop it (else the store goes unnoticed)
    a('LDX', '#', 1); a('LDA', '#', 0); a('JSR', 'a', 'idx_sub')
    copy(R6, 0x6C80, 'r6'); a('JSR', 'a', 0x6C80)
    a('LDX', '#', 1); a('LDA', '#', 0x77); a('JSR', 'a', 'idx_sub'); sub_result(0x6C80, 0x0312, 0x0355)
    # a block from $70F8 runs into $71 and rewrites its own LDA operand there
    # with STA abs,Y (MMC1/MMC3 translate it as a call to mapper_write)
    copy(SMC_Y, 0x70F8, 'smcy')
    a('LDX', '#', 1); label('smcy_loop'); a('JSR', 'a', 0x70F8)
    a('LDA', 'a', 0x0317); a('STA', 'a,x', 0x03A0)
    a('INX'); a('CPX', '#', 16); a('BNE', 'r', 'smcy_loop')
    a('LDA', '#', 0xA5); a('STA', 'a', 0x03FF)
    label('done'); a('JMP', 'a', 'done')
    label('store_sub'); a('STA', 'a', 0x6001); a('RTS')
    label('idx_sub'); a('STA', 'a,x', 0x6C80); a('RTS')
    label('sub7'); a('LDX', '#', 0); label('s7'); a('INX'); a('CPX', '#', 7); a('BNE', 'r', 's7')
    a('TXA'); a('STA', 'a', 0x0314); a('RTS')
    label('nmi'); a('RTI')
    label('routine'); p.append(('.byte', ROUTINE))
    for name, data in tables:
        label(name); p.append(('.byte', data))
    return p


def program_border():
    p, a, label, copy, sub_result, tables = assembler_helpers()
    label('reset'); a('SEI'); a('CLD'); a('LDX', '#', 0xFF); a('TXS')
    label('go'); a('LDA', 'a', 0x0700); a('CMP', '#', 0x5A); a('BNE', 'r', 'go')
    a('LDA', '#', 0); a('LDX', '#', 0)
    label('clear'); a('STA', 'a,x', 0x0300); a('INX'); a('BNE', 'r', 'clear')
    copy(R1, 0x6CF8, 'r1'); copy(R2, 0x6AF9, 'r2')
    a('LDY', '#', 20); label('l1'); a('JSR', 'a', 0x6CF8); a('DEY'); a('BNE', 'r', 'l1')
    a('LDX', '#', 0)
    label('l2'); a('TXA'); a('STA', 'a', 0x6D00); a('LDA', '#', 0); a('STA', 'a', 0x0312)
    a('JSR', 'a', 0x6CF8); a('LDA', 'a', 0x0312); a('STA', 'a,x', 0x0390)
    a('INX'); a('CPX', '#', 8); a('BNE', 'r', 'l2')
    a('LDA', 'a', 0x6D00); a('STA', 'a', 0x0398)
    a('LDY', '#', 20); label('l3'); a('JSR', 'a', 0x6AF9); a('DEY'); a('BNE', 'r', 'l3')
    sub_result(0x6AF9, 0x0312, 0x0399)
    a('LDA', '#', 0xC3); a('STA', 'a', 0x6B00)
    sub_result(0x6AF9, 0x0312, 0x039A)
    # $6F turns volatile, a block from $6EF9 runs into it (per instruction)
    copy(R3, 0x6F80, 'r3'); copy(R4, 0x6EF9, 'r4'); copy(R5, 0x6E10, 'r5')
    for i in range(3):
        a('JSR', 'a', 0x6F80)
    for i in range(6):
        a('LDA', '#', 0x80 + i); a('STA', 'a', 0x6F81); sub_result(0x6F80, 0x0312, 0x03A0 + i)
    for i in range(4):
        a('LDA', '#', 0x90 + i); a('STA', 'a', 0x6F00); sub_result(0x6EF9, 0x0312, 0x03A8 + i)
    # then (the test counts again) one early in $6E stays cached
    a('LDA', '#', 1); a('STA', 'a', 0x03FE)
    label('go2'); a('LDA', 'a', 0x0701); a('CMP', '#', 0x5A); a('BNE', 'r', 'go2')
    for i in range(4):
        a('JSR', 'a', 0x6E10)
    sub_result(0x6E10, 0x0312, 0x03AC)
    a('LDA', '#', 0xA5); a('STA', 'a', 0x03FF)
    label('done'); a('JMP', 'a', 'done')
    label('nmi'); a('RTI')
    for name, data in tables:
        label(name); p.append(('.byte', data))
    return p


def program_sprites_line0():
    """SPR0 (NROM, CHR RAM): sprites on, PPUCTRL changes sprite size and
    pattern table at scanline 0 (the delay after the NMI is in $0700, set by
    the test). Before: 8x8 from $1000 (red); from line 0: 8x16 from $0000
    (white). Nothing is above line 0, so only white 8x16 sprites show."""
    p, a, label, copy, sub_result, tables = assembler_helpers()
    label('reset'); a('SEI'); a('CLD'); a('LDX', '#', 0xFF); a('TXS')
    label('v1'); a('BIT', 'a', 0x2002); a('BPL', 'r', 'v1')
    label('v2'); a('BIT', 'a', 0x2002); a('BPL', 'r', 'v2')
    def ppu_addr(addr):
        a('LDA', '#', addr >> 8); a('STA', 'a', 0x2006); a('LDA', '#', addr & 0xFF); a('STA', 'a', 0x2006)
    def ppu_bytes(values):
        for v in values:
            a('LDA', '#', v); a('STA', 'a', 0x2007)
    ppu_addr(0x0000); ppu_bytes([0xFF] * 8 + [0x00] * 8 + [0xFF] * 8 + [0x00] * 8)	# tiles 0, 1: color 1
    ppu_addr(0x1000); ppu_bytes([0x00] * 8 + [0xFF] * 8)				# $1000 tile 0: color 2
    ppu_addr(0x2000)							# nametable 0: tile 2 (empty)
    a('LDA', '#', 2); a('LDY', '#', 4); a('LDX', '#', 0)
    label('nt'); a('STA', 'a', 0x2007); a('INX'); a('BNE', 'r', 'nt'); a('DEY'); a('BNE', 'r', 'nt')
    ppu_addr(0x3F00); ppu_bytes([0x0F]); ppu_addr(0x3F11); ppu_bytes([0x30, 0x16])
    a('LDX', '#', 0)					# OAM: 8 sprites at y 100, x 24 apart, then hidden
    label('oam'); a('LDA', '#', 0xF0); a('STA', 'a,x', 0x0200); a('INX'); a('BNE', 'r', 'oam')
    for i in range(8):
        for k, v in enumerate([100, 0, 0, 24 + 24 * i]):
            a('LDA', '#', v); a('STA', 'a', 0x0200 + 4 * i + k)
    a('LDA', '#', 0); a('STA', 'a', 0x2005); a('STA', 'a', 0x2005)
    a('LDA', '#', 0x88); a('STA', 'a', 0x2000)		# NMI on, sprites 8x8 from $1000
    a('LDA', '#', 0x1E); a('STA', 'a', 0x2001)		# background and sprites, no clipping
    label('main'); a('JMP', 'a', 'main')
    label('nmi'); a('PHA')
    a('LDA', '#', 2); a('STA', 'a', 0x4014)
    a('LDA', '#', 0x88); a('STA', 'a', 0x2000)
    a('LDX', 'a', 0x0700)
    label('d1'); a('LDY', '#', 0)
    label('d2'); a('INY'); a('CPY', '#', 10); a('BNE', 'r', 'd2')
    a('DEX'); a('BNE', 'r', 'd1')
    a('LDY', 'a', 0x0701); a('INY')			# fine delay: $0701 + 1 short loops
    label('d3'); a('DEY'); a('BNE', 'r', 'd3')
    a('LDA', '#', 0xA0); a('STA', 'a', 0x2000)		# 8x16 from $0000
    a('INC', 'a', 0x0300)
    a('PLA'); a('RTI')
    return p


def program_sprites_mid():
    """SPRM (NROM, CHR RAM): PPUCTRL changes the sprite pattern table in the
    middle of the screen every frame (a delay after the NMI), so the sprites
    above are drawn there (pace_mid_sprites); one frame in 16 has 24 sprites
    on lines 40-71, the others none: the frame with them follows light ones."""
    p, a, label, copy, sub_result, tables = assembler_helpers()
    label('reset'); a('SEI'); a('CLD'); a('LDX', '#', 0xFF); a('TXS')
    label('v1'); a('BIT', 'a', 0x2002); a('BPL', 'r', 'v1')
    label('v2'); a('BIT', 'a', 0x2002); a('BPL', 'r', 'v2')
    def ppu_addr(addr):
        a('LDA', '#', addr >> 8); a('STA', 'a', 0x2006); a('LDA', '#', addr & 0xFF); a('STA', 'a', 0x2006)
    ppu_addr(0x0000)						# tile 0: color 1
    for v in [0xFF] * 8 + [0x00] * 8:
        a('LDA', '#', v); a('STA', 'a', 0x2007)
    ppu_addr(0x2000)						# nametable 0: tile 2 (empty)
    a('LDA', '#', 2); a('LDY', '#', 4); a('LDX', '#', 0)
    label('nt'); a('STA', 'a', 0x2007); a('INX'); a('BNE', 'r', 'nt'); a('DEY'); a('BNE', 'r', 'nt')
    ppu_addr(0x3F00); a('LDA', '#', 0x0F); a('STA', 'a', 0x2007)
    ppu_addr(0x3F11); a('LDA', '#', 0x30); a('STA', 'a', 0x2007)
    a('LDX', '#', 0)						# page 2: 24 sprites (4 rows of 6), page 3: none
    label('oam'); a('LDA', '#', 0xF0); a('STA', 'a,x', 0x0200); a('STA', 'a,x', 0x0300); a('INX'); a('BNE', 'r', 'oam')
    for i in range(24):
        for k, v in enumerate([40 + 8 * (i // 6), 0, 0, 40 + 30 * (i % 6)]):
            a('LDA', '#', v); a('STA', 'a', 0x0200 + 4 * i + k)
    a('LDA', '#', 0); a('STA', 'a', 0x2005); a('STA', 'a', 0x2005); a('STA', 'a', 0x0400)
    a('LDA', '#', 0x80); a('STA', 'a', 0x2000)		# NMI on, sprites from $0000
    a('LDA', '#', 0x1E); a('STA', 'a', 0x2001)
    label('main'); a('JMP', 'a', 'main')
    label('nmi'); a('PHA'); a('TXA'); a('PHA'); a('TYA'); a('PHA')
    a('INC', 'a', 0x0400); a('LDA', 'a', 0x0400)
    a('AND', '#', 0x0F); a('BEQ', 'r', 'many')		# one frame in 16: page 2 (sprites), else page 3
    a('LDA', '#', 3); a('BNE', 'r', 'dma')
    label('many'); a('LDA', '#', 2)
    label('dma'); a('STA', 'a', 0x4014)
    a('LDA', '#', 0x80); a('STA', 'a', 0x2000)
    a('LDX', '#', 200)					# ~100 lines
    label('d1'); a('LDY', '#', 0)
    label('d2'); a('INY'); a('CPY', '#', 10); a('BNE', 'r', 'd2')
    a('DEX'); a('BNE', 'r', 'd1')
    a('LDA', '#', 0x88); a('STA', 'a', 0x2000)		# sprites from $1000 from here on
    a('PLA'); a('TAY'); a('PLA'); a('TAX'); a('PLA'); a('RTI')
    return p


def assemble(p, org):
    labels, pc = {}, org
    for ins in p:	# pass 1: addresses
        if len(ins) == 1:
            labels[ins[0]] = pc
        elif ins[0] == '.byte':
            pc += len(ins[1])
        else:
            pc += SIZE[ins[1]]
    out, pc = bytearray(), org
    for ins in p:
        if len(ins) == 1:
            continue
        if ins[0] == '.byte':
            out += bytes(ins[1]); pc += len(ins[1]); continue
        mode, val = ins[1], ins[2]
        val = labels[val] if isinstance(val, str) else val
        out.append(OPS[(ins[0], mode)])
        if mode == 'r':
            off = val - (pc + 2)
            assert -128 <= off < 128, ins
            out.append(off & 0xFF)
        elif SIZE[mode] == 2:
            out.append(val & 0xFF)
        elif SIZE[mode] == 3:
            out += val.to_bytes(2, 'little')
        pc += SIZE[mode]
    return bytes(out), labels


def rom(prog=None, size=0x8000, banks=()):
    """PRG ROM: the program at $C000 (the last 16 KB), vectors, and
    (file offset, bytes) pieces"""
    code, labels = assemble((prog or program)(), 0xC000)
    prg = bytearray(size)
    prg[size - 0x4000:size - 0x4000 + len(code)] = code
    for offset, data in banks:
        prg[offset:offset + len(data)] = data
    prg[size - 6:size] = (labels['nmi'].to_bytes(2, 'little') + labels['reset'].to_bytes(2, 'little') +
                          labels['nmi'].to_bytes(2, 'little'))
    return bytes(prg)


def appvar(name, data):
    """A TI-84 Plus CE appvar file (.8xv), archived, as FamiCalc's converters write them."""
    body = len(data).to_bytes(2, 'little') + data
    entry = (b'\x0d\x00' + len(body).to_bytes(2, 'little') + b'\x15' + name.encode().ljust(8, b'\0') +
             b'\x00\x80' + len(body).to_bytes(2, 'little') + body)
    return (b'**TI83F*\x1a\x0a\x00' + b'FamiCalc WRAM test ROM'.ljust(42, b'\0') +
            len(entry).to_bytes(2, 'little') + entry + (sum(entry) & 0xFFFF).to_bytes(2, 'little'))


# name, mapper, mirroring (bit 1: four-screen), program, PRG size, other PRG pieces
GAMES = [('WRAMT', 1, 0, program, 0x8000, ()), ('WRAMN', 0, 0, program, 0x8000, ()),
         ('WRAMM', 4, 0, program, 0x8000, ()), ('WRAMB', 1, 0, program_border, 0x8000, ()),
         ('SPR0', 0, 0, program_sprites_line0, 0x8000, ()), ('SPRM', 0, 0, program_sprites_mid, 0x8000, ())]


def main():
    out = sys.argv[1]
    os.makedirs(out, exist_ok=True)
    # FamiCalc's header appvar: "FAMICALC", mapper, mirroring, PRG pages of 16 KB, 0 CHR (CHR RAM);
    # the PRG in appvars of 48 KB at most (P0, P1, ... as the converters write them)
    files = {}
    for name, mapper, mirroring, prog, size, banks in GAMES:
        files[name + '.8xv'] = appvar(name, b'FAMICALC' + bytes([mapper, mirroring, size // 0x4000, 0]) +
                                      b'WRAM code test\0')
        prg = rom(prog, size, banks)
        for k in range(0, size, 0xC000):
            files['%sP%d.8xv' % (name, k // 0xC000)] = appvar('%sP%d' % (name, k // 0xC000), prg[k:k + 0xC000])
    for name, data in files.items():
        path = os.path.join(out, name)
        if not os.path.exists(path) or open(path, 'rb').read() != data:
            open(path, 'wb').write(data)


if __name__ == '__main__':
    main()
