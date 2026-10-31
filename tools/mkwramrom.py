#!/usr/bin/env python3
"""mkwramrom.py OUTDIR: the WRAMT test ROM (MMC1, 32 KB PRG, CHR RAM) as
FamiCalc appvars (WRAMT.8xv, WRAMTP0.8xv) for tests/pc/fctest.py wram_code.

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
}
SIZE = {'': 1, '#': 2, 'z': 2, '(z),y': 2, 'r': 2, 'a': 3, 'a,x': 3}

RESULT = 0x0310
ROUTINE = [0xA9, 0x11, 0x8D, RESULT & 0xFF, RESULT >> 8, 0x60]	# LDA #$11 / STA $0310 / RTS
PAGES = [0x60, 0x61, 0x62, 0x63, 0x64, 0x65]


def program():
    p = []
    a = lambda op, mode='', val=None: p.append((op, mode, val))
    label = lambda name: p.append((name,))

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
    a('LDA', '#', 0xA5); a('STA', 'a', 0x03FF)
    label('done'); a('JMP', 'a', 'done')
    label('store_sub'); a('STA', 'a', 0x6001); a('RTS')
    label('nmi'); a('RTI')
    label('routine'); p.append(('.byte', ROUTINE))
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


def rom():
    code, labels = assemble(program(), 0xC000)
    prg = bytearray(0x8000)
    prg[0x4000:0x4000 + len(code)] = code
    prg[0x7FFA:0x8000] = (labels['nmi'].to_bytes(2, 'little') + labels['reset'].to_bytes(2, 'little') +
                          labels['nmi'].to_bytes(2, 'little'))
    return bytes(prg)


def appvar(name, data):
    """A TI-84 Plus CE appvar file (.8xv), archived, as FamiCalc's converters write them."""
    body = len(data).to_bytes(2, 'little') + data
    entry = (b'\x0d\x00' + len(body).to_bytes(2, 'little') + b'\x15' + name.encode().ljust(8, b'\0') +
             b'\x00\x80' + len(body).to_bytes(2, 'little') + body)
    return (b'**TI83F*\x1a\x0a\x00' + b'FamiCalc WRAM test ROM'.ljust(42, b'\0') +
            len(entry).to_bytes(2, 'little') + entry + (sum(entry) & 0xFFFF).to_bytes(2, 'little'))


def main():
    out = sys.argv[1]
    os.makedirs(out, exist_ok=True)
    # FamiCalc's header appvar: "FAMICALC", mapper 1, mirroring 0, 2 PRG pages of 16 KB, 0 CHR (CHR RAM)
    files = {'WRAMT.8xv': appvar('WRAMT', b'FAMICALC' + bytes([1, 0, 2, 0]) + b'WRAM code test\0'),
             'WRAMTP0.8xv': appvar('WRAMTP0', rom())}
    for name, data in files.items():
        path = os.path.join(out, name)
        if not os.path.exists(path) or open(path, 'rb').read() != data:
            open(path, 'wb').write(data)


if __name__ == '__main__':
    main()
