#!/usr/bin/env python3
"""Which 6502 code the translated code's time goes to.

    python3 jitprof.py SCENE [seconds]

Samples the program counter while the scene plays (like fctest.py profile),
then reads the JIT cache and finds, for each sample in translated code, the
block it is in (each block starts with `ld hl,<6502 address>` and a call to
jit_scanline, codegen.asm emit_block_header). Prints the busiest blocks with
their 6502 code (read through the translation buffer: the banks mapped now)."""
import bisect
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import fctest  # noqa: E402

OPS = {}
for line in '''
00 BRK imp 1;01 ORA izx 2;05 ORA zp 2;06 ASL zp 2;08 PHP imp 1;09 ORA imm 2;0A ASL acc 1;0D ORA abs 3;0E ASL abs 3
10 BPL rel 2;11 ORA izy 2;15 ORA zpx 2;16 ASL zpx 2;18 CLC imp 1;19 ORA aby 3;1D ORA abx 3;1E ASL abx 3
20 JSR abs 3;21 AND izx 2;24 BIT zp 2;25 AND zp 2;26 ROL zp 2;28 PLP imp 1;29 AND imm 2;2A ROL acc 1;2C BIT abs 3;2D AND abs 3;2E ROL abs 3
30 BMI rel 2;31 AND izy 2;35 AND zpx 2;36 ROL zpx 2;38 SEC imp 1;39 AND aby 3;3D AND abx 3;3E ROL abx 3
40 RTI imp 1;41 EOR izx 2;45 EOR zp 2;46 LSR zp 2;48 PHA imp 1;49 EOR imm 2;4A LSR acc 1;4C JMP abs 3;4D EOR abs 3;4E LSR abs 3
50 BVC rel 2;51 EOR izy 2;55 EOR zpx 2;56 LSR zpx 2;58 CLI imp 1;59 EOR aby 3;5D EOR abx 3;5E LSR abx 3
60 RTS imp 1;61 ADC izx 2;65 ADC zp 2;66 ROR zp 2;68 PLA imp 1;69 ADC imm 2;6A ROR acc 1;6C JMP ind 3;6D ADC abs 3;6E ROR abs 3
70 BVS rel 2;71 ADC izy 2;75 ADC zpx 2;76 ROR zpx 2;78 SEI imp 1;79 ADC aby 3;7D ADC abx 3;7E ROR abx 3
81 STA izx 2;84 STY zp 2;85 STA zp 2;86 STX zp 2;88 DEY imp 1;8A TXA imp 1;8C STY abs 3;8D STA abs 3;8E STX abs 3
90 BCC rel 2;91 STA izy 2;94 STY zpx 2;95 STA zpx 2;96 STX zpy 2;98 TYA imp 1;99 STA aby 3;9A TXS imp 1;9D STA abx 3
A0 LDY imm 2;A1 LDA izx 2;A2 LDX imm 2;A4 LDY zp 2;A5 LDA zp 2;A6 LDX zp 2;A8 TAY imp 1;A9 LDA imm 2;AA TAX imp 1;AC LDY abs 3;AD LDA abs 3;AE LDX abs 3
B0 BCS rel 2;B1 LDA izy 2;B4 LDY zpx 2;B5 LDA zpx 2;B6 LDX zpy 2;B8 CLV imp 1;B9 LDA aby 3;BA TSX imp 1;BC LDY abx 3;BD LDA abx 3;BE LDX aby 3
C0 CPY imm 2;C1 CMP izx 2;C4 CPY zp 2;C5 CMP zp 2;C6 DEC zp 2;C8 INY imp 1;C9 CMP imm 2;CA DEX imp 1;CC CPY abs 3;CD CMP abs 3;CE DEC abs 3
D0 BNE rel 2;D1 CMP izy 2;D5 CMP zpx 2;D6 DEC zpx 2;D8 CLD imp 1;D9 CMP aby 3;DD CMP abx 3;DE DEC abx 3
E0 CPX imm 2;E1 SBC izx 2;E4 CPX zp 2;E5 SBC zp 2;E6 INC zp 2;E8 INX imp 1;E9 SBC imm 2;EA NOP imp 1;EC CPX abs 3;ED SBC abs 3;EE INC abs 3
F0 BEQ rel 2;F1 SBC izy 2;F5 SBC zpx 2;F6 INC zpx 2;F8 SED imp 1;F9 SBC aby 3;FD SBC abx 3;FE INC abx 3
'''.replace('\n', ';').split(';'):
    if line.strip():
        op, name, mode, n = line.split()
        OPS[int(op, 16)] = (name, mode, int(n))


def dis(read, addr, n=12):
    out = []
    for _ in range(n):
        op = read(addr)
        name, mode, size = OPS.get(op, ('???', 'imp', 1))
        b = [read((addr + i) & 0xFFFF) for i in range(1, size)]
        arg = ''
        if mode == 'imm':
            arg = '#$%02X' % b[0]
        elif mode in ('zp', 'zpx', 'zpy', 'izx', 'izy'):
            arg = {'zp': '$%02X', 'zpx': '$%02X,X', 'zpy': '$%02X,Y', 'izx': '($%02X,X)', 'izy': '($%02X),Y'}[mode] % b[0]
        elif mode in ('abs', 'abx', 'aby', 'ind'):
            v = b[0] | b[1] << 8
            arg = {'abs': '$%04X', 'abx': '$%04X,X', 'aby': '$%04X,Y', 'ind': '($%04X)'}[mode] % v
        elif mode == 'rel':
            arg = '$%04X' % ((addr + 2 + (b[0] - 256 if b[0] > 127 else b[0])) & 0xFFFF)
        elif mode == 'acc':
            arg = 'A'
        out.append('    %04X  %s %s' % (addr, name, arg))
        addr = (addr + size) & 0xFFFF
        if name in ('RTS', 'RTI', 'JMP'):
            break
    return '\n'.join(out)


def main():
    name = sys.argv[1]
    seconds = float(sys.argv[2]) if len(sys.argv) > 2 else 3
    game, steps, play = ([(g, st, None) for n, g, st in fctest.BENCH if n == name]
                         or [fctest.BENCH_PLAY[name]])[0]
    syms = fctest.symbols()
    calc = fctest.start_famicalc([game])
    fctest.select_game(calc, [game], game)
    F = fctest.Frames(calc, syms)
    fctest.run_steps(F, steps)
    calc.cmd('profile-on 10')
    if play:
        play(calc, syms, seconds)
    else:
        calc.run(int(seconds * 1000))
    out = calc.cmd('profile-off').split()
    total = int(out[0])
    samples = {}
    for item in out[1:]:
        a, c = item.split('=')
        a = int(a, 16) & 0xFFFFFF
        samples[a] = samples.get(a, 0) + int(c)
    regions = [(0xD50C00, 0xD5FC00)]
    ext0, ext1 = calc.u24(syms['_jit_cache_extend']), calc.u24(syms['_jit_cache_extend_end'])
    if ext1 > ext0:
        regions.append((ext0, ext1))
    js = syms['jit_scanline'].to_bytes(3, 'little')
    headers = []
    mem = {}
    for lo, hi in regions:
        data = b''
        for a in range(lo, hi, 0x4000):
            data += calc.peek(a, min(0x4000, hi - a))
        mem[lo] = data
        i = 0
        while True:
            i = data.find(b'\xCD' + js, i)
            if i < 0:
                break
            if i >= 4 and data[i - 4] == 0x21:
                headers.append((lo + i - 4, data[i - 3] | data[i - 2] << 8))
            i += 1
    headers.sort()
    starts = [h[0] for h in headers]
    tb = calc.peek(syms.get('jit_translation_buffer', 0xD61000), 768) if False else calc.peek(0xD61000, 768)
    cache6502 = {}

    def read(addr):
        page = addr >> 8
        host = int.from_bytes(tb[page * 3:page * 3 + 3], 'little') + (addr & 0xFF) - 128
        if host not in cache6502:
            cache6502[host] = calc.peek(host, 1)[0]
        return cache6502[host]

    per_block = {}
    in_code = 0
    for a, c in samples.items():
        if not any(lo <= a < hi for lo, hi in regions):
            continue
        in_code += c
        i = bisect.bisect_right(starts, a) - 1
        origin = headers[i][1] if i >= 0 else -1
        per_block[origin] = per_block.get(origin, 0) + c
    print('%d samples, %.1f%% in translated code, %d blocks' % (total, 100 * in_code / total, len(headers)))
    for origin, c in sorted(per_block.items(), key=lambda x: -x[1])[:15]:
        print('\n%5.1f%% of all time: block $%04X' % (100 * c / total, origin))
        if origin >= 0:
            print(dis(read, origin))
    calc.close()


if __name__ == '__main__':
    main()
