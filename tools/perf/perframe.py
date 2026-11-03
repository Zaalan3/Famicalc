# perframe.py PROG OUTFILE F0 F1 [watch]: Zelda attract mode; NES RAM ($0000-$07FF) + OAM copied at
# every ppu_video_start (exact emulated point) for NES frames F0..F1, one md5 per frame.
# watch: also note every CE write to the WRAM copy (jit_nes_ewram, 8 KB) from game pick to F1.
import sys, os, hashlib, time
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../tests/pc'))
import fctest
prog, out, f0, f1 = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4])
watch = 'watch' in sys.argv[5:]
syms = fctest.symbols(prog)
calc = fctest.start_famicalc(['ZELDA'], prog)
fctest.select_game(calc, ['ZELDA'], 'ZELDA')
F = fctest.Frames(calc, syms)
ew = syms['jit_nes_ewram']
w0 = calc.peek(ew, 0x2000)
if watch:
    calc.cmd('watch-write %06X %d' % (ew, 0x2000))
    calc.cmd('watch-hits')
vs = syms['ppu_video_start']
F.wait(max(0, f0 - 20 - F.now()))
calc.cmd('snap %06X %06X %d' % (vs, 0xD50000, 0x800))
calc.cmd('snap %06X %06X %d' % (vs, 0xD60A00, 256))
tmp = out + '.tmp'
prev = F.now()
calc.cmd('snaps-all %06X %s' % (vs, tmp))
f = open(out, 'w')
CH = 0x800 + 256
while prev < f1:
    calc.run(50)
    now = F.now()
    r = calc.cmd('snaps-all %06X %s' % (vs, tmp))
    n = int(r.split()[0])
    for _ in range(200):
        data = open(tmp, 'rb').read()
        if len(data) == n * CH: break
        time.sleep(0.01)
    assert len(data) == n * CH, (len(data), n)
    assert n == now - prev, (n, now, prev)
    for i in range(n):
        fr = prev + 1 + i
        if f0 <= fr <= f1:
            c = data[i*CH:(i+1)*CH]
            f.write('%d %s %s\n' % (fr, hashlib.md5(c[:0x800]).hexdigest()[:12], hashlib.md5(c[0x800:]).hexdigest()[:12]))
    prev = now
f.close()
w1 = calc.peek(ew, 0x2000)
print('frames %d-%d; WRAM md5 at pick %s, at end %s, bytes changed %d' % (f0, f1, hashlib.md5(w0).hexdigest()[:10],
      hashlib.md5(w1).hexdigest()[:10], sum(a != b for a, b in zip(w0, w1))))
if watch:
    print('WRAM write PCs:', calc.cmd('watch-hits'))
calc.close()
