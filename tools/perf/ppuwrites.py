# ppuwrites.py SCENE [seconds] [wide]: the game's PPU register pattern per NES frame.
# Traces the register handlers (the value written is in e) and rebuilds the
# $2006 address latch, so each $2007 byte gets its VRAM address. Per frame:
# writes to $2000/$2001/$2005/$2006/$2007/$4014 and $2002 reads, split into
# vblank (ppu_video_end -> ppu_video_start) and rendering (mid-frame, which the
# renderer turns into events); $2007 bytes by target (pattern table / nametable
# / attribute / palette) and the runs (address, length); the scroll at the end
# of vblank and every mid-frame change. Needs fc-headless with "cycle address
# hl de af" trace lines. PROG=path.8xp for another build.
import sys, os, collections
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../tests/pc'))
import fctest
args = sys.argv[1:]; name = args[0]; wide = 'wide' in args
rest = [a for a in args[1:] if a != 'wide']; secs = float(rest[0]) if rest else 1
prog = os.environ.get('PROG')
game, steps, play = ([(g, st, None) for n, g, st in fctest.BENCH if n == name] or [fctest.BENCH_PLAY[name]])[0]
syms = fctest.symbols(prog)
calc = fctest.start_famicalc([game], prog)
fctest.select_game(calc, [game], game)
F = fctest.Frames(calc, syms)
fctest.run_steps(F, steps)
if wide: fctest.menu(calc, 'Picture'); F.wait(60)
ev = {'write_ppu_control': '2000', 'write_ppu_mask': '2001', 'read_ppu_status': '2002r', 'write_ppu_scroll': '2005',
      'write_ppu_address': '2006', 'write_ppu_data': '2007', 'write_oam_dma': '4014', 'ppu_video_start': 'VS',
      'ppu_video_end': 'VE', 'read_ppu_data': '2007r'}
ev = {syms[k]: v for k, v in ev.items() if k in syms}
calc.cmd('trace ' + ' '.join('%06X' % a for a in ev))
f0 = F.now()
if play: play(calc, syms, secs)
else: calc.run(int(secs * 1000))
f1 = F.now()
path = os.path.join(fctest.OUT, 'ppuw-%d.txt' % os.getpid()); calc.cmd('trace-dump ' + path)
log = []
for l in open(path):
    p = l.split()
    log.append((int(p[0]), ev[int(p[1], 16)], int(p[3], 16) & 0xFF))
os.remove(path)
calc.close()
# replay the PPU's latch: t = $2006/$2005 shared toggle, v = VRAM address
frames = []; cur = None; vblank = True
latch = 0; hi = 0; v = 0; inc = 1; sx = sy = 0; ctrl = 0
for c, n, e in log:
    if n == 'VS':
        vblank = False
        if cur: cur['scroll_vs'] = (sx, sy, ctrl & 3)
        continue
    if n == 'VE':
        vblank = True
        if cur: frames.append(cur)
        cur = {'n': collections.Counter(), 'runs': [], 'mid': [], 'tgt': collections.Counter()}
        continue
    if cur is None: continue
    where = 'vb' if vblank else 'mid'
    cur['n'][n + '.' + where] += 1
    if n == '2002r': latch = 0
    elif n == '2000':
        ctrl = e; inc = 32 if e & 4 else 1
        if not vblank: cur['mid'].append('$2000=%02X' % e)
    elif n == '2005':
        if latch == 0: sx = e
        else: sy = e
        if not vblank: cur['mid'].append('%s=%02X' % ('X' if latch == 0 else 'Y', e))
        latch ^= 1
    elif n == '2006':
        if latch == 0: hi = e & 0x3F
        else:
            v = hi << 8 | e
            if not vblank: cur['mid'].append('$2006=%04X' % v)
        latch ^= 1
    elif n == '2001':
        if not vblank: cur['mid'].append('$2001=%02X' % e)
    elif n == '2007':
        a = v & 0x3FFF
        t = 'chr' if a < 0x2000 else ('pal' if a >= 0x3F00 else ('attr' if (a & 0x3FF) >= 0x3C0 else 'nt'))
        cur['tgt'][t] += 1
        if cur['runs'] and cur['runs'][-1][2] == a and cur['runs'][-1][3] == inc:
            cur['runs'][-1][1] += 1; cur['runs'][-1][2] = (a + inc) & 0x3FFF
        else:
            cur['runs'].append([a, 1, (a + inc) & 0x3FFF, inc])
        v = (v + inc) & 0x7FFF
nf = len(frames)
print('%s%s: %d NES frames (%d traced from VE to VE)' % (name, ' wide' if wide else '', f1 - f0, nf))
tot = collections.Counter(); tgt = collections.Counter()
for f in frames: tot.update(f['n']); tgt.update(f['tgt'])
print('per frame: ' + '  '.join('%s %.2f' % (k, c / nf) for k, c in sorted(tot.items())))
print('$2007 bytes per frame by target: ' + '  '.join('%s %.2f' % (k, c / nf) for k, c in sorted(tgt.items())))
mids = collections.Counter(len(f['mid']) for f in frames)
print('frames by mid-frame register writes: %s' % dict(sorted(mids.items())))
print('\nfirst 24 frames: scroll (x, y, nametable) at the end of vblank | $2007 runs (address x length, +inc) | mid-frame writes')
for f in frames[:24]:
    runs = ' '.join('%04X x%d%s' % (r[0], r[1], '' if r[3] == 1 else '+32') for r in f['runs'])
    print('  %s | %s | %s' % (f.get('scroll_vs'), runs or '-', ' '.join(f['mid']) or '-'))
