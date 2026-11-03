# tools/perf/pacetrace.py SCENE [wide] [fps] [seconds] [stall=SYM:MIN[:MAX[:ONE_IN]]]   (PROG=path.8xp: another build; fps: the FPS counter on)
# Per-NES-frame pacing events with LCD phases (units of 256 cycles after the LCD's transfer start).
# Written by the pacing planner of workflow run wf_ead75987-57e (2026-10-30); the LCD anchor and
# the displayed-frame count were added by the pacing implementer.
#
# The LCD's transfer starts come from its read address: every `ld hl,(mpLcdUpcurr)` in pacing.asm
# is traced with hl (the read address at that moment), and a read address inside the frame buffer
# gives the transfer's start (the LCD reads it linearly, lcd_transfer_time long; period 3120).
# Phase 0 is the frame's sync (the panel starts a frame), a line (16 units) before the first read.
# A drawn frame is displayed iff the LCD starts a transfer after the drawing ended
# (row_note_overlays, right after render_draw) while the panel is unlocked (pace_do_unlock
# after the last spiLock), before the next frame's drawing starts (row_frame_start).
# torn = a transfer started while the panel was unlocked and a frame was being drawn
# (row_frame_start .. row_note_overlays).
import sys, os
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))  # this tools/perf/ -> repo root
sys.path.insert(0, os.path.join(ROOT, 'tests/pc'))
os.chdir(ROOT)
import fctest
args = sys.argv[1:]; name = args[0]; wide = 'wide' in args
stall = [a[6:] for a in args if a.startswith('stall=')]
rest = [a for a in args[1:] if a not in ('wide', 'fps') and '=' not in a]; secs = float(rest[0]) if rest else 2
prog = os.environ.get('PROG')
game, steps, play = ([(g, st, None) for n, g, st in fctest.BENCH if n == name] or [fctest.BENCH_PLAY[name]])[0]
syms = fctest.symbols(prog)
calc = fctest.start_famicalc([game], prog)
fctest.select_game(calc, [game], game)
F = fctest.Frames(calc, syms)
fctest.run_steps(F, steps)
if wide: fctest.menu(calc, 'Picture'); F.wait(60)
if 'fps' in args: fctest.menu(calc, 'Show FPS'); F.wait(60)
if play: play(calc, syms, 2)
for st in stall:
    # stall=SYM:MIN[:MAX[:ONE_IN]] (units): time passes there (fc-headless stall)
    p = st.split(':'); lo = int(p[1]) * 256; hi = int(p[2]) * 256 if len(p) > 2 else lo
    calc.cmd('stall %06X %d %d %d' % (syms[p[0]], lo, hi, int(p[3]) if len(p) > 3 else 1))
pts = ['ppu_video_start', 'pace_draw_start', 'pace_draw_start.abort', 'pace_draw_start.lock', 'pace_late_lock',
       'pace_sprite_guard.sending', 'pace_do_unlock', 'pace_draw_end', 'pace_frame_end', 'spiLock', 'spiScroll',
       'pace_sprite_guard', 'render_sprites', 'row_frame_start', 'row_note_overlays', 'pace_draw_start.behind']
addr = {syms[p]: p for p in pts if p in syms}
# lcd_sync (not in the map): the second call in pace_resync
b = calc.peek(syms['pace_resync'], 8)
if b[0] == 0xCD and b[4] == 0xCD:
    addr[int.from_bytes(b[5:8], 'little')] = 'lcd_sync'
# every `ld hl,(mpLcdUpcurr)` in the pacing code: the instruction after it sees hl = the read address
lo = syms['pace_init']; code = calc.peek(lo, 0x800); pat = bytes([0x2A, 0x2C, 0x00, 0xE3])
i = code.find(pat)
while i >= 0:
    addr[lo + i + 4] = 'UP'; i = code.find(pat, i + 1)
# every `ld (lcd_ref),hl`: the instruction after it sees hl = lcd_ref (timer units); pace_now's ret
# (after `ld hl,(mpTmr1Counter+1)`) sees hl = the timer: together they give lcd_ref's LCD phase
if 'lcd_ref' in syms:
    pat = bytes([0x22]) + syms['lcd_ref'].to_bytes(3, 'little'); i = code.find(pat)
    while i >= 0:
        addr[lo + i + 4] = 'REF'; i = code.find(pat, i + 1)
i = code.find(bytes([0x2A, 0x01, 0x00, 0xF2, 0xC9]))
if i >= 0: addr[lo + i + 4] = 'NOW'
# pace_frame_end + 17 (jp m,.late): hl = the frame's deadline - now
fe_code = calc.peek(syms['pace_frame_end'], 20)
if fe_code[17] == 0xFA:
    addr[syms['pace_frame_end'] + 17] = 'DUE'
fb = calc.u24(syms['game_fb']); fe = calc.u24(syms['lcd_frame_end'])
# read addresses sampled before tracing (scenes that never read it during a transfer)
peeks = []
for _ in range(40):
    peeks.append((calc.counts()[0], 'UP', calc.u24(0xE3002C))); calc.run(1)
calc.cmd('trace ' + ' '.join('%06X' % a for a in addr))
f0 = F.now()
if play: play(calc, syms, secs)
else: calc.run(int(secs * 1000))
f1 = F.now()
path = os.path.join(fctest.OUT, 'pacetrace-%d.txt' % os.getpid()); calc.cmd('trace-dump ' + path); calc.close()
ev = []
for l in open(path):
    t, a, hl = l.split()[:3]
    ev.append((int(t), addr[int(a, 16)], int(hl, 16)))
os.remove(path)
P = 798720; xfer = (1138 if wide else 910) * 256
# anchor: transfer starts from read addresses inside the frame buffer (circular median)
st = [t - (h - fb) * xfer // (fe - fb) for t, n, h in ev + peeks if n == 'UP' and fb <= h < fe]
if st:
    ref = st[0] % P
    rel = sorted((s - ref + P // 2) % P - P // 2 for s in st)
    # (the frame's sync, when the panel starts a frame and the read address leaves the end, is a line
    # of 1040 pixels before the first line is read)
    tick = (ref + rel[len(rel) // 2] - 1040 * 4) % P
    print('LCD anchor from %d read addresses: spread %d..%d units around the median' % (len(rel), (rel[0] - rel[len(rel)//2]) // 256, (rel[-1] - rel[len(rel)//2]) // 256))
else:
    tick = 0; print('no read addresses inside the frame buffer: phases are not LCD phases')
def ph(t): return ((t - tick) % P) // 256
def next_start(t): return t + (tick - t) % P	# first transfer start at or after t
print('%d NES frames traced; phases are units after the LCD transfer start (transfer lasts %d, period 3120)' % (f1 - f0, xfer//256))
frames = []; cur = None
for t, n, h in ev:
    if n == 'ppu_video_start':
        if cur: frames.append(cur)
        cur = {'start': t, 'ev': []}
        continue
    if cur is None: continue
    if n == 'DUE':
        cur['due'] = h - (1 << 24) if h & 0x800000 else h
        continue
    if n in ('REF', 'NOW'): continue
    cur['ev'].append((t, n))
if cur: frames.append(cur)
# lock state over time
locks = [(t, 1 if n == 'spiLock' else 0) for t, n, h in ev if n in ('spiLock', 'pace_do_unlock')]
def locked_at(t):
    s = 1 - locks[0][1] if locks else 0	# before the first lock/unlock: the opposite state
    for u, v in locks:
        if u > t: break
        s = v
    return s
draws = []	# (row_frame_start, row_note_overlays, frame index)
stats = {}
lines = []
for i, f in enumerate(frames):
    e = {}
    for t, n in f['ev']:
        if n in ('pace_draw_start', 'pace_draw_end', 'pace_frame_end', 'pace_sprite_guard', 'render_sprites', 'row_frame_start', 'row_note_overlays'):
            e.setdefault(n, t)
    tags = []
    for t, n in f['ev']:
        if n == 'pace_sprite_guard.sending' and not ('pace_sprite_guard' in e and t >= e['pace_sprite_guard']): continue
        short = {'pace_draw_start.abort': 'ABORT', 'pace_draw_start.lock': 'LOCK', 'pace_late_lock': 'LATE', 'pace_do_unlock': 'unlock',
                 'pace_sprite_guard.sending': 'sprwait', 'spiLock': 'spiLock', 'spiScroll': 'spiScroll', 'lcd_sync': 'sync',
                 'pace_draw_start.behind': 'BEHIND'}.get(n)
        if short:
            tags.append('%s@%d' % (short, ph(t))); stats[short] = stats.get(short, 0) + 1
    s = 'f%-4d start@%4d' % (i, ph(f['start']))
    if 'pace_draw_start' in e:
        s += ' draw@%4d' % ph(e['pace_draw_start'])
        if 'row_frame_start' in e and 'pace_draw_end' in e:
            w = (e['row_frame_start'] - e['pace_draw_start']) // 256
            s += ' (+%4d)' % w if w > 8 else '        '
            s += ' took %4d' % ((e['pace_draw_end'] - e['row_frame_start']) // 256)
            draws.append((e['row_frame_start'], e.get('row_note_overlays', e['pace_draw_end']), i))
            if 'pace_sprite_guard' in e and 'render_sprites' in e:
                s += ' (bg %4d sprwait %3d spr %4d)' % ((e['pace_sprite_guard'] - e['row_frame_start'])//256, (e['render_sprites'] - e['pace_sprite_guard'])//256, (e['pace_draw_end'] - e['render_sprites'])//256)
        else:
            s += '         took   --'; stats['took--'] = stats.get('took--', 0) + 1
    else:
        s += ' (no draw)          '
    if 'pace_frame_end' in e:
        s += ' end@%4d' % ph(e['pace_frame_end'])
        if i + 1 < len(frames):
            s += ' wait %4d' % ((frames[i+1]['start'] - e['pace_frame_end']) // 256)
        if 'due' in f:
            s += ' due %+5d' % f['due']
    s += '  ' + ' '.join(tags)
    lines.append(s)
# displayed / torn
shown = {}; torn = []
for k, (s0, e0, i) in enumerate(draws):
    if k + 1 >= len(draws): break		# the last one: unknown yet
    nxt = draws[k + 1][0]
    t = next_start(e0); ok = False
    while t <= nxt:
        if locked_at(t) == 0:
            ok = True; break
        t += P
    shown[i] = ok
    t = next_start(s0)
    while t < e0:
        if locked_at(t) == 0: torn.append(i)
        t += P
for k, s in enumerate(lines):
    i = int(s[1:5])
    if i in shown and not shown[i]: lines[k] = s + '  NOT-SHOWN'
    if i in torn: lines[k] += '  TORN'
print('\n'.join(lines))
n = len(frames)
print('events per NES frame:', ' '.join('%s %.3f' % (k, v / n) for k, v in sorted(stats.items())))
now = [(t, h) for t, n, h in ev if n == 'NOW']
refs = [(t, h) for t, n, h in ev if n == 'REF']
if now and refs:
    off = sorted(t - h * 256 for t, h in now)[len(now) // 2]	# cycle time of timer unit 0 (the counter's low byte aside)
    rp = sorted(((h * 256 + off - tick + P // 2) % P - P // 2) // 256 for t, h in refs)
    print('lcd_ref (units after the sync, i.e. the anchor): median %d, range %d..%d over %d writes' % (rp[len(rp) // 2], rp[0], rp[-1], len(rp)))
print('drawn %d, displayed %d of the %d checked, torn %d, NES frames %d' % (len(draws), sum(shown.values()), len(shown), len(torn), n))
