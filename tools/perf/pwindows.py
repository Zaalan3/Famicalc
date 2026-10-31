# pwindows.py GAME TOTAL_S STEP_S [wide] [wait=N] [png]: a long input-free run cut into windows
# of STEP_S seconds of emulated time. Per window: NES fps, drawn fps and, per NES
# frame, emulation / drawing / idle time in units of 256 cycles (from trace
# timestamps: emu = end of the last frame's wait -> ppu_video_end, draw =
# ppu_video_end -> pace_frame_end (aborted frames included), idle =
# pace_frame_end's wait), pace_draw_start outcomes per NES frame (behind = drawn
# behind the LCD, kept = already locked, lock / wait+lock = locked now, abort),
# late locks, frames skipped by pace_render_frame, and counts of key labels.
# wait=N: NES frames to run after the game is picked before the first window
# (default 60, as the zelda-title scene). png: a screenshot at each window's end.
# PROG=path.8xp for another build. FC_OUT for the screenshots and trace files.
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../tests/pc'))
import fctest
args = sys.argv[1:]
game, total, step = args[0], float(args[1]), float(args[2])
wide = 'wide' in args; png = 'png' in args
wait = int(([a[5:] for a in args if a.startswith('wait=')] or ['60'])[0])
prog = os.environ.get('PROG')
syms = fctest.symbols(prog)
calc = fctest.start_famicalc([game], prog)
fctest.select_game(calc, [game], game)
F = fctest.Frames(calc, syms)
F.wait(wait)
if wide:
    fctest.menu(calc, 'Picture'); F.wait(60)
# unexported places, found in the code
ds = syms['pace_draw_start']
code = calc.peek(ds, 0x100)
lcd_ref = syms['lcd_ref'].to_bytes(3, 'little')
behind = ds + code.index(b'\x2A' + lcd_ref)			# fits, unlocked, panel has the frame: guard set
wait_lp = syms['pace_draw_start.abort'] + code[syms['pace_draw_start.abort'] - ds:].index(b'\x01\x00\x20\x00')
pfe = syms['pace_frame_end']
vcode = calc.peek(syms['ppu_video_end'], 0x100)
after_wait = syms['ppu_video_end'] + vcode.index(b'\xCD' + pfe.to_bytes(3, 'little')) + 4
CNT = ['ppu_video_start', 'pace_draw_end', 'pace_draw_start', 'pace_draw_start.abort', 'pace_draw_start.lock',
       'pace_late_lock', 'pace_row_guard.ahead', 'pace_sprite_guard.sending', 'spiLock', 'pace_do_unlock',
       'jit_convert', 'jit_convert_ram', 'jit_add_block', 'flush_cache', 'update_chr_ram', 'write_ppu_data',
       'write_ppu_address', 'write_ppu_scroll', 'read_ppu_status', 'mapper_write', 'prg_bank_swap', 'chr_bank_swap',
       'jit_scanline', 'jit_scanline_skip', 'row_unchanged', 'row_unchanged.skip', 'row_unchanged.draw',
       'render_sprites_wide' if wide else 'render_sprites', 'spr_restore', 'load_jit_search', 'write_oam_dma']
CNT = [c for c in CNT if c in syms]
addr = {c: syms[c] for c in CNT}
addr['ds.behind'] = behind; addr['ds.wait'] = wait_lp
for a in addr.values(): calc.count(a)
ev = {syms['ppu_video_end']: 'VE', pfe: 'FE', after_wait: 'RET', ds: 'DS', syms['pace_draw_end']: 'DE'}
calc.cmd('trace ' + ' '.join('%06X' % a for a in ev))
tpath = os.path.join(fctest.OUT, 'pwin-%d.txt' % os.getpid())
U = 256.0
hdr = '%-9s %6s %6s %6s %5s %5s %5s | %5s %5s %5s %5s %5s %5s %5s | %5s %6s %6s %6s %6s %6s %6s %6s' % (
    'window', 'frame', 'NES', 'drawn', 'emu', 'draw', 'idle', 'skip', 'bhind', 'kept', 'lock', 'wlock', 'abort', 'late',
    'flush', 'conv', 'convR', 'addbl', 'chr', '2007', '2006', '2002')
print('%s %s%s, from NES frame %d (wait=%d); per NES frame; times in units of 256 cycles (3120 = a frame)' % (
    game, 'wide ' if wide else '', os.path.basename(prog or fctest.PROGRAM), F.now(), wait))
print(hdr)
c0, h0 = calc.counts()
last_ret = None
t = 0.0
rows = []
while t < total - 1e-9:
    calc.run(int(step * 1000))
    c1, h1 = calc.counts()
    calc.cmd('trace-dump ' + tpath)
    log = [(int(l.split()[0]), ev[int(l.split()[1], 16)]) for l in open(tpath)]
    emu = draw = idle = 0; ve = fe = None; nf = 0
    for c, n in log:
        if n == 'VE':
            if last_ret is not None: emu += c - last_ret; nf += 1
            ve = c
        elif n == 'FE' and ve is not None:
            draw += c - ve; fe = c
        elif n == 'RET':
            if fe is not None: idle += c - fe
            last_ret = c; fe = None
    d = {k: h1[a] - h0[a] for k, a in addr.items()}
    fr = max(d['ppu_video_start'], 1); sec = (c1 - c0) / 48e6
    nds = d['pace_draw_start']; kept = nds - d['pace_draw_start.abort'] - d['pace_draw_start.lock'] - d['ds.behind']
    skip = fr - nds
    pf = lambda v: v / fr
    row = '%3.0f-%3.0fs %6d %6.1f %6.1f %5.0f %5.0f %5.0f | %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f | %5.2f %6.1f %6.1f %6.2f %6.2f %6.1f %6.1f %6.1f' % (
        t, t + step, F.now(), d['ppu_video_start'] / sec, d['pace_draw_end'] / sec,
        emu / U / max(nf, 1), draw / U / max(nf, 1), idle / U / max(nf, 1),
        pf(skip), pf(d['ds.behind']), pf(kept), pf(d['pace_draw_start.lock'] - d['ds.wait']), pf(d['ds.wait']),
        pf(d['pace_draw_start.abort']), pf(d['pace_late_lock']),
        pf(d['flush_cache']), pf(d['jit_convert']), pf(d['jit_convert_ram']), pf(d['jit_add_block']),
        pf(d['update_chr_ram']), pf(d['write_ppu_data']), pf(d['write_ppu_address']), pf(d['read_ppu_status']))
    print(row, flush=True)
    rows.append((t, d, fr, sec))
    if png: calc.screenshot('pwin-%s%s-%03d' % (game, '-wide' if wide else '', int(t + step)))
    c0, h0 = c1, h1
    t += step
if os.environ.get('PWIN_ALL'):
    print('\nall counters per NES frame:')
    print('%-28s' % 'label' + ''.join('%7.0f' % (t + step) for t, _, _, _ in rows))
    for k in addr:
        print('%-28s' % k + ''.join('%7.1f' % (d[k] / fr) for _, d, fr, _ in rows))
os.remove(tpath)
calc.close()
