# bgtrace.py SCENE [wide] [seconds]: time per drawn frame between traced points, units of 256 cycles
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../tests/pc'))
import fctest
args = sys.argv[1:]; name = args[0]; wide = 'wide' in args
rest = [a for a in args[1:] if a != 'wide']; secs = float(rest[0]) if rest else 2
prog = os.environ.get('PROG')
game, steps, play = ([(g, st, None) for n, g, st in fctest.BENCH if n == name] or [fctest.BENCH_PLAY[name]])[0]
syms = fctest.symbols(prog)
calc = fctest.start_famicalc([game], prog)
fctest.select_game(calc, [game], game)
F = fctest.Frames(calc, syms)
fctest.run_steps(F, steps)
if wide: fctest.menu(calc, 'Picture'); F.wait(60)
if play: play(calc, syms, 2)
pts = ['ppu_video_end', 'render_background', 'bank_select', 'scroll_pick', 'pace_draw_start', 'row_frame_start', 'spr_restore',
       'row_unchanged', 'run_tiles.compare', 'render_background.nextevent', 'render_background_loop.nodraw',
       'render_sprites', 'row_note_overlays', 'pace_draw_end', 'pace_frame_end', 'rs_fixup', 'row_partial', 'fetch_tile',
       'attribute_update', 'render_draw', 'sprite_save', 'high_priority_sprite', 'low_priority_sprite'] + os.environ.get('PTS', '').split()
pts = [p for p in pts if p in syms]
addr = {syms[p]: p for p in pts}
addr[0xE30BC0] = 'draw_tile'
calc.cmd('trace ' + ' '.join('%06X' % a for a in addr))
f0 = F.now()
if play: play(calc, syms, secs)
else: calc.run(int(secs * 1000))
f1 = F.now()
path = os.path.join(fctest.OUT, 'bgtrace-%d.txt' % os.getpid()); calc.cmd('trace-dump ' + path); calc.close()
ev = [(int(l.split()[0]), addr[int(l.split()[1], 16)]) for l in open(path)]
tot = {}; cnt = {}; drawn = 0; inbg = False
for (c, n), (c2, _) in zip(ev, ev[1:]):
    if n == 'render_draw': inbg = True
    if n == 'pace_draw_end': drawn += 1
    if n == 'pace_frame_end': inbg = False
    if not inbg: continue
    tot[n] = tot.get(n, 0) + (c2 - c); cnt[n] = cnt.get(n, 0) + 1
print('%d NES frames, %d drawn' % (f1 - f0, drawn))
s = 0
for n, t in sorted(tot.items(), key=lambda x: -x[1]):
    s += t
    print('%7.0f units/drawn  %6.1f calls/drawn  %s' % (t / 256 / drawn, cnt[n] / drawn, n))
print('sum %.0f' % (s / 256 / drawn))
