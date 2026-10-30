import sys, os, re
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../tests/pc'))
import fctest
name = sys.argv[1]; wide = 'wide' in sys.argv[2:]
prog = os.environ.get('PROG')
game, steps, play = ([(g, st, None) for n, g, st in fctest.BENCH if n == name] or [fctest.BENCH_PLAY[name]])[0]
syms = fctest.symbols(prog)
calc = fctest.start_famicalc([game], prog)
fctest.select_game(calc, [game], game)
F = fctest.Frames(calc, syms)
fctest.run_steps(F, steps)
if wide: fctest.menu(calc, 'Picture'); F.wait(60)
if play: play(calc, syms, 2)
else: calc.run(1000)
calc.count(syms['pace_draw_end'])
c0, h0 = calc.counts()
res = fctest.profile(calc, syms, ms=3000, wide=wide, play=(lambda c, s, sec: play(c, s, 3)) if play else None)
c1, h1 = calc.counts()
nes = h1[F.addr] - h0[F.addr]; drawn = h1[syms['pace_draw_end']] - h0[syms['pace_draw_end']]
total = (c1 - c0) / 256
print('%d NES frames, %d drawn; units per NES frame %.0f' % (nes, drawn, total / nes))
groups = {}
for share, place in res:
    key = place if (place.startswith('[') or os.environ.get('FULL')) else place.split('.')[0]
    groups[key] = groups.get(key, 0) + share * total
for k, u in sorted(groups.items(), key=lambda x: -x[1]):
    if u / drawn >= 4: print('%7.0f per drawn  %6.0f per NES  %s' % (u / drawn, u / nes, k))
calc.close()
