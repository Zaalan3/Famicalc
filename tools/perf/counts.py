# counts.py SCENE [wide] sym... : execution counts per NES frame during a bench scene
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../tests/pc'))
import fctest
name = sys.argv[1]; wide = 'wide' in sys.argv; names = [a for a in sys.argv[2:] if a != 'wide']
prog = os.environ.get('PROG')
game, steps, play = ([(g, st, None) for n, g, st in fctest.BENCH if n == name] or [fctest.BENCH_PLAY[name]])[0]
syms = fctest.symbols(prog)
calc = fctest.start_famicalc([game], prog)
fctest.select_game(calc, [game], game)
F = fctest.Frames(calc, syms)
fctest.run_steps(F, steps)
if wide: fctest.menu(calc, 'Picture'); F.wait(60)
addrs = {}
for n in names:
    a = int(n, 16) if n.startswith('0x') else syms[n]
    addrs[n] = a; calc.count(a)
if play: play(calc, syms, 1)
c0, h0 = calc.counts()
if play: play(calc, syms, 2)
else: calc.run(2000)
c1, h1 = calc.counts()
fr = h1[F.addr] - h0[F.addr]
print('%d NES frames, %.0f cycles/frame' % (fr, (c1 - c0) / fr))
for n, a in addrs.items():
    print('%-30s %9.1f per frame' % (n, (h1[a] - h0[a]) / fr))
calc.close()
