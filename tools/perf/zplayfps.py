# zplayfps.py PROG [wide]: Zelda overworld from the 0.2.1 state (slot 0): NES fps / drawn fps standing still and walking
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../tests/pc'))
import fctest
prog = sys.argv[1]; wide = 'wide' in sys.argv
syms = fctest.symbols(prog)
calc = fctest.start_famicalc(['ZELDA'], prog)
fctest.select_game(calc, ['ZELDA'], 'ZELDA')
F = fctest.Frames(calc, syms)
F.wait(120)
if wide: fctest.menu(calc, 'Picture'); F.wait(60)
calc.key('ln', ms=100, after=100)
F.wait(240)
de = syms['pace_draw_end']; calc.count(de)
def measure(label, act):
    c0, h0 = calc.counts(); act(); c1, h1 = calc.counts()
    s = (c1 - c0) / 48e6
    print('%-6s %s %5.1f s  NES %5.1f  drawn %5.1f' % (label, 'wide' if wide else 'normal', s, (h1[F.addr] - h0[F.addr]) / s, (h1[de] - h0[de]) / s))
measure('still', lambda: calc.run(4000))
def walk():
    for b in ['right', 'up', 'left', 'down', 'right', 'up']: F.hold(b, 60)
measure('walk', walk)
calc.close()
