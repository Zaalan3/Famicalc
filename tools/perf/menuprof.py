"""tools/perf/menuprof.py [PROGRAM.8xp] [wide] [MS]: where the time goes from
[clear] going down to MS ms later (default 130), program counter samples
every 10 us, labelled with the map (static C functions count under the public
symbol before them)."""
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../tests/pc'))
import fctest
args = sys.argv[1:]
prog = next((a for a in args if a.endswith('.8xp')), None)
wide = 'wide' in args
ms = int(next((a for a in args if a.isdigit()), 130))
syms = fctest.symbols(prog)
os.makedirs(fctest.OUT, exist_ok=True)
calc = fctest.start_famicalc(['TETRIS'], prog)
fctest.select_game(calc, ['TETRIS'], 'TETRIS')
F = fctest.Frames(calc, syms)
fctest.run_steps(F, [('wait', 200)] + [('tap', 'start'), ('wait', 60)] * 5)
if wide:
    fctest.menu(calc, 'Picture')
    F.wait(60)
F.wait(30)
def play(c, s, sec):
    c.press('clear')
    c.run(ms)
    c.release('clear')
res = fctest.profile(calc, syms, ms=ms, us=10, wide=wide, play=play)
for share, place in res[:40]:
    print('%6.1f ms  %s' % (share * ms, place))
calc.close()
