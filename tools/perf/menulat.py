"""tools/perf/menulat.py [PROGRAM.8xp] [scene...] [wide] [states]: ms from
[clear] going down to the pause menu on the panel (emulated time; see
fctest.menu_latency). Scenes: zelda smb3 tetris ff1 (default all). states: save
3 states first (the menu reads the slots when it opens). Prints the first
call of each traced function too."""
import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../tests/pc'))
import fctest

SCENES = {
    'zelda': ('ZELDA', [('wait', 990)]),
    'smb3': ('MARIO3', [('wait', 240), ('tap', 'start'), ('wait', 120), ('tap', 'start'), ('wait', 240),
                        ('tap', 'right'), ('wait', 60), ('tap', 'up'), ('wait', 60), ('tap', 'A'), ('wait', 240)]),
    'tetris': ('TETRIS', [('wait', 200)] + [('tap', 'start'), ('wait', 60)] * 5),
    'ff1': ('FINALF', [('wait', 300)]),
}
args = sys.argv[1:]
prog = next((a for a in args if a.endswith('.8xp')), None)
wide = 'wide' in args
states = 'states' in args
names = [a for a in args if a in SCENES] or list(SCENES)
syms = fctest.symbols(prog)
os.makedirs(fctest.OUT, exist_ok=True)
for name in names:
    game, steps = SCENES[name]
    calc = fctest.start_famicalc([game], prog, log=os.path.join(fctest.OUT, 'lat-%s.log' % name))
    fctest.select_game(calc, [game], game)
    F = fctest.Frames(calc, syms)
    fctest.run_steps(F, steps)
    if wide:
        fctest.menu(calc, 'Picture')
        F.wait(60)
    if states:
        for slot in (1, 2, 3):
            fctest.menu(calc, 'Save state', slot)
            F.wait(30)
    F.wait(30)
    tag = '%s%s%s' % (name, '-wide' if wide else '', '-states' if states else '')
    start, whole, first = fctest.menu_latency(calc, syms, tag)
    calc.close()
    print('%-18s first menu pixels %6.1f ms   whole menu %6.1f ms' % (tag, start or -1, whole or -1))
    for n, ms in sorted(first.items(), key=lambda x: x[1]):
        print('      %7.1f ms  %s' % (ms, n))
    sys.stdout.flush()
