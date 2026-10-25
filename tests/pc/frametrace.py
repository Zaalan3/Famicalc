#!/usr/bin/env python3
"""Where each frame's time goes, from cycle timestamps (fc-headless "trace").

    python3 frametrace.py SCENE [wide] [seconds] [program.8xp]

SCENE is a bench scene (fctest.py BENCH / BENCH_PLAY). Prints a line per
frame (times in units of 256 CPU cycles, like pacing.asm; a frame is 3120)
and a summary: emulation, set-up before drawing, background, sprites and the
rest, waiting; drawn, aborted and locked frames."""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import fctest  # noqa: E402

UNIT = 256


def main():
    args = sys.argv[1:]
    name = args[0]
    wide = 'wide' in args[1:]
    rest = [a for a in args[1:] if a != 'wide']
    seconds = float(rest[0]) if rest and not rest[0].endswith('.8xp') else 3
    prog = [a for a in rest if a.endswith('.8xp')]
    prog = prog[0] if prog else None
    game, steps, play = ([(g, st, None) for n, g, st in fctest.BENCH if n == name]
                         or [fctest.BENCH_PLAY[name]])[0]
    syms = fctest.symbols(prog)
    calc = fctest.start_famicalc([game], prog)
    fctest.select_game(calc, [game], game)
    F = fctest.Frames(calc, syms)
    fctest.run_steps(F, steps)
    if wide:
        fctest.menu(calc, 'Picture')
        F.wait(60)
    # the instruction after ppu_video_end's call to pace_frame_end
    pfe = syms['pace_frame_end']
    code = calc.peek(syms['ppu_video_end'], 0x100)
    call = bytes([0xCD]) + pfe.to_bytes(3, 'little')
    after_wait = syms['ppu_video_end'] + code.index(call) + 4
    events = {
        'VS': syms['ppu_video_start'], 'VE': syms['ppu_video_end'],
        'DS': syms['pace_draw_start'], 'RF': syms['row_frame_start'],
        'SP': syms['render_sprites_wide' if wide else 'render_sprites'],
        'DE': syms['pace_draw_end'], 'FE': pfe, 'RET': after_wait,
        'LK': syms['spiLock'], 'UL': syms['pace_do_unlock'],
        'NO': syms['row_note_overlays'], 'LJ': syms['load_jit_search'],
    }
    names = {a: n for n, a in events.items()}
    calc.cmd('trace ' + ' '.join('%06X' % a for a in events.values()))
    if play:
        play(calc, syms, seconds)
    else:
        calc.run(int(seconds * 1000))
    path = os.path.join(fctest.OUT, 'trace-%s%s.txt' % (name, '-wide' if wide else ''))
    calc.cmd('trace-dump ' + path)
    calc.close()

    log = []
    for line in open(path):
        c, a = line.split()
        log.append((int(c), names[int(a, 16)]))
    # frames: from one VE to the next
    frames = []
    cur = None
    last_ret = None
    for c, n in log:
        if n == 'VE':
            if cur:
                frames.append(cur)
            cur = {'VE': c, 'emu': (c - last_ret) if last_ret else None, 'SP': [], 'LK': 0, 'UL': 0}
        elif cur is None:
            if n == 'RET':
                last_ret = c
            continue
        elif n == 'SP':
            cur['SP'].append(c)
        elif n in ('LK', 'UL'):
            cur[n] += 1
        elif n == 'RET':
            cur['RET'] = c
            last_ret = c
        else:
            cur.setdefault(n, c)
    tot = {k: 0 for k in ('emu', 'pre', 'bg', 'spr', 'wait')}
    drawn = aborted = skipped = locked = 0
    rows = []
    for f in frames:
        if 'RET' not in f or f['emu'] is None:
            continue
        u = lambda a, b: (f[b] - f[a]) / UNIT
        wait = u('FE', 'RET')
        tot['emu'] += f['emu'] / UNIT
        tot['wait'] += wait
        if 'RF' in f and 'DE' in f:
            drawn += 1
            last_sp = f['SP'][-1] if f['SP'] else f['DE']
            pre, bg, spr = u('VE', 'RF'), (last_sp - f['RF']) / UNIT, (f['DE'] - last_sp) / UNIT
            tot['pre'] += pre
            tot['bg'] += bg
            tot['spr'] += spr
            kind = 'locked' if f['LK'] else 'drawn'
            locked += bool(f['LK'])
            if 'NO' in f:
                sp_only, overl = (f['NO'] - last_sp) / UNIT, (f['DE'] - f['NO']) / UNIT
                tot['sprites'] = tot.get('sprites', 0) + sp_only
                tot['after'] = tot.get('after', 0) + overl
            rows.append('%6.0f emu %5.0f pre %5.0f bg %5.0f spr %5.0f (%4.0f+%4.0f) wait %5.0f %s' % (
                (f['VE'] - frames[0]['VE']) / UNIT, f['emu'] / UNIT, pre, bg, spr, sp_only, overl, wait, kind))
        else:
            if 'DS' in f:
                aborted += 1
                kind = 'ABORTED'
                tot['pre'] += u('VE', 'FE')
            else:
                skipped += 1
                kind = 'skipped'
            rows.append('%6.0f emu %5.0f %39s wait %5.0f %s' % (
                (f['VE'] - frames[0]['VE']) / UNIT, f['emu'] / UNIT, '', wait, kind))
    print('\n'.join(rows[-40:]))
    n = drawn + aborted + skipped
    print('\n%d frames: %d drawn (%d locked), %d aborted, %d skipped' % (n, drawn, locked, aborted, skipped))
    print('per frame (units of 256 cycles; a frame is 3120):')
    for k, v in tot.items():
        d = k in ('pre', 'bg', 'spr', 'sprites', 'after')
        per = v / (drawn if d else n)
        print('  %-7s %6.0f %s' % (k, per, '(per drawn frame)' if d else ''))


if __name__ == '__main__':
    main()
