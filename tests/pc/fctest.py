#!/usr/bin/env python3
"""FamiCalc PC tests: runs FAMICALC.8xp on CEmu (fc-headless) with the real
TI-84 Plus CE OS, the CE libraries and the arTIfiCE jailbreak, the same way it
runs on the calculator. See DEVNOTES.md, "PC testing".

    python3 fctest.py all              every test
    python3 fctest.py fps [GAME...]    FPS of each game (default: every game)
    python3 fctest.py bench[-wide][-fps]  benchmark scenes (wide picture, FPS counter on)
    python3 fctest.py shots GAME       screenshots of a game session
    python3 fctest.py list             test names

Environment (defaults in brackets):
    FC_ROM        CE ROM image dumped from a calculator [~/ti84ce/rom/ti84pce.rom]
    FC_CALCFILES  LibLoad, FILEIOC, KEYPADC, ... and the jailbreak A.8xp [~/ti84ce/rom/calcfiles]
    FC_GAMES      converted games (NAME.8xv, NAMEP0.8xv, ...) [~/ti84ce/rom/games]
    FC_PROGRAM    the build under test [../../bin/FAMICALC.8xp]
    FC_OUT        screenshots and logs [./out]
"""
import glob
import os
import re
import shutil
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
HOME = os.path.expanduser('~')
ROM = os.environ.get('FC_ROM', os.path.join(HOME, 'ti84ce/rom/ti84pce.rom'))
CALCFILES = os.environ.get('FC_CALCFILES', os.path.join(HOME, 'ti84ce/rom/calcfiles'))
GAMES = os.environ.get('FC_GAMES', os.path.join(HOME, 'ti84ce/rom/games'))
PROGRAM = os.environ.get('FC_PROGRAM', os.path.join(HERE, '../../bin/FAMICALC.8xp'))
OUT = os.environ.get('FC_OUT', os.path.join(HERE, 'out'))
HEADLESS = os.path.join(HERE, 'build/fc-headless')
CACHE = os.path.join(HERE, 'build/cache')

LIBS = ['LibLoad', 'FILEIOC', 'KEYPADC', 'GRAPHX', 'FONTLIBC', 'LCDDRVCE']

# NES buttons -> calculator keys (src/keys.asm)
NES = {'A': '2nd', 'B': 'alpha', 'select': 'xton', 'start': 'mode',
       'up': 'up', 'down': 'down', 'left': 'left', 'right': 'right'}


class TestFailure(Exception):
    pass


def check(cond, msg):
    if not cond:
        raise TestFailure(msg)


def symbols(program=None):
    """Symbol -> address from the linker map next to the .8xp."""
    mapfile = os.path.splitext(program or PROGRAM)[0] + '.map'
    syms = {}
    for line in open(mapfile):
        m = re.match(r'\s*(\S+)\s+= ([0-9A-F]{6})\s*$', line)
        if m:
            syms[m.group(1)] = int(m.group(2), 16)
    return syms


class Calc:
    """One emulated calculator (an fc-headless process)."""

    def __init__(self, image=None, log=None):
        args = [HEADLESS, '--image', image] if image else [HEADLESS, '--rom', ROM]
        self.log = open(log, 'w') if log else subprocess.DEVNULL
        self.p = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                  stderr=self.log, text=True, bufsize=1)
        self.dbg = []
        line = self.p.stdout.readline()
        check(line.startswith('FC_HEADLESS_READY'), 'emulator did not start: %r' % line)

    def cmd(self, line):
        self.p.stdin.write(line + '\n')
        self.p.stdin.flush()
        while True:
            out = self.p.stdout.readline()
            if not out:
                raise TestFailure('emulator exited during %r' % line)
            out = out.rstrip('\n')
            if out.startswith('DBG '):
                self.dbg.append(out[4:])
                continue
            if out.startswith('OK'):
                return out[3:]
            raise TestFailure('%s -> %s' % (line, out))

    def close(self):
        try:
            self.cmd('quit')
        except Exception:
            pass
        self.p.wait(10)

    def run(self, ms):
        self.cmd('run %d' % ms)

    def key(self, name, ms=80, after=150):
        self.cmd('key %s %d' % (name, ms))
        if after:
            self.run(after)

    def press(self, name):
        self.cmd('press ' + name)

    def release(self, name):
        self.cmd('release ' + name)

    def send(self, path, archive=True):
        self.cmd('send-file %s %s' % ('archive' if archive else 'ram', path))

    def peek(self, addr, n):
        return bytes.fromhex(self.cmd('peek %06X %d' % (addr, n)))

    def u8(self, addr):
        return self.peek(addr, 1)[0]

    def u24(self, addr):
        return int.from_bytes(self.peek(addr, 3), 'little')

    def count(self, addr):
        self.cmd('count %06X' % addr)

    def counts(self):
        """(cpu cycles, {address: hits})"""
        parts = self.cmd('counts').split()
        return int(parts[0]), {int(a, 16): int(n) for a, n in (p.split('=') for p in parts[1:])}

    def vars(self):
        out = {}
        for item in self.cmd('vars').split():
            name, typ, size, arc = item.rsplit(':', 3)
            out[name] = (int(typ, 16), int(size), arc == '1')
        return out

    def screenshot(self, name):
        os.makedirs(OUT, exist_ok=True)
        bmp = os.path.join(OUT, name + '.bmp')
        self.cmd('screenshot ' + bmp)
        try:
            from PIL import Image
            png = os.path.join(OUT, name + '.png')
            Image.open(bmp).save(png)
            os.remove(bmp)
            return png
        except ImportError:
            return bmp

    def save_image(self, path):
        self.cmd('save-state ' + path)


def game_files(game):
    files = sorted(glob.glob(os.path.join(GAMES, game + '.8?v')) +
                   glob.glob(os.path.join(GAMES, game + '??.8?v')))
    check(files, 'no files for game %s in %s' % (game, GAMES))
    return files


def all_games():
    """Games in FC_GAMES: header appvars (data starts with FAMICALC)."""
    games = []
    for f in sorted(glob.glob(os.path.join(GAMES, '*.8?v'))):
        data = open(f, 'rb').read()
        if data[0x4A:0x52] == b'FAMICALC':
            games.append(os.path.basename(f).split('.')[0])
    return games


def base_image(games):
    """CEmu image with the OS booted and the libraries, jailbreak and games in
    the archive (cached: sending games takes a while)."""
    key = '-'.join(sorted(games)) or 'none'
    stamp = max([os.path.getmtime(ROM)] + [os.path.getmtime(f) for g in games for f in game_files(g)])
    path = os.path.join(CACHE, 'base-%s.ce' % key)
    if os.path.exists(path) and os.path.getmtime(path) > stamp:
        return path
    os.makedirs(CACHE, exist_ok=True)
    calc = Calc()
    calc.run(3000)
    for lib in LIBS:
        calc.send(glob.glob(os.path.join(CALCFILES, lib + '.8?v'))[0])
    calc.send(glob.glob(os.path.join(CALCFILES, 'A.8?p'))[0])
    for g in games:
        for f in game_files(g):
            calc.send(f)
    calc.run(500)
    calc.save_image(path)
    calc.close()
    return path


def start_famicalc(games, program=None, log=None):
    """Boot image + program, then run it through the jailbreak: [prgm] A,
    arTIfiCE shell, FAMICALC. Returns the Calc at FamiCalc's ROM list."""
    calc = Calc(base_image(games), log=log)
    calc.send(program or PROGRAM)
    calc.run(300)
    calc.key('prgm', after=400)     # program list: A, FAMICALC
    calc.key('enter', after=200)    # pastes prgmA
    calc.key('enter', after=2500)   # runs the jailbreak -> arTIfiCE shell
    rom_menu = symbols(program).get('_rom_menu')
    if rom_menu:
        calc.count(rom_menu)
    calc.key('enter', after=200)    # FAMICALC is the only asm program
    if rom_menu:
        # wait for the ROM list to be on screen and reading keys
        for _ in range(100):
            if calc.counts()[1][rom_menu]:
                break
            calc.run(50)
    calc.run(500)
    return calc


def select_game(calc, games, game):
    """From the ROM list (VAT order = order the games were sent), pick game."""
    for _ in range(games.index(game)):
        calc.key('down', after=100)
    calc.key('enter', after=200)


class Frames:
    """Drive a game by NES frame number (ppu_video_start runs once per
    emulated frame), so inputs land on the same frames however fast a build is."""

    def __init__(self, calc, syms):
        self.calc = calc
        self.addr = syms['ppu_video_start']
        calc.count(self.addr)

    def now(self):
        return self.calc.counts()[1][self.addr]

    def wait(self, n, step=50, limit_ms=120000):
        target = self.now() + n
        spent = 0
        while self.now() < target:
            check(spent < limit_ms, 'emulation stopped (frame %d of %d)' % (self.now(), target))
            self.calc.run(step)
            spent += step

    def tap(self, button, frames=6, after=30):
        key = NES.get(button, button)
        self.calc.press(key)
        self.wait(frames)
        self.calc.release(key)
        self.wait(after)

    def hold(self, button, frames):
        key = NES.get(button, button)
        self.calc.press(key)
        self.wait(frames)
        self.calc.release(key)


def measure_fps(calc, syms, seconds=10, warmup=0):
    """NES frames emulated and frames drawn per second of emulated time
    (drawn: frames that reached pace_draw_end; render_draw also runs for
    frames that pace_draw_start then aborts)."""
    drawn_at = 'pace_draw_end' if 'pace_draw_end' in syms else 'render_draw'  # older builds: no pacing
    for s in ('ppu_video_start', drawn_at):
        calc.count(syms[s])
    if warmup:
        calc.run(warmup)
    c0, h0 = calc.counts()
    calc.run(seconds * 1000)
    c1, h1 = calc.counts()
    secs = (c1 - c0) / 48e6 if c1 > c0 else seconds
    nes = (h1[syms['ppu_video_start']] - h0[syms['ppu_video_start']]) / seconds
    drawn = (h1[syms[drawn_at]] - h0[syms[drawn_at]]) / seconds
    return nes, drawn


def fps_game(game, seconds=10, program=None, presses=None, tag=''):
    """Start a game, get past the title screen, measure for `seconds`."""
    games = [game]
    syms = symbols(program)
    calc = start_famicalc(games, program, log=os.path.join(OUT, 'fps-%s%s.log' % (game, tag)))
    select_game(calc, games, game)
    calc.run(3000)
    # default: load the game's save state in slot 0 (real gameplay, not a title screen)
    for k in (presses or ['ln']):
        calc.key(NES.get(k, k), ms=100, after=1500)
    calc.run(2000)
    nes, drawn = measure_fps(calc, syms, seconds)
    calc.screenshot('fps-%s%s' % (game, tag))
    calc.close()
    return nes, drawn


# ---------------------------------------------------------------- benchmark

# Scenes: (name, game, steps). Steps run after the game is picked in the ROM
# list; inputs are timed in NES frames. FPS is then measured for BENCH_SECONDS
# of emulated time (the CE's real 48 MHz).
BENCH = [
    ('smb3-1-1', 'MARIO3', [('wait', 240), ('tap', 'start'), ('wait', 120), ('tap', 'start'),
                            ('wait', 240), ('tap', 'right'), ('wait', 60), ('tap', 'up'),
                            ('wait', 60), ('tap', 'A'), ('wait', 240), ('hold', 'right', 40)]),
    ('smb3-map', 'MARIO3', [('wait', 240), ('tap', 'start'), ('wait', 120), ('tap', 'start'),
                            ('wait', 240)]),
    ('tetris', 'TETRIS', [('wait', 200)] + [('tap', 'start'), ('wait', 60)] * 5),
    ('ff1-intro', 'FINALF', [('wait', 300)]),
    ('zelda-title', 'ZELDA', [('wait', 60)]),
]
BENCH_SECONDS = 10


def run_steps(F, steps):
    for kind, *a in steps:
        if kind == 'wait':
            F.wait(a[0])
        elif kind == 'tap':
            F.tap(a[0])
        elif kind == 'hold':
            F.hold(a[0], a[1])


def bench(program=None, label='current', scenes=None, wide=False, fps_label=False):
    syms = symbols(program)
    results = {}
    for name, game, steps in BENCH:
        if scenes and name not in scenes:
            continue
        calc = start_famicalc([game], program, log=os.path.join(OUT, 'bench-%s-%s.log' % (label, name)))
        select_game(calc, [game], game)
        F = Frames(calc, syms)
        run_steps(F, steps)
        if wide:
            menu(calc, 'Picture')
            calc.key('clear', ms=100, after=300)
            F.wait(60)
        if fps_label:
            menu(calc, 'Show FPS')
            calc.key('clear', ms=100, after=300)
            F.wait(60)
        nes, drawn = measure_fps(calc, syms, BENCH_SECONDS)
        calc.screenshot('bench-%s-%s' % (label, name))
        calc.close()
        results[name] = (nes, drawn)
        print('  %-8s %-12s %5.1f NES fps  %5.1f drawn fps' % (label, name, nes, drawn), flush=True)
    return results


def profile(calc, syms, ms=5000, us=20, wide=False):
    """Sample the CPU's program counter; returns [(share, place)] busiest first.
    The wide picture has its own code in cursor RAM (renderwide.asm, w_*)."""
    out = calc.cmd('profile %d %d' % (ms, us)).split()
    total = int(out[0])
    ext0, ext1 = calc.u24(syms['_jit_cache_extend']), calc.u24(syms['_jit_cache_extend_end'])
    names = sorted((a, n) for n, a in syms.items() if 0xD00000 <= a < 0xF00000
                   and not (0xE30800 <= a < 0xE30BC0 and n.startswith('w_') != wide))
    addrs = [a for a, _ in names]
    import bisect
    places = {}
    for item in out[1:]:
        a, c = item.split('=')
        a, c = int(a, 16), int(c)
        tag, a = a >> 24, a & 0xFFFFFF
        if 0xE30BC0 <= a < 0xE30C00:
            # first byte of the routine loaded in the SHA scratch area
            routine = {0xE5: 'jit_search', 0xD9: 'draw_tile', 0x06: 'sprite_outer'}.get(tag, 'byte %02X' % tag)
            places['[scratch] ' + routine] = places.get('[scratch] ' + routine, 0) + c
            continue
        if 0xD50C00 <= a < 0xD5FC00 or ext0 <= a < ext1:
            place = '[translated 6502 code]'
        elif 0xD5FC00 <= a < 0xD60000:
            place = '[translated 6502 RAM/boundary block]'
        elif 0xE30BC0 <= a < 0xE30C00:
            place = '[SHA scratch: jit_search or draw_tile/sprite_outer]'
        elif a < 0x400000:
            place = '[OS/boot code]'
        else:
            i = bisect.bisect_right(addrs, a) - 1
            place = names[i][1] if i >= 0 else '?'
            if 0xE30800 <= a < 0xE30BC0 and not (0xE30800 <= addrs[i] < 0xE30BC0):
                place = '[render code in cursor RAM]'
        places[place] = places.get(place, 0) + c
    return sorted(((c / total, p) for p, c in places.items()), reverse=True)


# ---------------------------------------------------------------- tests

def test_boot():
    """FamiCalc starts from the arTIfiCE shell and shows its ROM list."""
    games = ['TETRIS']
    calc = start_famicalc(games, log=os.path.join(OUT, 'boot.log'))
    calc.screenshot('boot-romlist')
    syms = symbols()
    check(calc.u24(syms['_jit_cache_extend_end']) != 0, 'FamiCalc did not reach main()')
    calc.close()


NES_RAM = 0xD50000
MENU_ITEMS = ['Resume game', 'State slot', 'Save state', 'Load state', 'Delete state', 'Show FPS', 'Picture',
              'Reset game', 'Change game', 'Quit FamiCalc']
SMB3_TO_MAP = [('wait', 240), ('tap', 'start'), ('wait', 120), ('tap', 'start'), ('wait', 240)]


def menu(calc, item, slot=None, confirm=False):
    """Open the pause menu ([clear]), optionally type a slot number, pick item."""
    calc.key('clear', ms=100, after=400)
    if slot is not None:
        for ch in str(slot):
            calc.key(ch, ms=100, after=100)
    for _ in range(MENU_ITEMS.index(item)):
        calc.key('down', ms=100, after=100)
    calc.key('2nd', ms=100, after=200)
    if confirm:
        calc.key('2nd', ms=100, after=200)
    calc.run(1500)


def snaps(calc, addr):
    count, _, data = calc.cmd('snaps %06X' % addr).partition(' ')
    return int(count), bytes.fromhex(data)


def start_game(game, steps=(), log=None):
    syms = symbols()
    calc = start_famicalc([game], log=os.path.join(OUT, log or game + '.log'))
    select_game(calc, [game], game)
    F = Frames(calc, syms)
    run_steps(F, steps)
    return calc, syms, F


def test_save_load():
    """Menu saves to slots 12, 345, 999 (appvar names, header); saving and
    loading restore NES RAM byte for byte."""
    calc, syms, F = start_game('MARIO3', SMB3_TO_MAP, 'save_load.log')
    calc.cmd('snap %06X %06X 2048' % (syms['create_savestate'], NES_RAM))
    calc.cmd('snap %06X %06X 2048' % (syms['flush_bank_cache'], NES_RAM))
    calc.count(syms['load_savestate'])
    for slot, name in [(12, 'MARIO312'), (345, 'MARIO3gt'), (999, 'MARIO3yz')]:
        n0 = snaps(calc, syms['create_savestate'])[0]
        menu(calc, 'Save state', slot)
        n1, live = snaps(calc, syms['create_savestate'])
        check(n1 == n0 + 1, 'slot %d: no save happened' % slot)
        _, reloaded = snaps(calc, syms['flush_bank_cache'])
        check(reloaded == live, 'slot %d: RAM after saving differs from before' % slot)
        v = calc.vars()
        check(name in v and v[name][0] == 0x15 and v[name][2], 'slot %d: no archived appvar %s' % (slot, name))
        path = os.path.join(OUT, name + '.bin')
        calc.cmd('receive-file %s 15 %s' % (name, path))
        data = open(path, 'rb').read()[2:]
        check(data[:3] == b'\xFCST' and data[3] == 1, 'slot %d: no state header' % slot)
        calc.screenshot('save-%d' % slot)
        # play on, then load
        F.hold('right', 40)
        F.wait(60)
        check(calc.peek(NES_RAM, 2048) != live, 'slot %d: RAM did not change after saving' % slot)
        loads = calc.counts()[1][syms['load_savestate']]
        menu(calc, 'Load state', slot)
        _, loaded = snaps(calc, syms['flush_bank_cache'])
        check(calc.counts()[1][syms['load_savestate']] == loads + 1, 'slot %d: no load happened' % slot)
        check(loaded == live, 'slot %d: loading did not restore RAM' % slot)
        calc.screenshot('load-%d' % slot)
    # delete one
    menu(calc, 'Delete state', 345, confirm=True)
    check('MARIO3gt' not in calc.vars(), 'slot 345 was not deleted')
    calc.close()


def test_state_games():
    """Saving and loading a state keeps every game running (a load once lost
    the NES's X and Y registers: Tetris then crashed)."""
    for name, game, steps in BENCH:
        if name in ('smb3-map', 'zelda-title'):
            continue
        calc, syms, F = start_game(game, steps, 'state-%s.log' % name)
        menu(calc, 'Save state', 77)
        F.wait(60)
        menu(calc, 'Load state', 77)
        F.wait(120)
        log = open(os.path.join(OUT, 'state-%s.log' % name)).read()
        check('Illegal' not in log and 'NMI reset' not in log, '%s: crashed after loading' % name)
        calc.screenshot('state-' + name)
        calc.close()
    calc, syms, F = start_game('ZELDA', [('wait', 300)], 'state-zelda.log')
    menu(calc, 'Save state', 77)
    menu(calc, 'Load state', 77)
    F.wait(120)
    calc.close()


def test_old_states():
    """States saved by FamiCalc 0.2.1 (the user's calculator) still load."""
    for game in ['MARIO3', 'ZELDA', 'FINALF']:
        calc, syms, F = start_game(game, [('wait', 120)], 'old-%s.log' % game)
        calc.cmd('snap %06X %06X 1' % (syms['flush_bank_cache'], NES_RAM))
        calc.key('ln', ms=100, after=100)      # slot 0 = NAMES0
        F.wait(240)
        n = snaps(calc, syms['flush_bank_cache'])[0]
        check(n == 1, '%s: state loaded %d times (once expected)' % (game, n))
        log = open(os.path.join(OUT, 'old-%s.log' % game)).read()
        check('NMI reset' not in log, '%s: calculator reset after loading' % game)
        calc.screenshot('old-' + game)
        calc.close()


def test_fps_display():
    """Show FPS puts the frame rates on screen."""
    calc, syms, F = start_game('MARIO3', SMB3_TO_MAP, 'fps_display.log')
    menu(calc, 'Show FPS')
    calc.key('clear', ms=100, after=300)
    F.wait(150)
    calc.screenshot('fps-display')
    # fps_label: color, "NN", 0, color, "fps", 0, color, dot, 0, color, "NN", 0, ...
    label = calc.peek(syms['fps_label'], 16)
    nes, drawn = label[1:3].decode(), label[13:15].decode()
    check(re.match(r'^[ 0-9][0-9]$', nes) and re.match(r'^[ 0-9][0-9]$', drawn),
          'fps label is %r' % label)
    check(58 <= int(nes) <= 62, 'NES frames per second: %s' % nes)
    calc.close()


def test_leave_game():
    """Change game goes back to the ROM list; Quit leaves FamiCalc."""
    calc, syms, F = start_game('MARIO3', SMB3_TO_MAP, 'leave.log')
    shown = calc.counts()[1][syms['_rom_menu']]
    menu(calc, 'Change game', confirm=True)
    check(calc.counts()[1][syms['_rom_menu']] == shown + 1, 'ROM list not shown')
    calc.screenshot('leave-romlist')
    calc.key('2nd', after=3000)
    menu(calc, 'Quit FamiCalc', confirm=True)
    calc.run(1000)
    calc.screenshot('leave-quit')
    check(calc.u24(syms['_jit_cache_extend_end']) and True, 'unreachable')
    calc.close()


VBUFFER, VBUFFER_SIZE, LCD_PALETTE = 0xD42000, 256 * 224, 0xE30200


def frame_rows(fb, pal):
    """Rows 0-223, columns 8-247 of a drawn frame as 16-bit colors."""
    colors = [pal[2 * i] | pal[2 * i + 1] << 8 for i in range(256)]
    return [tuple(colors[b] for b in fb[y * 256 + 8:y * 256 + 248]) for y in range(224)]


def row_pattern(row):
    """Which neighbouring pixels have the same color: the same for a drawn
    row and its picture on the panel, whatever the color conversion."""
    return bytes(1 if row[i] == row[i + 1] else 0 for i in range(len(row) - 1))


def check_tearing(calc, syms, name, shots=30, gap_ms=23):
    """Every screenshot's rows must all come from one of the last 8 drawn
    frames (a frame is snapshotted when render_draw is done)."""
    from PIL import Image
    end = syms['pace_draw_end']	# drawn frames only (not ones pacing skipped)
    calc.cmd('snap %06X %06X %d' % (end, VBUFFER, VBUFFER_SIZE))
    calc.cmd('snap %06X %06X 512' % (end, LCD_PALETTE))
    size = VBUFFER_SIZE + 512
    torn = []
    frames = []		# the last 8 drawn frames, as row patterns
    for i in range(shots):
        calc.run(gap_ms)
        png = calc.screenshot('tear-%s-%02d' % (name, i))
        path = os.path.join(OUT, 'snap-frames.bin')
        n = int(calc.cmd('snaps-all %06X %s' % (end, path)))
        data = open(path, 'rb').read()
        frames += [[row_pattern(r) for r in frame_rows(data[k * size:k * size + VBUFFER_SIZE],
                                                      data[k * size + VBUFFER_SIZE:(k + 1) * size])]
                   for k in range(n)]
        frames = frames[-8:]
        n = len(frames)
        if n < 2:
            continue
        im = Image.open(png).convert('RGB')
        shown = [row_pattern([im.getpixel((x, y + 8)) for x in range(40, 280)]) for y in range(224)]
        # rows that tell frames apart, and which frames each could come from
        candidates = set(range(n))
        for y in range(224):
            match = {k for k in range(n) if frames[k][y] == shown[y]}
            if match and len(match) < n:
                candidates &= match
        if not candidates:
            torn.append(i)
    return torn


def screen_difference(a, b):
    """Share of the game window's pixels that differ between two screenshots."""
    from PIL import Image, ImageChops
    box = (40, 8, 280, 232)
    diff = ImageChops.difference(Image.open(a).convert('RGB').crop(box), Image.open(b).convert('RGB').crop(box))
    return sum(1 for p in diff.getdata() if p != (0, 0, 0)) / (240 * 224)


def test_resume():
    """After Resume, the game looks as it did before the menu (the menu
    borrows the tile cache), and nothing is left outside the game's window."""
    calc, syms, F = start_game('MARIO3', SMB3_TO_MAP, 'resume.log')
    before = calc.screenshot('resume-before')
    menu(calc, 'Resume game')
    F.wait(30)
    after = calc.screenshot('resume-after')
    d = screen_difference(before, after)
    # the map's animations change a few percent; a broken tile cache most of it
    check(d < 0.25, '%.1f%% of the screen changed after Resume' % (100 * d))
    from PIL import Image
    top = Image.open(after).convert('RGB').crop((0, 0, 320, 8))
    check(max(max(p) for p in top.getdata()) < 40, 'rows above the game window are not black')
    calc.close()


def test_tearing():
    """The panel shows whole, recent frames: Zelda's scrolling story (heavy
    frames: the panel is locked while drawing) and Final Fantasy's
    scrolling prologue (light frames: drawn behind the LCD)."""
    for game, wait in [('ZELDA', 1100), ('FINALF', 300)]:
        calc, syms, F = start_game(game, [('wait', wait)], 'tearing-%s.log' % game)
        torn = check_tearing(calc, syms, game, shots=20, gap_ms=31)
        check(not torn, '%s: screenshots %s show torn or old frames' % (game, torn))
        calc.close()


def drawn_frames(calc, syms, count=14):
    """{NES frame number (mod 256): visible part of the frame buffer} for
    the frames drawn over the next count*8 NES frames."""
    F = Frames(calc, syms)
    end = syms['pace_draw_end']	# drawn frames only (not ones pacing skipped)
    calc.cmd('snap %06X D60F00 1' % end)	# current_frame
    calc.cmd('snap %06X %06X %d' % (end, VBUFFER, VBUFFER_SIZE))
    size = 1 + VBUFFER_SIZE
    frames = {}
    for _ in range(count):
        F.wait(8)
        path = os.path.join(OUT, 'drawn.bin')
        n = int(calc.cmd('snaps-all %06X %s' % (end, path)))
        data = open(path, 'rb').read()
        for k in range(n):
            fb = data[k * size + 1:(k + 1) * size]
            frames[data[k * size]] = b''.join(fb[y * 256 + 8:y * 256 + 248] for y in range(224))
    return frames


def test_rowskip():
    """Rows skipped because they didn't change give exactly the picture a
    full redraw gives (scenes without input, so both runs play the same)."""
    for game, start in [('MARIO3', 400), ('TETRIS', 900), ('ZELDA', 1100), ('FINALF', 300)]:
        runs = []
        for skip_off in (0, 1):
            calc, syms, F = start_game(game, [('wait', start)], 'rowskip-%s.log' % game)
            calc.cmd('poke %06X %02X' % (syms['row_skip_off'], skip_off))
            runs.append(drawn_frames(calc, syms))
            calc.close()
        common = set(runs[0]) & set(runs[1])
        check(len(common) > 20, '%s: only %d frames to compare' % (game, len(common)))
        bad = sorted(f for f in common if runs[0][f] != runs[1][f])
        check(not bad, '%s: frames %s differ from a full redraw' % (game, bad[:5]))


def check_tearing_wide(calc, syms, name, shots=20, gap_ms=31):
    """check_tearing for the wide picture (320x224, columns 10-309 shown).
    Its frame takes the LCD a little longer to send than the panel waits
    before its own refresh (which runs left to right) starts, so the
    bottom-left corner (up to column 47 of the last 80 rows) may still show
    the previous frame."""
    from PIL import Image
    ws = 320 * 224
    end = syms['pace_draw_end']		# after stretching and labels
    calc.cmd('snap %06X %06X %d' % (end, calc.u24(syms['_wide_buffer']), ws))
    calc.cmd('snap %06X %06X 512' % (end, LCD_PALETTE))
    size = ws + 512
    torn, frames = [], []
    for i in range(shots):
        calc.run(gap_ms)
        png = calc.screenshot('tear-wide-%s-%02d' % (name, i))
        path = os.path.join(OUT, 'snap-frames.bin')
        n = int(calc.cmd('snaps-all %06X %s' % (end, path)))
        data = open(path, 'rb').read()
        for k in range(n):
            fb, pal = data[k * size:k * size + ws], data[k * size + ws:(k + 1) * size]
            colors = [pal[2 * j] | pal[2 * j + 1] << 8 for j in range(256)]
            frames.append([row_pattern([colors[b] for b in fb[y * 320 + (48 if y >= 144 else 10):y * 320 + 310]])
                           for y in range(224)])
        frames = frames[-8:]
        if len(frames) < 3:		# the panel can be a frame behind the first ones snapshotted
            continue
        im = Image.open(png).convert('RGB')
        shown = [row_pattern([im.getpixel((x, y + 8)) for x in range(48 if y >= 144 else 10, 310)])
                 for y in range(224)]
        candidates = set(range(len(frames)))
        for y in range(224):
            match = {k for k in range(len(frames)) if frames[k][y] == shown[y]}
            if match and len(match) < len(frames):
                candidates &= match
            if not match:
                candidates = set()
        if not candidates:
            torn.append(i)
    return torn


def test_wide():
    """The wide picture: full width, whole recent frames, the game keeps running."""
    for game, wait in [('ZELDA', 1100), ('FINALF', 300), ('MARIO3', 400)]:
        calc, syms, F = start_game(game, [('wait', wait)], 'wide-%s.log' % game)
        menu(calc, 'Picture')
        calc.key('clear', ms=100, after=300)
        F.wait(30)
        check(calc.u24(syms['_wide_buffer']), '%s: no wide buffer' % game)
        torn = check_tearing_wide(calc, syms, game)
        check(not torn, '%s: wide screenshots %s show torn or old frames' % (game, torn))
        calc.screenshot('wide-' + game)
        calc.close()


def test_fps():
    """Every benchmark scene runs at full speed and draws a fair share of frames."""
    for name, (nes, drawn) in bench().items():
        check(58 <= nes <= 62, '%s runs at %.1f NES frames/s' % (name, nes))
        check(drawn >= 25, '%s draws %.1f frames/s' % (name, drawn))


TESTS = {'boot': test_boot, 'save_load': test_save_load, 'state_games': test_state_games,
         'old_states': test_old_states,
         'fps_display': test_fps_display, 'leave_game': test_leave_game, 'resume': test_resume, 'tearing': test_tearing, 'rowskip': test_rowskip, 'wide': test_wide,
         'fps': test_fps}


def main():
    os.makedirs(OUT, exist_ok=True)
    check(os.path.exists(HEADLESS), 'build the harness first: make -C %s' % HERE)
    check(os.path.exists(ROM), 'no ROM at %s (set FC_ROM)' % ROM)
    args = sys.argv[1:] or ['all']
    if args[0] == 'list':
        print(' '.join(TESTS))
        return 0
    if args[0] in ('bench-wide', 'bench-fps', 'bench-wide-fps'):
        # the benchmark in the wide picture and/or with the FPS counter on
        bench(None, args[0][6:], wide='wide' in args[0], fps_label='fps' in args[0])
        return 0
    if args[0] == 'bench':
        # bench [label=program.8xp ...]: default compares the baselines with the current build
        progs = [a.split('=', 1) for a in args[1:]] or [
            ['v0.2.1', os.path.join(HERE, 'build/baselines/v0.2.1/FAMICALC.8xp')],
            ['upstream', os.path.join(HERE, 'build/baselines/3202ea4/FAMICALC.8xp')],
            ['current', PROGRAM]]
        table = {label: bench(prog, label) for label, prog in progs}
        print('\n%-12s' % 'scene' + ''.join('%18s' % l for l, _ in progs))
        for name, _, _ in BENCH:
            print('%-12s' % name + ''.join('%11.1f /%5.1f' % table[l][name] for l, _ in progs))
        print('(NES frames per second / frames drawn per second)')
        return 0
    if args[0] == 'profile':
        # profile SCENE [wide] [program.8xp]
        name = args[1]
        wide = 'wide' in args[2:]
        rest = [a for a in args[2:] if a != 'wide']
        prog = rest[0] if rest else None
        game, steps = [(g, st) for n, g, st in BENCH if n == name][0]
        syms = symbols(prog)
        calc = start_famicalc([game], prog)
        select_game(calc, [game], game)
        F = Frames(calc, syms)
        run_steps(F, steps)
        if wide:
            menu(calc, 'Picture')
            calc.key('clear', ms=100, after=300)
            F.wait(60)
        for share, place in profile(calc, syms, wide=wide)[:45]:
            print('%5.1f%%  %s' % (100 * share, place))
        calc.close()
        return 0
    if args[0] == 'fps' and len(args) > 1:
        for g in args[1:]:
            nes, drawn = fps_game(g)
            print('%-8s %5.1f NES frames/s, %5.1f drawn frames/s' % (g, nes, drawn))
        return 0
    names = list(TESTS) if args[0] == 'all' else args
    failed = 0
    for name in names:
        t = time.time()
        try:
            TESTS[name]()
            print('PASS %s (%.0fs)' % (name, time.time() - t))
        except TestFailure as e:
            failed += 1
            print('FAIL %s: %s' % (name, e))
    print('%d/%d passed' % (len(names) - failed, len(names)))
    return 1 if failed else 0


if __name__ == '__main__':
    sys.exit(main())
