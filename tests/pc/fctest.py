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
SCANLINE_VARS = 0xD60F80	# jit_scanline_vars (vars.inc): ix
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
        if path.endswith('.8xp'):
            self.program = path		# (menu: its symbols)
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


def measure_fps(calc, syms, seconds=10, warmup=0, play=None):
    """NES frames emulated and frames drawn per second of emulated time
    (drawn: frames that reached pace_draw_end; render_draw also runs for
    frames that pace_draw_start then aborts). play(calc, syms, seconds), if
    given, plays during the measurement instead of waiting."""
    drawn_at = 'pace_draw_end' if 'pace_draw_end' in syms else 'render_draw'  # older builds: no pacing
    for s in ('ppu_video_start', drawn_at):
        calc.count(syms[s])
    if warmup:
        calc.run(warmup)
    c0, h0 = calc.counts()
    if play:
        play(calc, syms, seconds)
    else:
        calc.run(seconds * 1000)
    c1, h1 = calc.counts()
    if play:
        seconds = (c1 - c0) / 48e6
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
    # Zelda's attract mode, no input. NES frames counted from the game's pick
    # (vblank scroll): title 61-974 (static), story text scrolling up 975-1438
    # (y + 1 every 2 frames), story still 1439-1696, story and treasure list
    # scrolling on 1697-4258 (y + 1 every 2 frames; the items' sprite code runs
    # from NES RAM from ~1900, a screen full of items from ~2400), then the
    # title again. The wide run starts 68 frames later (menu + wait).
    # zelda-story: 6 s (frames 990-1350, wide 1058-1418), scrolling only.
    ('zelda-story', 'ZELDA', [('wait', 990)]),
    ('zelda-items', 'ZELDA', [('wait', 2450)]),
]
BENCH_SECONDS = 10
BENCH_SCENE_SECONDS = {'zelda-story': 6}	# scenes measured for less than BENCH_SECONDS
# scenes the PC tests (fps, state_games) leave out: below the fps test's
# limits for now, and Zelda's save/load already has its own test
BENCH_UNTESTED = ('zelda-story',)


def run_right(calc, syms, seconds):
    """SMB3's 1-1 with a star kept on (NES RAM $0553): run right, jump every
    half second. The level scrolls all along for about 7 seconds."""
    F = Frames(calc, syms)
    calc.press(NES['right'])
    calc.press(NES['B'])
    for _ in range(int(seconds * 2)):
        calc.cmd('poke %06X FF' % (NES_RAM + 0x553))
        F.tap('A', frames=12, after=18)
    calc.release(NES['B'])
    calc.release(NES['right'])


# scenes measured while playing: name -> (game, steps, play)
BENCH_PLAY = {
    'smb3-run': ('MARIO3', [('wait', 240), ('tap', 'start'), ('wait', 120), ('tap', 'start'),
                            ('wait', 240), ('tap', 'right'), ('wait', 60), ('tap', 'up'),
                            ('wait', 60), ('tap', 'A'), ('wait', 240)], run_right),
}


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
    for name, game, steps, play in [b + (None,) for b in BENCH] + [(n,) + v for n, v in BENCH_PLAY.items()]:
        if scenes and name not in scenes:
            continue
        seconds = 6 if play else BENCH_SCENE_SECONDS.get(name, BENCH_SECONDS)
        calc = start_famicalc([game], program, log=os.path.join(OUT, 'bench-%s-%s.log' % (label, name)))
        select_game(calc, [game], game)
        F = Frames(calc, syms)
        run_steps(F, steps)
        if wide:
            menu(calc, 'Picture')
            F.wait(60)
        if fps_label:
            menu(calc, 'Show FPS')
            F.wait(60)
        check(bool(calc.u24(syms['_wide_buffer'])) == wide and calc.u8(syms['_show_fps']) == fps_label,
              '%s: the picture or the FPS counter is not as asked' % name)
        nes, drawn = measure_fps(calc, syms, seconds, play=play)
        calc.screenshot('bench-%s-%s' % (label, name))
        calc.close()
        results[name] = (nes, drawn)
        print('  %-8s %-12s %5.1f NES fps  %5.1f drawn fps' % (label, name, nes, drawn), flush=True)
    return results


def profile(calc, syms, ms=5000, us=20, wide=False, play=None):
    """Sample the CPU's program counter; returns [(share, place)] busiest first.
    The wide picture has its own code in cursor RAM (renderwide.asm, w_*).
    play(calc, syms, seconds), if given, plays while sampling."""
    if play:
        calc.cmd('profile-on %d' % us)
        play(calc, syms, ms / 1000)
        out = calc.cmd('profile-off').split()
    else:
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
MENU_ITEMS = ['Resume', 'Save state', 'Load state', 'Settings', 'Controls', 'Reset game', 'Change game',
              'Quit FamiCalc']
SETTINGS_ITEMS = ['Picture', 'Show FPS', 'Brightness', 'Save on quit', 'State pictures']
BACKLIGHT = 0xF60024		# 0 brightest, 255 darkest
SMB3_TO_MAP = [('wait', 240), ('tap', 'start'), ('wait', 120), ('tap', 'start'), ('wait', 240)]


def menu(calc, item, slot=None, confirm=False):
    """Open the pause menu ([clear]) and pick item: a row of the main page
    (confirm: answer its question with [2nd]); a setting (Settings, change
    it, close the menu); Save state or Load state in a slot (typed in the
    slot list; confirm: save over it); Delete state (Save state's list,
    [del], yes, close the menu)."""
    def key(name, after=150):
        calc.key(name, ms=100, after=after)
    def row(name):
        for _ in range(MENU_ITEMS.index(name)):
            key('down', 100)
    # in the wide picture the menu saves a state first (it draws over the
    # JIT's extra cache): it shows ~800 ms after [clear], and keys pressed
    # before went to the game or the menu's first page (a "wide fps" bench
    # loaded slot 1 instead of turning the FPS counter on)
    wide = calc.u24(symbols(getattr(calc, 'program', None))['_wide_buffer'])
    key('clear', 1500 if wide else 400)
    if item in SETTINGS_ITEMS:
        row('Settings')
        key('2nd', 300)
        for _ in range(SETTINGS_ITEMS.index(item)):
            key('down', 100)
        key('2nd', 300)
        key('clear', 300)
        # the game resumes as late as before the submenus (the wide test's
        # Zelda scene tears more 90 frames later)
        calc.run(1500)
        key('clear', 300)
        return
    elif item in ('Save state', 'Load state', 'Delete state'):
        row('Load state' if item == 'Load state' else 'Save state')
        key('2nd', 300)
        if slot is not None:
            for ch in str(slot):
                key(ch, 100)
        if item == 'Delete state':
            key('del', 200)
            key('2nd', 300)
            key('clear', 300)
            key('clear', 200)
        else:
            key('2nd', 200)
            if confirm:
                key('2nd', 200)
    else:
        row(item)
        key('2nd', 200)
        if confirm:
            key('2nd', 200)
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
        if name in ('smb3-map', 'zelda-title', 'zelda-items') or name in BENCH_UNTESTED:
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


def test_wram_code():
    """Code in cartridge WRAM ($6000-$7FFF), which the JIT caches, rewritten
    while it is cached (test ROMs from tools/mkwramrom.py, which lists the
    cases), with MMC1, NROM and MMC3: after every kind of store the next
    call runs the new code, also a block that rewrites itself, one that runs
    into the next page and a page rewritten over and over (it turns
    volatile). Then the page border (WRAMB): a block that ends at a page's
    last byte doesn't make the next page code (writes to it would flush the
    cache and turn the page volatile: per instruction), one that runs into
    it does."""
    global GAMES
    GAMES = os.path.join(HERE, 'build/wramrom')
    subprocess.run([sys.executable, os.path.join(HERE, '../../tools/mkwramrom.py'), GAMES], check=True)
    expected = {0x20 + i: 0x11 for i in range(6)}
    expected.update({0x30 + i: v for i, v in enumerate([0x22, 0x33, 0x44, 0x12, 0x12, 0x66] * 2)})
    expected.update({0x40 + i: 0x70 + i for i in range(10)})
    expected[0x4A] = 0x99
    expected.update({0x50: 0x11, 0x51: 0xC3, 0x52: 0x5A, 0x53: 0x07, 0x54: 0x5C, 0x55: 0x77})
    expected.update({0x60 + x: x for x in range(1, 16)})
    expected.update({0x70 + x: x for x in range(1, 16)})
    expected.update({0x80 + i: 0x80 + i for i in range(6)})
    expected.update({0x88 + i: 0x90 + i for i in range(4)})
    expected[0x8C] = 0xE5
    expected.update({0xA0 + x: x for x in range(1, 16)})
    for game, kind in [('WRAMT', 'MMC1'), ('WRAMN', 'NROM'), ('WRAMM', 'MMC3')]:
        calc, syms, F = start_game(game, [('wait', 60)], 'wram_code-%s.log' % game)
        ram = calc.peek(NES_RAM + 0x300, 0x100)
        calc.close()
        check(ram[0xFF] == 0xA5, '%s: the test ROM did not finish ($03FF = %02X)' % (kind, ram[0xFF]))
        bad = ['$03%02X=%02X, not %02X' % (k, ram[k], v) for k, v in sorted(expected.items()) if ram[k] != v]
        check(not bad, '%s: old WRAM code ran: %s' % (kind, '; '.join(bad)))

    calc, syms, F = start_game('WRAMB', [], 'wram_code-border.log')
    names = ['flush_cache', 'jit_convert_ram']
    for name in names:
        calc.count(syms[name])

    def counts():
        h = calc.counts()[1]
        return [h[syms[n]] for n in names]
    c0 = counts()
    calc.cmd('poke %06X 5A' % (NES_RAM + 0x700))	# go
    for _ in range(60):
        F.wait(2)
        if calc.u8(NES_RAM + 0x3FE) == 1:
            break
    c1 = counts()
    calc.cmd('poke %06X 5A' % (NES_RAM + 0x701))	# go on
    F.wait(30)
    c2 = counts()
    ram = calc.peek(NES_RAM + 0x300, 0x100)
    calc.close()
    check(ram[0xFF] == 0xA5, 'border: the test ROM did not finish ($03FF = %02X)' % ram[0xFF])
    expected = {0x90 + i: 0x11 for i in range(8)}
    expected.update({0x98: 0x07, 0x99: 0x11, 0x9A: 0xC3, 0xAC: 0xE5})
    expected.update({0xA0 + i: 0x80 + i for i in range(6)})
    expected.update({0xA8 + i: 0x90 + i for i in range(4)})
    bad = ['$03%02X=%02X, not %02X' % (k, ram[k], v) for k, v in sorted(expected.items()) if ram[k] != v]
    check(not bad, 'border: old WRAM code ran: ' + '; '.join(bad))
    # the store into the block from $6AF9 and 3 rewrites of $6F80 (the 4th
    # turns $6F volatile; that flush comes later); no page's first code
    # flushes: no store to one was inlined unchecked before
    flushes = c1[0] - c0[0]
    check(flushes <= 4, 'border: %d cache flushes (4 expected): data writes next to a page holding '
          'code count as writes to code' % flushes)
    # per instruction: $6F's routine after it turned volatile (3 calls of 3
    # instructions) and the block from $6EF9 that runs into it (4 calls of 9)
    check(c1[1] - c0[1] <= 3 * 3 + 4 * 9, 'border: %d instructions translated one at a time (45 expected): '
          'a page next to one with data writes went per instruction' % (c1[1] - c0[1]))
    check(c2[1] == c1[1], 'border: the routine at $6E10 ran per instruction (%d), though no block from there '
          'reaches the volatile page $6F' % (c2[1] - c1[1]))


def test_jit_mappers():
    """JIT and mapper cases (test ROMs from tools/mkwramrom.py, which lists
    them): MMC1 WRAM banks written where they are read (WBNK); MMC3
    four-screen nametables, apart from the JIT's tables and kept in states
    (FOUR); a WRAM block ending at $7FFF or running on into $8000 finds the
    selected bank there, and STX/STY abs end their block when they switch the bank under it
    (JITM); with a 1 KB JIT cache (patched in), flushes inside jit_call_local
    and jit_branch_local_bankswap leave the stack right (JITF)."""
    global GAMES
    GAMES = os.path.join(HERE, 'build/wramrom')
    subprocess.run([sys.executable, os.path.join(HERE, '../../tools/mkwramrom.py'), GAMES], check=True)

    failed = []

    def soft(cond, msg):
        if not cond:
            failed.append(msg)
        return cond

    def done(calc, name):
        ram = calc.peek(NES_RAM + 0x300, 0x100)
        soft(ram[0xFF] == 0xA5, '%s: the test ROM did not finish ($03FF = %02X)' % (name, ram[0xFF]))
        return ram

    def rom_wbnk():
        calc, syms, F = start_game('WBNK', [('wait', 30)], 'jit-WBNK.log')
        ram = done(calc, 'WBNK')
        calc.close()
        soft(list(ram[0xB0:0xB4]) == [0x11, 0x44, 0x22, 0x55],
              'WBNK: WRAM banks 0/1 read back %s (11 44 22 55 expected)' % ram[0xB0:0xB4].hex(' '))


    def rom_four():
        calc, syms, F = start_game('FOUR', [('wait', 30)], 'jit-FOUR.log')
        ram = done(calc, 'FOUR')
        bad = []
        for nt in range(4):
            for k, off in enumerate([0x000, 0x3B0]):
                for i in range(16):
                    want = ((off + i) & 0xFF) | (nt << 6)
                    got = ram[0x20 * nt + 0x10 * k + i]
                    if got != want:
                        bad.append('NT%d+$%03X: %02X not %02X' % (nt, off + i, got, want))
        soft(not bad, 'FOUR: nametables read back wrong: ' + '; '.join(bad[:8]))
        soft(ram[0x80] == 8, 'FOUR: the JSRs after the nametables ran %d times (8)' % ram[0x80])
        nt23 = calc.peek(0xD63000, 4096)			# nametables 2 and 3 (vars.inc ppu_nametables + 4096)
        calc.key('sto', after=2000)
        calc.cmd('poke D63000 ' + '00' * 64)
        calc.cmd('poke D63800 ' + '00' * 64)
        calc.key('ln', after=2000)
        # (each NES byte is the even one of two: the odd one holds drawing flags)
        soft(calc.peek(0xD63000, 4096)[::2] == nt23[::2], 'FOUR: a state did not keep nametables 2 and 3')
        calc.close()


    def rom_jitm():
        calc, syms, F = start_game('JITM', [('wait', 30)], 'jit-JITM.log')
        ram = done(calc, 'JITM')
        calc.close()
        soft(list(ram[0xC0:0xC4]) == [0xB0, 0xB2, 0xB0, 0xB2],
              'JITM: the block ending at $7FFF ran into banks %s at $8000 (B0 B2 B0 B2 expected)' % ram[0xC0:0xC4].hex(' '))
        soft(list(ram[0xC8:0xCC]) == [0xB0, 0xB2, 0xB0, 0xB2],
             'JITM: the block running on from $7FFC into $8000 took banks %s there (B0 B2 B0 B2 expected)'
             % ram[0xC8:0xCC].hex(' '))
        soft(list(ram[0xC4:0xC8]) == [0xC2, 0xC3, 0xD2, 0xD3],
              'JITM: after STX/STY switched the bank under the code (to 2, then 3), %s ran (C2 C3 D2 D3 expected)'
              % ram[0xC4:0xC8].hex(' '))


    def rom_jitf():
        calc, syms, F = start_game('JITF', [], 'jit-JITF.log')
        code = calc.peek(syms['jit_add_block'], 0x100)
        # jit_add_block's ld bc,jit_cache_end ($D5EC00; $D5FC00 before the four-screen nametables)
        at = max(code.find(bytes([0x01, 0x00, 0xEC, 0xD5])), code.find(bytes([0x01, 0x00, 0xFC, 0xD5])))
        check(at >= 0, 'jit_add_block changed: update the JIT cache knob')
        calc.cmd('poke %06X %s' % (syms['jit_add_block'] + at + 1, (syms['jit_cache_start'] + 0x400).to_bytes(3, 'little').hex()))
        calc.cmd('poke %06X %s' % (syms['_jit_cache_extend_end'], calc.peek(syms['_jit_cache_extend'], 3).hex()))
        paths = [n for n in ('jit_call_local.flush', 'jit_branch_local_bankswap.flushed', 'flush_cache') if n in syms]
        for n in paths:
            calc.count(syms[n])
        # a block's calls into the runtime find the stack at most at its bottom - 12
        # (jit_call_stack_bot - 9, empty, and the block's return address)
        bot = int(re.search(r'jit_call_stack_bot\s*:= \$([0-9A-F]+)', open(os.path.join(HERE, '../../src/vars.inc')).read()).group(1), 16)
        calc.cmd('trace %06X %06X' % (syms['jit_branch_local'], syms['jit_branch_global']))
        calc.cmd('poke %06X 5A' % (NES_RAM + 0x700))	# go
        for _ in range(40):
            F.wait(30)
            if calc.u8(NES_RAM + 0x3FF) == 0xA5:
                break
        hits = calc.counts()[1]
        path = os.path.join(OUT, 'jitf-%d.txt' % os.getpid())
        calc.cmd('trace-dump ' + path)
        sps = [int(l.split()[5], 16) for l in open(path)]
        ram = done(calc, 'JITF')
        calc.close()
        high = [sp for sp in sps if sp > bot - 12]
        soft(sps and not high, 'JITF: blocks ran with the stack at %s (at most bottom-12 = %06X): a flush left SP wrong'
              % (sorted(set('%06X' % sp for sp in high))[:4], bot - 12))
        soft(ram[0x30] == 60 and list(ram[0x40:0x58]) == [60] * 24 and list(ram[0x60:0x78]) == [60] * 24,
              'JITF: counts %s %s (60 each expected)' % (ram[0x30], ram[0x40:0x58].hex(' ')))
        for n in paths:
            soft(hits[syms[n]] > 0, 'JITF: no flush in %s (the knob did not work)' % n)

    for name, run in [('WBNK', rom_wbnk), ('FOUR', rom_four), ('JITM', rom_jitm), ('JITF', rom_jitf)]:
        try:
            run()
        except TestFailure as e:		# (the emulator stopped: a crash)
            failed.append('%s: %s' % (name, e))
    check(not failed, '; '.join(failed))


def test_sprites_line0():
    """A sprite size and pattern table change at scanline 0 (SPR0 from
    tools/mkwramrom.py; the delay that puts the write there is found first:
    s_botclip = $FF): no sprites above it are drawn (only white 8x16 ones
    show, none of the red 8x8 ones before it), and pacing doesn't wait for
    the LCD to read a line past the frame (its .ahead loop times out)."""
    from PIL import Image
    global GAMES
    GAMES = os.path.join(HERE, 'build/wramrom')
    subprocess.run([sys.executable, os.path.join(HERE, '../../tools/mkwramrom.py'), GAMES], check=True)
    calc, syms, F = start_game('SPR0', [('wait', 30)], 'sprites_line0.log')
    calc.cmd('snap %06X %06X 1' % (syms['pace_mid_sprites'], SCANLINE_VARS + 108))	# s_botclip
    found = None
    for coarse in range(16, 48):
        for fine in (0, 4, 8):
            calc.cmd('poke %06X %02X%02X' % (NES_RAM + 0x700, coarse, fine))
            F.wait(6)
            n, data = snaps(calc, syms['pace_mid_sprites'])
            if n and data[0] == 0xFF:
                found = (coarse, fine)
                break
        if found:
            break
    check(found, 'SPR0: no delay put the PPUCTRL write at line 0 (s_botclip $FF)')
    calc.cmd('trace %06X %06X' % (syms['pace_mid_sprites'], syms['render_sprites']))
    F.wait(60)
    path = os.path.join(OUT, 'sprites_line0-%d.txt' % os.getpid())
    calc.cmd('trace-dump ' + path)
    ev = [(int(l.split()[0]), int(l.split()[1], 16)) for l in open(path)]
    waits = [(t2 - t) // 256 for (t, a), (t2, a2) in zip(ev, ev[1:])
             if a == syms['pace_mid_sprites'] and a2 == syms['render_sprites']]
    calc.run(20)
    calc.cmd('panel-idle')
    png = calc.screenshot('sprites-line0')
    calc.close()
    check(waits and max(waits) < 100, 'SPR0: pace_mid_sprites waited %s units before the sprites' % max(waits or [0]))
    im = Image.open(png).convert('RGB')
    px = im.load()
    red = sum(1 for x in range(320) for y in range(240) if px[x, y][0] > 150 and px[x, y][1] < 100)
    white_rows = [y for y in range(240) if any(min(px[x, y]) > 200 for x in range(40, 280))]
    check(red == 0, 'SPR0: %d pixels of the 8x8 sprites from before line 0 show' % red)
    check(len(white_rows) == 16, 'SPR0: the white sprites are %d lines tall (16 expected)' % len(white_rows))


def test_panel_sprites():
    """Sprites drawn in the middle of a frame (where the sprite pattern table
    changes: SMB3's playfield sprites at its status bar) are checked before
    they are drawn with their time in the last drawn frame. SPRM
    (tools/mkwramrom.py) has 24 of them (~700 units) in one frame of 16 and
    none in the others, so that check takes them as short; with the frame
    drawn at other phases of the LCD (frame_delay, one per sprite frame),
    no piece of sprites drawn with the panel unlocked may still be drawn
    when the LCD's next transfer starts (pace_sprite_tick locks it every 8
    sprites if needed), normal and wide. The transfer starts come from the
    LCD's read address (as pacetrace.py)."""
    global GAMES
    GAMES = os.path.join(HERE, 'build/wramrom')
    subprocess.run([sys.executable, os.path.join(HERE, '../../tools/mkwramrom.py'), GAMES], check=True)
    failed = []
    for wide in (False, True):
        kind = 'wide' if wide else 'normal'
        calc, syms, F = start_game('SPRM', [('wait', 30)], 'panel-sprites-%s.log' % kind)
        if wide:
            menu(calc, 'Picture')
            F.wait(30)
        delay = frame_delay(calc, syms)
        names = {syms['render_sprites']: 'spr', syms['load_drawtile']: 'end', syms['pace_sprites_done']: 'end',
                 syms['spiLock']: 'lock', syms['pace_do_unlock']: 'unlock'}
        lo = syms['pace_init']		# every ld hl,(mpLcdUpcurr) in pacing.asm: hl is the address after it
        code = calc.peek(lo, 0x900)
        k = code.find(bytes([0x2A, 0x2C, 0x00, 0xE3]))
        while k >= 0:
            names[lo + k + 4] = 'read'
            k = code.find(bytes([0x2A, 0x2C, 0x00, 0xE3]), k + 1)
        fb, fe = calc.u24(syms['game_fb']), calc.u24(syms['lcd_frame_end'])
        calc.cmd('trace ' + ' '.join('%06X' % a for a in names))
        for d in range(2200, 3100, 15):
            delay(d)
            F.wait(16)
        path = os.path.join(OUT, 'panel-sprites-%d.txt' % os.getpid())
        calc.cmd('trace-dump ' + path)
        calc.close()
        ev = [(int(l.split()[0]), names[int(l.split()[1], 16)], int(l.split()[2], 16)) for l in open(path)]
        period, xfer = 798720, (1138 if wide else 910) * 256
        starts = sorted(t - (h - fb) * xfer // (fe - fb) for t, n, h in ev if n == 'read' and fb <= h < fe)
        check(starts, '%s: no read address inside the frame buffer' % kind)
        ref = starts[len(starts) // 2] % period
        locked, piece, late, heavy = 0, None, [], 0
        for t, n, h in ev:
            if n in ('lock', 'unlock'):
                locked = n == 'lock'
            elif n == 'spr':
                piece = (t, locked)
            elif piece:
                t0, l0 = piece
                piece = None
                heavy += (t - t0) > 400 * 256
                nxt = t0 + (ref - t0) % period		# the next transfer's start
                if not l0 and not locked and t > nxt:
                    late.append(((t0 - ref) % period // 256, (t - t0) // 256))
        check(heavy >= 10, '%s: only %d frames with the 24 sprites' % (kind, heavy))
        if late:
            failed.append('%s: %d pieces of sprites drawn unlocked into a transfer (phase, units: %s)'
                          % (kind, len(late), late[:6]))
    check(not failed, '; '.join(failed))


def test_lcd_reads():
    """Every read of the LCD's read address (mpLcdUpcurr, $E3002C) goes
    through lcd_read_address (pacing.asm): a single 3-byte read while the
    LCD's DMA moves it can be 64 KB or 256 bytes off (the sprite restores'
    "has the LCD read this line" checks read it once). Looks at the program
    in RAM: an absolute load of the address is ld rr,($E3002C) or ld a,."""
    syms = symbols()
    calc = start_famicalc(['MARIO3'], log=os.path.join(OUT, 'lcd-reads.log'))
    lo = syms['__start']
    hi = max(a for a in syms.values() if a < 0xD40000)
    code = calc.peek(lo, hi - lo + 64)
    calc.close()
    at = [lo + m.start() for m in re.finditer(re.escape(bytes([0x2C, 0x00, 0xE3])), code)]
    own = range(syms['lcd_read_address'], syms['lcd_read_address'] + 12) if 'lcd_read_address' in syms else ()
    other = ['%06X' % a for a in at if a not in own]
    check(not other, 'mpLcdUpcurr read outside lcd_read_address at %s' % other)
    check(len(at) == 2, 'lcd_read_address changed: it should read the address twice')


def test_lcd_timeouts():
    """The waits for the LCD give up after about two wide transfers (1138
    units each) when what they wait for doesn't come: the wait for it to
    read a row (pace_row_guard: game_fb pointed past memory, so no row is
    ever read) and the wait for it to finish sending (pace_sprite_guard,
    the same loop as pace_draw_start's: lcd_frame_end pointed past memory).
    Final Fantasy's prologue, drawn behind the LCD. (Polls read the address
    twice, lcd_read_address: the old counts waited ~8,000 units.)"""
    calc, syms, F = start_game('FINALF', [('wait', 300)], 'lcd-timeouts.log')
    out = []
    for loop, var in [('pace_row_guard.ahead', 'game_fb'), ('pace_sprite_guard.sending', 'lcd_frame_end')]:
        old = calc.peek(syms[var], 3)
        calc.cmd('trace %06X' % syms[loop])
        calc.cmd('poke %06X 0000F0' % syms[var])	# $F00000 (+ a row's offset stays below $FFFFFF)
        calc.run(60)
        calc.cmd('poke %06X %s' % (syms[var], old.hex()))
        path = os.path.join(OUT, 'lcd-timeouts-%d.txt' % os.getpid())
        calc.cmd('trace-dump ' + path)
        hits = [int(l.split()[0]) for l in open(path)]
        runs, start, n = [], None, 0		# polls in a row (less than 20 units apart)
        for a, b in zip([None] + hits, hits):
            if a is None or b - a > 20 * 256:
                if start is not None:
                    runs.append(((a - start) // 256, n))
                start, n = b, 0
            n += 1
        if start is not None:
            runs.append(((hits[-1] - start) // 256, n))
        runs = [r for r in runs if r[1] > 100]
        check(runs, '%s: no wait timed out' % loop)
        out.append((loop, runs))
        calc.run(200)		# (back to normal)
    calc.close()
    print('timeouts (units, polls):', out)
    bad = [(loop, runs) for loop, runs in out if not all(1500 <= u <= 3000 for u, n in runs)]
    check(not bad, 'timeouts (units, polls): %s; 1,500-3,000 units expected' % bad)


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
    """Show FPS puts the frame rates on screen, outside the game's picture:
    in the black band left of the normal picture, above the wide one. They
    go into the panel's memory: the game's frame buffer never has them. The
    numbers reach the panel every second (fps_panel_try: SMB3's map in the
    wide picture and Tetris drawn locked up to the LCD's sync found no
    moment for them for seconds)."""
    from PIL import Image
    failed = []
    for game, steps in [('MARIO3', SMB3_TO_MAP), ('TETRIS', [b for b in BENCH if b[0] == 'tetris'][0][2])]:
        calc, syms, F = start_game(game, steps, 'fps_display-%s.log' % game)
        menu(calc, 'Show FPS')
        for picture in ('normal', 'wide'):
            if picture == 'wide':
                menu(calc, 'Picture')
            F.wait(150)
            check(calc.u8(syms['_show_fps']) == 1 and bool(calc.u24(syms['_wide_buffer'])) == (picture == 'wide'),
                  '%s %s: the settings did not take' % (game, picture))
            # every second: a new label (fps_reset, at its end) and the numbers on the panel
            calc.count(syms['fps_panel_numbers'])
            calc.count(syms['fps_reset'])
            written = []
            for sec in range(6):
                c0 = calc.counts()[1]
                F.wait(60)
                c1 = calc.counts()[1]
                written.append((c1[syms['fps_reset']] - c0[syms['fps_reset']],
                                c1[syms['fps_panel_numbers']] - c0[syms['fps_panel_numbers']]))
            if any(n == 0 for u, n in written[1:]) or sum(n for u, n in written) < 5:
                failed.append('%s %s: (new labels, numbers written) per second %s' % (game, picture, written))
            if game != 'MARIO3':
                continue
            im = Image.open(calc.screenshot('fps-display' + ('-wide' if picture == 'wide' else ''))).convert('RGB')
            # fps_label: parts of color, text, 0, 0: "NN" at 1, "fps", dot, "NN" at 16, "drawn"
            label = calc.peek(syms['fps_label'], 20)
            nes, drawn = label[1:3].decode(), label[16:18].decode()
            check(re.match(r'^[ 0-9][0-9]$', nes) and re.match(r'^[ 0-9][0-9]$', drawn),
                  'fps label is %r' % label)
            check(58 <= int(nes) <= 62, 'NES frames per second: %s' % nes)
            # the first number in white: left of the picture, or above it
            box = (13, 8, 27, 19) if picture == 'normal' else (10, 0, 21, 8)
            lit = sum(1 for p in im.crop(box).getdata() if min(p) > 200)
            check(lit >= 10, '%s picture: no FPS number at %s' % (picture, box))
            fb_addr, half = calc.u24(syms['game_fb']), (256 if picture == 'normal' else 320) * 112
            fb = calc.peek(fb_addr, half) + calc.peek(fb_addr + half, half)
            check(max(fb) < 0xE0, '%s picture: menu colors in the game\'s frame buffer' % picture)
        calc.close()
    check(not failed, '; '.join(failed))


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
FB_SCROLL = 0xD60F80 + 77	# fb_scroll (vars.inc): the frame buffer's rotation


def shown_line(fb, y, scroll):
    """Columns 8-247 of line y as the panel shows them: it scrolls the frame
    buffer by its rotation (column x + scroll at x)."""
    line = fb[y * 256:(y + 1) * 256]
    line = line[scroll:] + line[:scroll]
    return line[8:248]


def frame_rows(fb, pal, scroll=0):
    """Rows 0-223, columns 8-247 of a drawn frame as 16-bit colors."""
    colors = [pal[2 * i] | pal[2 * i + 1] << 8 for i in range(256)]
    return [tuple(colors[b] for b in shown_line(fb, y, scroll)) for y in range(224)]


def row_pattern(row):
    """Which neighbouring pixels have the same color: the same for a drawn
    row and its picture on the panel, whatever the color conversion."""
    return bytes(1 if row[i] == row[i + 1] else 0 for i in range(len(row) - 1))


def check_tearing(calc, syms, name, shots=30, gap_ms=23, each=None):
    """Every screenshot's rows must all come from one of the last 8 drawn
    frames (a frame is snapshotted when render_draw is done), the same one
    (rows that look the same in several frames match any of them): a
    screenshot with a row from no recent frame (torn, at another rotation,
    or the panel stuck on an old frame) or rows from two frames (SMB3's
    status bar a frame older than the playfield) is torn. each(i), if
    given, runs before screenshot i."""
    from PIL import Image
    end = syms['pace_draw_end']	# drawn frames only (not ones pacing skipped)
    calc.cmd('snap %06X %06X %d' % (end, VBUFFER, VBUFFER_SIZE))
    calc.cmd('snap %06X %06X 512' % (end, LCD_PALETTE))
    calc.cmd('snap %06X %06X 1' % (end, FB_SCROLL))
    size = VBUFFER_SIZE + 512 + 1
    torn = []
    frames = []		# the last 8 drawn frames, as row patterns
    for i in range(shots):
        if each:
            each(i)
        calc.run(gap_ms)
        calc.cmd('panel-idle')		# a whole refresh of the panel
        png = calc.screenshot('tear-%s-%02d' % (name, i))
        path = os.path.join(OUT, 'snap-frames-%d.bin' % os.getpid())
        n = int(calc.cmd('snaps-all %06X %s' % (end, path)))
        data = open(path, 'rb').read()
        frames += [[row_pattern(r) for r in frame_rows(data[k * size:k * size + VBUFFER_SIZE],
                                                      data[k * size + VBUFFER_SIZE:(k + 1) * size - 1],
                                                      data[(k + 1) * size - 1])]
                   for k in range(n)]
        frames = frames[-8:]
        n = len(frames)
        if n < 3:		# the panel can be a frame behind the first ones snapshotted
            continue
        im = Image.open(png).convert('RGB')
        shown = [row_pattern([im.getpixel((x, y + 8)) for x in range(40, 280)]) for y in range(224)]
        # rows that tell frames apart, and which frames each could come from
        candidates = set(range(n))
        for y in range(224):
            match = {k for k in range(n) if frames[k][y] == shown[y]}
            if match and len(match) < n:
                candidates &= match
            if not match:		# a row of no recent frame: torn, shifted or old
                candidates = set()
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
    menu(calc, 'Resume')
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
    calc.cmd('snap %06X %06X 1' % (end, FB_SCROLL))
    calc.cmd('snap %06X %06X %d' % (end, VBUFFER, VBUFFER_SIZE))
    size = 2 + VBUFFER_SIZE
    frames = {}
    lap, last = 0, None
    for _ in range(count):
        F.wait(8)
        path = os.path.join(OUT, 'drawn-%d.bin' % os.getpid())	# (tests may run side by side)
        n = int(calc.cmd('snaps-all %06X %s' % (end, path)))
        data = open(path, 'rb').read()
        for k in range(n):
            fb = data[k * size + 2:(k + 1) * size]
            # the counter is 8 bits: count on past 255 (snaps come in order)
            if last is not None and data[k * size] < last:
                lap += 256
            last = data[k * size]
            frames[lap + last] = b''.join(shown_line(fb, y, data[k * size + 1]) for y in range(224))
    return frames


def test_rowskip():
    """Rows skipped because they didn't change give exactly the picture a
    full redraw gives (scenes without input, so both runs play the same)."""
    # (Zelda's story is drawn every other frame: a run may draw the other
    # half, so it is watched longer for enough frames in common)
    for game, start, count in [('MARIO3', 400, 14), ('TETRIS', 900, 14), ('ZELDA', 1100, 30),
                               ('FINALF', 300, 14)]:
        runs = []
        for skip_off in (0, 1):
            calc, syms, F = start_game(game, [('wait', start)], 'rowskip-%s.log' % game)
            calc.cmd('poke %06X %02X' % (syms['row_skip_off'], skip_off))
            runs.append(drawn_frames(calc, syms, count))
            calc.close()
        common = set(runs[0]) & set(runs[1])
        check(len(common) > 20, '%s: only %d frames to compare' % (game, len(common)))
        bad = sorted(f for f in common if runs[0][f] != runs[1][f])
        check(not bad, '%s: frames %s differ from a full redraw' % (game, bad[:5]))


def drawn_frames_wide(calc, syms, count=14):
    """{NES frame number: the shown part of the wide buffer} for the frames
    drawn over the next count*8 NES frames (as drawn_frames, wide picture)."""
    F = Frames(calc, syms)
    end = syms['pace_draw_end']
    size = 320 * 224
    calc.cmd('snap %06X D60F00 1' % end)	# current_frame
    calc.cmd('snap %06X %06X %d' % (end, calc.u24(syms['_wide_buffer']), size))
    frames = {}
    lap, last = 0, None
    for _ in range(count):
        F.wait(8)
        path = os.path.join(OUT, 'drawn-wide-%d.bin' % os.getpid())
        n = int(calc.cmd('snaps-all %06X %s' % (end, path)))
        data = open(path, 'rb').read()
        for k in range(n):
            if last is not None and data[k * (size + 1)] < last:
                lap += 256
            last = data[k * (size + 1)]
            fb = data[k * (size + 1) + 1:(k + 1) * (size + 1)]
            # columns 10-309: the ones the panel shows (partial mode)
            frames[lap + last] = b''.join(fb[y * 320 + 10:y * 320 + 310] for y in range(224))
    return frames


def drawn_frames_wide_shown(calc, syms, count=14):
    """{NES frame number: the wide picture as the panel shows it} for the
    frames drawn over the next count*8 NES frames: columns 10-309, column x
    from the wide buffer's x + S_w (its rotation, rowskip.asm fb_scroll_w)."""
    F = Frames(calc, syms)
    end = syms['pace_draw_end']
    size = 320 * 224
    calc.cmd('snap %06X D60F00 1' % end)	# current_frame
    calc.cmd('snap %06X %06X 3' % (end, syms['fb_scroll_w']))
    calc.cmd('snap %06X %06X %d' % (end, calc.u24(syms['_wide_buffer']), size))
    rec = 4 + size
    frames = {}
    lap, last = 0, None
    for _ in range(count):
        F.wait(8)
        path = os.path.join(OUT, 'drawn-wide-%d.bin' % os.getpid())
        n = int(calc.cmd('snaps-all %06X %s' % (end, path)))
        data = open(path, 'rb').read()
        for k in range(n):
            r = data[k * rec:(k + 1) * rec]
            if last is not None and r[0] < last:
                lap += 256
            last = r[0]
            sw = int.from_bytes(r[1:4], 'little')
            fb = r[4:]
            lines = []
            for y in range(224):
                line = fb[y * 320:(y + 1) * 320]
                line = line[sw:] + line[:sw]
                lines.append(line[10:310])
            frames[lap + last] = b''.join(lines)
    return frames


def test_rotation_wide(label=False):
    """The rotated wide picture (hardware scrolling) while SMB3 1-1 scrolls:
    the same picture as an unrotated one (as rotation_scroll). label: a state
    saved with [sto] first, so its message is drawn in both."""
    def run(rot_off, later):
        calc, syms, F = start_game('MARIO3', BENCH_PLAY['smb3-run'][1], 'rotation-wide.log')
        menu(calc, 'Picture')
        F.wait(30)
        calc.cmd('poke %06X %02X' % (syms['rot_off'], rot_off))
        F.wait(later)
        calc.cmd('poke %06X FF' % (NES_RAM + 0x553))
        calc.press(NES['right'])
        calc.press(NES['B'])
        F.wait(90)
        if label:
            # a message on screen all along (ui.asm overlay_message): one of
            # the program's texts, shown for 255 frames
            prog = calc.peek(syms['_main'] & ~0xFFF, 0x10000) + calc.peek((syms['_main'] & ~0xFFF) + 0x10000, 0x4000)
            text = syms['_main'] & ~0xFFF
            text += prog.index(b'Nothing saved in slot ')
            calc.cmd('poke %06X %s' % (SCANLINE_VARS - 75, text.to_bytes(3, 'little').hex()))
            calc.cmd('poke %06X FF' % (SCANLINE_VARS - 76))
        frames = drawn_frames_wide_shown(calc, syms, count=24)
        calc.close()
        return frames
    rotated = run(0, 0)
    best = None
    for later in range(4):
        plain = run(1, later)
        common = set(rotated) & set(plain)
        bad = sorted(f for f in common if rotated[f] != plain[f])
        if len(common) > 10 and (best is None or len(bad) < len(best)):
            best = bad
        if best == []:
            break
    check(best is not None, 'too few frames to compare')
    check(not best, 'wide frames %s differ from the unrotated picture' % best[:5])


def test_rotation_wide_label():
    """rotation_wide with a message drawn (ui.asm label_blit)."""
    test_rotation_wide(label=True)


def test_rowskip_wide():
    """The same in the wide picture: rows skipped and the backgrounds kept
    under sprites (w_sprite_save) give exactly the picture a full redraw gives."""
    for game, start in [('ZELDA', 60), ('MARIO3', 400), ('FINALF', 300)]:
        runs = []
        for skip_off in (0, 1):
            calc, syms, F = start_game(game, [('wait', start)], 'rowskip-wide-%s.log' % game)
            menu(calc, 'Picture')
            F.wait(30)
            calc.cmd('poke %06X %02X' % (syms['row_skip_off'], skip_off))
            runs.append(drawn_frames_wide(calc, syms))
            calc.close()
        common = set(runs[0]) & set(runs[1])
        check(len(common) > 10, '%s: only %d frames to compare' % (game, len(common)))
        bad = sorted(f for f in common if runs[0][f] != runs[1][f])
        check(not bad, '%s: wide frames %s differ from a full redraw' % (game, bad[:5]))


def test_rotation():
    """The rotated frame buffer (the panel scrolls it back) shows exactly the
    picture of an unrotated one, scrolling or not (scenes without input)."""
    for game, start in [('MARIO3', 400), ('ZELDA', 1100), ('FINALF', 300), ('TETRIS', 900)]:
        runs = []
        # a game drawn every other frame can draw the other half of the
        # frames without rotation: then start that run a frame or two later
        for rot_off, later in [(0, 0), (1, 0), (1, 1), (1, 2)]:
            calc, syms, F = start_game(game, [('wait', start + later)], 'rotation-%s.log' % game)
            calc.cmd('poke %06X %02X' % (syms['rot_off'], rot_off))
            runs[rot_off:] = [drawn_frames(calc, syms)]
            calc.close()
            if rot_off and len(set(runs[0]) & set(runs[1])) > 20:
                break
        common = set(runs[0]) & set(runs[1])
        check(len(common) > 20, '%s: only %d frames to compare' % (game, len(common)))
        bad = sorted(f for f in common if runs[0][f] != runs[1][f])
        check(not bad, '%s: frames %s differ from the unrotated picture' % (game, bad[:5]))


def test_rotation_scroll():
    """The rotated frame buffer while a level scrolls (SMB3 1-1, right held
    with a star): the same picture as an unrotated one. Every slot of the
    frame buffer gets drawn somewhere on the screen, slot 31 too (a tile
    drawn there used to land on every other line). The press is seen a
    frame or so apart in the two runs: the unrotated run that matches best
    counts."""
    def run(rot_off, later):
        calc, syms, F = start_game('MARIO3', BENCH_PLAY['smb3-run'][1], 'rotation-scroll.log')
        calc.cmd('poke %06X %02X' % (syms['rot_off'], rot_off))
        F.wait(later)
        calc.cmd('poke %06X FF' % (NES_RAM + 0x553))
        calc.press(NES['right'])
        calc.press(NES['B'])
        F.wait(90)
        frames = drawn_frames(calc, syms, count=30)
        calc.close()
        return frames
    rotated = run(0, 0)
    best = None
    for later in range(4):
        plain = run(1, later)
        common = set(rotated) & set(plain)
        bad = sorted(f for f in common if rotated[f] != plain[f])
        if len(common) > 20 and (best is None or len(bad) < len(best)):
            best = bad
        if best == []:
            break
    check(best is not None, 'too few frames to compare')
    check(not best, 'frames %s differ from the unrotated picture' % best[:5])


def check_tearing_wide(calc, syms, name, shots=20, gap_ms=31, each=None):
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
    calc.cmd('snap %06X %06X 3' % (end, syms['fb_scroll_w']))	# its rotation (the panel shows x + S_w at x)
    size = ws + 512 + 3
    torn, frames = [], []
    for i in range(shots):
        if each:
            each(i)
        calc.run(gap_ms)
        calc.cmd('panel-idle')
        png = calc.screenshot('tear-wide-%s-%02d' % (name, i))
        path = os.path.join(OUT, 'snap-frames-%d.bin' % os.getpid())
        n = int(calc.cmd('snaps-all %06X %s' % (end, path)))
        data = open(path, 'rb').read()
        for k in range(n):
            fb, pal = data[k * size:k * size + ws], data[k * size + ws:k * size + ws + 512]
            sw = int.from_bytes(data[k * size + ws + 512:(k + 1) * size], 'little')
            colors = [pal[2 * j] | pal[2 * j + 1] << 8 for j in range(256)]
            lines = [fb[y * 320:(y + 1) * 320] for y in range(224)]
            lines = [line[sw:] + line[:sw] for line in lines]
            frames.append([row_pattern([colors[b] for b in lines[y][48 if y >= 144 else 10:310]])
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
        F.wait(30)
        check(calc.u24(syms['_wide_buffer']), '%s: no wide buffer' % game)
        torn = check_tearing_wide(calc, syms, game)
        check(not torn, '%s: wide screenshots %s show torn or old frames' % (game, torn))
        calc.screenshot('wide-' + game)
        calc.close()


def bar_profiles(png):
    """SMB3's status bar (panel rows 196-225) as a column profile and a row
    profile (rows 186-235: the bar and what is next to it)."""
    from PIL import Image
    px = Image.open(png).convert('L').load()
    return ([sum(px[x, y] for y in range(196, 226)) for x in range(320)],
            [sum(px[x, y] for x in range(60, 260)) for y in range(186, 236)])


def bar_shift(png, ref):
    """(dx, dy): how far SMB3's status bar is in a screenshot from where
    the reference profiles (bar_profiles) have it, in pixels."""
    cols, rows = bar_profiles(png)
    dx = min(range(-40, 41), key=lambda s: sum(abs(cols[x] - ref[0][x - s]) for x in range(60, 260)))
    dy = min(range(-10, 11), key=lambda s: sum(abs(rows[y] - ref[1][y - s]) for y in range(10, 40)))
    return dx, dy


def smb3_scene(calc, syms, F, scene):
    """Puts SMB3 in a scene for the panel tests (from the bench's 1-1 start
    or the map): 'run' runs right with a star (nothing stops it), 'fly'
    makes Mario a raccoon flying up and right with a full P-meter (the
    camera moves both ways), 'map' waits. Returns each(i) to keep it going
    before screenshot i (about a NES frame apart)."""
    poke = lambda a, v: calc.cmd('poke %06X %02X' % (NES_RAM + a, v))
    if scene == 'map':
        return None
    if scene == 'fly':
        poke(0xED, 3)		# Player_Suit: raccoon
        F.wait(30)
    poke(0x553, 0xFF)		# a star
    calc.press(NES['right'])
    calc.press(NES['B'])
    F.wait(40)

    def each(i):
        poke(0x553, 0xFF)
        if scene == 'fly':
            poke(0x56E, 0xFF)	# Player_FlyTime
            poke(0x3DD, 0x7F)	# the P-meter, full
            (calc.press if i % 4 < 2 else calc.release)(NES['A'])
    return each


def smb3_panel_check(scenes, wide, fps=False, delay=False, shots=120, gap_ms=17):
    """SMB3 on the panel, for each scene of smb3_scene: every screenshot
    shows one whole recent frame (check_tearing: no row from another frame,
    so the status bar and the playfield come from the same NES frame) and
    the status bar where the first screenshot has it (bar_shift: not moved
    sideways by the frame buffer's rotation, nor up or down). delay: frames
    drawn at other phases of the LCD (frame_delay). fps: the FPS counter on."""
    kind = ('wide' if wide else 'normal') + (' fps' if fps else '') + (' phases' if delay else '')
    failed = []
    for scene in scenes:
        name = '%s-%s' % (scene, kind.replace(' ', '-'))
        calc, syms, F = start_game('MARIO3', SMB3_TO_MAP if scene == 'map' else BENCH_PLAY['smb3-run'][1],
                                   'panel-%s.log' % name)
        if wide:
            menu(calc, 'Picture')
        if fps:
            F.wait(60)
            menu(calc, 'Show FPS')
        set_delay = frame_delay(calc, syms) if delay else None
        F.wait(60)
        check(bool(calc.u24(syms['_wide_buffer'])) == wide and calc.u8(syms['_show_fps']) == fps,
              '%s: the picture or the FPS counter is not as asked' % name)
        keep = smb3_scene(calc, syms, F, scene)
        ys = set()

        def each(i):
            if keep:
                keep(i)
            if set_delay:
                set_delay(i * 389 % 1500)
            ys.add(calc.peek(SCANLINE_VARS + 2, 3).hex())	# the PPU's x, y scroll at the frame's start
        torn = (check_tearing_wide if wide else check_tearing)(calc, syms, name, shots=shots, gap_ms=gap_ms,
                                                             each=each)
        calc.close()
        pngs = [os.path.join(OUT, ('tear-wide-%s-%02d.png' if wide else 'tear-%s-%02d.png') % (name, i))
                for i in range(shots)]
        ref = bar_profiles(pngs[0])
        moved = [(i, s) for i, s in ((i, bar_shift(png, ref)) for i, png in enumerate(pngs)) if s != (0, 0)]
        if scene == 'fly':
            check(len({y[2:] for y in ys}) >= 10, '%s: the camera did not move up (%d y scrolls)' % (name, len(ys)))
        if scene != 'map':
            check(len({y[:2] for y in ys}) >= 10, '%s: the camera did not move right' % name)
        if moved:
            failed.append('%s: the status bar moved in screenshots %s (by %s)'
                          % (name, [m[0] for m in moved[:8]], [m[1] for m in moved[:8]]))
        if torn:
            failed.append('%s: screenshots %s show a torn frame or rows of two frames' % (name, torn[:12]))
    check(not failed, '; '.join(failed))


def test_panel_frames(fps=False):
    """What the panel shows, not just what is drawn: a frame must reach it
    whole and with the rotation it was drawn for (smb3_panel_check). SMB3
    1-1 running right (the status bar stays put while the level scrolls
    under it), flying (the camera moves up too) and the map (its animation
    redraws many tiles: a frame that takes longer than estimated mustn't
    show half drawn), normal and wide."""
    failed = []
    for wide in (False, True):
        try:
            smb3_panel_check(['run', 'fly', 'map'], wide, fps=fps)
        except TestFailure as e:
            failed.append(str(e))
    check(not failed, '; '.join(failed))


def test_panel_zelda():
    """Zelda's overworld from the 0.2.1 state (slot 0, as tools/perf/zplayfps.py:
    its code runs in WRAM), Link walking right into the next screen: the HUD
    (panel rows 16-55) stays put while the playfield scrolls under it, normal
    and wide, and every screenshot shows rows of one recent frame (normal)."""
    from PIL import Image

    def hud(png):
        px = Image.open(png).convert('L').load()
        return [sum(px[x, y] for y in range(16, 56)) for x in range(320)]

    def shift(p, ref):
        return min(range(-40, 41), key=lambda s: sum(abs(p[x] - ref[x - s]) for x in range(60, 260)))
    for wide in (False, True):
        kind = 'wide' if wide else 'normal'
        calc, syms, F = start_game('ZELDA', [('wait', 120)], 'panel-zelda-%s.log' % kind)
        if wide:
            menu(calc, 'Picture')
            F.wait(60)
        calc.key('ln', ms=100, after=100)	# the 0.2.1 state (slot 0)
        F.wait(240)
        calc.press(NES['right'])
        F.wait(90)			# Link at the screen's right edge
        shots, torn, pngs = 90, [], []
        if wide:
            for i in range(shots):
                calc.run(17)
                calc.cmd('panel-idle')
                pngs.append(calc.screenshot('zelda-wide-%02d' % i))
        else:
            torn = check_tearing(calc, syms, 'zelda-normal', shots=shots, gap_ms=17)
            pngs = [os.path.join(OUT, 'tear-zelda-normal-%02d.png' % i) for i in range(shots)]
        calc.close()
        moved = max(screen_difference(pngs[0], png) for png in pngs[::5])
        check(moved > 0.2, '%s: the next screen did not scroll in (%.0f%% changed at most)' % (kind, 100 * moved))
        ref = hud(pngs[0])
        shifts = [shift(hud(png), ref) for png in pngs]
        bad = [i for i, sh in enumerate(shifts) if sh]
        check(not bad, '%s: the HUD moved in screenshots %s (by %s)' % (kind, bad[:8], [shifts[i] for i in bad[:8]]))
        check(not torn, '%s: screenshots %s show torn frames' % (kind, torn))


def test_panel_frames_fps():
    """panel_frames with the FPS counter on (the panel is locked for its
    numbers once a second, and frames end at other phases of the LCD)."""
    test_panel_frames(fps=True)


def frame_delay(calc, syms):
    """Test knob: makes pace_render_frame wait first (timer 1, no LCD reads),
    so frames are emulated and drawn at other phases of the LCD. Patches the
    program in the emulator's RAM (its first 4 bytes, `ld a,(ix-102)` and
    `or a,a`, become a call to a routine at jit_reserved, which waits and
    does them); returns set(units) to change the wait."""
    at = syms['pace_render_frame']
    check(calc.peek(at, 4) == bytes.fromhex('DD7E9AB7'), 'pace_render_frame changed: update frame_delay')
    routine = 0xD5FF00		# jit_reserved (vars.inc), unused below +128
    # push hl, push de, ld hl,(timer 1 / 256), ld de,wait, add hl,de, ex de,hl,
    # loop: ld hl,(timer 1 / 256), or a,a, sbc hl,de, jr c,loop, pop de, pop hl,
    # ld a,(ix-102), or a,a, ret
    calc.cmd('poke %06X %s' % (routine, 'E5D5' '2A0100F2' '11000000' '19EB' '2A0100F2' 'B7ED52' '38F7'
                                         'D1E1' 'DD7E9A' 'B7C9'))
    calc.cmd('poke %06X CD%s' % (at, routine.to_bytes(3, 'little').hex().upper()))

    def set_delay(units):
        calc.cmd('poke %06X %s' % (routine + 7, (units & 0xFFFFFF).to_bytes(3, 'little').hex().upper()))
    return set_delay


def test_panel_phases():
    """panel_frames with frames drawn at other phases of the LCD (a wait
    before each frame, changed for every screenshot, frame_delay): the
    guards that lock the panel before a piece of the drawing must hold
    wherever the drawing starts and ends, and so must the unlock."""
    failed = []
    for wide in (False, True):
        try:
            smb3_panel_check(['run', 'fly', 'map'], wide, delay=True)
        except TestFailure as e:
            failed.append(str(e))
    check(not failed, '; '.join(failed))


def test_fps():
    """Every benchmark scene runs at full speed and draws a fair share of frames."""
    scenes = [b[0] for b in BENCH if b[0] not in BENCH_UNTESTED] + list(BENCH_PLAY)
    for name, (nes, drawn) in bench(scenes=scenes).items():
        check(58 <= nes <= 62, '%s runs at %.1f NES frames/s' % (name, nes))
        check(drawn >= 25, '%s draws %.1f frames/s' % (name, drawn))


def test_menus():
    """The pause menu's pages (screenshots menu-*): saving over a used slot
    asks first, [clear] answers no and goes back a page, the Load list
    deletes with [del], and [clear] on the main page resumes the game."""
    calc, syms, F = start_game('MARIO3', SMB3_TO_MAP, 'menus.log')
    menu(calc, 'Save state', 3)
    menu(calc, 'Save state', 12)
    check({'MARIO3S3', 'MARIO312'} <= set(calc.vars()), 'states 3 and 12 not saved')
    calc.count(syms['create_savestate'])

    def key(name, after=200):
        calc.key(name, ms=100, after=after)
    key('clear', 400)
    calc.screenshot('menu-main')
    key('down')
    calc.screenshot('menu-main-save')
    key('2nd', 400)
    calc.screenshot('menu-save')
    key('3')
    key('2nd')
    calc.screenshot('menu-save-confirm')
    key('clear')
    key('clear', 400)
    check(calc.counts()[1][syms['create_savestate']] == 0, 'saved over slot 3 without asking')
    key('down')
    key('2nd', 400)
    calc.screenshot('menu-load')
    key('up')
    key('del')
    calc.screenshot('menu-load-delete')
    key('2nd', 400)
    calc.screenshot('menu-load-deleted')
    check('MARIO3S3' not in calc.vars() and 'MARIO312' in calc.vars(), 'the Load list deleted the wrong state')
    key('clear', 400)
    key('down')
    key('2nd', 400)
    calc.screenshot('menu-settings')
    key('clear', 400)
    key('down')
    key('2nd', 400)
    calc.screenshot('menu-controls')
    key('clear', 400)
    key('down')
    key('2nd')
    calc.screenshot('menu-reset-confirm')
    key('clear')
    key('clear', 1500)
    shown = calc.screenshot('menu-closed')
    from PIL import Image
    top = Image.open(shown).convert('RGB').crop((0, 0, 320, 8))
    check(max(max(p) for p in top.getdata()) < 40, 'the menu did not close')
    check(calc.counts()[1][syms['create_savestate']] == 0, 'something was saved')
    calc.close()


def state_file(calc, name):
    """An appvar's data (after its 2 size bytes)."""
    path = os.path.join(OUT, name + '.bin')
    calc.cmd('receive-file %s 15 %s' % (name, path))
    return open(path, 'rb').read()[2:]


def test_save_on_quit():
    """Leaving a game saves it to the Auto save (with a picture), and the
    library continues from it; [del] in a game saves too, then quits and
    puts the calculator's brightness back."""
    backlight = None
    calc, syms, F = start_game('MARIO3', SMB3_TO_MAP, 'quit.log')
    calc.cmd('snap %06X %06X 2048' % (syms['create_savestate'], NES_RAM))
    calc.cmd('snap %06X %06X 2048' % (syms['flush_bank_cache'], NES_RAM))
    calc.count(syms['load_savestate'])
    calc.count(syms['_ui_cleanup'])
    menu(calc, 'Change game', confirm=True)
    n, saved = snaps(calc, syms['create_savestate'])
    check(n == 1, 'leaving the game did not save it')
    v = calc.vars()
    check('MARIO3AU' in v and v['MARIO3AU'][2], 'no archived Auto save (MARIO3AU)')
    data = state_file(calc, 'MARIO3AU')
    check(data[:3] == b'\xFCST' and data[4] & 1, 'the Auto save has no picture')
    calc.screenshot('quit-library')
    loads = calc.counts()[1][syms['load_savestate']]
    calc.key('2nd', after=3000)		# Continue
    check(calc.counts()[1][syms['load_savestate']] == loads + 1, 'Continue did not load the Auto save')
    _, loaded = snaps(calc, syms['flush_bank_cache'])
    check(loaded == saved, 'Continue did not bring the game back as it was')
    F.wait(30)
    calc.key('del', after=3000)
    check(snaps(calc, syms['create_savestate'])[0] == n + 1, '[del] did not save the game first')
    check(calc.counts()[1][syms['_ui_cleanup']] == 1, '[del] did not quit FamiCalc')
    calc.close()


def test_game_options():
    """The library's game options: Start over plays without a state, Play
    from a state loads the one picked, Delete all states deletes them."""
    calc, syms, F = start_game('MARIO3', SMB3_TO_MAP, 'options.log')
    calc.cmd('snap %06X %06X 2048' % (syms['create_savestate'], NES_RAM))
    calc.cmd('snap %06X %06X 2048' % (syms['flush_bank_cache'], NES_RAM))
    menu(calc, 'Save state', 5)
    _, slot5 = snaps(calc, syms['create_savestate'])
    F.hold('right', 40)
    F.wait(60)
    menu(calc, 'Change game', confirm=True)
    calc.count(syms['load_savestate'])
    # Start over (the options open on Continue)
    for k in ('alpha', 'down', 'down'):
        calc.key(k, after=500)
    calc.screenshot('options-start-over')
    calc.key('2nd', after=3000)
    check(calc.counts()[1][syms['load_savestate']] == 0, 'Start over loaded a state')
    # Play from a state: slot 5
    menu(calc, 'Change game', confirm=True)
    calc.key('alpha', after=500)
    calc.key('down', after=500)
    calc.key('2nd', after=800)
    calc.screenshot('options-from-state')
    calc.key('5', after=500)
    calc.key('2nd', after=3000)
    check(calc.counts()[1][syms['load_savestate']] == 1, 'Play from a state did not load it')
    _, loaded = snaps(calc, syms['flush_bank_cache'])
    check(loaded == slot5, 'Play from a state did not load slot 5')
    # Delete all states (the last option, above the first)
    menu(calc, 'Change game', confirm=True)
    calc.key('alpha', after=500)
    calc.key('up', after=500)
    calc.key('2nd', after=600)
    calc.screenshot('options-delete')
    calc.key('2nd', after=2000)
    left = [k for k in calc.vars() if k.startswith('MARIO3') and len(k) == 8 and k[6:] not in
            ['P%X' % i for i in range(16)] + ['C%X' % i for i in range(16)] + ['SV']]
    check(not left, 'Delete all states left %s' % left)
    calc.close()


def test_hotkeys():
    """In a game: ) and ( pick the next and the previous slot, log loads the
    newest state, vars turns the FPS counter on and off, + and - change the
    brightness."""
    calc, syms, F = start_game('MARIO3', SMB3_TO_MAP, 'hotkeys.log')
    slot = lambda: calc.u24(syms['_save_slot'])
    calc.key(')', after=300)
    check(slot() == 1, ') picked slot %d' % slot())
    calc.key('(', after=300)
    calc.key('(', after=300)
    check(slot() == 999, '( picked slot %d' % slot())
    calc.screenshot('hotkey-slot')
    calc.key('vars', after=600)
    check(calc.peek(syms['_show_fps'], 1)[0] == 1, 'vars did not turn the FPS counter on')
    F.wait(120)
    calc.screenshot('hotkey-fps')
    calc.key('vars', after=600)
    check(calc.peek(syms['_show_fps'], 1)[0] == 0, 'vars did not turn the FPS counter off')
    b0 = calc.peek(BACKLIGHT, 1)[0]
    calc.key('-', after=300)
    b1 = calc.peek(BACKLIGHT, 1)[0]
    check(b1 > b0, '- did not darken the screen (%d -> %d)' % (b0, b1))
    calc.key('+', after=300)
    check(calc.peek(BACKLIGHT, 1)[0] == b0, '+ did not undo it')
    # log: the newest state (sto saves to slot 999 now)
    calc.cmd('snap %06X %06X 2048' % (syms['create_savestate'], NES_RAM))
    calc.cmd('snap %06X %06X 2048' % (syms['flush_bank_cache'], NES_RAM))
    calc.count(syms['load_savestate'])
    calc.key('sto', after=2000)
    _, saved = snaps(calc, syms['create_savestate'])
    F.hold('right', 40)
    F.wait(60)
    calc.key('log', after=2000)
    check(calc.counts()[1][syms['load_savestate']] == 1, 'log did not load a state')
    _, loaded = snaps(calc, syms['flush_bank_cache'])
    check(loaded == saved, 'log did not load the newest state')
    calc.close()


def test_controls():
    """Controls: a hotkey moved to another key works there; the NES's
    buttons can move too (their table changes)."""
    calc, syms, F = start_game('MARIO3', SMB3_TO_MAP, 'controls.log')
    def key(name, after=400):
        calc.key(name, ms=100, after=after)
    key('clear', 600)
    for _ in range(MENU_ITEMS.index('Controls')):
        key('down', 150)
    key('2nd', 600)
    key('2nd', 500)		# A
    key('graph', 600)
    check(calc.peek(syms['_nes_keys'], 2) == bytes([1, 1]), 'A is not on graph: %s' % calc.peek(syms['_nes_keys'], 2).hex())
    key('2nd', 500)
    key('2nd', 600)		# A back on 2nd
    key('right', 500)		# Hotkeys
    key('2nd', 500)		# Save state
    key('graph', 600)
    calc.screenshot('controls-moved')
    key('clear', 400)
    key('clear', 400)
    calc.run(1000)
    calc.count(syms['create_savestate'])
    key('graph', 2000)
    check(calc.counts()[1][syms['create_savestate']] == 1, 'graph did not save after moving Save state there')
    calc.close()


def test_pictures_off():
    """State pictures off: states are saved without one."""
    calc, syms, F = start_game('MARIO3', SMB3_TO_MAP, 'nopic.log')
    menu(calc, 'State pictures')
    menu(calc, 'Save state', 3)
    data = state_file(calc, 'MARIO3S3')
    check(data[:3] == b'\xFCST' and not data[4] & 1, 'a picture was saved with pictures off')
    calc.close()



TESTS = {'boot': test_boot, 'save_load': test_save_load, 'state_games': test_state_games,
         'old_states': test_old_states, 'menus': test_menus,
         'fps_display': test_fps_display, 'leave_game': test_leave_game, 'resume': test_resume, 'tearing': test_tearing, 'rowskip': test_rowskip,
         'rowskip_wide': test_rowskip_wide, 'rotation_wide': test_rotation_wide, 'rotation_wide_label': test_rotation_wide_label, 'rotation': test_rotation, 'rotation_scroll': test_rotation_scroll, 'wide': test_wide,
         'fps': test_fps, 'save_on_quit': test_save_on_quit, 'game_options': test_game_options,
         'hotkeys': test_hotkeys, 'controls': test_controls, 'pictures_off': test_pictures_off,
         'panel_frames': test_panel_frames, 'panel_phases': test_panel_phases, 'panel_zelda': test_panel_zelda,
         'panel_frames_fps': test_panel_frames_fps, 'jit_mappers': test_jit_mappers,
         'sprites_line0': test_sprites_line0, 'lcd_timeouts': test_lcd_timeouts,
         'lcd_reads': test_lcd_reads, 'panel_sprites': test_panel_sprites,
         'wram_code': test_wram_code}


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
        for name in [b[0] for b in BENCH] + list(BENCH_PLAY):
            print('%-12s' % name + ''.join('%11.1f /%5.1f' % table[l][name] for l, _ in progs))
        print('(NES frames per second / frames drawn per second)')
        return 0
    if args[0] == 'profile':
        # profile SCENE [wide] [program.8xp]
        name = args[1]
        wide = 'wide' in args[2:]
        rest = [a for a in args[2:] if a != 'wide']
        prog = rest[0] if rest else None
        game, steps, play = ([(g, st, None) for n, g, st in BENCH if n == name]
                             or [BENCH_PLAY[name]])[0]
        syms = symbols(prog)
        calc = start_famicalc([game], prog)
        select_game(calc, [game], game)
        F = Frames(calc, syms)
        run_steps(F, steps)
        if wide:
            menu(calc, 'Picture')
            F.wait(60)
        for share, place in profile(calc, syms, wide=wide, play=play)[:int(os.environ.get("FC_TOP", "45"))]:
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
