# FamiCalc developer notes

Notes for working on this fork (branch `menu-and-speed`). The upstream README
covers building and using FamiCalc; this file covers how the parts added here
work, how to test on a PC, and what was learned along the way.

## Building

CE C toolchain **v14.2** (v15.0 fails to link FamiCalc). `make` gives
`bin/FAMICALC.8xp` and `bin/FAMICALC.map`. The map lists only symbols that
something references; a label you want to find in tests must be referenced.

fasmg rules that bite: `public`/`private` must be declared in the section that
defines the label; a label without a dot starts a new namespace for `.local`
labels after it; `jr` is ±127 bytes.

## Memory map (vars.inc)

| Range | Use |
|---|---|
| `$D031F6-$D177B6` | C `.bss` and asm `.bss` (render_cache 45 KB, NES SRAM 32 KB, ...); ~1.6 KB free |
| `$D40000` | CHR RAM (8 KB) / render_banks |
| `$D42000-$D4FFFF` | the game's frame buffer: 256x224, 8 bpp, 256 bytes per line |
| `$D50000` page | NES RAM (MB-relative), scanline events, JIT cache (60 KB) |
| `$D60000-$D657FF` | tile set, OAM, palettes, translation tables, scanline vars (ix) |
| `$E30800-$E30BFF` | LCD cursor RAM: hot render code (cheaper to fetch than RAM); `$E30BC0` holds jit_search, draw_tile or sprite_outer depending on the moment |
| free user RAM | extended JIT cache (`jit_cache_extend`) |

`ui_cleanup` clears pixelShadow (`$D031F6`), i.e. the C program's variables;
only call it when FamiCalc exits (`ui_restart` re-initialises the screen
between games).

## Save states

* Slots 0-999, appvar `ROMNAME` + 2 characters (`states.c`): 0-9 `S0`-`S9`
  (old FamiCalc names), 10-99 two digits, 100-999 `a0`-`yz`.
* New files start with a 16 byte header (`FC 'S' 'T'`, version, date/time);
  the LZ data follows. Headerless files are older states.
* **FamiCalc 0.2.1** (what the calculator had) saved the 256 bytes of scanline
  variables from `ix+0`; 0.2.2 and later from `ix-128`. `state_block_is_v021`
  tells them apart by the `t_nametable` pointers (`$D62xxx-$D63xxx`), and such
  states are loaded the way 0.2.1 loaded them.
* `[sto]`/`[ln]` act once per press.

## Pause menu and ROM list (menu.c, pause.asm, ui.asm)

* `[clear]` in a game -> `open_menu` saves the emulator's registers, switches
  to the C stack and calls `pause_menu()`; the result is resume / save / load /
  leave (`exit_reason`: reset, ROM list, quit).
* Menus use the whole 320x240 screen (`ui_screen_full`, 8 bpp, 320 bytes per
  line at `$D42000`). That frame buffer runs into `$D50000`, so the pause menu
  copies the game's picture (half size) and that memory into `render_cache`
  and restores them on closing; **the tile cache must be flushed afterwards**
  (`flush_bank_cache` in `open_menu`).
* Colours: palette entries `$E0-$EC` (`C_*` in menu.c and vars.inc).
* Text: the 8x8 font with proportional spacing (`glyph_left/width`), drawn by
  `ui_glyph`/`ui_fill` (asm, stride `ui_stride`).
* In-game labels (FPS counter, messages) are drawn by `overlay_label` after
  each drawn frame.

## Frame pacing (pacing.asm)

* Timer 1 runs freely at 48 MHz; times are in units of 256 cycles. Each NES
  frame has a deadline (3120 units = 60.096 Hz); the emulator waits when it is
  early.
* The LCD refreshes exactly once per NES frame (`lcdTiming`: HFP 13, VFP 135,
  798,720 cycles) and the deadlines are lined up with the start of the LCD's
  transfers (`lcd_ref`, re-measured from the VCOMP flag while waiting).
* `pace_draw_start`: if the frame will be drawn before the LCD's next transfer
  (estimated from the last drawing time, `draw_estimate`), draw it behind the
  LCD's reads (they are ~2.6x faster than drawing) without stopping the panel.
  Otherwise wait for the transfer to end and lock the panel (`spiLock`) until
  the frame is done; `pace_unlock` unlocks between transfers at every frame end.
* A frame is drawn when the emulator is no later than it can still draw before
  the next transfer; skipped frames catch up; at least every 4th is drawn.
* Before this, the frameskip heuristic let games run at up to 74 frames per
  second (178 in Final Fantasy's prologue) and drew at most 30.

## Row skipping (rowskip.asm)

Before each tile row of the background is drawn, `row_unchanged` compares its
place and scroll with the same row of the last drawn frame and, if those
match, a hash of its tiles' cache entries. Unchanged rows are not drawn unless
sprites or labels covered their lines in the last drawn frame
(`row_note_overlays`, sprites are drawn from row `OAM y - 8`). `row_forget`
(cache flushes, menu, state loads, blank frames) makes the next frame draw
everything. `row_skip_off` turns it off for tests.

## Wide picture (rowskip.asm row_scale, ui.asm, main.c set_wide)

* Pause menu, Picture: Wide. The game is still drawn into vbuffer (256 per
  line); the lines that changed this frame (drawn rows, `lines_drawn`, and
  this frame's sprite rows) are stretched into a 320x224 buffer, every 4th
  pixel twice (`scale_line`, ~15 cycles per pixel). Labels are drawn straight
  into the wide buffer.
* The buffer (70 KB) is reserved at the top of free RAM (`set_wide`), out of
  the JIT's extra cache; switching restarts the emulator from its state
  (`state_reload_only`: create_savestate without a file) so the JIT forgets
  the memory it lost. Needs `WIDE_SIZE + WIDE_MIN_FREE` free RAM.
* The LCD can't do more than 1024 pixels per line: wide mode sends 70 lines of
  1024 (`lcdTimingWide`), same period; the panel window is 320x224 without a
  partial area (`spiInitVSyncWide`); a transfer takes 1138 units instead of
  910 (`lcd_transfer_time`).
* Drawn fps in wide mode: SMB3 1-1 23, SMB3 map 28, Tetris 41, FF1 prologue
  60, Zelda title 15. Drawing straight at 320 per line (10-pixel tiles in the
  tile cache, wide sprite loops) would roughly halve the cost of changed lines.

## Pitfalls met here

* At the end of `load_state_from_buffer`, bc holds the NES's X and Y (and af'
  A, the alternate set S and P): anything added there must keep them. Losing
  them crashed Tetris after a load; `test_state_games` checks every game.
* The menus borrow render_cache (the tile cache): flush it afterwards.
* With frames lined up to the LCD, "unlock the panel when the LCD is between
  transfers" checks at event scanlines kept missing; `pace_unlock` runs at
  every frame end.

## PC testing (tests/)

* `tests/unit`: `make -C tests/unit` builds and runs host tests of `states.c`.
* `tests/pc`: `make -C tests/pc` builds `build/fc-headless`, CEmu's core
  (with its debugger, LCD DMA and panel emulation) driven over stdin; then
  `python3 tests/pc/fctest.py all` (or a test name, `bench`, `profile SCENE`).
  It needs:
  * a CE ROM dumped from a calculator (`FC_ROM`, default
    `~/ti84ce/rom/ti84pce.rom`; CEmu's DUMP program, ROMData parts rebuilt by
    `~/ti84ce/rom/build_rom.py`);
  * the CE libraries and the jailbreak program `A` (arTIfiCE 2.1 shell for OS
    5.8) in `FC_CALCFILES`, games converted to appvars in `FC_GAMES`.
  FamiCalc is started exactly as on the calculator: `[prgm]` A, the arTIfiCE
  shell, FAMICALC. Inputs are timed in NES frames (`Frames`), FPS is measured
  in emulated time, screenshots come from the emulated panel.
* Harness commands besides CEmu's: `press/release`, `peek/poke`, `count`
  (execution counters), `profile` (program counter sampling, labelled with the
  map), `snap`/`snaps-all` (copy memory each time an address runs), `vars`,
  `receive-file`.
* Tests: boot, save/load (slots 12/345/999 restore NES RAM byte for byte),
  save and load in every game, old 0.2.1 states, FPS label, leaving a game, Resume, tearing (every
  screenshot shows rows of one recent frame), row skipping (frames with and
  without skipping are identical in scenes without input), FPS of the
  benchmark scenes.
* Same-build runs are deterministic; between two builds, a button press can
  last a frame more or less (inputs are polled every 50 ms), so compare
  builds only in scenes without input.
* CEmu quirk: right after `spiSetup`'s panel reset, CEmu's LCD can stop
  producing frames for a while (it ties the LCD to the emulated panel). LCD
  waits time out after ~25 ms (`lcd_wait_frame`).

Benchmark (`fctest.py bench`, NES fps / drawn fps):

| scene | v0.2.1 | upstream 3202ea4 | this branch |
|---|---|---|---|
| SMB3 1-1 | 64.5 / 29.2 | 71.9 / 29.1 | 59.8 / 47.0 |
| SMB3 map | 65.0 / 30.0 | 73.7 / 29.9 | 60.1 / 60.1 |
| Tetris | 60.0 / 30.0 | 74.8 / 29.9 | 60.1 / 60.1 |
| FF1 prologue | 178.7 / 40.2 | 71.8 / 35.9 | 60.1 / 60.1 |
| Zelda title | 68.2 / 29.9 | 68.2 / 30.0 | 60.0 / 30.0 |

Where the time goes (profile of SMB3 1-1): copying tile rows (`draw_tile`,
~7-10 cycles per pixel: RAM reads cost 4 cycles, writes 2) is most of a drawn
frame; emulating a frame takes ~240k of the 800k cycles.

## ROM files

TI-OS variables hold at most ~64 KB, so a game stays several appvars on the
calculator (`NAME`, `NAMEP0..`, `NAMEC0..`). Both converters (`convert.py` for
the web page, `tools/converter.pyw`) already write one `.b84` bundle per game
that TI Connect CE sends in one go (convbin can also make `.8xg` groups, which
TiLP reads). Ways to go further: let a PC tool convert and send a `.nes` in one
step, or let FamiCalc import a `.nes` from a USB stick (usbdrvce, msddrvce,
fatdrvce) into appvars itself.
