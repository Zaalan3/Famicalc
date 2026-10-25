# FamiCalc developer notes

Notes for working on this fork (branch `menu-and-speed`). The upstream README
covers building and using FamiCalc; this file covers how the parts added here
work, how to test on a PC, and what was learned along the way.

## Building

CE C toolchain **v14.2** (v15.0 fails to link FamiCalc). `make` gives
`bin/FAMICALC.8xp` and `bin/FAMICALC.map`. The map lists only symbols that
something references; a label you want to find in tests must be referenced.

`tools/deploy.sh` builds, runs the unit and PC tests, keeps the build in
~/ti84ce/builds/famicalc, backs up the calculator's FAMICALC, sends the new one
into Archive with ce-link and reads it back (`--no-test`, `--check`; log in
obj/deploy.log). The link needs the calculator on the home screen (it answers
`ce-link info` anywhere), so the script waits for that.

fasmg rules that bite: `public`/`private` must be declared in the section that
defines the label; a label without a dot starts a new namespace for `.local`
labels after it; `jr` is ±127 bytes.

## Memory map (vars.inc)

| Range | Use |
|---|---|
| `$D031F6-$D177B6` | C `.bss` and asm `.bss` (render_cache 45 KB, NES SRAM 32 KB, ...); ~1.6 KB free |
| `$D40000` | CHR RAM (8 KB) / render_banks |
| `$D42000-$D4FFFF` | the game's frame buffer: 256x224, 8 bpp, 256 bytes per line (unused by the wide picture) |
| `$D50000` page | NES RAM (MB-relative), scanline events, JIT cache (60 KB) |
| `$D60000-$D657FF` | tile set, OAM, palettes, translation tables, scanline vars (ix) |
| `$E30800-$E30BFF` | LCD cursor RAM: hot render code (cheaper to fetch than RAM), the normal picture's (render.asm) or the wide one's (renderwide.asm), copied by `render_load_code`; `$E30BC0` holds jit_search, draw_tile or sprite_outer depending on the moment |
| free user RAM | extended JIT cache (`jit_cache_extend`) |

`ui_cleanup` clears pixelShadow (`$D031F6`), i.e. the C program's variables;
only call it when FamiCalc exits (`ui_restart` re-initialises the screen
between games).

## Save states

* Slots 0-999, appvar `ROMNAME` + 2 characters (`states.c`): 0-9 `S0`-`S9`
  (old FamiCalc names), 10-99 two digits, 100-999 `a0`-`yz`.
* New files start with a 16 byte header (`FC 'S' 'T'`, version, flags,
  date/time, the picture's offset); the LZ data follows, then a picture
  (State pictures). Headerless files are older states.
* **FamiCalc 0.2.1** (what the calculator had) saved the 256 bytes of scanline
  variables from `ix+0`; 0.2.2 and later from `ix-128`. `state_block_is_v021`
  tells them apart by the `t_nametable` pointers (`$D62xxx-$D63xxx`), and such
  states are loaded the way 0.2.1 loaded them.
* `[sto]`/`[ln]` act once per press.

## Screens (menu.c, pictures.c, pause.asm, ui.asm)

* `[clear]` in a game -> `open_menu` saves the emulator's registers, switches
  to the C stack and calls `pause_menu()`; the result is resume / save / load /
  leave (`exit_reason`: reset, ROM list, quit). `rom_menu()` is the library.
* One design system (mockups and their code: ~/ti84ce/builds/famicalc/design):
  a header (title in the display font, a subtitle, the clock and battery at
  the right; a back arrow in the margin), a list on the left (rows 18 apart
  from y 44, text at x 16, the focused row on a rounded pill), a detail pane
  at the right margin (a 120x112 picture with a shadow, a hairline and round
  corners, then a caption: what the row does), key caps at y 219. Dialogs
  (`dialog`/`confirm`) ask before saving over a state, deleting, resetting,
  leaving; everything behind them dims through the palette (they have their
  own entries). The background is a dark gradient tinted by the pane's
  picture (`finish`: its average color; bands are palette entries, so the
  tint is set after drawing).
* Pause menu: Resume, Save state, Load state, Settings, Controls, Reset
  game, Change game, Quit FamiCalc (the pane shows the game when the menu
  opened, or the newest state on Load). Its pages and the library's run in
  `sub_page`: Save state (all 1000 slots), Load state (the used ones, the
  Auto save first), Settings, Controls. In the slot lists the number keys
  type a slot, left/right move by ten, `[del]` deletes. Saving or loading a
  numbered slot sets `save_slot` (what `[sto]`/`[ln]` use in a game). The
  used slots are a bitmap (`scan_states`).
* Library: the games played last first (`find_roms` sorts by the newest
  state's date, then by title). `[2nd]` and the number keys continue (load
  the newest state) or start a game; `[alpha]`: Continue, Play from a state
  (the Load page for that game), Start over, Delete all states; `[mode]`:
  Settings (with a Controls row; Picture applies to the next game there).
  The state to start from goes to `start_slot`; keys.asm loads it at the
  first frame (`start_load`), `getSaveSlot` takes it instead of `save_slot`.
* Settings (FamiCfg version 3): Picture, Show FPS, Brightness (1-10: the
  backlight register 0xF60024 = (10 - level) * 25; the calculator's own
  value is put back on the way out), Save on quit, State pictures, the keys.
* Controls: the NES's A, B, Select, Start (`nes_keys`, keys.asm; the D-pad
  stays on the arrows) and 8 hotkeys (`hot_keys`): keypad group and mask
  each, taken from the next key pressed ([clear], [del] and the arrows
  can't be taken). The keypad drawing lights a key from its group and bit
  (`keypad`: rows 3-9 are the matrix itself).
* Hotkeys: `set_save_slot` (every frame) checks the table, once per press
  (`hot_state`): save and load go straight to create/load_savestate, the
  others through `open_hotkey` -> `c_call` -> `hotkey()` (menu.c), which
  returns what to do like the menu: load the newest (`start_slot`), next or
  previous slot, FPS counter (`ui_screen_game` sets the panel up again;
  `fps_panel_erase` blacks out the wide picture's strip), brightness. Their
  messages use the in-game label (`MESSAGE_PTR`/`LEN`, scanline variables).
* Save on quit: Change game, Quit and `[del]` save to the Auto slot first
  (`AUTO_SLOT` 1000, suffix `AU`): `quit_saving` makes saveToSlot use it and
  create_savestate leave right after (`quit_game` for `[del]`, pause.asm for
  the menu). **The OS's archive calls leave the keypad scanning once**
  (mode 1: any key shows in every group, so alpha looked like [del]):
  a state load sets continuous scanning again, and so must anything that
  skips it (the quit path, `rom_menu`).
* Drawing: every change draws the whole screen while the panel keeps the
  last one (`spiLock`), then `finish` shows it (`ui_screen_full`, between two
  transfers). ~40 ms a screen; keys are polled while drawing too
  (`poll_keys`), so a short press isn't lost. Whole-line fills (`ui_fill`,
  320 bytes) are pushes from the end, interrupts off. Keep slow calls out of
  redraws (the battery level is read when a screen opens: it takes ~0.3 s).
* Menus use the whole 320x240 screen (`ui_screen_full`, 8 bpp, 320 bytes per
  line at `$D42000`). That frame buffer runs into `$D50000`, so the pause menu
  copies that memory into render_cache and restores it on closing; **the tile
  cache must be flushed afterwards** (`flush_bank_cache` in `open_menu`). The
  game's picture when the menu opened (`NOW`) follows that copy.
* Colors: `$00-$9F` the picture shown (its own colors, from the picture),
  `$A0-$C1` the menus' (`P_*` in menu.c; vars.inc has the ones the in-game
  labels and the FPS counter use).
* Text: two pixel fonts drawn for FamiCalc (`tools/fonts/make_fonts.py` ->
  `src/fonts.c`): body (10 rows, 1 pixel strokes, tabular digits) and
  display (14 rows, bold), proportional, special glyphs below 32 (arrows,
  theta, ellipsis, the D-pad). `ui_text` (ui.asm) draws one (`struct ui_font`;
  `fonts_init` sets the glyph offsets), with `ui_stride` bytes per line.
  Icons: 11x11 line icons (`tools/icons/make_icons.py` -> `src/icons.h`).
* In-game messages are drawn by `overlay_label` after each drawn frame (a
  12 line label, body font). The FPS counter isn't in the picture (see FPS
  counter).

## State pictures (pictures.c)

* A state keeps a picture of the game after its compressed data: the frame
  at half size (every other pixel of every other line: 120x112, the frame
  buffer's own values), its 160 palette entries, then the pixels in runs
  (0x00-0x9F a pixel, 0xA0-0xFF the last one again 2-97 times; decoded by
  `pic_draw_runs`, asm). The header's flag and 24 bit offset say where
  (`state_header_picture`); FamiCalc 0.3 loads these files too (it only
  reads the compressed data). 3-5 KB a picture.
* `saveToSlot` writes it: `pic_write` runs twice (counting, then writing in
  64 byte chunks), from the frame on screen (a hotkey save), or from the
  picture the pause menu took when it opened (`state_picture_next`, left in
  the normal frame buffer's last 13.8 KB: render_cache holds the state by
  then). The wide picture's buffer is in free RAM, which the new file takes
  from, so its picture is taken first (also into that spot).
* A big state may use the wide picture's buffer as room: the picture is
  drawn again after a save. So `WIDE_MIN_FREE` (8 KB) is only for the JIT's
  extra cache.

## Frame pacing (pacing.asm)

* Timer 1 runs freely at 48 MHz; times are in units of 256 cycles. Each NES
  frame has a deadline (3120 units = 60.096 Hz); the emulator waits when it is
  early.
* The LCD refreshes exactly once per NES frame (`lcdTiming`: HFP 13, VFP 135,
  798,720 cycles) and the deadlines are lined up with the start of the LCD's
  transfers (`lcd_ref`, re-measured from the VCOMP flag while waiting).
* `pace_draw_start`: if the frame will be drawn before the LCD's next transfer
  (estimated from the last drawing times, `draw_estimate`), draw it behind the
  LCD's reads (they are ~2.6x faster than drawing) without stopping the panel.
  Otherwise the panel is locked (`spiLock`) on the last frame until this one
  is done and unlocked between transfers (`lcd_unlock_if_sent`, also called at
  scanline events).
* Lock and unlock decisions use the LCD's read address (`mpLcdUpcurr` past the
  end of the frame buffer = between transfers), not the timer: the timer's
  idea of the LCD's phase was off by up to a transfer in the wide picture.
  The read address reaches the end ~100 pixels before the LCD's FIFO has sent
  them, so both wait ~1200 cycles first (else the last or first line tears).
* The panel must take a whole transfer between an unlock and the next lock,
  or it keeps showing an old frame (it froze for seconds after slow frames).
  If a frame can't be drawn without breaking that (unlocked in the same gap
  between transfers), it isn't drawn (`pace_aborted`): ppu_video_end skips
  the sprite bookkeeping and the drawing time for it.
* A frame is drawn when the emulator is no later than it can still draw before
  the next transfer; skipped frames catch up; at least every 4th is drawn.
* Before this, the frameskip heuristic let games run at up to 74 frames per
  second (178 in Final Fantasy's prologue) and drew at most 30.

## Row skipping (rowskip.asm)

Before each tile row of the background is drawn, `row_unchanged` compares its
place and scroll (line, lines, fine y, x start, nametable lengths, mask, coarse
y, nametable select) with the record of the row drawn at the same place (one
record per band of 8 lines, so a split that moves by a line only affects the
rows next to it) and, if those match, a hash of its tiles' cache entries.
Unchanged rows are not drawn again, except the tiles that sprites or labels
covered in the last drawn frame: `row_note_overlays` marks them (a byte per
tile per band; a sprite at x covers tiles x/8 - 1 to (x+7)/8, one more to the
left in the wide picture) and `row_partial` redraws those runs (`cpir` finds
them; it calls draw_tile per run, without using the stack).

The hash itself (~4,600 cycles a row) is skipped when nothing that changes
tiles happened since the last drawn frame (`rs_same`: no nametable write,
`nt_changed`, no CHR RAM change, no tile translated) and the row comes from
the same banks and nametables (a signature in the record).

`row_forget` (cache flushes, menu, state loads, blank frames) makes the next
frame draw everything. `row_skip_off` turns it off for tests.

## Wide picture (renderwide.asm, main.c set_wide)

* Pause menu, Picture: Wide. The game is drawn straight into a 320x224 frame
  buffer (`wide_buffer`, 320 bytes per line), 5 pixels for each 4: tiles are
  cached 10 pixels wide (pixels 3 and 7 twice, `w_fetch_tile`; the tile cache
  holds 576 of them instead of 720) and sprites are drawn 10 wide on the same
  grid (their x from `xtab`, lined up with the background's fine scroll).
* renderwide.asm has its own copies of the cursor RAM code (row loop, tile
  translation, sprite rows, the 64-byte draw_tile: a tile row is `ldir` of 10
  bytes, then +310 with `add hl,sp`); render_background, the event handling
  and row skipping are shared. Messages are drawn into the wide buffer.
* The panel shows columns 10-309 (partial mode; the NES's 8-247, like the
  normal picture). The LCD can't do more than 1024 pixels per line: 70 lines
  of 1024, same period; a transfer takes 1138 units instead of 910, a bit
  more than the panel waits before refreshing (left to right), so the
  bottom-left corner can show the previous frame for a moment.
* The buffer (70 KB) is reserved at the top of free RAM (`set_wide`), out of
  the JIT's extra cache; switching restarts the emulator from its state
  (`state_reload_only`) so the JIT forgets the memory it lost, and flushes the
  tile cache (different tile size). Needs `WIDE_SIZE + WIDE_MIN_FREE` free RAM.

## FPS counter (ui.asm fps_panel_*, pacing.asm fps_panel_try)

* It is outside the game's picture, in the panel's own memory where the LCD
  doesn't write: in the normal picture the black band left of it, in the
  wide one the strip above it (lines 0-7). The game's frame buffer never has
  it, so it costs no tile redraws.
* Normal picture: the LCD's window is columns 40-295 and the panel's
  scrolling area the same 256 columns (fixed 0-39 and 296-319), so the band
  doesn't move with the frame buffer's rotation; `spiScroll` sends 40 + S + 8
  (column 40 shows the frame buffer's 8 + S). Partial area 0-279 with the
  counter on, 40-279 off. The wide picture doesn't scroll the panel.
* Written over SPI (`panel_put`: window, `$2C`, 3 bytes per pixel = the
  panel's default 18 bits; a palette entry's bits 0-4, 5-9 with 15, 10-14)
  only while the panel doesn't take the LCD's data (`spiLock`, RM = 0), and
  the window is put back before it does. All of it in `ui_screen_game` (the
  menus leave the rest of the panel C_SHADOW); the numbers once a second
  (`fps_update` sets `fps_panel_dirty`) by `fps_panel_try`, while waiting
  for a frame's deadline or after a frame: at once if pacing has the panel
  locked, else locked just for that between two transfers, if the panel took
  a whole transfer since it was unlocked and the next one is at least
  `fps_panel_time` (800 units) away. The write takes ~450 units (~0.25% of
  the time); frames drawn per second are the same with and without it.
* Program size counts: the program is in RAM, and the wide picture needs
  `WIDE_SIZE + WIDE_MIN_FREE` free (see State pictures). The `spi` macro
  emits `call spi_inline` and the bytes as data (6 bytes less per parameter
  than `ld c`/`call`). The numbers are drawn in the display font in the
  normal picture's band, and in capitals in the wide one's 8 line strip.

## Pitfalls met here

* Menu loops: `for (bool draw = true;; draw = false)` resets `draw` on every
  `continue`, so a change set before `continue` never redraws. Clear it
  right after drawing instead.
* At the end of `load_state_from_buffer`, bc holds the NES's X and Y (and af'
  A, the alternate set S and P): anything added there must keep them. Losing
  them crashed Tetris after a load; `test_state_games` checks every game.
* The menus borrow render_cache (the tile cache): flush it afterwards.
* With frames lined up to the LCD, "unlock the panel when the LCD is between
  transfers" checks at event scanlines kept missing; `pace_unlock` runs at
  every frame end.
* Code in RAM is slow: an instruction costs ~10-20 cycles (4 per opcode byte
  fetched, 4 per byte read). The wide sprite set-up took ~5,000 cycles per
  sprite before it was cut down; count instructions on hot paths.
* A wait loop's timeout must outlast what it waits for: 256 polls of ~200
  cycles gave up halfway through a wide transfer and locked the panel early.
* tests/pc snapshots drawn frames at `pace_draw_end`, which aborted frames
  (pacing) don't reach.
* Don't walk the VAT with `os_NextSymEntry` the way `while ((entry =
  os_NextSymEntry(entry, ...)))` does: it left out the newest variable (a
  state just saved was missing from the lists and counts). `ti_Detect(&p,
  NULL)` lists every appvar.

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
  Save on quit and Continue (the Auto save, [del]), the game options, the
  hotkeys, Controls (a moved hotkey works), State pictures off,
  save and load in every game, old 0.2.1 states, the pause menu's pages
  (`menus`: screenshots `menu-*.png`, confirmations, deleting), the FPS counter
  (outside both pictures, never in the frame buffer), leaving a game, Resume, tearing (every
  screenshot shows rows of one recent frame), row skipping (frames with and
  without skipping are identical in scenes without input), rotation (the
  same; a game drawn every other frame can draw the other half without
  rotation, so that run may start a frame or two later), FPS of the
  benchmark scenes.
* Same-build runs are deterministic; between two builds, a button press can
  last a frame more or less (inputs are polled every 50 ms), so compare
  builds only in scenes without input.
* CEmu quirk: right after `spiSetup`'s panel reset, CEmu's LCD can stop
  producing frames for a while (it ties the LCD to the emulated panel). LCD
  waits time out after ~25 ms (`lcd_wait_frame`).

Benchmark (`fctest.py bench` and `bench-wide`, NES fps / drawn fps):

| scene | v0.2.1 | upstream 3202ea4 | this branch | this branch, wide |
|---|---|---|---|---|
| SMB3 1-1 | 64.5 / 29.2 | 71.9 / 29.1 | 59.7 / 56.3 | 59.2 / 45.1 |
| SMB3 map | 65.0 / 30.0 | 73.7 / 29.9 | 60.1 / 60.0 | 60.1 / 56.7 |
| Tetris | 60.0 / 30.0 | 74.8 / 29.9 | 60.1 / 60.1 | 60.1 / 60.1 |
| FF1 prologue | 178.7 / 40.2 | 71.8 / 35.9 | 60.1 / 60.1 | 60.1 / 60.1 |
| Zelda title | 68.2 / 29.9 | 68.2 / 30.0 | 60.1 / 52.2 | 60.1 / 30.1 |

(The SMB3 1-1 scene ends on the map: Mario runs into the first Goomba.)

Where the time goes while SMB3 1-1 scrolls: copying tile rows (`draw_tile`,
~7-10 cycles per pixel: RAM reads cost 4 cycles, writes 2) is a third of the
time and the emulated 6502 another third. A scrolling frame redraws every
row: ~450k cycles normal, ~720k wide, plus ~200k to emulate a frame, out of
800k; scrolling in the wide picture can't get much past ~48 drawn fps this
way. Zelda's title draws 44 sprites (~350k cycles: ~50 cycles a pixel).

## ROM files

TI-OS variables hold at most ~64 KB, so a game stays several appvars on the
calculator (`NAME`, `NAMEP0..`, `NAMEC0..`). Both converters (`convert.py` for
the web page, `tools/converter.pyw`) already write one `.b84` bundle per game
that TI Connect CE sends in one go (convbin can also make `.8xg` groups, which
TiLP reads). Ways to go further: let a PC tool convert and send a `.nes` in one
step, or let FamiCalc import a `.nes` from a USB stick (usbdrvce, msddrvce,
fatdrvce) into appvars itself.
