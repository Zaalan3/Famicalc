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
| `$D031F6-$D177B6` | C `.bss` and asm `.bss` (render_cache 45 KB, NES SRAM 32 KB, ...); free: `___heaptop - ___heapbot` in the map (75 bytes now) |
| `$D40000` | CHR RAM (8 KB) / render_banks |
| `$D42000-$D4FFFF` | the game's frame buffer: 256x224, 8 bpp, 256 bytes per line (unused by the wide picture) |
| `$D50000` page | NES RAM (MB-relative), scanline events, JIT cache (`$D50C00-$D5EC00`, 56 KB; a block's code runs past the end: 1 KB kept free), `render_palettes`, `jit_translation_buffer` and `wram_pages` (`$D5F000-$D5F840`; menus and saves don't write there, the UI's GC keeps them), RAM-block and border-block scratch |
| `$D60000-$D657FF` | tile set, OAM, palettes, scanline vars (ix), 4 nametables of 2 KB (`$D62000`; 2 and 3 are four-screen MMC3's, kept in states), buckets, call stack, event list |
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
  picture (`finish`: its average color; its 8 bands are palette entries, so
  the tint is set after drawing). The bands are a few 5 bit steps apart, so
  `ui_background` (ui.asm) fades each into the next over its 30 lines with a
  4x4 ordered dither. The neutral colors are multiples of 8 (whole 5 bit
  steps: in-between values came out tinted). The focus pill (`P_SURFACE_HI`,
  also the key caps) is lifted to 2 steps above band 1, the brightest
  behind a row, where its own color isn't (`tint`): on a bright picture's
  tint (SMB3, Zelda's title) it had no contrast left.
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
  each ([clear], [del] and the arrows can't be taken). `capture_key` lights
  the key held on the keypad drawing (the one it replaces in gray) and takes
  it when it is let go. The keypad drawing lights a key from its group and
  mask (`keypad`: rows 3-9 are the matrix itself; the arrows right of rows
  1-2).
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
  320 bytes) are pushes from the end, interrupts off, and so is the
  background (`ui_background`: a line's 4 dithered pixels in 4 registers).
  Keep slow calls out of redraws: the battery level takes ~150 ms
  (`read_battery`), the archive's free space ~27 ms (`scan_slots`).
* Opening the pause menu: [clear] is seen at the next NES frame's start
  (`set_save_slot`), then the copy of the emulator's memory (~3 ms), the
  game's picture (`pic_capture`, ui.asm, ~9 ms: it took 250 ms in C, 550
  wide), the first screen (~35 ms), the switch (`ui_screen_full`: the next
  LCD frame's end, then the panel takes ~16 ms). `open_screen` gives the
  LCD, while the panel is locked, the OS's timing with long lines
  (`ui_screen_lcd`: 16.9 ms frames, an 8 line porch), so the OS's own timing
  applies right after the frame `ui_screen_full` waits for and the panel
  takes the menu at once (~18 ms sooner than from the game's timing). Only
  then: the newest state and the free space (`scan_slots` opens every
  file; keys are polled at each), and the battery if the last reading is
  from another 4-minute block, once no key was down for half a second
  (`read_battery` takes ~150 ms without a poll: a tap in it was lost; the
  library reads it when it opens; the screen is drawn again with it). The whole menu is on the panel 65-90 ms after the key
  (emulated, normal and wide; `menu_latency` test); 2d637c7 took 536-545
  ms, wide 804-825, and lost keys pressed meanwhile.
* Resuming: on [clear] going down (only other keys held are waited for);
  `clear_held` (keys.asm) keeps it from opening the menu again until it is
  let go. `close_screen` sends one black frame (all palette entries black
  for a frame: the panel keeps what the game's window leaves out), clears
  the game's frame buffer with pushes and goes back to the game's window.
  The game is on the panel 96-132 ms after the key (`menu_latency`);
  2d637c7 took 188-224 ms, wide 376-544 (it waited for the key to be let
  go, filled the screen black, and restarted the wide emulator through a
  compressed state with 6 frames not drawn).
* Menus use the whole 320x240 screen (`ui_screen_full`, 8 bpp, 320 bytes per
  line at `$D42000`). That frame buffer runs into `$D50000`, so the pause menu
  copies that memory into render_cache and restores it on closing; **the tile
  cache must be flushed afterwards** (`flush_bank_cache` in `open_menu`). In
  the wide picture the pause menu draws over the wide buffer and the 5 KB
  of free RAM below it instead (`menu_fb`), so the JIT's extra cache (the
  normal frame buffer then) stays and the game resumes as in the normal
  picture; without that free RAM it draws at `$D42000` and the emulator
  restarts from its state (`MENU_RELOAD`). The
  game's picture when the menu opened (`NOW`) follows that copy (to byte
  33,216), and a dialog keeps the colors it dims at byte 40,000
  (`DIALOG_SAVED`): render_cache must stay at least ~40,400 bytes.
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
* `pic_capture` (ui.asm; the pause menu's and a hotkey save's) takes
  the same pixels as `frame_pixel` (pictures.c) at ~20 cycles a pixel: per
  row a run to the line's end, then the rest from the line's start (where
  it splits is worked out once); the wide picture's columns are 2, 3, 2, 3
  apart (an `inc hl` patched in). The `menu_picture` test compares it with
  the frame at several rotations.
* `saveToSlot` writes it: `pic_write` runs twice (counting, then writing in
  64 byte chunks), from the picture the pause menu took when it opened
  (`state_picture_next`, left in the normal frame buffer's last 13.8 KB:
  render_cache holds the state by then), or (a hotkey save) the frame on
  screen, taken first (the wide picture's buffer is in free RAM, which the
  new file takes from) into `$D50000`, where the uncompressed state was:
  `lz_decompress` writes it again after. A hotkey save took 593-791 ms in
  saveToSlot (`frame_pixel` in C, twice), now ~330 (`hotkey_picture` test:
  the stored picture is the frame's, byte for byte). On 2d637c7 a wide
  hotkey save's stored picture differed from the frame (1,604-1,804 pixels:
  the captured copy was right, its runs in the file were not; not found why).
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
* The panel must never take a frame half drawn: with hardware scrolling it
  shows the whole transfer at the new frame's rotation, so the rows still
  holding the last frame land shifted (SMB3's status bar jumped by the
  scroll for a frame or two; the map tore when its animation redrew many
  tiles). Drawing behind the LCD therefore needs the panel to have taken
  the last transfer (else the frame isn't drawn), and guards hold before
  every piece of the drawing: `pace_row_guard` (before each background row,
  from row_unchanged) waits while the LCD hasn't read the row yet (rows
  found unchanged can put the drawing ahead of it) and, from
  `2*guard_margin` before the LCD's next transfer, locks the panel on the
  last frame (`pace_late_lock`, which also sends that frame's rotation
  back); `pace_skip_guard` checks after each row found unchanged,
  `pace_mid_sprites` before the sprites drawn in the middle of the frame
  (SMB3's playfield sprites at its status bar: 300-500 units; it also waits
  for the LCD to read their lines) and `pace_sprite_guard` before the
  sprites, both with their time in the last drawn frame. A frame that fits
  while the panel is locked is drawn locked and unlocked after. (The
  margin: a row took up to 193 units, ff1-intro wide; with `guard_margin`
  alone a row started 160 units before the transfer ran 26 past it, and the
  unguarded middle sprites ran up to 450 past it when frames were drawn at
  other phases: tests/pc `panel_phases`.)
* An unlock just before the LCD's sync can let the panel take the new frame
  at the old rotation (wide SMB3's status bar moved by 10 pixels: unlocked 3
  units before the sync, the scroll sent 1 after it): `spiUnlock` sends the
  scroll before the unlock command, and `lcd_unlock_if_sent` doesn't unlock
  in the last `unlock_margin` (29) units before lcd_ref's next phase 0. The
  margin is what the unlock needs (panel-trace: RM is on 3.1-3.7 units
  after pace_do_unlock, 4.8 with a new scroll, whose VSP is in after 3.5);
  with it every RM and VSP change in zelda-title, smb3-run and wide
  smb3-1-1/smb3-run came 11.8 or more units before the panel's frame start
  (2d637c7: some after it). The phase is read after the FIFO wait, with the
  read address read again. 48 units cost zelda-title with the FPS counter
  on 56.8 -> 35.1 drawn fps (its frames end just before the sync).
* `lcd_ref` follows the LCD every frame (`lcd_track`, pace_draw_start: the
  read address says how long ago the transfer started, 63 bytes a unit).
  From the VCOMP flag alone it went stale by a transfer after a menu
  (zelda-story with the FPS counter on: 915 units off; the panel took no
  frame for seconds while 59 frames/s were "drawn").
* Waits for the LCD give up after ~2,300 units (`lcd_polls`, `row_polls`;
  tests/pc `lcd_timeouts`): longer than two wide transfers, and short enough
  that waiting for what doesn't come (a line past the frame, a stopped LCD)
  costs less than a frame. Polls read the address twice.
* Sprites are checked before they are drawn with their time in the last
  drawn frame, and every 8 while they are drawn (`pace_sprite_tick`, every
  4 in the wide picture): the first frame with many more sprites than the
  last one (tests/pc `panel_sprites`: 24 in one frame of 16, ~700 units)
  ran into the transfer with the panel unlocked. A sprite change at line 0
  (`s_botclip` = $FF) has no sprites above it: render_sprites draws none,
  pace_mid_sprites doesn't wait for line 248 (tests/pc `sprites_line0`).
* Scenes drawn up to the sync (zelda-title: each unit is ~4 frames shown in
  6 s) feel every cost: the checks after rows found unchanged are skipped
  while locked, spr_restore puts the bytes back without a check per row
  while locked (~700 rows) and reads the LCD's address only for a line past
  the last one it read.
* CEmu's panel scans a frame in ~3190 units (9.8 MHz), longer than the
  LCD's period, so it takes every other sync only (30 Hz); the LCD still
  writes every transfer into the panel's memory. The harness's `panel-idle`
  used to scan with a stale SPI time (the whole frame at once): the
  screenshot then showed the memory with a transfer the panel never scanned
  at the last scan's rotation, a torn or shifted picture that the panel never
  shows (padded builds "failed" panel_frames that way).
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

## WRAM code cache (jit.asm, codegen.asm)

Code in cartridge WRAM (`$6000-$7FFF`; Zelda runs much of its game there) is
cached like ROM code (bucket keys are physical pages). What the cache knows
per NES page is in `wram_pages` (vars.inc): bit 7 holds cached code (until
the game starts again), bit 6 a store to it was translated unchecked, bit 5
volatile, bits 0-2 invalidations. Games without WRAM start with every page
volatile.

* Stores: an absolute store to a page holding code is translated as a call
  to mapper_write (`wram_checked_store`), and so is NROM's indexed one when
  its page or the next holds code (SXROM/TXROM's indexed stores always are);
  other stores to WRAM are inlined unchecked (bit 6). Every mapper's WRAM
  branch calls `wram_write_check`: the first write to a code page after a
  flush counts an invalidation and sets `wram_dirty` (jit_search's threshold
  byte), so the next WRAM lookup goes to jit_add_block, which flushes.
* A page's first code flushes the cache if a store to it was inlined (bit
  6): that store would go unnoticed. A block marks its own page
  (jit_add_block) and, if it really runs into the next one (its last byte,
  at the end of jit_convert), that one too, then is translated again. Not
  from a start offset: a short block late in a page made the next page code,
  and data writes there flushed the cache and turned both pages volatile.
* In a block in WRAM, a store that may hit a page holding code (absolute:
  its page; indexed: its page and the next, unless from `$xx00`) ends the
  block (`wram_store_eob`, phase1), so the next instruction is looked up
  again and the bytes are translated as they are now: code that rewrites
  its own operands sees the new value from the first time. Stores through
  `(zp),Y` and `(zp,X)` end every block anyway.
* `WRAM_VOLATILE` (4): the 4th invalidation of a page makes it volatile:
  per instruction (as internal RAM) until the game starts again, so a page
  mixing data and code can't flush over and over. block_not_found sends a
  block per instruction if its page is volatile, or the next one and a block
  from there (64 bytes at most, offset `$C1` on) can reach it.
* A block in WRAM ends at `$7FFF` (block_not_found's length limit) and gets
  the WRAM footer (jit_branch_global, by its origin: one ending at `$8000`
  had the ROM one, which patched itself): what follows at `$8000` is looked
  up in the bank there now. A block running into the next WRAM page is
  translated again after that page is marked (its stores into its own tail
  end it then). STX/STY abs have `flags.abs` (mapper_test_bankswap sees
  their bank switches), and the mapper footer checks the block's own 8 KB
  slot (bits 6-5 of the address; bits 7-6 checked the `$C000` slot for code
  at `$8000-$BFFF`). MMC1 WRAM writes go to the selected WRAM bank, through
  the address table as reads do.
* By design: a page that turns volatile has cached blocks, so it sets
  `wram_dirty` like any invalidation; WRAM lookups then miss until the next
  one of a page that isn't volatile flushes the cache (the volatile page's
  blocks must go, and buckets have no per-page removal): one flush per page
  and game start, after its 4th invalidation; no bench scene turns a page
  volatile. A game that legitimately reloads code in a page more than 4
  times runs that page per instruction afterwards (slow, not wrong). An
  instruction across `$7FFF`/`$8000` takes its operand from the bank there
  when it was translated.
* A flush in jit_search resets the call stack: callers must take what they
  need from it first (jit_call's path for calls from RAM code pops the
  block's return address before jit_search).
* Tests: `wram_code` (tools/mkwramrom.py builds the test ROMs: MMC1, NROM,
  MMC3, and the page border with flush and per-instruction counts) and
  `jit_mappers` (WBNK: MMC1 WRAM banks; FOUR: four-screen nametables and
  states; JITM: `$7FFF`/`$8000`, STX/STY bank switches under the code;
  JITF: flushes in jit_call_local and jit_branch_local_bankswap with a 1 KB
  cache patched in, the stack checked at every block).

## Wide picture (renderwide.asm, main.c set_wide)

* Pause menu, Picture: Wide. The game is drawn straight into a 320x224 frame
  buffer (`wide_buffer`, 320 bytes per line), 5 pixels for each 4: tiles are
  cached 10 pixels wide (pixels 3 and 7 twice, `w_fetch_tile`; the tile cache
  holds 400 of them instead of 500) and sprites are drawn 10 wide on the same
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
  tile cache (different tile size). That restart skips the compression (the
  state is in place at `$D50000`: ~110 ms) and the 6 frames a load leaves
  undrawn. Needs `WIDE_SIZE + WIDE_MIN_FREE` free RAM.
* Hardware scrolling, as in the normal picture (Row skipping), when the FPS
  counter is off (its strip above the picture would scroll too:
  `rs_rotating`): the panel's scrolling area is all 320 columns and the
  frame is rotated by S_w = `xtab[S]` wide columns (`fb_scroll_w`, sent as
  `scroll_want_w`), so tile column c of a row at the frame's scroll is at
  column 10c (mod 320): those rows are drawn in compare mode (`w_run_tiles`,
  slots of 10 columns). Rows at another scroll (status bars) are drawn in up
  to 3 pieces (`w_rot_row`: where the right nametable starts and where the
  lines end); the tile across the end of the lines writes its last k columns
  on the next lines, moved up after (`w_lines_up`). A sprite across the end
  (column 311 on) has the background of those k columns moved down a line
  first and back up after (`w_across`), so its last columns land on its own
  lines; its saved background is copied in two parts too.
* Sprites keep the background under them as in the normal picture
  (`w_sprite_save`: their rows of 10 bytes, `w_spr_restore`), and their rows
  are clipped to [top, top + range) like the normal picture's. In-game labels
  (ui.asm `label_blit`) and state pictures (pictures.c) follow the rotation.

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
  at the end of a frame's drawing (pace_frame_drawn) or of the frame: if
  pacing has the panel locked and the write is done before the unlock's last
  moment, else locked just for it between two transfers, if the panel took
  a whole transfer since it was unlocked and the write is done by then too.
  It takes 986-1,057 units in the normal picture and 565-616 in the wide one
  (`fps_panel_time`, `fps_panel_time_wide`). One that ran past the LCD's
  sync kept the panel locked there with the next frame's scroll already
  sent (SMB3's status bar 6 px off; tests/pc `panel_frames_fps`). Numbers
  that found no such moment for 40 frames (`fps_wait`: scenes drawn locked
  up to the sync, Tetris or wide SMB3's map, went seconds without) are
  written at the next one anyway and the panel left locked: a frame reaches
  it a transfer later (tests/pc `fps_display` counts the writes each
  second). After a write the panel is unlocked only while the LCD is
  between transfers (`lcd_unlock_if_sent`), in time or forced, else pacing
  unlocks it after that transfer: a frame shown a refresh later, not a
  shifted one (see Pitfalls; tests/pc `panel_tetris`). The VCOMP flag is
  cleared after a write, so lcd_sync doesn't measure a transfer end seen
  that late.
* Program size counts: the program is in RAM, and the wide picture needs
  `WIDE_SIZE + WIDE_MIN_FREE` free (see State pictures). The `spi` macro
  emits `call spi_inline` and the bytes as data (6 bytes less per parameter
  than `ld c`/`call`). Its own font (`font_hud`, menu.c; panel_text's font
  1): bold tabular digits on 11 lines in the normal picture's band, FPS and
  DRAWN in small caps (lines 0-4) under them or, in the wide picture's 8
  line strip, beside the body font's digits.

## Pitfalls met here

* `ldir` moves all 24 bits of de: a tile line ending at column 255 of the
  rotated frame buffer (slot 31, or a tile across the end of the lines)
  carries into d, and on line $FF into de's upper byte. `draw_tile` takes
  each line's d from a and reloads de from iy for each tile; with `inc d`
  slot 31's tiles landed on every other line, leaving garbage columns that
  scrolled with the level (the tests only compared columns 8-247, which the
  unrotated picture draws).
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
* The LCD's read address (`mpLcdUpcurr`) is read a byte at a time while
  the LCD's DMA moves it: across `$xxFFFF` one read took the upper byte after
  the carry and the others before, 64 KB too far (past the frame buffer's
  end: "between transfers"), and pacing locked the panel 22 lines into a
  transfer (wide Zelda's HUD 30 pixels off). Every read goes through
  `lcd_read_address` (two reads less than 256 bytes apart; tests/pc
  `lcd_reads` looks for any other). tests/pc `panel-trace` logs the panel's
  scans (their rotation) and RM/VSP changes for such cases.
* A wait loop's timeout must outlast what it waits for: 256 polls of ~200
  cycles gave up halfway through a wide transfer and locked the panel early.
* Unlocking the panel (RAMCTRL RM 0 -> 1) restarts its write address at the
  window's first pixel. Done while the LCD sends a frame, the rest of that
  frame lands from the top-left corner: the panel shows the picture shifted
  and wrapped until the next transfer (Tetris "flashed" when a piece was
  placed, with the FPS counter on: its write, then 800 units estimated for
  ~1,000, ran into the next transfer after late frames and unlocked in it).
  Unlock only between transfers.
* tests/pc snapshots drawn frames at `pace_draw_end`, which aborted frames
  (pacing) don't reach. Comparing those (rotation, rowskip) can't see what
  the panel shows: a frame half drawn or shown at the wrong rotation needs
  screenshots (`panel_frames`). Take them with `panel-idle` first: the
  panel refreshes column by column, and a screenshot in the middle of a
  refresh shows two of them side by side.
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
  (execution counters), `panel-idle` (run until the panel is between two
  refreshes), `panel-trace` (run in small steps, logging the panel's scan
  starts with their rotation and its VSP/RM changes), `profile` (program
  counter sampling, labelled with the
  map), `snap`/`snaps-all` (copy memory each time an address runs),
  `watch-write`/`watch-hits` (the program counters of writes to some bytes),
  `vars`, `receive-file`.
* Tests: boot, save/load (slots 12/345/999 restore NES RAM byte for byte),
  Save on quit and Continue (the Auto save, [del]), the game options, the
  hotkeys, Controls (a moved hotkey works), State pictures off,
  save and load in every game, old 0.2.1 states, the pause menu's pages
  (`menus`: screenshots `menu-*.png`, confirmations, deleting), the FPS counter
  (outside both pictures, never in the frame buffer), leaving a game, Resume,
  how soon the pause menu is up (`menu_latency`: screenshots every emulated
  ms from [clear] until the whole menu is on the panel, at most 100 ms, in
  4 games, normal and wide; `tools/perf/menulat.py` prints the same with
  the first call of each traced step, `menuprof.py` samples it; and back to
  the game within 150 ms), the menu's picture of the game (`menu_picture`),
  a hotkey save's picture (`hotkey_picture`), tearing (every
  screenshot shows rows of one recent frame), row skipping (frames with and
  without skipping are identical in scenes without input), rotation (the
  same; a game drawn every other frame can draw the other half without
  rotation, so that run may start a frame or two later), rotation while
  SMB3 1-1 scrolls (`rotation_scroll`: every frame buffer slot is drawn
  somewhere on the screen), what the panel shows (`panel_frames`: SMB3 1-1
  running right, flying as a raccoon (the camera moves up too) and the map,
  normal and wide: every screenshot is one whole recent frame, row by row,
  so the status bar and the playfield come from the same NES frame, and the
  status bar is where the first screenshot has it, not moved sideways or up;
  `panel_frames_fps`: the same with the FPS counter on; `panel_phases`: the
  same with frames drawn at other phases of the LCD; `panel_zelda`: Zelda's
  HUD stays put while the next screen scrolls in; `panel_tetris`: Tetris
  with the FPS counter on, pieces placed, frames at every phase of the LCD:
  no shifted or torn picture; `panel_sprites`: sprites much heavier than in
  the last frame never run into a transfer unlocked; `sprites_line0`), the
  LCD's waits (`lcd_timeouts`, `lcd_reads`), the FPS
  counter's writes (`fps_display`: every second, normal and wide), code in
  WRAM and the JIT's mapper cases (`wram_code`, `jit_mappers`, see WRAM code
  cache), FPS of the benchmark scenes.
* Same-build runs are deterministic; between two builds, a button press can
  last a frame more or less (inputs are polled every 50 ms), so compare
  builds only in scenes without input.
* `menu()` presses [clear] and waits for the menu's first screen
  (`ui_screen_full` runs), not a fixed time: ~90 ms, longer when a wide
  menu has to restart the emulator from a state first. With a fixed wait,
  the keys of a second menu() after Picture once went to the menu's first
  page (the "wide fps" benches loaded slot 1 with the FPS counter off);
  bench() and the panel tests check the picture and the counter are as
  asked.
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
