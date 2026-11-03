# FamiCalc (NES emulator for the TI-84 Plus CE) — rules for every agent

Read DEVNOTES.md for the design (memory map, pacing, row skipping, wide
picture, PC testing, pitfalls). Goal of the perf work: 60 drawn fps in every
game, normal and wide picture, without breaking a single PC test. 60 is the
baseline, not the finish line: the user prefers 120-capable headroom (frame
time well under half the LCD period), so a scene already at 60 is still worth
speeding up when the gain is real, and the slowest game is the metric.

## Build and test (all from the repo root)
- Toolchain: `export PATH=$HOME/ti84ce/toolchain/v14.2/CEdev/bin:$PATH; make`
  (v15.0 does not link). Output bin/FAMICALC.8xp + .map. Keep the program
  small: the user wants a low file size (60-64 KB now); RAM is tighter still
  (Wide needs WIDE_SIZE + WIDE_MIN_FREE free, code growth eats it).
- Unit tests: `make -B -C tests/unit`.
- PC harness (CEmu + real OS, deterministic): `make -C tests/pc` once, then
  `tools/perf/ptest.sh` (all 32 tests, 6 in parallel, ~3 min) or
  `python3 tests/pc/fctest.py NAME` for one. Screenshots land in tests/pc/out.
  Look at the PNGs of what you changed; a passing test is not proof the panel
  looks right.
- Benchmarks (NES fps / drawn fps, emulated time, independent of CPU load):
  `tools/perf/pbench.sh LABEL [wide] [fps] [OTHER.8xp]` (fps: the FPS
  counter on). Compare against the baseline table in DEVNOTES.md "PC
  testing" or a baseline build given as OTHER.8xp. Same build + same scene =
  same numbers; a change under 0.3 fps is noise only when inputs are
  involved (see DEVNOTES). Drawn is not shown: `tools/perf/pacetrace.py
  SCENE [wide] [fps] SECONDS` counts the frames the panel takes ("displayed").
- Where time goes: `tools/perf/bgtrace.py SCENE [wide]` (renderer phases from
  trace timestamps, trustworthy for copies), `tools/perf/prof2.py SCENE [wide]`
  (PC sampling, overcharges ldir), `python3 tests/pc/jitprof.py`,
  `tools/perf/counts.py SCENE sym...`. Set PROG=path.8xp for another build.
  Scenes: smb3-1-1 smb3-map tetris ff1-intro zelda-title smb3-run zelda-story zelda-items.
  Units are 256 cycles; a 60 fps frame is 3120 units (~800k cycles).

## Hard rules
- Never run tools/deploy.sh or ce-link: the calculator is shared with other
  sessions and only the main session deploys, on the user's explicit request.
- render_cache must stay 43,200 contiguous bytes (menus and saves reuse it);
  cursor RAM $E30800-$E30BBF is 960 bytes and nearly full; see DEVNOTES
  "Memory map" and "Pitfalls" before moving anything.
- Wide must stay 4:3 (300x224). No frameskip heuristics that run the NES
  faster than 60: the fix is drawing faster, not emulating less.
- Every change: build, run the PC tests that cover the area (at least
  rotation, rowskip, tearing, panel_frames, panel_frames_fps, panel_phases,
  wide ones for renderwide), bench the scenes it should speed up AND
  zelda-title + smb3-run (they regress easiest), normal, wide, and with the
  FPS counter on (`pbench.sh LABEL fps`, `LABEL wide fps`). Report the
  before/after table, not adjectives.
- Commit in the worktree you are in with a message that says what changed
  and the measured fps delta. Do not push.
